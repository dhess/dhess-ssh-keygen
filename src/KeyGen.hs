{-# LANGUAGE OverloadedStrings #-}

module KeyGen
       ( Comment(..)
       , KeyType(..)
       , Options(..)
       , TimeStamp(..)
       , UserId(..)
       , generate
       , generateEd25519
       , keyFileName
       , keyTypeToText
       , optionsParser
       , sshdir
       , today
       ) where

import qualified Control.Foldl as Fold
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock (getCurrentTime)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString, encode)
import Options.Applicative
import Prelude hiding (FilePath)
import qualified System.Directory as Directory (getTemporaryDirectory)
import Turtle

data Options =
  Options { clobber :: Bool }

optionsParser :: Parser Options
optionsParser = Options
  <$> flag False True
        (  long "clobber"
        <> short 'c'
        <> help "Clobber an existing key with the same name"
        )

newtype UserId = UserId { userId :: Text }

newtype Comment = Comment { comment :: Text }

data KeyType = Ed25519 deriving (Eq)

instance Show KeyType where
  show Ed25519 = "ed25519"

keyTypeToText :: KeyType -> Text
keyTypeToText = T.pack . show

newtype TimeStamp = TimeStamp { timeStamp :: Text }

commentWithTimeStamp :: Comment -> TimeStamp -> Comment
commentWithTimeStamp c ts = Comment (comment c <> " (" <> timeStamp ts <> ")")

sshdir :: MonadIO m => m FilePath
sshdir =
  do hd <- home
     return $ hd </> ".ssh"

tmpdir :: MonadIO m => m FilePath
tmpdir =
  do dir <- liftIO Directory.getTemporaryDirectory
     return $ Filesystem.decodeString dir

today :: MonadIO m => m TimeStamp
today =
  do now <- liftIO $ getCurrentTime
     return $ TimeStamp (T.pack $ formatTime defaultTimeLocale "%Y%m%d" now)

keyFileName :: UserId -> TimeStamp -> KeyType -> FilePath
keyFileName u ts kt = fromText (userId u <> "_id_" <> keyTypeToText kt <> "_" <> timeStamp ts)

fileNameToText :: FilePath -> Text
fileNameToText path =
  case toText path of
    Left t -> t <> " (note: invalid encoding)"
    Right t -> t

generateEd25519 :: Options -> UserId -> Comment -> Shell Text
generateEd25519 = generate Ed25519

generate :: KeyType -> Options -> UserId -> Comment -> Shell Text
generate k o u c =
  do yyyymmdd <- today
     sshDir <- sshdir
     let kfn = keyFileName u yyyymmdd k

     -- To handle the "clobber" case, we create the new key in a
     -- temporary directory, then move it to the user's .ssh directory
     -- (overwriting the old key in the process).

     exists <- testfile $ sshDir </> kfn
     case (exists && (not $ clobber o)) of
       True -> die ("An SSH key named "
                   <> fileNameToText kfn
                   <> " already exists, refusing to overwrite it.")
       False ->
         do tmpDirBase <- tmpdir
            tmpDir <- using (mktempdir tmpDirBase "ssh-keygen")
            let tmpKeyFile = tmpDir </> kfn

            inproc "ssh-keygen" ["-t", keyTypeToText k, "-f", Filesystem.encode tmpKeyFile, "-C", (comment $ commentWithTimeStamp c yyyymmdd)] empty
