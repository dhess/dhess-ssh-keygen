{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module KeyGen
       ( Comment(..)
       , KeyType(..)
       , Options(..)
       , TimeStamp(..)
       , UserId(..)
       , generate
       , generateEd25519
       , optionsParser
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Options.Applicative
import Prelude hiding (FilePath)
import qualified Prelude as Prelude (FilePath)
import Shelly
import System.Directory (getAppUserDataDirectory)
import Filesystem.Path.CurrentOS (decodeString)

default (T.Text)

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

newtype TimeStamp = TimeStamp { timeStamp :: Text }

tshow :: (Show a) => a -> Text
tshow = T.pack . show

commentWithTimeStamp :: Comment -> TimeStamp -> Comment
commentWithTimeStamp c ts = Comment (comment c <> " (" <> timeStamp ts <> ")")

today :: Sh TimeStamp
today =
  do now <- liftIO getCurrentTime
     return $ TimeStamp (T.pack $ formatTime defaultTimeLocale "%Y%m%d" now)

keyFileName :: UserId -> TimeStamp -> KeyType -> FilePath
keyFileName u ts kt = fromText (userId u <> "_id_" <> tshow kt <> "_" <> timeStamp ts)

pubKeyFileName :: FilePath -> FilePath
pubKeyFileName priv = priv <.> "pub"

generateEd25519 :: Options -> UserId -> Comment -> Sh ()
generateEd25519 = generate Ed25519

fpToFp :: Prelude.FilePath -> FilePath
fpToFp = decodeString

sshDirectory :: Sh FilePath
sshDirectory = liftIO (getAppUserDataDirectory "ssh") >>= return . fpToFp

generate :: KeyType -> Options -> UserId -> Comment -> Sh ()
generate k o u c =
  do yyyymmdd <- today
     sshDir <- sshDirectory
     let kfn = keyFileName u yyyymmdd k
         keyFile = sshDir </> kfn
         pubKeyFile = pubKeyFileName keyFile

     -- To handle the "clobber" case, we create the new key in a
     -- temporary directory, then move it to the user's .ssh directory
     -- (overwriting the old key in the process).

     exists <- test_f keyFile
     when (exists && not (clobber o)) $
       errorExit ("An SSH key named "
                 <> toTextIgnore kfn
                 <> " already exists, refusing to overwrite it.")

     withTmpDir $ \tmpDir ->
       let tmpKeyFile = tmpDir </> kfn
           tmpPubKeyFile = pubKeyFileName tmpKeyFile
       in
         do cmd "ssh-keygen" "-t" (tshow k) "-f" tmpKeyFile "-C" (comment $ commentWithTimeStamp c yyyymmdd)
            mv tmpKeyFile keyFile
            mv tmpPubKeyFile pubKeyFile

     echo ""
     echo $ "Created new SSH key " <> toTextIgnore keyFile
