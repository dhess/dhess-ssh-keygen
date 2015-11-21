{-# LANGUAGE OverloadedStrings #-}

module KeyGen
       ( Comment(..)
       , KeyType(..)
       , TimeStamp(..)
       , UserId(..)
       , generate
       , keyFilePath
       , keyTypeToText
       , sshDir
       , today
       ) where

import Data.Text (Text)
import Data.Time
import Data.Time.Clock (getCurrentTime)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Turtle

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

sshDir :: MonadIO m => m FilePath
sshDir =
  do hd <- home
     return $ hd </> ".ssh"

today :: MonadIO m => m TimeStamp
today =
  do now <- liftIO $ getCurrentTime
     return $ TimeStamp (T.pack $ formatTime defaultTimeLocale "%Y%m%d" now)

keyFilePath :: MonadIO m => UserId -> TimeStamp -> KeyType -> m FilePath
keyFilePath u ts kt =
  do sshdir <- sshDir
     return $ sshdir </> fromText (userId u <> "_id_" <> keyTypeToText kt <> "_" <> timeStamp ts)

generate :: KeyType -> UserId -> Comment -> IO ()
generate k u c = sh $
  do yyyymmdd <- today
     keyName <- keyFilePath u yyyymmdd k
     echo $ either id id (toText keyName)
     echo (comment $ commentWithTimeStamp c yyyymmdd)

