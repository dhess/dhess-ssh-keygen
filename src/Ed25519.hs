{-# LANGUAGE OverloadedStrings #-}

module Ed25519
       ( UserId(..)
       , Comment(..)
       , generate
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Turtle

newtype UserId = UserId { userId :: Text }

newtype Comment = Comment { comment :: Text }

generate :: UserId -> Comment -> IO ()
generate u c = sh $
  do echo $ userId u
     echo $ comment c
