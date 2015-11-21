{-# LANGUAGE OverloadedStrings #-}


import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative hiding (command)
import qualified Options.Applicative as Options (command)
import Turtle
import Prelude hiding (FilePath)

data Verbosity = Normal | Verbose

data GlobalOptions =
  GlobalOptions { verbose :: Verbosity
                , cmd :: Command }

type SUserId = String

type SComment = String

data Command
  = Ed25519 SUserId SComment

ed25519 :: Parser Command
ed25519 = Ed25519
  <$> (argument str (metavar "USERID"))
  <*> (argument str (metavar "\"COMMENT\""))

keyGen :: Parser Command
keyGen =
  hsubparser
    (Options.command "ed25519" (info ed25519 (progDesc "Generate a new ed25519 key")))

runCmd :: Command -> IO ()
runCmd (Ed25519 userid comment) = genEd25519 (UserId $ T.pack userid) (Comment $ T.pack comment)

main :: IO ()
main = execParser opts >>= runCmd
  where opts =
          info (helper <*> keyGen)
               (fullDesc
                  <> progDesc "An ssh-keygen helper"
                  <> header "dhess-ssh-keygen")

newtype UserId = UserId { userId :: Text }

newtype Comment = Comment { comment :: Text }

genEd25519 :: UserId -> Comment -> IO ()
genEd25519 u c = sh $
  do echo $ userId u
     echo $ comment c

