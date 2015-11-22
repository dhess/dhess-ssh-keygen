{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Options.Applicative hiding (command)
import qualified Options.Applicative as Options (command)
import KeyGen (Comment(..), KeyType(..), Options(..), UserId(..),  generateEd25519, optionsParser)

data Verbosity = Normal | Verbose

data GlobalOptions =
  GlobalOptions { verbose :: Verbosity
                , cmd :: Command }

type SUserId = String

type SComment = String

data Command
  = Ed25519Cmd Options SUserId SComment

ed25519 :: Parser Command
ed25519 = Ed25519Cmd
  <$> optionsParser
  <*> (argument str (metavar "USERID"))
  <*> (argument str (metavar "\"COMMENT\""))

keyGen :: Parser Command
keyGen =
  hsubparser
    (Options.command "ed25519" (info ed25519 (progDesc "Generate a new ed25519 key")))

runCmd :: Command -> IO ()
runCmd (Ed25519Cmd options userid comment) =
  generateEd25519 options (UserId $ T.pack userid) (Comment $ T.pack comment)

main :: IO ()
main = execParser opts >>= runCmd
  where opts =
          info (helper <*> keyGen)
               (fullDesc
                  <> progDesc "An ssh-keygen helper"
                  <> header "dhess-ssh-keygen")
