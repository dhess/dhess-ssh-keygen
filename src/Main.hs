{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Options.Applicative hiding (command)
import qualified Options.Applicative as Options (command)
import KeyGen (UserId(..), Comment(..), KeyType(..), generate)

data Verbosity = Normal | Verbose

data GlobalOptions =
  GlobalOptions { verbose :: Verbosity
                , cmd :: Command }

type SUserId = String

type SComment = String

data Command
  = Ed25519Cmd SUserId SComment

ed25519 :: Parser Command
ed25519 = Ed25519Cmd
  <$> (argument str (metavar "USERID"))
  <*> (argument str (metavar "\"COMMENT\""))

keyGen :: Parser Command
keyGen =
  hsubparser
    (Options.command "ed25519" (info ed25519 (progDesc "Generate a new ed25519 key")))

runCmd :: Command -> IO ()
runCmd (Ed25519Cmd userid comment) =
  generate Ed25519 (UserId $ T.pack userid) (Comment $ T.pack comment)

main :: IO ()
main = execParser opts >>= runCmd
  where opts =
          info (helper <*> keyGen)
               (fullDesc
                  <> progDesc "An ssh-keygen helper"
                  <> header "dhess-ssh-keygen")
