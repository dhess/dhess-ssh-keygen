{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Ed25519 as Ed25519 (UserId(..), Comment(..), generate)
import Options.Applicative hiding (command)
import qualified Options.Applicative as Options (command)

data Verbosity = Normal | Verbose

data GlobalOptions =
  GlobalOptions { verbose :: Verbosity
                , cmd :: Command }

type UserId = String

type Comment = String

data Command
  = Ed25519 UserId Comment

ed25519 :: Parser Command
ed25519 = Ed25519
  <$> (argument str (metavar "USERID"))
  <*> (argument str (metavar "\"COMMENT\""))

keyGen :: Parser Command
keyGen =
  hsubparser
    (Options.command "ed25519" (info ed25519 (progDesc "Generate a new ed25519 key")))

runCmd :: Command -> IO ()
runCmd (Ed25519 userid comment) =
  Ed25519.generate (Ed25519.UserId $ T.pack userid) (Ed25519.Comment $ T.pack comment)

main :: IO ()
main = execParser opts >>= runCmd
  where opts =
          info (helper <*> keyGen)
               (fullDesc
                  <> progDesc "An ssh-keygen helper"
                  <> header "dhess-ssh-keygen")
