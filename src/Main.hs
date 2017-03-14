{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import qualified Data.Text as T
import KeyGen (Comment(..), Options(..), UserId(..),  generateEd25519, optionsParser)
import Options.Applicative hiding (command)
import qualified Options.Applicative as Options (command)
import Prelude hiding (FilePath)
import Shelly (fromText, shelly)

data GlobalOptions =
  GlobalOptions { gpgPath :: !T.Text
                , cmd :: !Command }

type SUserId = String

type SComment = String

data Command
  = Ed25519Cmd Options !SUserId !SComment

ed25519 :: Parser Command
ed25519 = Ed25519Cmd
  <$> optionsParser
  <*> argument str (metavar "USERID")
  <*> argument str (metavar "\"COMMENT\"")

keyGen :: Parser GlobalOptions
keyGen =
  GlobalOptions <$>
    option auto (long "gpg-path" <>
                 short 'g' <>
                 metavar "GPGPATH" <>
                 value "gpg2" <>
                 showDefault <>
                 help "Specify the path to GPG") <*>
  hsubparser
    (Options.command "ed25519" (info ed25519 (progDesc "Generate a new ed25519 key")))

runCmd :: GlobalOptions -> IO ()
runCmd (GlobalOptions gpg (Ed25519Cmd options userid cmt)) =
  shelly $ generateEd25519 (fromText gpg) options (UserId $ T.pack userid) (Comment $ T.pack cmt)

main :: IO ()
main = execParser opts >>= runCmd
  where opts =
          info (helper <*> keyGen)
               (fullDesc
                  <> progDesc "An ssh-keygen helper"
                  <> header "dhess-ssh-keygen")
