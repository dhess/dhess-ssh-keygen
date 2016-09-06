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

import Control.Monad (void)
import qualified Data.ByteString as B (length)
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Data.Time.Clock (getCurrentTime)
import qualified Filesystem.Path.CurrentOS as FP (decodeString, encodeString)
import Options.Applicative
import Prelude hiding (FilePath)
import Shelly
import System.Directory (getAppUserDataDirectory)
import System.Entropy (getEntropy)
import System.Posix.Files (ownerModes, ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.Types (FileMode)

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

gpgFileName :: FilePath -> FilePath
gpgFileName priv = priv <.> "gpg"

sshDirectory :: Sh FilePath
sshDirectory = liftIO (getAppUserDataDirectory "ssh") >>= return . FP.decodeString

ownerRW :: FileMode
ownerRW = unionFileModes ownerReadMode ownerWriteMode

generatePassphrase :: Int -> Sh Text
generatePassphrase entropyBytes =
  do randomBytes <- liftIO $ getEntropy entropyBytes
     -- Sanity check
     when (B.length randomBytes /= entropyBytes) $
       errorExit "generatePassphrase: getEntropy returned fewer bytes than expected!"

     return $ decodeUtf8 (B64.encode randomBytes)

generateEd25519 :: FilePath -> Options -> UserId -> Comment -> Sh ()
generateEd25519 = generate Ed25519

generate :: KeyType -> FilePath -> Options -> UserId -> Comment -> Sh ()
generate k gpg o u c =
  do yyyymmdd <- today
     sshDir <- sshDirectory
     let kfn = keyFileName u yyyymmdd k
         keyFile = sshDir </> kfn
         pubKeyFile = pubKeyFileName keyFile
         gpgFile = gpgFileName keyFile

     -- To handle the "clobber" case, we create the new key in a
     -- temporary directory, then move it to the user's .ssh directory
     -- (overwriting the old key in the process).

     exists <- test_f keyFile
     when (exists && not (clobber o)) $
       errorExit ("An SSH key named "
                 <> toTextIgnore kfn
                 <> " already exists, refusing to overwrite it.")

     passphrase <- generatePassphrase 32 -- 32*4 == 128 bits of entropy

     -- ssh-keygen reads the passphrase directly from /dev/tty and
     -- ignores stdin. Therefore, we write an expect script to the
     -- temporary directory which reads the passphrase from stdin, and
     -- then sends it to ssh-keygen.
     withTmpDir $ \tmpDir ->
       let tmpKeyFile = tmpDir </> kfn
           tmpPubKeyFile = pubKeyFileName tmpKeyFile
           tmpGpgFile = gpgFileName tmpKeyFile
           script = tmpDir </> "expect-script"
       in
         do writefile script expectProgram
            makeExecutable script
            -- Add a newline for expect_user to match in the script
            setStdin $ passphrase <> "\n"
            void $ cmd script (tshow k) tmpKeyFile (comment $ commentWithTimeStamp c yyyymmdd)

            -- Now encrypt the passphrase and set secure permissions on the file.
            setStdin passphrase
            void $ cmd gpg "--encrypt" "--default-recipient-self" "--output" tmpGpgFile
            liftIO $ setFileMode (FP.encodeString tmpGpgFile) ownerRW

            mv tmpKeyFile keyFile
            mv tmpPubKeyFile pubKeyFile
            mv tmpGpgFile gpgFile

     echo ""
     echo $ "Created new SSH key " <> toTextIgnore keyFile
     echo $ "Passphrase is encrypted in " <> toTextIgnore gpgFile

makeExecutable :: FilePath -> Sh ()
makeExecutable fn = liftIO $ setFileMode (FP.encodeString fn) ownerModes

expectProgram :: Text
expectProgram =
  "#!/usr/bin/expect\n\
   \\n\
   \if { $argc != 3 } {\n\
   \    puts \"usage: $argv0 key_type key_filename comment\\r\\r\"\n\
   \    puts \"Note: silently waits for passphrase when run.\\r\"\n\
   \    exit 1\n\
   \}\n\
   \\n\
   \set key_type [lindex $argv 0]\n\
   \set key_filename [lindex $argv 1]\n\
   \set comment [lindex $argv 2]\n\
   \\n\
   \stty -echo\n\
   \expect_user -re \"(.*)\\n\"\n\
   \set passphrase \"$expect_out(1,string)\\r\"\n\
   \stty echo\n\
   \\n\
   \set timeout 10\n\
   \spawn ssh-keygen -t \"$key_type\" -f \"$key_filename\" -C \"$comment\"\n\
   \expect {\n\
   \    timeout { send_user \"\\nTimed out waiting for passphrase prompt.\\n\"; exit 1 }\n\
   \    \"Enter passphrase (empty for no passphrase): \"\n\
   \}  \n\
   \send $passphrase\n\
   \expect {\n\
   \    timeout { send_user \"\\nTimed out waiting for passphrase confirmation.\\n\"; exit 1 }\n\
   \    \"Enter same passphrase again: \"\n\
   \}\n\
   \send $passphrase\n\
   \expect\n"
