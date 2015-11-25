# dhess-ssh-keygen

`dhess-ssh-keygen` is a wrapper around `ssh-keygen`. It does the
following:

- Generates an SSH v2 key using the best available modern ciphers
  (currently only Ed25519).

- Requires a comment for the generated public key file.
  `dhess-ssh-keygen` automatically adds the UTC date on which the key
  was generated to the user-provided comment text.

- Generates a name for the generated public and private key files,
  consisting of the user-provided user ID, the cipher used, and the
  UTC date on which the key was generated. (The date is useful for
  versioning.)

- Tells `ssh-keygen` to encrypt the generated private key file using
  the new OpenSSH key format, for increased resistance to brute-force
  passphrase cracking.

- Generates a random passphrase with 128 bits of entropy. (For
  compatibility with text-based programs, the generated random bytes
  are Base64-encoded to produce the actual passphrase.)

- Passes the passphrase to `ssh-keygen` at key creation time using an
  `expect` script, so that the passphrase can be read by `ssh-keygen`
  directly from `/dev/tty`, exactly as `ssh-keygen` does when
  prompting the user for a passphrase. The passphrase is passed to the
  `expect` script via `stdin`; therefore, the passphrase is never
  passed to any command via the command line.

- GPG-encrypts the passphrase using the user's default GnuPG key, and
  stores the resulting encrypted file along with the new private and
  public key files in the user's `~/.ssh` directory. (As with the
  `expect` script described above, the passphrase is passed to `gpg`
  via stdin, and never via the command line.)

## Requirements

`dhess-ssh-keygen` requires the the following:

- OpenSSH v6.5 or later.

- `gpg`, somewhere in the user's shell `PATH`.

Note that `dhess-ssh-keygen` has only been tested on OS X 10.11.

## Usage

General:

    dhess-ssh-keygen

    Usage: dhess-ssh-keygen COMMAND
    An ssh-keygen helper

    Available options:
    -h,--help                Show this help text

    Available commands:
    ed25519                  Generate a new ed25519 key

`ed25519` mode:

    Usage: dhess-ssh-keygen ed25519 [-c|--clobber] USERID "COMMENT"
      Generate a new ed25519 key

    Available options:
      -c,--clobber             Clobber an existing key with the same name
      -h,--help                Show this help text
