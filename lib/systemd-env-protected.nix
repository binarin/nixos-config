# Shared list of environment variable names that should never be
# overwritten when syncing from `systemctl show-environment --user`.
#
# Entries ending in "_" are prefix-matched (e.g. "ATUIN_" blocks
# ATUIN_HISTORY_ID, ATUIN_SESSION, ATUIN_PREEXEC_BACKEND, etc.).
[
  "PATH"
  "SHELL"
  "HOME"
  "USER"
  "LOGNAME"
  "SSH_AUTH_SOCK"
  "SSH_CLIENT"
  "SSH_CONNECTION"
  "SSH_TTY"
  "_"
  "ATUIN_"
  "STARSHIP_"
]
