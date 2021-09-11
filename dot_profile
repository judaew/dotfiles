# shellcheck shell=bash

# --- paths
# unix paths
PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:${PATH}"

MAC_GPG2_PATH="/usr/local/MacGPG2/bin"
MACPORTS_PATH="/opt/local/bin:/opt/local/sbin"
MACPORTS_GNUBIN_PATH="/opt/local/libexec/gnubin"

if [[ "${OSTYPE}" == "darwin"* ]]; then
    OS_SPEC_PATH="${MAC_GPG2_PATH}:${MACPORTS_PATH}:${MACPORTS_GNUBIN_PATH}"
else
    OS_SPEC_PATH=""
fi

PATH="${OS_SPEC_PATH}:${PATH}"
export PATH

MANPATH="/opt/local/man:/usr/local/share/man:/usr/share/man:/Library/Apple/usr/share/man:${MANPATH}"
export MANPATH

INFOPATH="/opt/local/info:/usr/local/share/info:${INFOPATH}"
export INFOPATH

# --- general variables
export DISPLAY=:0
# fix locale
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/password-store"

# Golang
export GOPATH="${HOME}/go"
export GOBIN="${GOPATH}/bin"
export PATH="${PATH}:${GOBIN}"

# Local path
export PATH="${PATH}:${HOME}/.local/bin"

# vim:ft=sh
