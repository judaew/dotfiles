# shellcheck shell=bash

#     +-------+
# --- | paths |
#     +-------+

# unix paths
PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:${PATH}"
MANPATH="/usr/local/share/man:/usr/share/man:${MANPATH}"
INFOPATH="/usr/local/share/info:${INFOPATH}"

if [[ "${OSTYPE}" == "darwin"* ]]; then
    # --- PATH
    # Macports -> MacPorts GnuBin -> MacOS
    OS_SPEC_PATH="/opt/local/bin:/opt/local/sbin"
    OS_SPEC_PATH="${OS_SPEC_PATH}:/opt/local/libexec/gnubin"
    OS_SPEC_PATH="${OS_SPEC_PATH}:/Library/Apple/usr/bin"

    # --- MANPATH
    # MacPorts -> MacOS
    OS_SPEC_MANPATH="/opt/local/share/man"
    OS_SPEC_MANPATH="${OS_SPEC_MANPATH}:/Library/Apple/usr/share/man"

    # --- INFOPATH (MacPorts)
    OS_SPEC_INFOPATH="/opt/local/share/info"
else
    OS_SPEC_PATH=""
    OS_SPEC_MANPATH=""
    OS_SPEC_INFOPATH=""
fi

export PATH="${OS_SPEC_PATH}:${PATH}"
export MANPATH="${OS_SPEC_MANPATH}:${MANPATH}"
export INFOPATH="${OS_SPEC_INFOPATH}:${INFOPATH}"
export OS_SPEC_CPATH="${OS_SPEC_CPATH}"

if [[ "${OSTYPE}" == "darwin"* ]]; then
    CPATH="/opt/local/include:${CPATH}"
else
    CPATH=""
fi
export CPATH

# Local PATH
export PATH="${PATH}:${HOME}/.local/bin"

#     +-------------------+
# --- | general variables |
#     +-------------------+

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

# JAVA
if [[ "${OSTYPE}" == "darwin"* ]]; then
    JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk17-temurin/Contents/Home"
    PATH="${PATH}:${JAVA_HOME}/bin"
    MANPATH="${MANPATH}:${JAVA_HOME}/man"
    CPATH="${CPATH}:${JAVA_HOME}/include"
fi

# vim:ft=sh
