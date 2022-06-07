# shellcheck shell=bash

# If not running interactively, don't do anything
[[ ${-} != *i* ]] && return

# --- plugins and third party

# source bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    source /opt/local/etc/profile.d/bash_completion.sh
fi

if [ -f /opt/local/share/bash-completion/completions/pass ]; then
    source /opt/local/share/bash-completion/completions/pass-otp
fi

# settings mcfly
export MCFLY_KEY_SCHEME=vim
export MCFLY_FUZZY=true
export MCFLY_RESULTS=50

# --- options

shopt -s cdable_vars
shopt -s checkhash
shopt -s checkjobs
shopt -s direxpand
shopt -s dirspell
shopt -s dotglob
shopt -s histappend
shopt -s nocaseglob

export HISTCONTROL=ignoredups

# --- colors and prompt

if [ -f "/opt/local/share/git/contrib/completion/git-prompt.sh" ]; then
    source /opt/local/share/git/contrib/completion/git-prompt.sh
elif [ -f "/usr/share/git/completion/git-prompt.sh" ]; then
    source /usr/share/git/completion/git-prompt.sh
elif [ -f "${HOME}/.config/git-prompt.sh" ]; then
    source "${HOME}"/.config/git-prompt.sh
fi

GIT_PS1_SHOWDIRTYSTATE='1'        # '*'=unstaged, '+'=staged
GIT_PS1_SHOWSTASHSHATE='1'        # '$'=stashed
GIT_PS1_SHOWUPSTREAM='verbose'    # 'u='=no difference, 'u+1'=ahead by 1 commit
GIT_PS1_STATESEPARATOR=''         # no space between branch and index status
GIT_PS1_DESCRIBE_STYLE='describe'

function __prompt_cmd() {
    local exit_status=${?}

    # cache the value in a variable before using pringf to avoid delay
    local GIT_PS1
    GIT_PS1="$(__git_ps1)"

    DIRTRIM_AWK=$(cat << 'EOF'
BEGIN { FS = OFS = "/" }
{
   sub(ENVIRON["HOME"], "~");
   if (length($0) > 35 && NF > 4)
      print $1,$2,".." NF-4 "..",$(NF-1),$NF
   else
      print $0
}
EOF
)
    DIRTRIM="$(printf "%b" "${PWD}" | awk "${DIRTRIM_AWK}")"

    local GRAY="\e[0;37m"
    local DARK_GRAY="\e[0;90m"
    local RED="\e[0;31m"
    local CYAN="\e[0;36m"
    local PURPLE="\e[0;35m"
    local NORMAL="\e[0;0m"

    # set window title bar
    if [ -z "${PROMPT_TITLE}" ]; then
        # the var is empty
        printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"
    else
        # the var isn't empty
        printf "\033]0;%s\a" "${PROMPT_TITLE}"
    fi

    # empty line before the prompt
    printf "\n"

    # check for ssh session
    if [[ -n "${SSH_CLIENT}" ]]; then
        printf "%b" "${GRAY}${USER}@${HOSTNAME%%.*} "
    fi

    # print current directory
    printf "%b" "${CYAN}${DIRTRIM}"

    # print git prompt
    printf "%b\n" "${DARK_GRAY}${GIT_PS1}${NORMAL}"

    # set prompt
    if [[ ${exit_status} = 0 ]]; then
        PS1="\[${PURPLE}\]❯ \[${NORMAL}\]"
    else
        PS1="\[${RED}\]❯ \[${NORMAL}\]"
    fi
}
PROMPT_COMMAND="__prompt_cmd"

# --- aliases
if [[ "${OSTYPE}" == "darwin"* ]]; then
    alias ls="ls -G"
else
    alias ls="ls --color=auto"
fi

alias la="ls -a"
alias ll="ls -al"
alias ldot="ls -ld .*"
alias cp="cp -i"
alias df="df -h"
alias du="du -h"

alias week="date +%V"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias random-mac="openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//'"

# darwin specific
if [[ "${OSTYPE}" == "darwin"* ]]; then
    alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
    alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"
    alias typora="open -a typora"
fi

# --- variables
# Most environment variables have been moved to ~/.profile
HISTCONTROL=ignoreboth:erasedups
HISTSIZE=100000
HISTFILESIZE=100000

# --- keybindings
set -o vi

if [[ "${OSTYPE}" == "darwin"* ]]; then
    if [[ -r "/opt/local/share/mcfly/mcfly.bash" ]]; then
        source /opt/local/share/mcfly/mcfly.bash
    fi
fi

# --- functions
function __path_remove() {
    PATH=$(echo -n "${PATH}" | awk -v RS=: -v ORS=: '$0 != "$1"' | sed 's/:$//');
    export PATH
}

function __java_check_current_version() {
    java -version 2>&1 | head -1 | cut -d'"' -f2 | sed '/^1\./s///' | cut -d'.' -f1
}

function java-set() {
    __path_remove "${JAVA_HOME}" && __path_remove "${JAVA_BIN}"
    JAVA_HOME=$(/usr/libexec/java_home -v "${1}")
    export JAVA_HOME
    JAVA_BIN=${JAVA_HOME}/bin
    export JAVA_BIN
    export PATH=${PATH}:${JAVA_BIN} && \
        echo "The current Java version is $(__java_check_current_version)"
}

function portlist-create() {
    true portlist-requested.txt
    cat <<'EOF' > portlist-requested.txt
This file contains packages I use on Darwin and install with Macports.
Install them with the followin command:

    for i in $(sed '1,13d' portlist-requested.txt); do
        sudo port -N install $i||echo $i failed
    done

To recreate this list after adding packages, use the follow command:

    portlist-create

Packages:

EOF
    port echo requested | cut -d ' ' -f 1 >> portlist-requested.txt
}

# `sprunge filename # post file to sprunge`
# `some_command | sprunge # pipe output to sprunge`
function sprunge() {
  if [[ ${1} ]]; then
    curl -F 'sprunge=<-' "http://sprunge.us" <"${1}"
  else
    curl -F 'sprunge=<-' "http://sprunge.us"
  fi
}

# Adds all available keys to SSH agent
function ssh-add-all() {
    for i in $(find "${HOME}"/.ssh/* -type f ! -name "*.*" | sed \
        's!.*/!!' | sed '/config/,/known_hosts/d; /.pub/d')
    do
        ssh-add --apple-use-keychain ~/.ssh/"${i}"||echo "${i}" failed
    done
}

function git-latest-branch() {
    local rev_list latest_tag
    rev_list=$(git rev-list --tags --max-count=1)
    latest_tag=$(git describe --tags "${rev_list}")
    git fetch --tags
    git checkout "${latest_tag}"
}

function lfmp() {
    lf "$(port dir "${1}")"
}

function title() {
    PROMPT_TITLE="${1}"
}

# vim:ft=sh
