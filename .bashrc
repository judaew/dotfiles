# shellcheck shell=bash

# If not running interactively, don't do anything
[[ ${-} != *i* ]] && return

# --- options

shopt -s cdable_vars
shopt -s checkhash
shopt -s checkjobs
shopt -s direxpand
shopt -s dirspell
shopt -s dotglob
shopt -s histappend
shopt -s nocaseglob

# --- colors and prompt

# TODO: LGTM, but need add ${OSTYPE} check
if [ -f "/opt/local/share/git/contrib/completion/git-prompt.sh" ]; then
    source /opt/local/share/git/contrib/completion/git-prompt.sh
elif [ -f "/usr/share/git/completion/git-prompt.sh" ]; then
    source /usr/share/git/completion/git-prompt.sh
elif [ -f "${HOME}/.config/git-prompt.sh" ]; then
    source "${HOME}"/.config/git-prompt.sh
fi

GIT_PS1_SHOWDIRTYSTATE='1'        # '*'=unstaged, '+'=staged
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

    if [ -n "${PROMPT_PRIVATE_SESSION}" ]; then
        printf "%b" "${RED}private "
    fi

    # check for ssh session
    if [[ -n "${SSH_CLIENT}" ]]; then
        printf "%b" "${GRAY}${USER}@${HOSTNAME%%.*} "
    fi

    # if this is a subshell, print shell level
    # ignore Tmux and lf levels
    if [[ ${SHLVL} != 1 ]]; then
        local desc
        local lvl=${SHLVL}

        # by default, Tmux does not allow you to run itself in Tmux session
        # therefore, extra "if" statements are not required
        if [[ ${TMUX} != "" ]]; then
            desc="${desc}tmux+"
            lvl=$((lvl-1))
        fi

        if [[ ${LF_LEVEL} != "" ]]; then
            if [[ ${LF_LEVEL} == "1" ]]; then
                desc="${desc}lf+"
            else
                desc="${desc}lf(${LF_LEVEL})+"
            fi
            lvl=$((lvl-LF_LEVEL))
        fi

        printf "%b" "${DARK_GRAY}${desc}${lvl} "
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
if [[ "${OSTYPE}" == "darwin"* && ! -f /opt/local/libexec/gnubin/ls ]] ; then
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

alias today="date +%Y-%m-%d"
alias week="date +%V"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias random-mac="openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//'"

# Make colors in ripgrep as in The Silver Searcher
alias rg="rg --colors 'match:fg:black' --colors 'match:bg:yellow'\
             --colors 'match:style:nobold' --colors 'path:fg:green'\
             --colors 'path:style:bold' --colors 'line:fg:yellow'"

alias proj="source projects.sh"

alias yt480p="yt-dlp -f 'bestvideo[height<=480]+bestaudio'"
alias yt720p="yt-dlp -f 'bestvideo[height<=720]+bestaudio'"
alias yt1080p="yt-dlp -f 'bestvideo[height<=1080]+bestaudio'"

# darwin specific
if [[ "${OSTYPE}" == "darwin"* ]]; then
    alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
    alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"
    alias vscode="/Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin/code"
    alias smerge="/Applications/Sublime\ Merge.app/Contents/SharedSupport/bin/smerge"
    alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
elif [[ "${OSTYPE}" == "linux-gnu"* ]]; then
    if [[ "${DESKTOP_SESSION}" == "plasma" ]]; then
        alias open="kioclient exec"
    else
        alias open="xdg-open"
    fi
fi

# --- variables
# Most environment variables have been moved to ~/.profile
HISTCONTROL=ignoreboth:erasedups:ignoredups
HISTSIZE=100000
HISTFILESIZE=100000

# --- keybindings
set -o vi

if [[ "${OSTYPE}" == "darwin"* ]]; then
    if [[ -r "/opt/local/share/mcfly/mcfly.bash" ]]; then
        source /opt/local/share/mcfly/mcfly.bash
    fi
elif [[ "${OSTYPE}" == "linux-gnu"* ]]; then
    if [[ -r "/usr/share/doc/mcfly/mcfly.bash" ]]; then
        source /usr/share/doc/mcfly/mcfly.bash
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

function git-latest-branch() {
    local rev_list latest_tag
    rev_list=$(git rev-list --tags --max-count=1)
    latest_tag=$(git describe --tags "${rev_list}")
    git fetch --tags
    git checkout "${latest_tag}"
}

function title() {
    PROMPT_TITLE="${1}"
}

function bash-private() {
    PROMPT_PRIVATE_SESSION="TRUE" HISTFILE=/dev/null bash
}

# --- plugins and third party

# run direnv hook
if command -v direnv &> /dev/null; then
    eval "$(direnv hook bash)"
fi

# TODO: for macos
# source bash-completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
fi

# FIXME: fix for archlinux
# if [ -f /opt/local/share/bash-completion/completions/pass ]; then
#     source /opt/local/share/bash-completion/completions/pass-otp
# fi

# settings mcfly
export MCFLY_KEY_SCHEME=vim
export MCFLY_FUZZY=true
export MCFLY_RESULTS=50

# settings colors for less
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

# speed-up fzf
export FZF_DEFAULT_COMMAND='fd --type f --color=never'
export FZF_ALT_C_COMMAND='fd --type d . --color=never'

# vim:ft=sh
