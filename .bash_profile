# shellcheck shell=bash

[[ -f ${HOME}/.profile ]] && source "${HOME}"/.profile && \
    [[ -f ${HOME}/.bashrc ]] && source "${HOME}"/.bashrc

if command -v direnv &> /dev/null; then
    # run direnv hook
    eval "$(direnv hook bash)"
fi

if command -v nvim &> /dev/null; then
    export EDITOR="nvim"
elif command -v vim &> /dev/null; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi

# vim:ft=sh
