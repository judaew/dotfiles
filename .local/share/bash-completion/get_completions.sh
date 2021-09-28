#!/usr/bin/env bash

COMPLETION_PATH="${HOME}/.local/share/bash-completion/completions"

TMUX_URL="https://raw.githubusercontent.com/imomaliev/tmux-bash-completion/master/completions/tmux"
wget ${TMUX_URL} -O ${COMPLETION_PATH}/tmux.bash && \
    echo "tmux.bash downloaded" || \
    echo "tmux.bash failed"
