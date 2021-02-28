#!/usr/bin/env bash

COMPLETION_PATH="$HOME/.local/share/bash-completion/completions"

TMUX_URL="https://raw.githubusercontent.com/imomaliev/tmux-bash-completion/master/completions/tmux"
wget $TMUX_URL -O $COMPLETION_PATH/tmux.bash && \
    echo "tmux.bash downloaded" || \
    echo "tmux.bash failed"

TMUXINATOR_URL="https://raw.githubusercontent.com/tmuxinator/tmuxinator/master/completion/tmuxinator.bash"
wget $TMUXINATOR_URL -O $COMPLETION_PATH/tmuxinator.bash && \
    echo "tmuxinator.bash downloaded" || \
    echo "tmuxinator.bash failed"

chezmoi completion bash --output $COMPLETION_PATH/chezmoi.bash && \
    echo "chezmoi.bash downloaded" || \
    echo "chezmoi.bash failed"

gh completion -s bash > $COMPLETION_PATH/gh.bash && \
    echo "gh.bash download" || \
    echo "gh.bash failed"
