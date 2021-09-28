#!/usr/bin/env bash

declare -A files=(
    # ~/
    [".bash_profile"]=""
    [".bashrc"]=""
    [".profile"]=""
    # ~/.config
    [".config/git"]=".config"
    [".config/kitty"]=".config"
    [".config/nvim/init.lua"]=".config/nvim"
    [".config/nvim/lua"]=".config/nvim"
    [".config/nvim/viml"]=".config/nvim"
    [".config/lf"]=".config"
    [".config/tmux"]=".config"
    # ~/.local/share
    [".local/share/bash-completion/get_completions.sh"]=".local/share/bash-completion"
)
