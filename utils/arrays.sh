#!/usr/bin/env bash

# shellcheck disable=SC2034
declare -A files=(
    # ~/
    [".bash_profile"]=""
    [".bashrc"]=""
    [".inputrc"]=""
    [".profile"]=""
    [".clang-format"]=""
    # ~/.config
    [".config/git"]=".config"
    [".config/kitty"]=".config"
    [".config/nvim/init.lua"]=".config/nvim"
    [".config/nvim/lua"]=".config/nvim"
    [".config/lf"]=".config"
    [".config/task"]=".config"
    [".config/tmux"]=".config"
    [".config/pacman"]=".config"
    # ~/.local/share
    [".local/bin"]=".local"
    [".local/share/bash-completion/get_completions.sh"]=".local/share/bash-completion"
)

