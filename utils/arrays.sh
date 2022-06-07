#!/usr/bin/env bash

# shellcheck disable=SC2034
declare -A files=(
    # ~/
    [".bash_profile"]=""
    [".bashrc"]=""
    [".inputrc"]=""
    [".profile"]=""
    # ~/.config
    [".config/git"]=".config"
    [".config/kitty"]=".config"
    [".config/nvim/init.vim"]=".config/nvim"
    [".config/nvim/autogroups.vim"]=".config/nvim"
    [".config/nvim/config.vim"]=".config/nvim"
    [".config/nvim/mappings.vim"]=".config/nvim"
    [".config/nvim/lua"]=".config/nvim"
    [".config/nvim/viml"]=".config/nvim"
    [".config/lf"]=".config"
    [".config/tmux"]=".config"
    [".config/ideavim/ideavimrc"]=".config/ideavim"
    # ~/.local/share
    [".local/share/bash-completion/get_completions.sh"]=".local/share/bash-completion"
)

