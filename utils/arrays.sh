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
    [".config/hypr"]=".config"
    [".config/networkmanager-dmenu"]=".config"
    [".config/nwg-bar"]=".config"
    [".config/nwg-dock-hyprland"]=".config"
    [".config/nwg-drawer"]=".config"
    [".config/nwg-panel"]=".config"
    [".config/pacman"]=".config"
    [".config/rofi"]=".config"
    [".config/swaync"]=".config"
    [".config/waybar"]=".config"
    # ~/.local/share
    [".local/bin"]=".local"
    [".local/share/bash-completion/get_completions.sh"]=".local/share/bash-completion"
)

