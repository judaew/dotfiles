#!/usr/bin/env bash

if [ -z "$(which pacman)" ]; then
    echo "pacman not found, this script is primarily for use with Arch Linux."
    exit 1
fi

green=$(tput setaf 2)
reset_color=$(tput sgr0)

# Prompted password immediately after startup
sudo echo > /dev/null 2>&1

echo
echo "${green}[pacman-reclaim.sh]${reset_color} getting a list of unclaimed pkgs..."
echo "-----------------------------------------------------------------------------"

pacman -Qdtq

while true; do
    echo
    read -rp "${green}[pacman-reclaim.sh]${reset_color} Do you wish to uninstall unclaimed pkgs [y/N]? " answer
    echo "-----------------------------------------------------------------------------"

    case ${answer} in
        [Yy]*)
            sudo pacman -Rns $(pacman -Qdtq)
            break;;
        [Nn]*|*)
            break;;
    esac
done

while true; do
    echo
    read -rp "${green}[pacman-reclaim.sh]${reset_color} Do you wish to uninstall old version pkgs from AUR [y/N]? " answer
    echo "-----------------------------------------------------------------------------"

    case ${answer} in
        [Yy]*)
            yay -Sc
            break;;
        [Nn]*|*)
            break;;
    esac
done

while true; do
    echo
    read -rp "${green}[pacman-reclaim.sh]${reset_color} Do you wish to uninstall old version pkgs [y/N]? " answer
    echo "-----------------------------------------------------------------------------"

    case ${answer} in
        [Yy]*)
            sudo paccache -rk2
            break;;
        [Nn]*|*)
            break;;
    esac
done

sudo -k

# vim: set ft=bash:
