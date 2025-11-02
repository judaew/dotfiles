#!/usr/bin/env bash

if [ -z "$(which journalctl)" ]; then
    echo "journalctl not found, this script is primarily for use with systemd."
    return 1
fi

green=$(tput setaf 2)
reset_color=$(tput sgr0)

# Prompted password immediately after startup
sudo echo > /dev/null 2>&1

while true; do
    echo
    read -rp "${green}[systemd-reclaim.sh]${reset_color} Do you wish to clean old logs [y/N]? " answer
    echo "-----------------------------------------------------------------------------"

    case ${answer} in
        [Yy]*)
            sudo journalctl --vacuum-size=200M
            sudo journalctl --vacuum-time=2weeks
            break;;
        [Nn]*|*)
            break;;
    esac
done

sudo -k

# vim: set ft=bash:
