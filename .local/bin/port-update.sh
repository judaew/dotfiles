#!/usr/bin/env sh

if [ -z "(which port)" ]; then
    echo "MacPorts not found, this script is primarily for use with MacPorts."
    exit 1
fi

green=$(tput setaf 2)
reset_color=$(tput sgr0)

# Prompted password immediately after startup
sudo echo &> /dev/null

echo
echo "${green}[port-update.sh]${reset_color} fetching..."
echo "------------------------------------------------------------"

cd /opt/local/var/macports/sources/github.com/judaew/macports-ports/
git checkout master
git pull upstream master && git push

echo
echo "${green}[port-update.sh]${reset_color} syncing..."
echo "------------------------------------------------------------"

sudo port -v sync

echo
echo "${green}[port-update.sh]${reset_color} outdated:"
echo "------------------------------------------------------------"

port outdated

while true; do
    echo
    read -p "${green}[port-update.sh]${reset_color} Do you wish to update outdated ports [y/N]? " answer
    echo "------------------------------------------------------------"

    case ${answer} in
        [Yy]*)
            sudo port upgrade outdated
            sudo -k
            break;;
        [Nn]*|*)
            sudo -k
            break;;
    esac
done

while true; do
    echo
    read -p "${green}[port-update.sh]${reset_color} Do you wish to exec 'port-livecheck.sh' [y/N]? " answer
    echo "------------------------------------------------------------"
    echo
    case ${answer} in
        [Yy]*)
            clear
            port-livecheck.sh
            break;;
        [Nn]*|*)
            exit 0;;
    esac
done

# vim: set ft=bash:
