#!/usr/bin/env sh

if [ -z "(which port)" ]; then
    echo "MacPorts not found, this script is primarily for use with MacPorts."
    exit 1
fi

green="$(tput setaf 2)"
reset_color="$(tput sgr0)"
maintainer="${1}"

echo
echo "${green}[maintainers]${reset_color}"
echo "----------------------------------------"

if [ "${maintainer}" != "" ]; then
    port livecheck maintainer:${maintainer}
else
    port livecheck maintainers:judaew
fi

echo
echo "${green}[nomaintainer]${reset_color}"
echo "----------------------------------------"

source "${XDG_DATA_HOME}/my_scripts/ports_nomaintainer-list.sh"

port livecheck ${ports_nomaintainer[@]}
