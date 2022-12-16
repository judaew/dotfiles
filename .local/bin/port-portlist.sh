#!/usr/bin/env sh

if [ -z "$(which port)" ]; then
    echo "MacPorts not found, this script is primarily for use with MacPorts."
    exit 1
fi

true portlist-requested.txt

cat <<'EOF' > portlist-requested.txt
This file contains packages I use on Darwin and install with Macports.
Install them with the followin command:

    for i in $(sed '1,13d' portlist-requested.txt); do
        sudo port -N install $i||echo $i failed
    done

To recreate this list after adding packages, use the follow command:

    port-portlist.sh

Packages:

EOF
port echo requested | cut -d ' ' -f 1 >> portlist-requested.txt
