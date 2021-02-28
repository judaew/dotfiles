#!/usr/bin/env bash

os="$(uname -s)"
echo "OS is $os"

if [ "${os}" = 'Linux' ]; then
    os='linux'
elif [ "$os" = 'Darwin' ]; then
    os='darwin'
fi

arch="$(uname -m)"
echo "arch is $arch"

if [ "$arch" = 'x86_64' ]; then
    arch='amd64'
elif [ "$arch" = 'i686' ]; then
    arch='i386'
elif [ "$arch" = 'armv7l' ]; then
    arch='arm'
elif [ "$arch" = 'aarch64' ]; then
    arch='arm64'
fi

tag_chezmoi="1.8.11"
url_chezmoi="https://github.com/twpayne/chezmoi/releases/download/"
filename_chezmoi="v${tag_chezmoi}/chezmoi_${tag_chezmoi}_${os}_${arch}.tar.gz"

mkdir -p /tmp/chezmoi
curl -L ${url_chezmoi}${filename_chezmoi} | tar xzf - -C /tmp/chezmoi
curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh --output "${HOME}"/.config/git-prompt.sh

/tmp/chezmoi/chezmoi init ${HOME}/dotfiles
/tmp/chezmoi/chezmoi apply

exit 0
