#!/usr/bin/env bash

c_main="$(tput setaf 0)$(tput setab 2)"
c_green="$(tput setaf 2)"
c_red="$(tput setaf 1)"
c_yellow="$(tput setaf 3)"
c_reset="$(tput sgr0)"

passed() {
    echo
    echo "${c_main} lint ${c_reset}${c_green} ${linter} passed${c_reset}"
    echo
}

error() {
    echo
    echo "${c_main} lint ${c_reset}${c_red} ${linter} failed${c_reset}"
    echo
}

skipped() {
    echo
    echo "${c_main} lint ${c_reset}${c_yellow} ${linter} skipped${c_reset}"
    echo
}

linter="shellcheck"
if command -v shellcheck &> /dev/null; then
    if SHELLCHECK_OPTS="-e SC1090 -e SC1091 -e SC2034" \
        shellcheck \
        dot_bash_profile dot_bashrc \
        dot_profile dot_config/lf/*.sh \
        utils/*.sh; then
    passed
    else
        error
    fi
else
    skipped
fi

linter="luacheck"
if command -v luacheck &> /dev/null; then
    if luacheck dot_config/nvim; then
        passed
    else
        error
    fi
else
    skipped
fi

linter="markdownlint"
if command -v mdl &> /dev/null; then
    if gem install mdl; then
        passed
    else
        error
    fi
else
    skipped
fi
