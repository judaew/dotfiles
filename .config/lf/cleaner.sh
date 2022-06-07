#!/usr/bin/env bash

FILE_PATH="${1}"
FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"

# handle_extension() {
#     case "${FILE_EXTENSION_LOWER}" in
#     esac
# }

handle_mime() {
    _mimetype="${1}"
    case "${MIMETYPE}" in
        */jpg | */jpeg | */png)
            kitty +kitten icat --clear --silent --transfer-mode file
    esac
}

MIMETYPE="$( file --dereference --brief --mime-type -- "${FILE_PATH}" )"

# handle_extension
handle_mime "${MIMETYPE}"

