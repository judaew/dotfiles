#!/usr/bin/env bash

set -o pipefail

FILE_PATH="${1}"
FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"

# Preview pane size
WIDTH=$((${2} - 1))
HEIGHT=$(( ${3} - 1))
X_POS=${4}
Y_POS=$(( ${5} + 1))

try() {
    "$@" 2>/dev/null && return 0
    return 1
}

handle_extension() {
    case "${FILE_EXTENSION_LOWER}" in
        # Archive
        a|bz|bz2|cpio|gz|lz|lzma|lzo|rz|t7z|tar|tbz|\
        tbz2|tgz|tlz|txz|tzo|tzst|xz|zip|zst)
            bsdtar --list --file "${FILE_PATH}"
            exit $?;;
        7z)
            7zz l -p -- "${FILE_PATH}"
            exit $?;;

        # Software archive
        deb|jar|mpkg|pkg|rpm)
            bsdtar --list --file "${FILE_PATH}"
            exit $?;;

        # PDF
        pdf)
            exiftool "${FILE_PATH}"
            exit $?;;

        # OpenDocument
        odt|ods|odp|sxw)
            pandoc -s -t markdown -- "${FILE_PATH}"
            exit $?;;

        # HTML
        htm|html|xhtml)
            pandoc -s -t markdown -- "${FILE_PATH}"
            exit $?;;

        # JSON (text/plain)
        json)
            jq --color-output . "${FILE_PATH}"
            exit $?;;

        # Databases
        sqlite | db)
            sqlite3 -readonly "${FILE_PATH}" .tables
            exit $?;;
    esac
}

handle_mime() {
    _mimetype="${1}"

    case "${MIMETYPE}" in
        *wordprocessingml.document|*/epub+zip|*/x-fictionbook+xml|*/rtf)
         pandoc -s -t markdown -- "${FILE_PATH}"
         exit $?;;

        text/* | */xml | */yaml | */javascript )
            env COLORTERM=8bit bat --color="always" --style="plain" \
                --pager="never" -- "${FILE_PATH}"
            exit $?;;

        */jpg | */jpeg | */png | */webp)
            printf "size: "
            kitten icat --print-window-size "${FILE_PATH}" &
            kitten icat \
                --stdin no --transfer-mode memory \
                --place "${WIDTH}x${HEIGHT}@${X_POS}x${Y_POS}" "${FILE_PATH}" < /dev/null > /dev/tty
            exit 1
            ;;

        image/* | video/*)
        exiftool "${FILE_PATH}"
        exit $?;;

        */x-executable | */x-mach-binary | */x-pie-executable | */x-object | */x-sharedlib | */x-elf)
            echo "$(basename "${FILE_PATH}"): $(file -b "${FILE_PATH}")"
            if [[ "${OSTYPE}" == "darwin"* ]]; then
                otool -L "${FILE_PATH}" |\
                    tail -n +2 | sed "s/.*\t//g" | sed 's/(compatibility.*$//g'
            elif [[ "${OSTYPE}" == "linux-gnu"* ]]; then
                objdump -f "${FILE_PATH}"
            fi
            exit $?;;

        *)
        file -b -- "${FILE_PATH}"
        echo
        file --dereference --brief --mime-type -- "${FILE_PATH}"
        exit $?;;
    esac
}

MIMETYPE="$( file --dereference --brief --mime-type -- "${FILE_PATH}" )"

handle_extension
handle_mime "${MIMETYPE}"
