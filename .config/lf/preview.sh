#!/usr/bin/env bash

FILE_PATH="${1}"
FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"

# Preview pane size
WIDTH=$((${2} - 1))
HEIGHT=$(( ${3} - 1))
X_POS=${4}
Y_POS=$(( ${5} + 1))

handle_extension() {
    case "${FILE_EXTENSION_LOWER}" in
        # Archive
        a|bz|bz2|cpio|gz|lz|lzma|lzo|rz|t7z|tar|tbz|\
        tbz2|tgz|tlz|txz|tzo|tzst|xz|zip|zst)
            bsdtar --list --file "${FILE_PATH}";;
        7z)
            7zz l "${FILE_PATH}" | tail -n +11;;

        # Software archive
        deb|jar|mpkg|pkg|rpm)
            bsdtar --list --file "${FILE_PATH}";;

        # PDF
        pdf)
            exiftool "${FILE_PATH}";;

        # OpenDocument
        odt|ods|odp|sxw)
            pandoc -s -t markdown -- "${FILE_PATH}";;

        # HTML
        htm|html|xhtml)
            pandoc -s -t markdown -- "${FILE_PATH}";;

        # JSON (text/plain)
        json)
            jq --color-output . "${FILE_PATH}";;

        # Databases
        sqlite | db)
            sqlite3 -readonly "${FILE_PATH}" .tables;;
    esac
}

handle_mime() {
    _mimetype="${1}"
    case "${MIMETYPE}" in
        *wordprocessingml.document|*/epub+zip|*/x-fictionbook+xml)
            pandoc -s -t markdown -- "${FILE_PATH}";;

        text/* | */xml)
            env COLORTERM=8bit bat --color="always" --style="plain" \
                --pager="never" -- "${FILE_PATH}";;

        # */jpg | */jpeg | */png)
        #     printf "size: "
        #     kitty +kitten icat --print-window-size "${FILE_PATH}" &
        #     kitty +kitten icat --transfer-mode file \
        #         --place "${WIDTH}x${HEIGHT}@${X_POS}x${Y_POS}" "${FILE_PATH}"
        #     exit 1
        #     ;;

        image/* | video/*)
            exiftool "${FILE_PATH}";;

        application/json)
            jq --color-output . "${FILE_PATH}";;

        application/x-mach-binary)
            OTOOL_OUTPUT="$(otool -L "${FILE_PATH}")"
            basename "$(echo "${OTOOL_OUTPUT}" | head -n 1)"
            echo
            echo "${OTOOL_OUTPUT}" |\
                tail -n +2 | sed "s/.*\t//g" | sed 's/(compatibility.*$//g'
            ;;
    esac
}

MIMETYPE="$( file --dereference --brief --mime-type -- "${FILE_PATH}" )"

handle_extension
handle_mime "${MIMETYPE}"
