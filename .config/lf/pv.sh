#!/bin/sh

FILE_PATH="${1}"
FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"

handle_extension() {
    case "${FILE_EXTENSION_LOWER}" in
        # Archive
        a|bz|bz2|cpio|gz|lz|lzma|lzo|rz|t7z|tar|tbz|\
        tbz2|tgz|tlz|txz|tzo|tzst|xz|zip|zst)
            bsdtar --list --file "${FILE_PATH}";;
        # 7z)
        #     7z l "${1}";;

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
    
        # JSON
        json)
            jq --color-output . "${FILE_PATH}";;
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

        image/* | video/*)
            exiftool "${FILE_PATH}";;
    esac
}

MIMETYPE="$( file --dereference --brief --mime-type -- "${FILE_PATH}" )"

handle_extension
handle_mime "${MIMETYPE}"
