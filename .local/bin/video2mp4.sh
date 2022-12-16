#!/usr/bin/env sh

if [ -z "(which ffmpeg)" ]; then
    echo "Error: FFmpeg not found."
    exit 1
fi

if [ "${1}" == "" ]; then
    echo 'Error: Enter the file you want to convert'
    exit 1
fi

FILE="$(basename "${1}")"
FILE_WITHOUT_EXTENSION="$(echo "${FILE}" | sed 's/\(.*\)\..*/\1/')"
ARGS="-c:v copy -c:a aac -movflags +faststart"

ffmpeg -i "${FILE}" ${ARGS} "${FILE_WITHOUT_EXTENSION}.mp4"
