#!/usr/bin/env bash

if [ -z "$(which ffmpeg)" ]; then
    echo "Error: FFmpeg not found."
    exit 1
fi

if [ "${1}" = "" ]; then
    echo 'Error: Enter the file you want to convert'
    exit 1
fi

FILENAME="$(basename "${1}")"
EXTENSION="${FILENAME##*.}"
EXTENSIONLESS="${FILENAME%.*}"
ARGS=(-c:v copy -c:a aac -movflags +faststart)

if [ "${EXTENSION}" != "mp4" ]; then
    ffmpeg -i "${FILENAME}" "${ARGS[@]}" "${EXTENSIONLESS}.mp4"
else
    mv "${FILENAME}" "${EXTENSIONLESS}-orig.mp4"
    ffmpeg -i "${EXTENSIONLESS}-orig.mp4" "${ARGS[@]}" "${EXTENSIONLESS}.mp4"
fi
