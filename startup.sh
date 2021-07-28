#!/bin/sh

set -e

secrets=/run/secrets/ca.crt

if [ ! -z "$CA_FOLDER" ]; then
    secrets="$CA_FOLDER/ca.crt"
fi

if [ -e "$secrets" ]; then
    cp "$secrets" /usr/local/share/ca-certificates/
    update-ca-certificates
else
    echo "No certificates found, proceeding"
fi

./futuLog
