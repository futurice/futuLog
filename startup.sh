#!/bin/sh

set -e

if [ -e /run/secrets/ca.crt ]; then
    cp /run/secrets/ca.crt /usr/local/share/ca-certificates/
    update-ca-certificates
fi

./office-tracker
