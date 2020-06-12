#!/bin/bash

set -eo pipefail

# Be sure to set SSH_USER to the FUM username or you will get errors

name="office-tracker-staging"
image="futurice/office-tracker"
tag="$(git rev-parse --short HEAD)"

dockerfile="Dockerfile"

if [ -n "$1" ]; then
    dockerfile="$1"
fi

docker build -t "$image:$tag" -f "$dockerfile" .

playswarm image:push -i "$image" -t "$tag"

if [[ "$(playswarm config:get DB_URL -n "$name" | tail -n +2)" == "Error! The specified key does not exist." ]]; then
    playswarm db:create:postgres -n "$name"
else
    echo "Database already exists, not creating a new one"
fi

confirm="yes"
if [ -z "$1" ]; then
    read -p "Are you sure you want to deploy to playswarm (yes/no)" confirm
fi
if [[ "$confirm" == "yes" ]]; then
    playswarm app:deploy -i "$image" -t "$tag" -n "$name"
else
    echo "aborting..."
fi
