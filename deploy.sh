#!/bin/bash

set -eo pipefail

# Be sure to set SSH_USER to the FUM username or you will get errors
eval $(ssh-agent)

name="futulog-staging"

if [[ "$1" == "production" ]]; then
    name="futulog"
fi

image="futurice/futulog"
tag="$(git rev-parse --short HEAD)"

if [[ "$1" != "deploy" ]]; then
    DOCKER_BUILDKIT=1 docker build -t "$image:$tag" .
fi

if [[ "$1" == "build" ]]; then
    exit 0
fi

playswarm image:push -i "$image" -t "$tag"

if [[ "$(playswarm config:get DB_URL -n "$name" | tail -n +2)" == "Error! The specified key does not exist." ]]; then
    playswarm db:create:postgres -n "$name"
else
    echo "Database already exists, not creating a new one"
fi

confirm="yes"
if [[ "$1" != "deploy" ]]; then
    read -p "Are you sure you want to deploy to playswarm (yes/no)" confirm
fi
if [[ "$confirm" == "yes" ]]; then
    playswarm app:deploy --open=true -i "$image" -t "$tag" -n "$name"
else
    echo "aborting..."
fi
