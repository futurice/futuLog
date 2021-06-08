#!/bin/bash

set -eo pipefail

if [ -z "$AWS_PROFILE" ] && [[ "$1" != "cd" ]]; then
    echo 'Please set AWS_PROFILE for credentials and run `aws sso login`'
    exit 1
fi

export NAME="futulog-staging"
export TAG="$(git rev-parse --short HEAD)"
repo=$(aws ecr describe-repositories --repository-names play/futulog --region=eu-central-1 --query "repositories[0].repositoryUri" --output text)

if [[ "$1" != "cd" ]]; then
    DOCKER_BUILDKIT=1 docker build -t "play/futulog:$TAG" .
fi
docker tag "play/futulog:$TAG" "$repo:$TAG"

if [[ "$1" == "build" ]]; then
    exit 0
fi

if [[ "$1" == "production" ]]; then
    export NAME="futulog"
fi

if [[ "$2" != "--dry-run" ]]; then
    aws ecr get-login-password --region eu-central-1 | \
        docker login --username AWS --password-stdin $repo

    docker push $repo:$TAG
    echo "Pushing image: $repo:$TAG"
fi

confirm="yes"
if [[ "$1" != "cd" ]] && [[ "$2" != "--dry-run" ]]; then
    read -p "Are you sure you want to deploy to futuEKS (yes/no)" confirm
fi
if [[ "$confirm" == "yes" ]]; then
    if [[ "$2" == "--dry-run" ]]; then
        envsubst <deployment.tmpl
    else
        envsubst <deployment.tmpl | \
            kubectl apply -f -
    fi
else
    echo "aborting..."
fi
