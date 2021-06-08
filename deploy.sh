#!/bin/bash

set -eo pipefail

if [ -z "$AWS_PROFILE" ]; then
    echo 'Please set AWS_PROFILE for credentials and run `aws sso login`'
    exit 1
fi

name="play/futulog"
tag="$(git rev-parse --short HEAD)"
repo=$(aws ecr describe-repositories --repository-names $name --region=eu-central-1 --query "repositories[0].repositoryUri" --output text)

DOCKER_BUILDKIT=1 docker build -t "$name:$tag" .
docker tag "$name:$tag" "$repo:$tag"

if [[ "$1" == "build" ]]; then
    exit 0
fi

aws ecr get-login-password --region eu-central-1 | \
    docker login --username AWS --password-stdin $repo

docker push $repo:$tag
echo "Pushing image: $repo:$tag"

confirm="yes"
if [[ "$1" != "cd" ]]; then
    read -p "Are you sure you want to deploy to playswarm (yes/no)" confirm
fi
if [[ "$confirm" == "yes" ]]; then
    sed -i -e "s/image: 794457758780\.dkr\.ecr\.eu-central-1\.amazonaws.com\/play\/futulog:.*/image: 794457758780.dkr.ecr.eu-central-1.amazonaws.com\/play\/futulog:$tag/" deployment.yaml
    kubectl apply -f deployment.yaml
else
    echo "aborting..."
fi
