# Entrance app

A website used to save how many people are using a certain office/room at some time slot

## Configuration

There are two config files:

-   [`offices.yaml`](./offices.yaml): This defines the maximum number of people per site
-   [`shifts.yaml`](./shifts.yaml): This defines which shifts exist for a given site and on which days of the week a shift is

## Build this

The easiest way is to use docker-compose, just run `docker-compose build` and wait until everything is done.
Note that the first clean build will take roughly 15min and Docker memory should be increased to 4GB.
After that however the dependencies are cached and the rebuilds are fast.

## Run this

`docker-compose up`. This will make the server available at port 8000

## Deploy this

To deploy this within Futurice, use the [`deploy.sh`](./deploy.sh) script. First configure a AWS profile as described [here](https://welcome.play.futurice.com/). Then set the environment variable `AWS_PROFILE` to that profile name. The script takes the following arguments:

```
$ export AWS_PROFILE=<name>
$ ./deploy.sh [staging|production|cd] [--dry-run]
```

`cd` is the same as staging, but it skips the explicit confirmation step and is intended to use for continous delivery to staging. `--dry-run` will only build the docker container, but neither push it to the registry nor deploy it. It will print the kubernetes yaml it would apply to stdout.
