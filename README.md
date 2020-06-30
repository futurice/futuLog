# Entrance app

A website used to save how many people are using a certain office/room at some time slot

## Configuration

There are two config files:

-   [`offices.yaml`](./offices.yaml): This defines the maximum number of people per site
-   [`shifts.yaml`](./shifts.yaml): This defines which shifts exist for a given site and on which days of the week a shift is
-   [`admins.yaml'`](./admins.yaml): A list of email addresses that should have access to the admin API

## Build this

The easiest way is to use docker-compose, just run `docker-compose build` and wait until everything is done.
 Note that the first clean build will take roughly 15min and Docker memory should be increased to 4GB.
 After that however the dependencies are cached and the rebuilds are fast.


## Run this

`docker-compose up`. This will make the server available at port 8000
