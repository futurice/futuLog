# Entrance app

A website used to save how many people are using a certain office/room at some time slot

## Configuration

All the available rooms are defined in the [`rooms.yaml`](./rooms.yaml). This file is read on startup and defines the name, site and maximum amount of people per room

## Build this

The easiest way is to use docker-compose, just run `docker-compose build` and wait until everything is done

## Run this

`docker-compose up`. This will make the server available at port 3000
