#!/bin/bash

# get the ids all docker containers that have exited but were not yet removed
ids=$(docker ps -a -q --filter "status=exited")

# if the length of ids > 0, then we have some containers that we want to remove
len=${#ids}
zero=0

if [ "$len" -gt "$zero" ]; then
echo "inside"
# Delete all the dockers that have been stopped but were not yet killed
docker rm $ids
fi
