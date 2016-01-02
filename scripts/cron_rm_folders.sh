#!/bin/bash

# Delete all the folders that were created more than 60 minutes ago;
find /tmp/projects/* -mmin +60 -type d -exec rm -rdf {} \; &> /dev/null

# If used in e.g. development env, the docker root-user creates the folders; It must then be the root user who deletes the folders (because of access rights).
# However, in a production setup (where the Docker user is cobo) you can ignore the following line.
# docker run --rm -v /tmp/projects:/tmp/projects ubuntu find /tmp/projects/* -mmin +60 -type d -exec rm -rdf {} \; &> /dev/null
