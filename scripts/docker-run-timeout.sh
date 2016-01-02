#!/bin/bash
to=$1
shift
cont=$(docker run -d "$@")
code=$(timeout "$to" docker wait "$cont" || true)
docker kill $cont &> /dev/null
if [ -z "$code" ]; then
    echo "Sorry, your program exceeds our time limit of $to and was therefore terminated by Codeboard."
fi
docker logs $cont | sed 's/^//'
docker rm $cont &> /dev/null
