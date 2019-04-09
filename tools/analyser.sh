#!/bin/sh

mongo_name=r6rs-mongodb

me=$0
my_dir=$(dirname $me)

port=27018

init() {
    if [ -d /tmp/mongo ]; then
	rm -rf /tmp/mongo
    fi
    mkdir -p /tmp/mongo/db
    mkdir -p /tmp/mongo/log
    
    docker run --name $mongo_name -p $port:27017 -d \
	   --mount "type=bind,src=/tmp/mongo/db,dst=/data/db" \
	   --mount "type=bind,src=/tmp/mongo/log,dst=/var/log/mongodb" \
	   mongo:4.0.8
    sleep 10
}

cleanup() {
    docker stop $mongo_name
    docker rm $mongo_name
}

run_script() {
    rm "$1.out"
    sash $my_dir/proxy.scm -o "$1.out" -m $port &
    sleep 1
    mongo localhost:27017/test "$1"
    sleep 1
    kill %1
}

init

for f in "$@"; do
    echo Executing $f
    run_script $f
done

cleanup
