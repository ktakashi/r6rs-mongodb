#!/bin/sh

mongo_name=r6rs-mongodb
network_name=r6rs-mongodb-network

me=$0
my_dir=$(dirname $me)

port=27018

run_docker() {
    name=$1
    my_port=$2
    port_opt=""
    if [ x"$my_port" != x"" ]; then
	port_opt="-p $port:27017"
    fi
    
    if [ -d /tmp/$name ]; then
	rm -rf /tmp/$name
    fi
    mkdir -p /tmp/$name/db
    mkdir -p /tmp/$name/log

    docker run --name $name $port_opt -d --net $network_name \
	   --mount "type=bind,src=/tmp/$name/db,dst=/data/db" \
	   --mount "type=bind,src=/tmp/$name/log,dst=/var/log/mongodb" \
	   mongo:4.0.8 --replSet 'rs1' --logpath /var/log/mongodb/mongo.log \
	     --dbpath /data/db
}

init() {
    docker network create $network_name
    run_docker $mongo_name $port
    run_docker "${mongo_name}1"
    run_docker "${mongo_name}2"
    
    sleep 10
    mongo localhost:$port/test --eval 'rs.initiate()'
    mongo localhost:$port/test --eval 'rs.add("r6rs-mongodb1")'
    mongo localhost:$port/test --eval 'rs.add("r6rs-mongodb2")'
    mongo localhost:$port/test --eval 'rs.conf()'
    mongo localhost:$port/test --eval 'rs.status()'
    mongo localhost:$port/test --eval 'db.createCollection("employees", {})'
}

cleanup() {
    docker rm -f $mongo_name
    docker rm -f "${mongo_name}1"
    docker rm -f "${mongo_name}2"
    docker network rm $network_name
}

run_script() {
    out_name="$1.out"
    if [ -f $out_name ]; then
	rm $out_name
    fi
    sash $my_dir/proxy.scm -o $out_name -m $port &
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
