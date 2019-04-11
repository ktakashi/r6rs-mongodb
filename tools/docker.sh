#!/bin/sh

mongo_name=r6rs-mongodb-test
network_name=r6rs-mongodb-test-network

run_docker() {
    name=$1
    my_port=$2
    port_opt=""
    if [ x"$my_port" != x"" ]; then
	port_opt="-p $my_port:27017"
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

start() {
    docker network create $network_name
    run_docker $mongo_name "27017"
    run_docker "${mongo_name}1"
    run_docker "${mongo_name}2"
    sleep 10
    
    mongo --eval 'rs.initiate()'
    mongo --eval "rs.add(\"${mongo_name}1\")"
    mongo --eval "rs.add(\"${mongo_name}2\")"
}

stop() {
    docker rm -f $mongo_name
    docker rm -f "${mongo_name}1"
    docker rm -f "${mongo_name}2"
    docker network rm $network_name
}

case $1 in
    start) start;;
    stop) stop;;
    *) echo "$0 start|stop"; exit 1;;
esac
