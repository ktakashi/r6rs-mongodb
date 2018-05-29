#!/bin/bash

declare -A implementations=([sagittarius@0.9.2]='false'
			    [larceny@1.3]='false'
			    [chez@v9.5]='true')

echo "Preparing for Chez Scheme"
create_symlink() {
    flag=$1
    target=$2
    src=$3
    if [ ! ${flag} ${src} ]; then
	ln -s ${target} ${src}
    fi
}
create_symlink -f %3a64.sls test/lib/srfi/:64.sls
create_symlink -d %3a64 test/lib/srfi/:64
create_symlink -f %3a98.sls test/lib/srfi/:98.sls
create_symlink -d %3a98 test/lib/srfi/:98

gcc -fPIC -shared -O3 src/mongodb/net/tcp/chez.c -o src/mongodb/net/tcp/chez.so

# check mongod is running or not
service mongod status | grep running > /dev/null
case $? in
    0)
	export MONGODB_RUNNING=yes
	echo MongoDB is running ... yes
	;;
    *) 	echo MongoDB is running ... no
esac

for impl in ${!implementations[@]}; do
    echo Testing with ${impl}
    case ${implementations[$impl]} in
	true) testpath="--loadpath test/lib" ;;
    esac
    for file in test/*.scm; do
	scheme-env run ${impl} \
		   --loadpath src $testpath \
		   --standard r6rs --program ${file}
    done
done
