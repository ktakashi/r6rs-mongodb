#!/bin/bash

set -e

declare -A implementations=([sagittarius@0.9.2]='false'
			    [larceny@1.3]='false'
			    [chez@v9.5]='true')

echo "Preparing for Chez Scheme"
if [ ! -f test/lib/srfi/:64.sls ]; then
    ln -s %3a64.sls test/lib/srfi/:64.sls
fi
if [ ! -d test/lib/srfi/:64 ]; then
    ln -s %3a64 test/lib/srfi/:64
fi

gcc -fPIC -shared -O3 src/mongodb/net/tcp/chez.c -o src/mongodb/net/tcp/chez.so

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
