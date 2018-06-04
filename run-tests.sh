#!/bin/bash

declare -a implementations=(sagittarius@0.9.2
			    larceny@1.3
			    chez@v9.5)

echo "Preparing for Chez Scheme"
create_symlink() {
    flag=$1
    target=$2
    src=$3
    if [ ! ${flag} ${src} ]; then
	ln -s ${target} ${src}
    fi
}
create_symlink -f %3a64.chezscheme.sls test/lib/srfi/:64.sls
create_symlink -d %3a64 test/lib/srfi/:64
create_symlink -f %3a98.chezscheme.sls test/lib/srfi/:98.sls
create_symlink -d %3a98 test/lib/srfi/:98

gcc -fPIC -shared -O3 src/mongodb/net/tcp/chez.c -o src/mongodb/net/tcp/chez.so

# check mongod is running or not
if [ x"${MONGODB_RUNNING}" = x"" ]; then
    service mongod status | grep running > /dev/null
    case $? in
	0)
	    export MONGODB_RUNNING=yes
	    echo Checking whether MongoDB is running ... yes
	    ;;
	*)
	    echo Checking whether MongoDB is running ... no
    esac
fi

check_output() {
    local status=0
    while IFS= read -r LINE; do
	echo $LINE
	case $LINE in
	    *FAIL*) status=255 ;;
	esac
    done
    return ${status}
}

EXIT_STATUS=0

for impl in ${implementations[@]}; do
    echo Testing with ${impl}
    for file in test/*.scm; do
	scheme-env run ${impl} \
		   --loadpath src $testpath \
		   --loadpath test/lib \
		   --standard r6rs --program ${file} | check_output
	case ${EXIT_STATUS} in
	    0) EXIT_STATUS=$? ;;
	esac
    done
    echo Done!
    echo
done

echo Exit status ${EXIT_STATUS}
exit ${EXIT_STATUS}
