#!/bin/sh

check_errs()
{
    # Function. Parameter 1 is the return code
    # Para. 2 is text to display on failure.
    if [ "${1}" -ne "0" ]; then
	echo "ERROR # ${1} : ${2}"
exit ${1}
fi
}

cd ./test

./local_test.sh
check_errs $? "failed on local smoke testing, abandoning further tests"

./remote_test.sh
check_errs $? "failed on remote smoke testing, abandoning further tests"

