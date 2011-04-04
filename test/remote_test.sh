#!/bin/sh

check_errs()
{
    # Function. Parameter 1 is the return code
    # Parameter 2 is text to display on failure.
    if [ "${1}" -ne "0" ]; then
	echo "ERROR # ${1} : ${2}"
	exit ${1}
    fi
}

ERTS_DIR=/usr/local/erlware/erts-5.8.2
RELEASE_PACKAGE=`ls ../_build/development/tar/*.tar.gz | head -n 1`
ROOT_DIR=./tmp/erlware

echo ""
echo "Installing standalone ciconia in separate root dir"
echo "All subsequent tests will be run in this context"
echo ""
echo "For Verbose logging in test 'export ERLP_LOG_LEVEL=1'"
echo ""

CMD="ciclocal install-release -e $ERTS_DIR -d $ROOT_DIR -f $RELEASE_PACKAGE"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on installing a release with the recently installed ciconia"

CMD="$ROOT_DIR/bin/ciconia update-cache -a faxien -r http://repo.erlware.org/pub"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed updating the package cache for a Faxien style repo."

CMD="$ROOT_DIR/bin/ciconia install-app -d $ROOT_DIR erlcron"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on installing the erlcron app"

CMD="$ROOT_DIR/bin/ciconia install-release -d $ROOT_DIR -fn 5.8.1 erl"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on installing the \"erl-5.8.1\" release"

CMD="$ROOT_DIR/bin/ciconia install-release -d $ROOT_DIR -f erl"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on installing the \"erl\" release"

CMD="$ROOT_DIR/bin/ciclocal rollback-release -d $ROOT_DIR erl"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on rolling back the \"erl\" release"

grep 'REL_VSN=5.8.1\s*$' $ROOT_DIR/bin/erl 
check_errs $? "Rollback failed. The bin file erl did not rollback to 5.8.1"


CMD="$ROOT_DIR/bin/erl -s init stop"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed starting up the erl shell (bad release install)"

CMD="$ROOT_DIR/bin/ciclocal managed"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on displaying managed contents"

CMD="$ROOT_DIR/bin/ciconia install-release -d ${ROOT_DIR}2 faxien"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on installing a release"

CMD="$ROOT_DIR/bin/ciclocal managed | grep faxien"
echo "\n***************"
echo "Running: $CMD"
$ROOT_DIR/bin/ciclocal managed | grep faxien
check_errs $? "failed on on displaying managed contents for all managed root dirs"

rm -rf tmp
