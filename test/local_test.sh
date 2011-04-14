#!/bin/sh

check_errs()
{
    # Function. Parameter 1 is the return code
    # Para. 2 is text to display on failure.
    if [ "${1}" -ne "0" ]; then
	echo "ERROR # ${1} : ${2}"
# as a bonus, make our script exit with the right error code.
exit ${1}
fi
}

ERTS_DIR=/usr/local/erlware/erts-5.8.2
RELEASE_PACKAGE=`ls ../_build/development/tar/*.tar.gz | head -n 1`
APP_PACKAGE=`ls ../_build/development/apps | head -n 1`
ROOT_DIR=./tmp/erlware
REPO_DIR=./tmp/repo

echo ""
echo "Running locally focused smoke testing."
echo ""
echo "For Verbose logging in test 'export ERLP_LOG_LEVEL=1'"
echo ""
echo "Installing standalone ciconia in separate root dir"
echo "All subsequent tests will be run in this context"
echo ""

CMD="ciclocal install-release -d $ROOT_DIR -e $ERTS_DIR -f $RELEASE_PACKAGE"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on installing a release"

CMD="$ROOT_DIR/bin/ciclocal version" 
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on version command"

CMD="$ROOT_DIR/bin/ciclocal install-app -d $ROOT_DIR -f ../_build/development/apps/$APP_PACKAGE"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on installing an application"

CMD="$ROOT_DIR/bin/ciclocal managed"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on displaying managed contents"

CMD="$ROOT_DIR/bin/ciclocal list -d $ROOT_DIR"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on listing the contents of the recently installed test release"

CMD="$ROOT_DIR/bin/ciclocal manage-root-dir $ROOT_DIR"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on installing an application"

CMD="$ROOT_DIR/bin/ciclocal managed"
echo "\n***************"
echo "Running: $CMD"
$ROOT_DIR/bin/ciclocal managed | grep ciconia
check_errs $? "failed on on displaying managed contents"

CMD="$ROOT_DIR/bin/ciconia publish -a faxien -r file://$REPO_DIR $RELEASE_PACKAGE"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on on publishing a release"
ls $REPO_DIR | grep "5.8.2"
check_errs $? "failed on on publishing a release"

CMD="$ROOT_DIR/bin/ciconia update-cache -a faxien -r file://$REPO_DIR"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on updating the cache from the filesystem"

CMD="$ROOT_DIR/bin/ciconia install-release -fd ${ROOT_DIR}2 ciconia"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on updating the cache from the filesystem"

CMD="${ROOT_DIR}2/bin/ciclocal managed"
echo "\n***************"
echo "Running: $CMD"
check_errs $? "failed on on displaying managed contents from ciconia installed from file based repo."

CMD="$ROOT_DIR/bin/ciclocal remove-app -fd ${ROOT_DIR}2 ciconia"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on removing the ciconia release"

CMD="ls ${ROOT_DIR}2/lib |grep ciconia | wc -l | grep 0"
echo "\n***************"
echo "Running: $CMD"
ls ${ROOT_DIR}2/lib | grep ciconia | wc -l | grep 0
check_errs $? "failed to remove the ciconia application"

CMD="$ROOT_DIR/bin/ciclocal remove-release -fd ${ROOT_DIR}2 ciconia"
echo "\n***************"
echo "Running: $CMD"
$CMD
check_errs $? "failed on removing the ciconia release"

CMD="ls ${ROOT_DIR}2/lib | wc -l | grep 0"
echo "\n***************"
echo "Running: $CMD"
ls ${ROOT_DIR}2/lib | wc -l | grep 0
check_errs $? "failed to remove the ciconia release"

rm -rf tmp
