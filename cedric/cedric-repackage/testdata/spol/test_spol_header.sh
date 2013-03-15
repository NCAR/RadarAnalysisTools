#! /bin/sh
#
# Convert UF data to CEDRIC format, then run cedric to dump the header
# information.


radxgrid=Radx2Grid

searchdirs=(/opt/local/radx/bin /opt/local/bin /usr/local/bin)

for d in $searchdirs ; do
    if [ -x "$searchdirs/Radx2Grid" ]; then
	radxgrid="$searchdirs/Radx2Grid"
    fi
done
echo "Using Radx2Grid path: $radxgrid"

# Find cedric executable, looking first for a sibling source tree.
cedric=cedric
testdir=../tests
basedir=`pwd`
while [ "$basedir" != "/" ]; do
    basedir=`dirname "$basedir"`
    if [ -x "$basedir/source/cedric" ]; then
	cedric="$basedir/source/cedric"
	testdir="$basedir/tests"
	break
    fi
done
echo "Using cedric path: $cedric"


update_params() # params-file
{
    params="$1"
    rm -f long_params.txt
    $radxgrid -params $params -print_params long > long_params.txt
    mv -f long_params.txt $params
}


convert_uf() # params-file uf-file
{
    # The ced output file is expected to go under the grid-output
    # subdirectory.  Clear it out first.
    rm -rf grid-output
    $radxgrid -params "$1" -f "$2"
    cedfile=`find grid-output -name "*.ced"`
    if [ -n "$cedfile" ]; then
	(set -x ; mv -f "$cedfile" .)
    else
	echo "No CEDRIC file found in grid-output!"
    fi
    rm -rf grid-output
}


read_volume() # ced-file
{
    $testdir/read_volume.sh "$1"
}



diff_cedric_output() # output-1 output-2
{
    diff --side-by-side --width=200 --text \
	--ignore-matching-lines='Using cedric path:' \
	--ignore-matching-lines='Cedric execution started' \
	--ignore-matching-lines='^DATE' \
	--ignore-matching-lines='^TIME' \
	--ignore-matching-lines='CEDRIC EXECUTION ENDED' \
	"$1" "$2"
    if [ $? -ne 0 ]; then
	echo "*** Differences found. ***"
    else
	echo "--- No differences found."
    fi
}


case "$1" in

    convert) 
	convert_uf Radx2Grid.jay *.uf
	;;

    update)
	update_params Radx2Grid.jay
	;;

    read_volume)
	read_volume *.ced
	;;

    diff)
	diff_cedric_output cedric-baseline.out cedric-latest.out
	;;
	
    dotest)
	read_volume *.ced >& cedric-latest.out
	diff_cedric_output cedric-baseline.out cedric-latest.out
	;;

esac
