#! /bin/sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <cedric-file>"
    exit 1
fi

# Find cedric executable, looking first for a sibling source tree.
cedric=cedric
basedir=`pwd`
while [ "$basedir" != "/" ]; do
    basedir=`dirname "$basedir"`
    if [ -x "$basedir/source/cedric" ]; then
	cedric="$basedir/source/cedric"
	break
    fi
done
# cedric=/home/granger/code/cedric-build/cedric_64.e
echo "Using cedric path: $cedric"

rm -f fort.11
ln -s "$1" fort.11

$cedric <<EOF
READVOL 11.0    NEXT                    YES
STATS   PRINT   Z       1.0     ALL                                     FULL
QUIT
EOF

rm -f .cededit .cedremap .sync .async fort.11
