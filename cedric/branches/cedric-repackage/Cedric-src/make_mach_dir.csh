#! /bin/csh
#
#   Build machine-dependent CEDRIC directory.
#   Filename = cedric.bld

set OS = `uname`
echo $OS
if ($OS == SunOS) set mach = solaris
if ($OS == OSF1)  set mach = alpha
if ($OS == Linux) set mach = linux
if ($OS == IRIX)  set mach = sgi

#   Build machine-dependent executable
#
echo "I will create Cedric-$mach"
mkdir Cedric-$mach
cd Cedric-$mach
ln -s ../Cedric-src/$mach/* .
ln -s ../Cedric-src/* .

exit
#
