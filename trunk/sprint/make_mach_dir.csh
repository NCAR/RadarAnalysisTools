#! /bin/csh
#
#   Build machine-dependent SPRINT executable.
#   Filename = sprint.bld

#   Select machine operating system
#
set OS = `uname`
echo $OS
if ($OS == SunOS) set mach = solaris
if ($OS == OSF1)  set mach = alpha
if ($OS == Linux) set mach = linux
if ($OS == IRIX)  set mach = sgi


#   Build machine-dependent executable
#
echo "I will create Sprint-$mach"
mkdir Sprint-$mach
cd Sprint-$mach
ln -s ../Sprint-src/$mach/* .
ln -s ../Sprint-src/* .

exit
#