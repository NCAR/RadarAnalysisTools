#! /bin/csh
#
#   Build machine-dependent PPI_MMM executable.
#   Filename = ppi_mmm.bld
#
#   This script was modified from a version that untared 
#   source code, created the Ppi_mmm-$mach directory 
#   and then called make.  This new script just creates
#   the Ppi_mmm-$mach directory.   -DFF July 13, 2009

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
echo "I am creating Ppi_mmm-$mach"
mkdir Ppi_mmm-$mach
cd Ppi_mmm-$mach
ln -s ../Ppi_mmm-src/* .
ln -s ../Ppi_mmm-src/Op_sys/op_sys.h-$mach         op_sys.h
ln -s ../Ppi_mmm-src/WORD_sizes/WORD.size.32-$mach WORD.size
ln -s ../Ppi_mmm-src/Makefiles/makefile.$mach      makefile
ln -s ../Ppi_mmm-src/DISP_wk.f                     DISP.f
ln -s ../Ppi_mmm-src/MYFRAME_wk.f                  MYFRAME.f
ln -s ../Ppi_mmm-src/dim_ppi.inc                   dim.inc
ln -s ../Ppi_mmm-src/ascii.topodata                topo.dat
if ($mach == linux)then
   /bin/rm PPI_MMM.f
   ln -s ../Ppi_mmm-src/PPI_MMM-linux.f PPI_MMM.f
endif

exit
#