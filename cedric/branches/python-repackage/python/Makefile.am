# -*- makefile -*-
#
# libcedric.a objects must have been compiled with -fPIC to be linked with
# the python shared module

LIBCEDRIC = ../source/libcedric.a
F2PY = f2py
F2PY_INCLUDES = -I../source $(NETCDFINCS) -I/usr/include

LIBS = $(LIBCEDRIC) $(NETCDFLIBS)

F2PY_MODULES = \
  ../source/SETVOL.F \
  ../source/BNDFIL.F90 \
  ../source/CALUVW3d.F90 \
  ../source/DWITER.F90 \
  ../source/INTGRT3d.F90 \
  ../source/PCONVG.F90

all-local: cedric.so

cedric.so: 
	$(F2PY) -c -m cedric $(F2PY_INCLUDES) $(F2PY_MODULES) $(LIBS)

clean-local:
	rm -f cedric.so
