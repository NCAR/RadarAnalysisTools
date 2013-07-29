# -*- makefile -*-
#
# libcedric.a objects must have been compiled with -fPIC to be linked with
# the python shared module

LIBCEDRIC = ../source/libcedric.a
F2PY = f2py
BDIR = --build-dir build
F2PY_INCLUDES = -I../source $(NETCDFINCS) -I/usr/include

LIBS = $(LIBCEDRIC) $(NETCDFLIBS)

F2PY_MODULES = \
  ../source/VERSOUT.F  ../source/SYMINT.F \
  ../source/CEDINIT.F ../source/CEDQUIT.F \
  ../source/READVL.F ../source/FETCHD.F \
  ../source/SETVOL.F \
  ../source/STATS.F \
  ../source/BNDFIL.F90 \
  ../source/CALUVW3d.F90 \
  ../source/DWITER.F90 \
  ../source/INTGRT3d.F90 \
  ../source/PCONVG.F90 ../source/ms3d.f90

all-local: cedric.so

PYF = cedric.pyf

cedric.so: $(PYF)
	$(F2PY) $(BDIR) -c -m cedric $(PYF) $(F2PY_INCLUDES) $(LIBS)

clean-local:
	rm -rf build fort.99 cedric.so $(PYF) .async .cededit .cedremap .sync

$(PYF): $(F2PY_MODULES)
	$(F2PY) -m cedric -h $(PYF) $(F2PY_MODULES)

