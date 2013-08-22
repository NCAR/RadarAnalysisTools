# -*- makefile -*-
#
# libcedric.a objects must have been compiled with -fPIC to be linked with
# the python shared module

LIBCEDRIC = ../../source/libcedric.a
F2PY = f2py

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
  ../source/PCONVGP.F90 ../source/ms3d.f90

MNAME = _cedric
PYF = $(MNAME).pyf
SOMODULE = cedric/$(MNAME).so

all-local: $(SOMODULE)

$(SOMODULE): $(PYF)
	cd cedric && $(F2PY) -c -m $(MNAME) ../$(PYF) $(LIBS)

clean-local:
	rm -rf fort.99 $(SOMODULE) $(PYF) .async .cededit .cedremap .sync

$(PYF): $(F2PY_MODULES)
	$(F2PY) -m $(MNAME) -h $(PYF) $(F2PY_MODULES)

