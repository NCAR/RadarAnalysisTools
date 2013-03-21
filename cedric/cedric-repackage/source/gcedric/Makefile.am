
sd = ../

include $(srcdir)/../Makefile.sources

bin_PROGRAMS = gcedric
gcedric_SOURCES = $(SRCS)

DEFINES = -DCEDRIC_USE_NCARG=1
SRCS = $(FSRCS) $(CSRCS) $(NGSRCS) $(MDVSRCS) $(GBYTESSRCS)
# roo:
# NG_LIBS = -L/opt/local/ncarg-5.2.1/lib -lncarg -lncarg_gks -lncarg_ras -lncarg_c
# XLIBS	= -lX11 -lXext -lpng -lz
gcedric_LDADD = $(NG_LIBS) $(XLIBS) $(NETCDFLIBS)
