.. CEDRIC Python documentation master file, created by
   sphinx-quickstart on Mon Aug 26 11:42:05 2013.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

CEDRIC Python Documentation
===========================

Contents:

.. toctree::
   :maxdepth: 2

   cedricapi

About CEDRIC Python
===================

CEDRIC is a FORTRAN application for the analysis of gridded radar data, and
this is a python wrapper to some of the core functionality of CEDRIC.

CEDRIC Library
--------------

By separating the CEDRIC code into a library archive and a main
application, both the application and the python module can be built by
linking to the same library.  The library modules must be built as
position-independent code (PIC) to allow them to be linked into the
sharable python module.

The basic CEDRIC library is built *without* NCAR Graphics enabled, since
that is the most reasonable form to embed in the python wrapper or in
custom applications.  The CEDRIC application which supports NCAR Graphics
(gcedric) is built by recompiling a few of the graphics-dependent modules
separately from the library.  These modules are linked to the `gcedric`
application *before* the non-graphical library, thus the graphical
implementations override the non-graphical routines in the library.

Python Wrapper Code
-------------------

The python interface to the FORTRAN code is generated with the f2py tool of
the `numpy <http://www.numpy.org/>`_ package.  The interface definition
file (`_cedric.pyf`) is generated from several FORTRAN routines, including
these modules used without change, mostly to support reading the CEDRIC
binary file format:

 * VERSOUT.F
 * SYMINT.F
 * CEDINIT.F
 * CEDQUIT.F
 * READVL.F
 * FETCHD.F
 * SETVOL.F
 * STATS.F

In addition, the CEDRIC library now includes rewrites of several core
computational routines.  Xiaowen Tang adapted these modules for the python
interface.  In particular, these modules have been updated to use newer
FORTRAN 90 syntax and features, and they handle an entire 3D field array
instead of processing only one level at a time:

 * BNDFIL.F90
 * CALUVW3d.F90
 * DWITER.F90
 * INTGRT3d.F90
 * PCONVGP.F90
 * ms3d.f90

`f2py` builds the `_cedric.so` dynamically-loaded python module using the
`_cedric.pyf` interface file and linking to the CEDRIC library.  The
`_cedric.so` python module is a private module inside the `cedric` python
package.  The `cedric` package is meant to provide a more convenient python
API to the underlying `f2py` interfaces.

cedric Python Package
---------------------

The `cedric` package encapsulates the CEDRIC library wrapper code and some
convenient python methods for accessing and processing CEDRIC data.

Of particular importance is the ability to read CEDRIC binary files through
the original FORTRAN code.  This means the python package should be able to
access all the same data files as the CEDRIC application with complete
compatibility, since the FORTRAN code handles all of the byte order,
swapping, or special cases in the decoding the file format.

Xiaowen Tang has also started extending the CEDRIC functionality with
python implementations.  For example, the `cedric.algorithms` module
provides the `vt_correction` method for doing fall speed correction on the
synthesized wind field.  The `cedric.plots` module provides convenience
methods for plotting radar fields with the `matplotlib` package.

The main interface to the `cedric` package is defined in the `__init__.py`
module.  This module provides simple entry points to the CEDRIC library
implementations.  It also integrates a high-level data structure called the
`Volume` class, defined in the `cedric.file` module.  The `Volume` class
encapsulates the metadata and data contained through the CEDRIC volume
block.  It contains the data for all of the radar fields, stored as `numpy`
arrays, and indexed by field name in the `Volume` object.  The fields
themselves are actually `Variable` instances.  The `Variable` instance can
be indexed like any `numpy` array, but it retains the radar metadata like
field name and the indexing levels.  So far this feature is primarily used
by the `cedric.plots` utilities to generate reasonable titles and axis
names.

The documentation for the python API is generated automatically here:
:doc:`cedricapi`.


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

