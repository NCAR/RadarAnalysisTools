# Python cedric package for wrapping the CEDRIC FORTRAN application

import os
import cedric._cedric as libcedric
import numpy as np
import logging

_logger = logging.getLogger(__name__)

import cedric.file as cfile

def _chrcopy(dest, slot, src):
    for i,c in enumerate(src):
        dest[slot,i] = c

def _setup_krd(*args):
    krd = np.chararray((10, 8), order='F')
    for i in xrange(10):
        _chrcopy(krd, i, "        ")
    for i, arg in enumerate(args):
        _chrcopy(krd, i, arg)
    return krd


_D2R = np.pi/180.


# From CEDRIC.INC:
#
# PARAMETER (NFMAX = 25)
# PARAMETER (MXCRT=35)
# PARAMETER (MAXX= 512,MAXY=512)
# PARAMETER (MAXAXIS= MAXX + MAXY)
# PARAMETER (MAXPLN= MAXX*MAXY)

NFMAX = 25
MXCRT = 35
MAXX = 512
MAXY = 512
MAXAXIS = MAXX + MAXY
MAXPLN = MAXX * MAXY
NID = 510

_unit_number = 10
_initialized = False
_begsec = None
_ibuf = np.zeros((MAXPLN,), dtype='int32', order='F')
_rbuf = np.zeros((MAXPLN,), dtype='float32', order='F')

def init():
    global _initialized
    global _begsec
    if not _initialized:
        _begsec = libcedric.cedinit()
        _initialized = True

def quit():
    for f in ['.cededit', '.cedremap', '.sync', '.async']:
        try:
            os.unlink(f)
        except os.error:
            pass
    libcedric.cedquit(_begsec)

def read_volume(filepath):
    # Apparently fortran will not actually re-read the file linked to
    # the given unit number, so we must use a new number each time.
    global _unit_number
    _unit_number += 1
    unitpath = "fort.%d" % (_unit_number)
    try:
        os.unlink(unitpath)
    except:
        pass
    os.symlink(filepath, unitpath)

    init()
    krd = _setup_krd("READVOL", "%2d.0" % (_unit_number),
                     "NEXT", "", "", "YES")

    # This is from CEDRIC.F which defines IBUF and then passes in
    # different columns of it as buffer space for the IBUF, RBUF, and
    # MAP parameters of READVL.  Or so I think.  Instead of that
    # approach, create the READVL parameters as typed in the READVL
    # declaration.
    #
    # DIMENSION IBUF(MAXPLN,MXCRT+27)
    # CHARACTER*8 KRD(10),GFIELD(NFMAX)
    # ibuf = np.zeros((MAXPLN, MXCRT+27), dtype='int32', order='F')

    # DIMENSION MAP(MAXAXIS,3)
    # IBUF(MAXPLN),RBUF(MAXPLN)
    pmap = np.zeros((MAXAXIS, 3), dtype='int32', order='F')

    # DATA QMARK/'Unknown?'/
    # do i=1,nfmax
    #   gfield(i)=qmark
    # end do
    gfield = np.chararray((NFMAX, 8), order='F')
    for i in xrange(NFMAX):
        _chrcopy(gfield, i, 'Unknown?')
    lin = 0
    lpr = 6
    icord = 0
    latlon = False
    # CALL READVL(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
    #X            LIN,LPR,ICORD,GFIELD,LATLON)
    libcedric.readvl(krd, _ibuf, _rbuf, pmap,
                     lin, lpr, icord, gfield, latlon)

    # After reading the volume from the file, we can create a Volume
    # object from the header ID array.
    v = cfile.Volume()
    v = cfile.volumeFromWords(v, libcedric.volume.id)
    v.title = os.path.basename(filepath)
    fillData(v)
    return v


def stats():
    krd = _setup_krd(
        "STATS", "PRINT", "Z", "1.0", "ALL", "", "", "", "", "FULL")
    # CALL STATS(KRD,IBUF(1,1),IBUF(1,2),IPR)
    ipr = 6
    libcedric.stats(krd, _ibuf, _rbuf, ipr)


def _fetchd(ilevel, ifield):

    # idd is not used
    idd = np.zeros((NID,), dtype='int32')
    bad = np.zeros((1,), dtype='float32')
    bad = -1000.0
    (nix, niy, rlev, nst) = libcedric.fetchd(0, idd, ilevel, ifield, 
                                             _ibuf, _rbuf, 3, bad)
    _logger.debug("fetchd(%d,%d) ==> nix=%d, niy=%d, rlev=%f" % 
                  (ilevel, ifield, nix, niy, rlev))
    # CALL FETCHD(IEDFL,ID,LEV,IFLD(I),IBUF,RBUF(1,I),
    #             NIX,NIY,3,BAD,RLEV,NST)

    # Reshape the result in rbuf according to nix and niy.
    field = _rbuf[0:nix*niy].reshape((nix,niy), order='F').transpose()
    return field


def fillData(volume):
    "Read all the levels for all of the fields into the volume object."
    for var in volume.values():
        data = np.ones((volume.grid.x.n, volume.grid.y.n, volume.grid.z.n),
                       dtype=np.float32, order='F') * -1000.0
        for z in xrange(volume.grid.z.n):
            data[:,:,z] = _fetchd(z+1, var.nid())
        volume.data[var.name()] = data
    return volume


def caluvw3d(v, v2):
    "Run caluvw3d on the velocity fields in volumes @param v and @param v2."

    rc = np.zeros((3,2), dtype=np.float32, order='F')
    gi = np.zeros((3,3), dtype=np.float32, order='F' )
    gi[0,0] = v.grid.x[0]
    gi[2,0] = v.grid.x[1]
    gi[0,1] = v.grid.y[0]
    gi[2,1] = v.grid.y[1]
    gi[0,2] = v.grid.z[0]
    gi[2,2] = v.grid.z[1]

    data1 = v.data
    inbuf = np.zeros(data1['DBZ'].shape+(4,2,), dtype=np.float32, order='F')
    inbuf[:,:,:,0,0] = data1['VG']
    inbuf[:,:,:,1,0] = data1['DBZ']
    inbuf[:,:,:,2,0] = data1['AZ'] * _D2R
    inbuf[:,:,:,3,0] = data1['EL'] * _D2R
    data2 = v2.data
    if data1['VG'].shape == data2['VG'].shape:
        inbuf[:,:,:,0,1] = data2['VG']
        inbuf[:,:,:,1,1] = data2['DBZ']
        inbuf[:,:,:,2,1] = data2['AZ'] * _D2R
        inbuf[:,:,:,3,1] = data2['EL'] * _D2R
    else:
        raise IndexError

    vtest  = np.asarray((.7, 7.5, 100.), dtype=np.float32)
    weight = np.asarray((1.,1.), dtype=np.float32)
    imoving= np.asarray((1 ,1 ), dtype=np.int32)
    outbuf = libcedric.caluvw3d(inbuf, rc, gi, vtest, 1, weight, 
                                imoving, 0, 0, -1000.)
    u  = outbuf[:,:,:,0]
    v  = outbuf[:,:,:,1]
    eu = outbuf[:,:,:,5]
    ev = outbuf[:,:,:,6]
    return (u, v, eu, ev)


def pconvg(u, v, nder, xydeli, bad):
    """
    @param U,V [nx,ny] input horizontal wind field
    @param CONV [nx,ny] ouput convergence
    @param NDER number of point to evaluate the derivative
    @param XYDELI [2] reciprocal of DX,DY
    @param BAD fill value of input/output field
    """
    return libcedric.pconvgp(u, v, nder, xydeli, bad)
