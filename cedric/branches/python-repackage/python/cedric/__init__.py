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


class Cedric(object):

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

    def __init__(self):
        self._initialized = False
        self.begsec = None
        self.ibuf = np.zeros((self.MAXPLN,), dtype='int32', order='F')
        self.rbuf = np.zeros((self.MAXPLN,), dtype='float32', order='F')

    def init(self):
        if not self._initialized:
            self.begsec = libcedric.cedinit()
            self._initialized = True

    def quit(self):
        for f in ['.cededit', '.cedremap', '.sync', '.async']:
            try:
                os.unlink(f)
            except os.error:
                pass
        libcedric.cedquit(self.begsec)

    def read_volume(self, filepath):
        # Apparently fortran will not actually re-read the file linked to
        # the given unit number, so we must use a new number each time.
        Cedric._unit_number += 1
        unitpath = "fort.%d" % (Cedric._unit_number)
        try:
            os.unlink(unitpath)
        except:
            pass
        os.symlink(filepath, unitpath)

        self.init()

        krd = _setup_krd("READVOL", "%2d.0" % (Cedric._unit_number),
                         "NEXT", "", "", "YES")

        # This is from CEDRIC.F which defines IBUF and then passes in
        # different columns of it as buffer space for the IBUF, RBUF, and
        # MAP parameters of READVL.  Or so I think.  Instead of that
        # approach, create the READVL parameters as typed in the READVL
        # declaration.
        #
        # DIMENSION IBUF(MAXPLN,MXCRT+27)
        # CHARACTER*8 KRD(10),GFIELD(NFMAX)
        # ibuf = np.zeros((self.MAXPLN, self.MXCRT+27), dtype='int32', order='F')

        # DIMENSION MAP(MAXAXIS,3)
        # IBUF(MAXPLN),RBUF(MAXPLN)
        pmap = np.zeros((self.MAXAXIS, 3), dtype='int32', order='F')

        # DATA QMARK/'Unknown?'/
        # do i=1,nfmax
        #   gfield(i)=qmark
        # end do
        gfield = np.chararray((self.NFMAX, 8), order='F')
        for i in xrange(self.NFMAX):
            _chrcopy(gfield, i, 'Unknown?')
        lin = 0
        lpr = 6
        icord = 0
        latlon = False
        # CALL READVL(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
        #X            LIN,LPR,ICORD,GFIELD,LATLON)
        libcedric.readvl(krd, self.ibuf, self.rbuf, pmap,
                         lin, lpr, icord, gfield, latlon)

        # After reading the volume from the file, we can create a Volume
        # object from the header ID array.
        v = cfile.Volume()
        v = cfile.volumeFromWords(v, libcedric.volume.id)
        v.title = filepath
        return v


    def stats(self):
        krd = _setup_krd(
            "STATS", "PRINT", "Z", "1.0", "ALL", "", "", "", "", "FULL")
        # CALL STATS(KRD,IBUF(1,1),IBUF(1,2),IPR)
        ipr = 6
        libcedric.stats(krd, self.ibuf, self.rbuf, ipr)


    def fetchd(self, ilevel, ifield):

        # idd is not used
        idd = np.zeros((self.NID,), dtype='int32')
        bad = np.zeros((1,), dtype='float32')
        bad = -1000.0
        (nix, niy, rlev, nst) = libcedric.fetchd(0, idd, ilevel, ifield, 
                                                 self.ibuf, self.rbuf, 3, bad)
        _logger.debug("fetchd(%d,%d) ==> nix=%d, niy=%d, rlev=%f" % 
                      (ilevel, ifield, nix, niy, rlev))
        # CALL FETCHD(IEDFL,ID,LEV,IFLD(I),IBUF,RBUF(1,I),
        #             NIX,NIY,3,BAD,RLEV,NST)

        # Reshape the result in rbuf according to nix and niy.
        field = self.rbuf[0:nix*niy].reshape((nix,niy), order='F').transpose()

        # These data are already scaled by cedric, but change the bad value
        # to a newer convention.
        # field[valid] /= vars[name_ls[ii]]
        # field[ field == -32768 ] = -1000.
        return field


    def fillData(self, volume):
        "Read all the levels for all of the fields into the volume object."

        for var in volume.vars.values():
            data = np.ones((volume.grid.x.n, volume.grid.y.n, volume.grid.z.n),
                           dtype=np.float32, order='F') * -1000.0
            for z in xrange(volume.grid.z.n):
                data[:,:,z] = self.fetchd(z+1, var.nid())
            volume.data[var.name()] = data
        return volume


    def caluvw3d(self, v, inbuf):
        "Run caluvw3d on the volume in @param v."

        rc = np.zeros((3,2), dtype=np.float32, order='F')
        gi = np.zeros((3,3), dtype=np.float32, order='F' )
        gi[0,0] = v.grid.x[0]
        gi[2,0] = v.grid.x[1]
        gi[0,1] = v.grid.y[0]
        gi[2,1] = v.grid.y[1]
        gi[0,2] = v.grid.z[0]
        gi[2,2] = v.grid.z[1]

        vtest  = np.asarray((.7, 7.5, 100.), dtype=np.float32)
        weight = np.asarray((1.,1.), dtype=np.float32)
        imoving= np.asarray((1 ,1 ), dtype=np.int32)
        outbuf = libcedric.caluvw3d(inbuf, rc, gi, vtest, 1, weight, 
                                    imoving, 0, 0, -1000.)
        return outbuf
