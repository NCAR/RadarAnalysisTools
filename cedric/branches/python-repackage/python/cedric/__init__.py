# Python cedric package for wrapping the CEDRIC FORTRAN application

import os
import cedric._cedric as libcedric
import numpy as np
import logging

_logger = logging.getLogger(__name__)


def _chrcopy(dest, slot, src):
    for i,c in enumerate(src):
        dest[slot,i] = c

def _setup_krd(krd, *args):
    for i in xrange(10):
        _chrcopy(krd, i, "        ")
    for i, arg in enumerate(args):
        _chrcopy(krd, i, arg)


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

    def __init__(self):
        self._initialized = False
        self.unitpath = None
        self.begsec = None
        self.ibuf = np.zeros((self.MAXPLN,), dtype='int32', order='F')
        self.rbuf = np.zeros((self.MAXPLN,), dtype='float32', order='F')

    def init(self):
        if not self._initialized:
            self.begsec = libcedric.cedinit()
            self._initialized = True

    def quit(self):
        for f in ['.cededit', '.cedremap', '.sync', '.async', self.unitpath]:
            try:
                os.unlink(self.unitpath)
            except os.error:
                pass
        libcedric.cedquit(self.begsec)

    def read_volume(self, filepath):
        self.unitpath = "fort.11"
        try:
            os.unlink(self.unitpath)
        except:
            pass
        os.symlink(filepath, self.unitpath)

        self.init()

        krd = np.chararray((10, 8), order='F')
        _setup_krd(krd, "READVOL", "11.0", "NEXT", "", "", "YES")

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

        _setup_krd(krd, "STATS", "PRINT", "Z", "1.0", "ALL", "", "", "", "", "FULL")
        # CALL STATS(KRD,IBUF(1,1),IBUF(1,2),IPR)
        ipr = lpr
        libcedric.stats(krd, self.ibuf, self.rbuf, ipr)


    def fetchd(self, ilevel, ifield):

        # idd is not used
        idd = np.zeros((self.NID,), dtype='int32')
        bad = np.zeros((1,), dtype='float32')
        (nix, niy, rlev, nst) = libcedric.fetchd(0, idd, ilevel, ifield, 
                                                 self.ibuf, self.rbuf, 3, bad)
        _logger.debug("fetchd(%d,%d) ==> nix=%d, niy=%d, rlev=%f" % 
                      (ilevel, ifield, nix, niy, rlev))
        # CALL FETCHD(IEDFL,ID,LEV,IFLD(I),IBUF,RBUF(1,I),
        #             NIX,NIY,3,BAD,RLEV,NST)

        # Reshape the result in rbuf according to nix and niy.
        #field = self.rbuf[0:nix*niy].reshape((nix,niy), order='F')
        field = self.rbuf.reshape((nix, niy))
        return field


