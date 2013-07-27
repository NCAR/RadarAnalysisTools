
import os
import cedric
import numpy as np


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

    def read_volume(self, filepath):
        unitpath = "fort.11"
        try:
            os.unlink(unitpath)
        except:
            pass
        os.symlink(filepath, unitpath)

        #krd = np.array((10, ), dtype='S100', order='F')
        krd = np.chararray((10, 80), order='F')
        # krd = np.array((10, ), dtype=np.str_, order='F')
        for i in xrange(krd.shape[0]):
            for j in xrange(krd.shape[1]):
                krd[i,j] = '\0'
        # krd[:,:] = ' '
        line = "READVOL 11.0    NEXT                    YES                                     "
        for i, c in enumerate(line):
            krd[0,i] = c
        print(krd)

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
        ibuf = np.zeros((self.MAXPLN,), dtype='int32', order='F')
        rbuf = np.zeros((self.MAXPLN,), dtype='float32', order='F')
        pmap = np.zeros((self.MAXAXIS, 3), dtype='int32', order='F')

        cedric.cedinit()

        # DATA QMARK/'Unknown?'/
        # do i=1,nfmax
        #   gfield(i)=qmark
        # end do
        gfield = np.chararray((self.NFMAX, 8), order='F')
        for i in xrange(self.NFMAX):
            for j, c in enumerate('Unknown?'):
                gfield[i, j] = c
        lin = 0
        lpr = 6
        icord = 0
        latlon = False
        # CALL READVL(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
        #X            LIN,LPR,ICORD,GFIELD,LATLON)
        cedric.readvl(krd, ibuf, rbuf, pmap,
                      lin, lpr, icord, gfield, latlon)

        krd[0] = "STATS   PRINT   Z       1.0     ALL                                     FULL"
        # CALL STATS(KRD,IBUF(1,1),IBUF(1,2),IPR)
        ipr = lpr
        cedric.stats(krd, ibuf[0,0], ibuf[0,1], ipr)

        # rm -f .cededit .cedremap .sync .async fort.11
        os.unlink(unitpath)



if __name__ == "__main__":

    filepath = "../testdata/spol/vol_20000629_233038_to_20000629_233433_SPOL_CRT.ced"
    core = Cedric()
    core.read_volume(filepath)




