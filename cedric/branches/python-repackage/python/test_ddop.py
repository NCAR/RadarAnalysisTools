#! /usr/bin/python

import sys
import logging
logging.basicConfig(level=logging.DEBUG)

import cedric
import matplotlib.pyplot as plt
import numpy as np
from cedric import libcedric
from cedric.algorithms import vt_correction
import cedric.plots as cp


if __name__ == "__main__":

    file1 = "../testdata/ddop/leg1-hires-TA.ced"
    file2 = "../testdata/ddop/leg1-hires-TF.ced"
    if len(sys.argv) not in [1,3]:
        print("Usage: %s file1 file2" % (sys.argv[0]))
        print("Default file1: %s" % (file1))
        print("Default file2: %s" % (file2))
        sys.exit(1)
    elif len(sys.argv) == 3:
        file1 = sys.argv[1]
        file2 = sys.argv[2]

    volume1 = cedric.read_volume(file1)
    data1 = volume1.data

    volume2 = cedric.read_volume(file2)
    data2 = volume2.data

    dbz = volume1['DBZ']
    fig = cp.compare_fields(dbz[:,:,1], volume2['DBZ'][:,:,1])
    plt.show()
    fig.savefig('ddop_dbz.png', dpi=100)
    plt.close(fig)
    fig = cp.compare_fields(volume1['VG'][:,:,1], volume2['VG'][:,:,1])
    plt.show()
    fig.savefig('ddop_vg.png', dpi=100)
    plt.close(fig)

    (u, v, eu, ev) = cedric.caluvw3d(volume1, volume2)

    eu[np.abs(eu*64)>=32768.] = -1000.
    ev[np.abs(ev*64)>=32768.] = -1000.
    print 'u:',u.min(),u.max()
    print 'v:',v.min(),v.max()    
    print 'eu:',eu.min(),eu.max()
    print 'ev:',ev.min(),ev.max()

    u[eu==-1000.] = -1000.
    v[ev==-1000.] = -1000.

    mz = dbz.data().copy()
    idx = dbz.data() < data2['DBZ']
    mz[idx] = data2['DBZ'][idx]
    grid1 = volume1.grid
    zz = np.arange(grid1.z[2],dtype=np.float32)*grid1.z[1]+grid1.z[0]
    rho= np.exp(-0.1*zz)
    print 'rho',rho
    #fallspeed correction
    vt_correction(u, v, eu, ev, mz, rho, 1.5, 0.105, 0.4, -1000.)

    #compute convergence
    convg  = np.zeros(dbz.data().shape[0:3], dtype=np.float32, order='F')
    xydeli = np.asarray((1./grid1.x.delta, 1./grid1.y.delta), dtype=np.float32)
    print 'xydeli',xydeli
    for iz in range(convg.shape[2]):
        convg[:,:,iz] = cedric.pconvg(u[:,:,iz], v[:,:,iz], 3, xydeli, -1000.)

    bnd_value = 0.0
    dx = grid1.x[1]
    dy = grid1.y[1]
    dz = grid1.z[1]
    c2r= np.asarray((0.01, 25.), dtype=np.float32)
    mw = libcedric.ms3d(u, v, eu, ev, convg, rho, -1, bnd_value, dx, dy, dz, -1000., c2r)

    # converge
    for iz in range(convg.shape[2]):
        convg[:,:,iz] = cedric.pconvg(u[:,:,iz], v[:,:,iz], 3, xydeli, -1000.)

    convg[np.abs(convg*64)>=32768.] = -1000.
    WU = libcedric.intgrt3d(convg, rho, zz, dz/2. , 1, 0.1, 0.0, -1000.)
    WV = libcedric.intgrt3d(convg, rho, zz, dz/-2., 3, 0.1, 0.0, -1000.)

    u = np.ma.masked_outside(u, -100., 100.)
    v = np.ma.masked_outside(v, -100., 100.)

    plt.figure(figsize=(5, 5))
    Q = plt.quiver(u[::3,::3,1], v[::3,::3,1],scale=2000)
    plt.quiverkey(Q, .1, .1, 30., '20 $ms^{-1}$')
    plt.tight_layout()
    plt.savefig('ddop_quiver.png', dpi=100)
    plt.show()
    plt.close()

    cedric.quit()




