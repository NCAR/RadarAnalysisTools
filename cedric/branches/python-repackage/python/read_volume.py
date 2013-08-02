#! /usr/bin/python

import sys
import logging
logging.basicConfig(level=logging.DEBUG)

import cedric
import matplotlib.pyplot as plt


if __name__ == "__main__":

    filepath = "../testdata/spol/vol_20000629_233038_to_20000629_233433_SPOL_CRT.ced"
    if len(sys.argv) > 1:
        filepath = sys.argv[1]

    core = cedric.Cedric()
    v = core.read_volume(filepath)
    core.stats()
    fid = 1
    var = v.vars.get('MAXDB')
    if var:
        fid = var.id
    iz = int((2.5 - v.grid.z.first + (v.grid.z.delta/2.0)) / v.grid.z.delta)
    iz = 15
    field = core.fetchd(fid, iz)
    print(field)
    plt.contourf(field)
    plt.show()

    core.quit()




