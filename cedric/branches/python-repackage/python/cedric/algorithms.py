"Useful algorithms for processing CEDRIC data."

import numpy as np

def vt_correction(u, v, eu, ev, dbz, rho, param1, param2, param3, fillv):
    rho3d        = np.zeros_like(dbz)
    vt           = np.zeros_like(dbz)
    valid_dbz    = dbz!=fillv
    rho3d[:,:,:] = rho[np.newaxis,np.newaxis,:]
    vt[valid_dbz]= -1.*param1*np.power(10., dbz[valid_dbz]*param2/10.)*np.power(rho[0]/rho3d[valid_dbz], param3)
    valid_uv     = np.logical_and(u!=fillv, v!=fillv)
    u[valid_uv] += vt[valid_uv]*eu[valid_uv]
    v[valid_uv] += vt[valid_uv]*ev[valid_uv]
    u[np.logical_not(valid_dbz)] = -1000.
    v[np.logical_not(valid_dbz)] = -1000.


