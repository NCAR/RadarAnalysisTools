c      COMMON DATA DECLARATION
       parameter (ndim=1200)
       real zh(ndim),zdr(ndim),phi_raw(ndim),rho(ndim)
       real range(ndim),phi_int(ndim),kdp(ndim)
       real plfile(-5:ndim,0:70),corr_height(-5:ndim)
       common /INIT/ range,zh,zdr,phi_raw,rho,phi_int,kdp
       common /PACK/ plfile,corr_height
