c
c----------------------------------------------------------------------X
c
      SUBROUTINE GAM_DSD(DAT,IOUT,IIN1,IIN2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF,NSCTP)
C
C  FUNCTION - Driver for computing parameters for drop size distribution
C             using Z and Zdr in linear units
C             Gamma DSD: N(D) = No*(D**mu)*exp(-lambda*D)
C             Units: [N(D)]   = #/m^3 per mm^-1
C                    [ mu ]   = dimensionless
C                    [lambda] = mm^-1
C     Returns six fields invoked by DSDrain:  See also FIELD and SAVFUN
C             DAT(I,J,IOUT  )=RAIN
C             DAT(I,J,IOUT+1)=KDP
C             DAT(I,J,IOUT+2)=ALOG10(N0)
C             DAT(I,J,IOUT+3)=MU
C             DAT(I,J,IOUT+4)=LAM
C             DAT(I,J,IOUT+5)=ALOG10(NT)
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -    "     "      "  (THRESHOLDING ON THIS FIELD)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      REAL N0,LAM,MU,RAIN,KDP,NT
      INTEGER TF
      CHARACTER*8 NSCTP

      DIMENSION DAT(MXR,MXA,MXF)

c      print *,'Gam_dsd: iout=',iout,' iin12=',iin1,iin2
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            DAT(I,J,IOUT+1)=BDVAL
            DAT(I,J,IOUT+2)=BDVAL
            DAT(I,J,IOUT+3)=BDVAL
            DAT(I,J,IOUT+4)=BDVAL
            DAT(I,J,IOUT+5)=BDVAL
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            IF(DATIN2.EQ.BDVAL)GO TO 90
            IF(DATIN1.EQ.BDVAL)GO TO 90
            REF=10.0**(0.1*DATIN1)
            ZDR=10.0**(0.1*DATIN2)
            CALL RDSD(REF,ZDR,TF,N0,MU,LAM,NT,RAIN,KDP)
c            write(6,1770)j,i,datin1,datin2,tf,n0,nt,mu,lam,rain,kdp
c 1770       format('R-dbz,zdr=',2i6,2f6.2,' tf=',i2,' No=',e14.4,
c     x           ' Nt,mu,lam=',3f8.2,' rain,kdp=',2f8.1)
            IF(TF.EQ.1)THEN
               DAT(I,J,IOUT  )=RAIN
               DAT(I,J,IOUT+1)=KDP
               DAT(I,J,IOUT+2)=ALOG10(N0)
               DAT(I,J,IOUT+3)=MU
               DAT(I,J,IOUT+4)=LAM
               DAT(I,J,IOUT+5)=ALOG10(NT)
            END IF
 90      CONTINUE
 100  CONTINUE
      RETURN
      END
c----------------------------------------------------------------------X
      subroutine rdsd(z, zdr, tf, n0, mu, lam, nt, rain, kdp)
C
C     Routine to calculate rainfall rate based on the gamma DSD.
C     The DSD parameters are determined from Z and Zdr
C     
C     Inputs:  
C           Z   - radar reflectivity factor 
C           Zdr - differential reflectivity (Zh/Zv)
C     Outputs: 
C           tf - (0) no solution, or (1) valid solution was found
C           n0, mu, lam - parameters of gamma function
C           nt = n0*Gamma(mu+1)/lam(mu+1) - droplet concentration #/m^-3
C           rain - rainrate in mm/hr
C           kdp  - differential propagation phase in deg/km.
C
      real ala, alb, alf, bta, btb, btf
      real N0, lam, mu, rain, kdp, Nt
      real mu1, mu2,fp,fc
      real wave, K2, fhh2, pi
      integer j, tf
      complex ix, jx, epr 
      external gammln, funcmu
      data ala,alb,alf,bta,btb,btf
     x     /4.26e-4,4.76e-4,1.33e-5,3.02,2.69,4.61/
      
      ix=(1.0,0.0)
      jx=(0.0,1.0)
      pi=3.1415926
      epr=80.205+jx*17.167
      K2=cabs((epr-1)/(epr+2))**2
      wave=107.0
      jt=80
      mu1=-1.0
      mu2=15.0
      dmu=(mu2-mu1)/float(jt)
      mu=mu1
      tf=0
      do 100 j=1,jt
         fp=funcmu(mu,zdr)
         mu=mu+dmu
         fc=funcmu(mu,zdr)
         if(fc*fp.lt.0.0) then
            mu=mu-abs(fc/(fc-fp))*dmu
            tf=1 
            goto 200
         endif
 100  continue
 200  continue
      if (tf.eq.0) then
         N0=-99.
         mu=-99.
         lam=-99.
         NT=-99.
         rain=6.84e-3*Z/zdr**(4.86)
         kdp=4.4e-5*Z**1.155/zdr**5.612
      else 
         ga=exp(gammln(2*bta+mu+1.))
         gb=exp(gammln(2*btb+mu+1.))
         lam=(1.213-(1.213**2-0.064*(mu+1.957))**0.5)/0.032
         fhh2=Z*pi**4*K2/4.0/wave**4
         D1=ala*ala/lam**(mu+2.0*bta+1)*ga
         N0=fhh2/(D1)
         NT=N0*exp(gammln(mu+1.))/lam**(mu+1.)
         gr=exp(gammln(3.67+mu+1.))
         rain=7.125132e-3*N0*gr/(lam**(3.67+mu+1.))
         C1=wave*alf*exp(gammln(mu+btf+1.))
         kdp=N0*C1/lam**(mu+btf+1)*180/pi/1e+3
      endif
c      write(18,*) Z, zdr,N0,mu,lam,NT,rain,kdp	
      return
      end
c----------------------------------------------------------------------X  
      function gammln(xx)
      real*8 cof(6),stp,half,one,fpf,x,tmp,ser
      data cof,stp/76.18009173D0,-86.50532033D0,24.01409822D0,
     x     -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      data half,one,fpf/0.5D0,1.0D0,5.5D0/
      x=xx-one
      tmp=x+fpf
      tmp=(x+half)*log(tmp)-tmp
      ser=one
      do 11 j=1,6
         x=x+one
         ser=ser+cof(j)/x
 11   continue
      gammln=tmp+log(stp*ser)
      return
      end
c----------------------------------------------------------------------X	
      function funcmu(mu,zdr)
      real mu,zdr, lam
      real ala, alb, bta, btb
      external gammln
      data ala,alb,bta,btb/4.26e-4,4.76e-4,3.02,2.69/
      
      ah2=ala*ala
      av2=alb*alb
      lam=(1.213-(1.213**2-0.064*(mu+1.957))**0.5)/0.032
      if(lam.ge.0.0) then
         ga=exp(gammln(2*bta+mu+1))
         gb=exp(gammln(2*btb+mu+1))
         cc=1.0/(lam**(2.0*(bta-btb)))
         funcmu=zdr-ah2/av2*cc*ga/gb
      end if
      return
      end
      
