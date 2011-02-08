c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTHZ(JNDAT,ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,
     X     IDAY,IMON,IYR,NETWORK,IRATYP,ICORD,IVOLOLD,ITPOLD,NFRAME,
     X     FXVOL,NFXMAX,NFXVOL,BGFLAG,H0,R0,DROLD,MNGATE,MXGATE)
C
C  PLOT ACTUAL HORIZONTAL DISTANCES and HEIGHTS associated with the
C  angles scanned for the entire volume.
C
C     DATA RE,REI/17000.0,1.17647E-04/
C     Height above a curved earth: Z=H0+SRNG*SINE+0.5*HRNG*HRNG*REI
C        where   H0   - height of the antenna feed
C              SRNG   - slant range
C              SINE   - sin(elevation angle)
C              HRNG   - horizontal range = SRNG*COSE
C              Rprime - 4/3 earth radius = 4/3 (6375) = 8500
C                       where Earth's radius is 6375 km from 
C                       Doviak and Zrnic, p. 15 Fig. 2.7
C              REI    - 1/Rprime = 1.17647E-04
C              RE     - 2*Rprime = 2*8500 = 17000
C
C     HMIN,HMAX - MIN/MAX PLOT BOUNDARIES FOR Horizontal distance from
C                 the radar
C     ZMIN,ZMAX - MIN/MAX PLOT BOUNDARIES FOR Heights (MSL) above a
C                 curved Earth.
C
C     ZMN_GRD,ZMX_GRD,ZD_GRD - Minimum, maximum, and spacing for
C                 the vertical (height) levels.
C
C     NFXVOL    - NUMBER OF FIXED ANGLES IN THE CURRENT VOLUME SCAN
C
      INCLUDE 'colors.inc'
      CHARACTER LAB*80,LABLS*3,IFMTX*6,IFMTY*6
      CHARACTER*3 MONTH(12),ISCTP(8)
      CHARACTER*8 JNDAT(10),NETWORK,IRATYP,ICORD
      CHARACTER*1 BGFLAG,FXFLAG
      CHARACTER*6 SMRK,LAB6
      CHARACTER*8 LAB8

      LOGICAL COLRFIL,PLTSW
      DATA XRT,YTP,SIDE/0.92,0.93,0.82/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     +           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA TORAD,TODEG,PI/0.017453293,57.29577951,3.141592654/
      DATA RE,REI/17000.0,1.17647E-04/
      DATA BWIDTH/0.5/

      DIMENSION AZVOL(NBMAX),ELVOL(NBMAX),FXVOL(NFXMAX)
      DIMENSION X(5),Y(5)


       CSIZ=10.0
c      SMRK='&KRL&E'
c      WRITE(LAB6,13)SMRK
c 13   FORMAT(A6)
c      CALL PLCHHQ (HRNG,ZFLT,LAB6,CSIZ,0.0,0.0)

      READ(JNDAT,5)HMIN,HMAX,ZMIN,ZMAX,ZMN_GRD,ZMX_GRD,ZD_GRD,
     +     HRNGMN,HRNGMX
 5    FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(HMIN.LE.0.0)HMIN=0.0
      IF(HMAX.LE.0.0)HMAX=150.0
      IF(ZMIN.LE.0.0)ZMIN=0.0
      IF(ZMAX.LE.0.0)ZMAX=15.0
      IF(ZMN_GRD.LT.ZMIN)ZMN_GRD=ZMIN
      IF(ZMX_GRD.GT.ZMAX)ZMX_GRD=ZMAX
      NZ=1+(ZMX_GRD-ZMN_GRD)/ZD_GRD

      IF(HMAX.LE.HMIN)THEN
         HMIN = 0.0
         HMAX = 150.0
      END IF
      IF(ZMAX.LE.ZMIN)THEN
         ZMIN = 0.0
         ZMAX = 15.0
      END IF
c      print *,'PLTHZ: hmin,hmax=',hmin,hmax
c      print *,'PLTHZ: zmin,zmax,fxflag=',zmin,zmax,fxflag
c      print *,'PLTHZ: ho,ro,drold=',h0,r0,drold
c      print *,'PLTHZ: mngate,mxgate=',mngate,mxgate
c      print *,'PLTHZ: nfxvol=',nfxvol
c      print *,'PLTHZ: zgrd=',zmn_grd,zmx_grd,zd_grd
c      print *,'PLTHZ: hgrd=',hrngmn,hrngmx
      
      CALL GSPLCI(1)
      CALL GSTXCI(1)

      X2=XRT
      X1=XRT-SIDE
      Y1=YTP-SIDE
      Y2=YTP
      CALL MAJMIN(HMIN,HMAX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(ZMIN,ZMAX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,HMIN,HMAX,ZMIN,ZMAX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

C     Get default line thickness (ILW) and reset to JLW.
C     Restore line thickness before leaving routine.
C
c      CALL GETUSV('LW',ILW)
c      JLW=1000
c      CALL SETUSV('LW',JLW)

C     Box centered on current slant range and elevation angle
C     Box dimensions:
C         Current range gate (+/-) DELR=0.5*range gate spacing
C         Current azimuth    (+/-) Bwidht =0.5*nominal beamwidth
C
C        >---4---+---3--->
C            |       |
C        >---6---c---7--->  c = current (R,E)
C            |       |
C        >---1---+---2--->

C     Plot HZ for (Fixed angle - BWIDTH), where 
C     BWIDTH is one-half the nominal beamwidth of one deg
C
      DELR=0.5*DROLD
      MNR=1
      MXR=INT(HMAX/DROLD)
      DO J=1,NFXVOL,1
         Ec=FXVOL(J)
         E1=Ec-Bwidth
         E2=Ec-Bwidth
         E3=Ec+Bwidth
         E4=Ec+Bwidth
         E5=E1
         E6=Ec
         E7=Ec
         print *,'PLTHZ: J,E1,Ec,E3=',j,E1,Ec,E3
         SINc=SIN(Ec*TORAD)
         COSc=COS(Ec*TORAD)
         SIN1=SIN(E1*TORAD)
         COS1=COS(E1*TORAD)
         SIN2=SIN(E2*TORAD)
         COS2=COS(E2*TORAD)
         SIN3=SIN(E3*TORAD)
         COS3=COS(E3*TORAD)
         SIN4=SIN(E4*TORAD)
         COS4=COS(E4*TORAD)
         SIN5=SIN1
         COS5=COS1
         SIN6=SIN(E6*TORAD)
         COS6=COS(E6*TORAD)
         SIN7=SIN(E7*TORAD)
         COS7=COS(E7*TORAD)

C     Fill the area bounded by 
         DO I=MNR,MXR
            Sc=Rc+(I-1)*DROLD
            Hc=Sc*COSc            
            Zc=H0+Sc*SINc+0.5*Hc*Hc*REI
            S1=Sc-DELR
            H1=S1*COS1
            Z1=H0+S1*SIN1+0.5*H1*H1*REI
            S2=Sc+DELR
            H2=S2*COS2
            Z2=H0+S2*SIN2+0.5*H2*H2*REI
            S3=Sc+DELR
            H3=S3*COS3
            Z3=H0+S3*SIN3+0.5*H3*H3*REI
            S4=Sc-DELR
            H4=S4*COS4
            Z4=H0+S4*SIN4+0.5*H4*H4*REI
            S5=S1
            H5=H1
            Z5=Z1
            S6=Sc-DELR
            H6=S6*COS6
            Z6=H0+S6*SIN6+0.5*H6*H6*REI
            S7=Sc+DELR
            H7=S7*COS7
            Z7=H0+S7*SIN7+0.5*H7*H7*REI
            IF((H1.GE.HMIN .AND. H1.LE.HMAX) .AND.
     +         (Z1.GE.ZMIN .AND. Z1.LE.ZMAX) .AND.
     +         (H2.GE.HMIN .AND. H2.LE.HMAX) .AND.
     +         (Z2.GE.ZMIN .AND. Z2.LE.ZMAX) .AND.
     +         (H3.GE.HMIN .AND. H3.LE.HMAX) .AND.
     +         (Z3.GE.ZMIN .AND. Z3.LE.ZMAX) .AND.
     +         (H4.GE.HMIN .AND. H4.LE.HMAX) .AND.
     +         (Z4.GE.ZMIN .AND. Z4.LE.ZMAX))THEN
               X(1)=H1
               Y(1)=Z1
               X(2)=H2
               Y(2)=Z2
               X(3)=H3
               Y(3)=Z3
               X(4)=H4
               Y(4)=Z4
               X(5)=H5
               Y(5)=Z5
               CALL FAREA(IGRAY-1,X,Y,5)
            ENDIF
            CALL SFLUSH
            CALL GSPLCI(1)
            IF((H1.GE.HMIN .AND. H1.LE.HMAX) .AND.
     +         (Z1.GE.ZMIN .AND. Z1.LE.ZMAX) .AND.
     +         (H2.GE.HMIN .AND. H2.LE.HMAX) .AND.
     +         (Z2.GE.ZMIN .AND. Z2.LE.ZMAX))THEN
               CALL LINE(H1,Z1,H2,Z2)
            ENDIF
            IF((H3.GE.HMIN .AND. H3.LE.HMAX) .AND.
     +         (Z3.GE.ZMIN .AND. Z3.LE.ZMAX) .AND.
     +         (H4.GE.HMIN .AND. H4.LE.HMAX) .AND.
     +         (Z4.GE.ZMIN .AND. Z4.LE.ZMAX))THEN
               CALL LINE(H3,Z3,H4,Z4)
            ENDIF
            CALL SFLUSH
            CALL GSPLCI (IRED)
            IF((H6.GE.HMIN .AND. H6.LE.HMAX) .AND.
     +         (Z6.GE.ZMIN .AND. Z6.LE.ZMAX) .AND.
     +         (H7.GE.HMIN .AND. H7.LE.HMAX) .AND.
     +         (Z7.GE.ZMIN .AND. Z7.LE.ZMAX))THEN
               CALL LINE(H6,Z6,H7,Z7)
               XF=H7
               YF=Z7
               IF((XF.LT.HMAX) .AND. (YF.GE.ZMAX-DROLD))YF=YF+2.0*DROLD
               IF((YF.LT.ZMAX) .AND. (XF.GE.HMAX-DROLD))XF=XF+2.0*DROLD
            ENDIF
            CALL SFLUSH
         ENDDO

         WRITE(LAB8,17)FXVOL(J)
 17      FORMAT(F8.2)
         CALL PLCHMQ (XF,YF,LAB8,CSIZ,0.0,0.0)
      ENDDO
      CALL SFLUSH

C     Add GRID heights as dashed lines
C
c     CALL DASHDB (O'116347')
      CALL DASHDB (O'070707')

      CALL GSPLCI (1)

C     Draw slant range lines at HRNGMN and HRNGMX
C
      Ef=FXVOL(1)
      SINf=SIN(Ef*TORAD)
      COSf=COS(Ef*TORAD)
      Sf=0.5*(HRNGMX+HRNGMN)
      Hf=Sf*COSf            
      Zf=H0+Sf*SINf+0.5*Hf*Hf*REI
      X1=Hf
      Y1=Zf
      DO J=2,NFXVOL,1
         Ef=FXVOL(J)
         SINf=SIN(Ef*TORAD)
         COSf=COS(Ef*TORAD)
         Hf=Sf*COSf            
         Zf=H0+Sf*SINf+0.5*Hf*Hf*REI
         X2=Hf
         Y2=Zf
         IF(Y2.GT.ZMAX)THEN
            Y2=ZMAX
         ENDIF
         print *,'PLTHZ: x1,y1,x2,y2=',x1,y1,x2,y2
         CALL LINE(X1,Y1,X2,Y2)
         X1=X2
         Y1=Y2
      ENDDO

      IF(NZ.GE.1)THEN
         DO K=1,NZ
            Z=ZMN_GRD+(K-1)*ZD_GRD
c            CALL LINED(HMIN,Z,HMAX,Z)
            CALL LINE(HMIN,Z,HMAX,Z)
         END DO
      ENDIF

      CALL LINE(HRNGMN,ZMIN,HRNGMN,ZMAX)
      CALL LINE(HRNGMX,ZMIN,HRNGMX,ZMAX)

C     Calculate elevation angle at HRNGMN and N=NZ/2 of
C     the vertical (height) grid
C     ZMN_GRD,ZMX_GRD,ZD_GRD - Minimum, maximum, and spacing forC
C
c      N=NZ/2
c      Z_grd=ZMN_GRD+(N-1)*ZD_GRD
c      H_grd=HRNGMN
c      Elev=ASIN((Z1-H0-0.5*H1*H1*REI)/S1)

      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      WRITE (LAB,31)IDAY,MONTH(IMON),IYR,NETWORK,IRATYP,ICORD,
     +     ISCTP(ITPOLD)
 31   FORMAT(I2,1X,A3,1X,I2.2,2X,A8,2X,A8,'ORIGIN=',A8,2X,A4)
      XP=XRT-SIDE
      YP=0.980
      CALL PLCHMQ (XP,YP,LAB,12.0,0.0,-1.0)
      ITM1=ITIMBOV
      IHR1=ITM1/10000
      IMN1=(ITM1-IHR1*10000)/100
      ISEC1=ITM1-IHR1*10000-IMN1*100
      ITM2=ITIMEOV
      IHR2=ITM2/10000
      IMN2=(ITM2-IHR2*10000)/100
      ISEC2=ITM2-IHR2*10000-IMN2*100
      WRITE(LAB,35)IHR1,IMN1,ISEC1,IHR2,IMN2,ISEC2,NBVOL
 35   FORMAT(I2,':',I2,':',I2,' TO ',I2,':',I2,':',I2,4X,
     +     'Number of beams in the volume ',I5)
      XP=XRT-SIDE
      YP=YP-0.02
      CALL PLCHMQ (XP,YP,LAB,12.0,0.0,-1.0)

      XP=0.5*(X1+X2)
      YP=Y1-0.05
c     CALL PLCHMQ (XP,YP,'AZIMUTH ANGLE (DEG)',12.0,0.0,0.)
      CALL PLCHMQ (XP,YP,'HORIZONTAL DISTANCE (KM)',12.0,0.0,0.)
      XP=X1-0.05
      YP=0.5*(Y1+Y2)
c     CALL PLCHMQ (XP,YP,'ELEVATION ANGLE (DEG)',12.0,90.0,0.)
      CALL PLCHMQ (XP,YP,'HEIGHT MSL (KM)',12.0,90.0,0.)
      LABLS='   '
      CALL MYFRAME(NFRAME,NPLT,Y1,LABLS)

      RETURN
      END




