      SUBROUTINE VADDRV(KRD,IBUF,OBUF,RBUF,SBUF,MAXBSZ,IPR,NST,
     X     KVD,NAMINF,NAMOUF,NAMDBZ)
C
C     Driver for VAD analyses of gridded radial velocity field
C     Adapted from /users/ljmill/Cedric-new/FILTER.f
C
C          IBUF- SCRATCH BUFFER F0R I/O
C          OBUF- AUXILLIARY BUFFER FOR DATA MANIPULATION
C          RBUF- I/O DATA BUFFER
C        MAXPLN- MAXIMUM DIMENSION OF RBUF,OBUF
C           IPR- PRINT FILE UNIT NUMBER
C           NST- STATUS FLAG:  0- O.K.
C
      INCLUDE 'CEDRIC.INC'
      INCLUDE 'vadwinds.inc'
      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXBSZ),
     X          NAX(3),SBUF(MAXBSZ)
      CHARACTER*8 KRD(10),COM
      CHARACTER*1 NPRNT
      CHARACTER*8 TYPE
      CHARACTER*1 IFAX,IWOP
C
C     CONEXT - (1) +X-axis angle (deg), (2) Origin X (km), (3) Origin Y (km)
C     CONA   - (1) X-coord (km), (2) Y-coord (km), (3) Z-coord (km)
C
      PARAMETER (MAXCON=4) 
      DIMENSION CONA(MAXCON),CONEXT(MAXCON)
C
C     Arrays for range-ring datasets (GETRINGS).
C
      DIMENSION SRNG(MAXX,MAXY),AZM(MAXX,MAXY)
      DIMENSION RADVEL(MAXX,MAXY),REFLEC(MAXX,MAXY)

      PARAMETER (MXR=512,MXA=400)
      DIMENSION RNG(MXR),Rring(MXR,MXA),Aring(MXR,MXA),NANG(MXR)
      DIMENSION VDAT(MXR,MXA),ZDAT(MXR,MXA)
C
C     COMMON block variables returned from LAT_LON or uses defaults.
C
      COMMON /HEMISPHERE/LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN,LATLON
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      LOGICAL LATLON

      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,NAMINF(4),NAMOUF(4),NAMDBZ(4),IBL
      COMMON /EDINFO/ IEDW(2,3),PEDW(2,3)
      LOGICAL NEWJ,MISV
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      EQUIVALENCE (NAX(1),I1),(NAX(2),I2),(NAX(3),I3)
      DATA MISV/.TRUE./
      DATA IBL/'  '/

C     INITIALIZATION OF ANALYSIS PARAMETERS
C
C     CNT    - MINIMUM NUMBER OF GOOD DATA POINTS FOR VAD ANALYSIS
C     GAP    - MAXIMUM ALLOWED AZIMUTH GAP         "   "      "
C     RMSERR -    "       "    RMS DIFFERENCE BETWEEN INPUT AND VAD WINDS
C     KFIT   - ORDER OF FIT [LINEAR INCLUDES A(1), A(2), B(1), B(2)]
C     NPRNT  - PRINT FLAG FOR VAD WINDS ('    ') NO, ('PRNT') YES,
C              ('FILE') YES and COEFFICIENTS TO ASCII FILE (fort.999).
C     NAMDBZ - Name of DBZ field to be averaged around an azimuth circle.
C
      DXY = AMAX1(CSP(3,1),CSP(3,2))
      READ (KRD,11)COM,NAMOUF,NAMINF,NAMDBZ,TYPE,CNT,GAP,RMSERR,
     X     IFAX,IWOP
 11   FORMAT(A8/4A2/4A2/4A2/A8/F8.0/F8.0/F8.0/A1/A1)
      VTYPE = TYPE
 20   CALL KARDIN(KRD)
      READ (KRD,21)RFIT,RBEG,REND,RDEL,NPRNT
 21   FORMAT(/F8.0/F8.0/F8.0/F8.0/A1)
      IF(KRD(1)(1:1).EQ.'*')GO TO 20
      IF(KRD(1)(1:3).EQ.'END')GO TO 40
      IF(KRD(1).NE.'        ')THEN
         WRITE(6,23)
 23      FORMAT(1X,'*** VAD: NO END LINE ENCOUNTERED ***')
         STOP
      END IF
 40   CONTINUE

      NR = INT(1.05 + (REND - RBEG)/RDEL)
      IF(NR.GT.MXR)THEN
         WRITE(6,43)MXR,NR,RBEG,REND,RDEL
 43      FORMAT(1X,'*** VADDRV: rbeg,rend,rdel=',3f8.3,/,
     X          1X,'*** VADDRV: Too many range rings - only ',
     X          I6,' allowed.',/,
     X          1X,'  Asked for ',I6,' *** ')
         STOP
      END IF
      KFIT = RFIT

C     GET3D: Build a 3-D dataset of a single field into the windowed region, 
C            inverse operation of PUT3D.
C
C            RBUF  - Contains the 3-D dataset upon return from GET3D
C            M1    - Number of X grid points in windowed region
C            M2    - Number of Y grid points in windowed region
C            M3    - Number of Z grid points in windowed region
C            BAD   - Missing data flag
C     COPRX: Copies one array to another
C
C     Get information about NAMINF = radial velocity field
C     Store 3-D field in SBUF
C
      NXY=NX*NY
      NXYZ=NX*NY*NZ
      KVD=NZ
      print *,'VADDRV NXYZ=',nx,ny,nz,nxy,nxyz
      print *,'VADDRV getting ',NAMINF
      CALL SHOEDF(IPR)
      IFLD=LOCFLDID(NAMINF,ID(176),5,NFL,4)
      IF(IFLD.EQ.0) THEN
         CALL CEDERX(501,1)
         RETURN
      END IF
c--------------------------------------------
C     Writing of radial velocity (NAMOUF) is not yet implemented.
c     comment out since not writing an output field
c      print *,'VADDRV checking ',NAMOUF
c      IF(NAMOUF(1).EQ.IBL) CALL COPCX(NAMOUF,NAMINF,4)
c      IV=MAPVID(IFLD,2)
c      ISCL=SCLFLD(IV)
c      CALL SHOEDF(IPR)
c      I=IADFLD(NAMOUF,ISCL,IPR)
c      IF (I.LT.0) RETURN
c      IF(I.EQ.0) THEN
c         NEWJ=.FALSE.
c      ELSE
c         NEWJ=.TRUE.
c         IFLD=LOCFLDID(NAMINF,ID(176),5,NFL,4)
c      END IF
c      JFLD=LOCFLDID(NAMOUF,ID(176),5,NFL,4)
c--------------------------------------------
      CALL GET3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X     RBUF,IEDW,NST)
      CALL COPRX(SBUF,RBUF,MAXBSZ)
c      print *,'DMPFLOAT SBUF-FLD nxy=',NAMINF,' ',nxy
c      CALL DMPFLOAT(SBUF,NXY)

C     Get information about NAMDBZ = reflectivity field
C     Store 3-D field in RBUF
C
      print *,'VADDRV getting ',NAMDBZ
      CALL SHOEDF(IPR)
      IFLD=LOCFLDID(NAMDBZ,ID(176),5,NFL,4)
      IF(IFLD.EQ.0) THEN
         CALL CEDERX(501,1)
         RETURN
      END IF
      IV=MAPVID(IFLD,2)
      ISCL=SCLFLD(IV)
      CALL GET3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
     X     RBUF,IEDW,NST)
c      print *,'DMPFLOAT RBUF-FLD nxy=',NAMDBZ,' ',nxy
c      CALL DMPFLOAT(RBUF,NXY)

C     Sets user supplied window arrays (IWIND,PWIND) either to full grid
C     (F) or to alternate window (W) depending upon the value of IWOP.
C     IWOP is parameter 10 on WINDOW command line.
C
      CALL WINSET(IEDW,PEDW,IWOP)
      CALL SETAXS(NAX,IFAX,IPR)
      N1=NCX(I1)
      N2=NCX(I2)
      N3=NCX(I3)
      NPLIN=N1*N2

C     INITIATE THE VAD ANALYSIS PROCESS
C
C     Send a summary of this command step to the print file
C        SHOEDW: Displays current WINDOW information
C        SHOEDF: Displays the current table of edit fields
C
      CALL SHOEDW(IPR)
      CALL SHOEDF(IPR)
C
C     Print VAD command parameters
C-------*-------*-------*-------*-------*-------*-------*-------*-------*-------
C       NAMOUT  NAMIN1  NAMDBZ  TYPEff.fCNT     GAP     RMSERR  KFIT   FFULL
C-------*-------*-------*-------*-------*-------*-------*-------*-------*-------
C
      WRITE(IPR,115) (NAMINF(I),I=1,4),(NAMOUF(I),I=1,4),
     X     (NAMDBZ(I),I=1,4),TYPE,CNT,GAP,RMSERR,FLOAT(KFIT),
     X     RBEG,REND,RDEL
 115  FORMAT(
     X     /,4X,'  ++++++++ROUTINE VADDRV+++++++++  ',/,
     X     /,4X,'  ++++VAD ANALYSIS PARAMETERS++++  ',/,
     X     /,4x,'Input Field: ',4A2,' Output Field: ',4A2,
     X     /,4x,'Requested reflectivity Field      : ',4A2,
     X     /,4X,'Type (Fourier series or LstSqr)   : ',A8,
     X     /,4X,'Minimum number of good data points: ',F8.0,
     X     /,4X,'Maximum allowed azimuthal gap     : ',F8.0,
     X     /,4X,'Maximum allowed rms difference    : ',F8.0,
     X     /,4X,'Order of Fourier series fit       : ',F8.0,
     X     /,4X,'Range rings from ',f6.1,' to ',f6.1,
     X          ' km - width ',f6.3,' km')

      PRINT *,'++++++++++++++++++++++++++++++++++++++++++++++++++++'
      PRINT *,'++ WARNING -- output field is not written to disk ++'
      PRINT *,'++ Calculate radial velocity with VADFLD function ++'
      PRINT *,'++++++++++++++++++++++++++++++++++++++++++++++++++++'

C     Note: Check if sufficient storage for windowed region
C     
      M1=IEDW(2,1)-IEDW(1,1)+1
      M2=IEDW(2,2)-IEDW(1,2)+1
      M3=IEDW(2,3)-IEDW(1,3)+1
c      print *,'m1,m2,m3,m1*2*3=',m1,m2,m3,m1*m2*m3
      IF(M1*M2*M3.GT.MAXBSZ) THEN
         WRITE(IPR,118) M1,M2,M3,MAXBSZ
 118     FORMAT(/5X,'+++  (X,Y,Z) WINDOWED REGION:  (',I3,',',I3,
     X        ',',I3,')'/10X,'MAY NOT CONTAIN MORE THAN ',I7,
     X        ' POINTS  +++')
         CALL CEDERX(518,1)
         RETURN
      END IF
      
C     VAD analysis steps: 
C         1. VADCOORD - Compute radar coordinates (R,A,E) for 
C            regular grid points (XYE or LLE).
C         2. VADRINGS - Extract datasets within each of NR range 
C            rings.
C         3. VAD - Calculate wind parameters.
C
C     SBUF --> RADVEL and RBUF --> REFLEC
C     RADVEL - Input 2-D radial velocity data set to be analyzed.
C     REFLEC - Input 2-D reflectivity data set to be analyzed.
C     Want to input MXR and MXA into VAD rather than M1-M3.
C
      DO J=1,MAXY
         DO I=1,MAXX
            RADVEL(I,J)=BAD
            REFLEC(I,J)=BAD
         END DO
      END DO
      DO I=1,MXR
         RNG(I)=BAD
         NANG(I)=BAD
         DO J=1,MXA
            Rring(I,J)=BAD
            Aring(I,J)=BAD
            VDAT(I,J)=BAD
            ZDAT(I,J)=BAD
         END DO
      END DO

      R0 = RBEG
      DR = RDEL
      DO I = 1, NR
         RNG(I) = R0 + (I-1)*DR
      END DO
c      ORGLAT=ID(33-35)
c      ORGLON=ID(36-38)
c      ZORG=0.001*ID(39)

C     CONEXT - (1) +X-axis angle (deg), (2) Origin X (km), (3) Origin Y (km)
C     CONA   - (1) X-coord (km), (2) Y-coord (km), (3) Z-coord (km)
C
      SF=1./FLOAT(ID(68))
      CONA(1)=SF*FLOAT(ID(315))
      CONA(2)=SF*FLOAT(ID(316))
      CONA(3)=0.001*FLOAT(ID(317))
      CONA(4)=0.0
      CF=1./FLOAT(ID(69))
      CONEXT(1)=CF*FLOAT(ID(40))
      CONEXT(2)=SF*FLOAT(ID(309))
      CONEXT(3)=SF*FLOAT(ID(310))

      IFLAT=1
      DO LEV = 1,NZ
         print *,'VADDRV: vad level=',lev
         CALL VADCORD(LEV,CONA,CONEXT,IFLAT,LATLON,
     X        HRNG,SRNG,AZM,ELEV,MAXX,MAXY)
         DO J = 1,NY
            DO I = 1,NX
               N = I + (J-1)*NX + (LEV-1)*NXY
               RADVEL(I,J)=SBUF(N)
               REFLEC(I,J)=RBUF(N)
            END DO
         END DO
c         print *,'DMPFLOAT-RADVEL, nxy=',nxy
c         CALL DMPFLOAT(RADVEL,NXY)
c         print *,'DMPFLOAT-REFLEC, nxy=',nxy
c         CALL DMPFLOAT(REFLEC,NXY)
c         print *, 'VADDRV: nr=',nr
         CALL VADRINGS(RADVEL,REFLEC,SRNG,AZM,ELEV,MAXX,MAXY,NX,NY,
     X        RNG,NANG,Rring,Aring,VDAT,ZDAT,MXR,MXA,NR,DR)
c         DO I=1,NR,10
c            print *,'Ring #',I,' Nang=',nang(i)
c            DO J=1,NANG(I)
c               write(6,1700)J,RNG(I),NANG(I),Rring(I,J),
c     +              Aring(I,J),VDAT(I,J),ZDAT(I,J)
c 1700          format(1x,'J=',i4,' Ring,n=',f8.2,I4,' RA=',2f8.3,
c     +              ' VZ=',2f10.3)
c            END DO
c         END DO
c         print *,'After vadrings'
         FXOLD = VALLEV(LEV)
         X0=CONA(1)
         Y0=CONA(2)
         Z0=CONA(3)
         CALL VAD(LEV,VDAT,ZDAT,MXR,MXA,RNG,Rring,Aring,NANG,
     X        BAD,FXOLD,NAMINF,NAMOUF,TYPE,CNT,GAP,RMSERR,RFIT,
     X        NPRNT,X0,Y0,H0,VNYQ_VOL)
      END DO

C     Writing of radial velocity (NAMOUF) is not yet implemented.
C     PUT3D: Places the data from a 3-D windowed region back onto the
C            external storage medium.  Inverse operation of GET3D.
C
c      CALL PUT3D(IEDFL,IBUF,OBUF,NX,NY,NZ,IFLD,JFLD,NEWJ,MISV,
c     X     RBUF,IEDW,NST)
C
      NST=0
      RETURN
      END
