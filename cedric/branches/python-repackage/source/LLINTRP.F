      SUBROUTINE LLINTRP(IROT,ITRANS,INWGRD,XOR,YOR,ZOR,CSPN,NCXN,
     X     NPLNEW,ANGXAX,ANGUSR,WXYC,IXYC,RELMAX,LCMB2,
     X     ISPEC,VNYQ,RTOP,RBOT,ROUT,ITEMP,IBUF,IDIM2,
     X     MEMUSE,ICORD,DE)
C     
C     THIS SUBROUTINE IS THE DRIVER FOR THE INTERPOLATION OF DATA
C     FROM EITHER CONSTANT ELEV OR CONSTANT HEIGHTS IN CARTESIAN OR
C     LON-LAT SPACE TO CARTESIAN OR LON-LAT SPACE.
C     
C-----Two-dimensional interpolation within constant elev or height surfaces.
C     ICORD: (4) ELE (XYE) --> LLE
C            (5) CRT (XYZ) --> LLZ
C            (6) LLE       --> ELE (XYE)
C            (7) LLZ       --> CRT (XYZ)
C
C     Note: VALLEV from the level header contains elevation angles for each
C           level.  Equally-spaced levels are implied by CSP(1,3), CSP(2,3), 
C           and CSP(3,3).  For coplane and elevation, the dimensions of the
C           levels is deg not km.  For coplane (x,y) are in the coplanes, not
C           at cartesian (x,y) as is the case for both cartesian and elevation.
C
C     CSP     -  OLD GRID SPECIFICATION
C     NCX     -  NUMBER OF X,Y, AND Z GRID POINTS IN OLD GRID
C     ANGUSR  -  ANGLE OF OLD X-AXIS, RELATIVE TO TRUE NORTH
C
C     INWGRD  -  FLAG INDICATING NEW GRID
C     IROT    -  FLAG INDICATING ROTATION
C     ITRANS  -  FLAG INDICATING TRANSLATION
C     XOR,YOR -  NEW ORIGIN SPECIFIED IN OLD COORDINATE SYSTEM
C     CSPN    -  NEW GRID SPECIFICATION
C     NCXN    -  NUMBER OF X,Y, AND Z GRID POINTS IN NEW GRID
C     NPLNEW  -  NUMBER OF X-Y GRID POINTS IN A Z PLANE OF NEW COORD. SYSTM.
C     ANGXAX  -  ANGLE OF NEW X-AXIS, RELATIVE TO TRUE NORTH
C
C     WXYC    -  WILL STORE WEIGHTING INFORMATION FOR THE INTERP
C     IXYC    -  WILL STORE INFO FOR INTERPOLATION
C     RELMAX  -  MAX DISTANCE TO RELOCATE ANY POINT FOR CLOSEST POINT METHOD
C     IDIM2   -  0 ==> 3-D INTERP.  1 ==> 2-D INTERP.
C     MEMUSE  -  MEMORY USE FLAG FOR REMAP
C     
      INCLUDE 'CEDRIC.INC'
      COMMON LCMB(1)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      DIMENSION CSPN(3,3),NCXN(3),ITEMP(1)
      DIMENSION WXYC(MAXP,MAXP,3),IXYC(MAXP,MAXP,3),LCMB2(1),
     X          IBUF(1),ROUT(1),RTOP(1)
      DIMENSION RBOT(1)
      COMMON /SCRATCH_GRD/ XGRD(MAXX,MAXY),YGRD(MAXX,MAXY)

      REAL REFLAT,REFLON,LATY,LONX
      REAL XVAL,YVAL,X,Y
      DATA EPS/0.000001/
      DATA SF/100./
      DATA NBITS/16/
      INTEGER CVMGP

      SF=1./FLOAT(ID(68))
      CF=1./FLOAT(ID(69))
      ATR=ATAN(1.)/45.
 
C
CLATITUDE AND LONGITUDE AND XY POS OF THE RADAR
C
      REFLAT = ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68)))
      REFLON = ID(36)+(ID(37)/60.)+(ID(38)/(3600.*ID(68)))
      ANGXAX = ID(40)/REAL(ID(69))
      print *,'LLINTRP: lat  ddmmss=',id(33),id(34),id(35)
      print *,'         lon  ddmmss=',id(36),id(37),id(38)
      print *,' radr lat/lon,angxax=',reflat,reflon,angxax
     
C      
CGet OLD GRID MIN AND MAX VALUES
C
      X1=CSP(1,1)
      X2=CSP(2,1)
      XD=CSP(3,1)
      Y1=CSP(1,2)
      Y2=CSP(2,2)
      YD=CSP(3,2)
      Z1=CSP(1,3)
      Z2=CSP(2,3)
      ZD=CSP(3,3)

C     
C     CHECK TO MAKE SURE WE DON'T DIVIDE BY ANY ZEROS
C     
      IF (ZD.GT.0.0) THEN
         ZDI=1./ZD
      ELSE 
         ZDI=1.0
      END IF
      
      IF (XD.GT.0.0) THEN
         XDI=1./XD
      ELSE
         XDI=1.0
      END IF
      
      IF (YD.GT.0.0) THEN
         YDI=1./YD
      ELSE
         YDI=1.0
      END IF
      
      NX=NCXN(1)
      NY=NCXN(2)
      NZ=NCXN(3)
      write(*,*)'LLINTRP: '
      write(*,*)'old x=',(csp(i,1),i=1,3),ncx(1)
      write(*,*)'old y=',(csp(i,2),i=1,3),ncx(2)
      write(*,*)'old e=',(csp(i,3),i=1,3),ncx(3)
      write(*,*)'old e=',(vallev(i),i=1,ncx(3))
      write(*,*)' '
      write(*,*)'new x=',(cspn(i,1),i=1,3),ncxn(1)
      write(*,*)'new y=',(cspn(i,2),i=1,3),ncxn(2)
      write(*,*)'new z=',(cspn(i,3),i=1,3),ncxn(3)
      write(*,*)' '
C
C     LOOP OVER ALL POINTS IN NEW COORD. SYS. AND FIND COORD. IN OLD SYSTEM
C
      DO 10 I=1,NZ
         DO 20 J=1,NY
            DO 30 K=1,NX
               
               WXYC(K,J,1)=0.0
               WXYC(K,J,2)=0.0
               WXYC(K,J,3)=0.0
               IXYC(K,J,1)=0
               IXYC(K,J,2)=0
               IXYC(K,J,3)=0
               
               IF(ICORD.EQ.4 .OR. ICORD.EQ. 5) THEN

C New (output) grid is LON-LAT and old (input) grid is XY.
C Convert the output LON-LAT grid points to XY values
C and interpolate in XY space since the input grid is XY.

                  LONX = CSPN(1,1) + (K-1)*CSPN(3,1)
                  LATY = CSPN(1,2) + (J-1)*CSPN(3,2)
                  XNEW = LONX
                  YNEW = LATY
                  IF(I.EQ.1)THEN
                     CALL LL2XYDRV(LATY,LONX,X,Y,REFLAT,REFLON,
     X                    ANGXAX)
                     XGRD(K,J) = X
                     YGRD(K,J) = Y
                  ELSE
                     X = XGRD(K,J)
                     Y = YGRD(K,J)
                  END IF
                  XOLD = X
                  YOLD = Y
                  RL = (X-X1)*XDI+1.0+EPS
                  RM = (Y-Y1)*YDI+1.0+EPS
                  
               ELSE IF(ICORD.EQ. 6 .OR. ICORD.EQ.7) THEN

C New (output) grid is XY and old (input) grid is LON-LAT.
C Convert the output XY grid points to LON-LAT values
C and interpolate in LON-LAT space since the input grid is LON-LAT.

                  XVAL = CSPN(1,1) + (K-1)*CSPN(3,1)
                  YVAL = CSPN(1,2) + (J-1)*CSPN(3,2)
                  XNEW = XVAL
                  YNEW = YVAL
                  IF(I.EQ.1)THEN
                     CALL XY2LLDRV(YLAT,XLON,XVAL,YVAL,REFLAT,REFLON,
     X                    ANGXAX) 
                     XGRD(K,J) = XLON
                     YGRD(K,J) = YLAT
                  ELSE
                     XLON = XGRD(K,J)
                     YLAT = YGRD(K,J)
                  END IF
                  XOLD = XLON
                  YOLD = YLAT
                  RL = (XLON-X1)*XDI+1.0+EPS
                  RM = (YLAT-Y1)*YDI+1.0+EPS
               ENDIF 
               
C     CALCULATE WEIGHTING FACTORS AND INDICES FOR BILINEAR INTERPOLATION
C     Since this is a two-dimensional, quasi-horizontal interpolation, we set
C     the output vertical index to the input vertical index (I) and the vertical 
C     weighting to 0.

               L = INT(RL)
               M = INT(RM)
               RN = I
               WXYC(K,J,1) = RL - INT(RL)
               WXYC(K,J,2) = RM - INT(RM)
               WXYC(K,J,3) = 0.0
               IXYC(K,J,1)=L
               IXYC(K,J,2)=M
               IXYC(K,J,3)=I
               
c--------------debug (ljm)
c               if( (j .eq. 1  .and. k .eq. 1) .or.
c     +             (j .eq. NY .and. k .eq. NX).or.
c     +             (mod(j,1) .eq. 0 .and. mod(k,1) .eq. 0) ) then
c                  wx=wxyc(k,j,1)
c                  wy=wxyc(k,j,2)
c                  wz=wxyc(k,j,3)
c                  write(*,1771)k,j,i,xnew,ynew,vallev(i),xold,yold,
c     +                 wx,wy,wz
c 1771             format(' KJI=',3i4,' new XYZ=',3f10.4,
c     +                 ' old XY=',2f10.4,' wxyz=',3f8.4)
c               end if  
c--------------debug (ljm)


 30         CONTINUE
 20      CONTINUE
         IDIM2 = 1
         CALL TRPCO(WXYC,IXYC,NX,NY,NZ,CSPN,I,RELMAX,ICORD,LCMB2,ISPEC,
     X        VNYQ,RTOP,RBOT,ROUT,IBUF,NCX(1),NCX(2),ITEMP,IDIM2,
     X        MEMUSE)
 10   CONTINUE

C     
C     VOLUME HAS BEEN REMAPPED. REDEFINE EDIT VOLUME CHARACTERISTICS AND
C     TRANSFER VOLUME TO EDIT SPACE
C     
      ID(451)=(NPLNEW-1)/(WORDSZ/16.0)+1
      ID(452)=NCXN(3)*ID(451)
      ID(400)=ID(452)
      
      NF=ID(175)
      NFL=ID(175)
      ID(160) = CSPN(1,1)
      DO 40 J=1,NF
         ITMP=MAPVID(J,2)
         DO 50 I=1,4
            NAMF(I,J)=NAMF(I,ITMP)
 50      CONTINUE
         SCLFLD(J)=SCLFLD(ITMP)
         ISETFL(J)=ISETFL(ITMP)
         MAPVID(J,1)=J
         MAPVID(J,2)=J
 40   CONTINUE
      
      DO 60 J=(NF+1),NFMAX
         MAPVID(J,1)=0
         MAPVID(J,2)=0
         SCLFLD(J)=0
         ISETFL(J)=0
         DO 70 I=1,4
            NAMF(I,J)=' '
 70      CONTINUE
 60   CONTINUE
      
      DO 80 I=1,NFMAX
         IRCP(I)=1+ID(400)*(I-1)
         ID(400+I)=IRCP(I)
 80   CONTINUE
      
      IF (MEMUSE.EQ.0) THEN
C     
C     EDIT VOLUME AND REMAP VOLUME ARE BOTH ON DISK; SWAP FILE POINTERS
C     
         CALL CFLSWAP()
      ELSE IF (MEMUSE.EQ.1) THEN
C     
C     EDIT VOLUME IN MEMORY, REMAP VOLUME ON DISK; TRANSFER TO MEMORY
C     
         NWL=ID(451)
         IBAD=ID(67)
         IBIT=0
         NSKIP=0
         NBITS=16
         DO 350 I=1,NFL
            DO 400 J=1,NZ
               SCALE=1./ID(175+I*5)
               LOCD=ID(400+I)
               NNPLANE=0
               NXX=NX
               FLNX=NX*16.0/WORDSZ
               INNX=INT(NX*16.0/WORDSZ)
               IDIFF = (FLNX - REAL(INNX))*(WORDSZ/16.0)
               IF (IDIFF.NE.0) THEN
                  NXX = NX + (WORDSZ/16.0) - IDIFF
                  NWL = (NXX*NY - 1)/(WORDSZ/16.0) + 1
                  LOCD = 1 + NWL*NZ*(MAPVID(I,2) - 1)
               END IF
               IF (MEMUSE.GE.1) THEN
                  MEM=1
               ELSE
                  MEM=0
               END IF
               IVOL=1
               N1=NX
               N2=NY
               NNPLANE=NXX*NY
               LOCD=LOCD+(J-1)*NWL
               CALL GETD(IBUF,NNPLANE,LOCD,IBIT,NBITS,NSKIP,NNPLANE,3,
     X              ID,NX,NY,NZ,J,ITEMP,NWL,LCMB2,MEM,IVOL)
               IF (IDIFF.NE.0) THEN
C     
C     UNDO PADDING
C     
                  K=1
                  L=1
                  DO 530 IJ=1,NY
                     DO 520 JJ=1,NX
                        ITEMP(L)=IBUF(K)
                        K=K+1
                        L=L+1
 520                 CONTINUE
                     K=K+(NXX-NX)
 530              CONTINUE
                  DO 540 IJ=1,(NY*NX)
                     IBUF(IJ)=ITEMP(IJ)
 540              CONTINUE
               END IF
               NPLANE=N1*N2
               DO 600 IJ=1,NPLANE
                  ITEST=IBUF(IJ)
                  ITEST=CVMGP(ITEST-65536,ITEST,ITEST-32768)
                  TEST = ITEST*SCALE
                  ROUT(IJ)=CVMGT(TEST,BAD,ITEST.NE.IBAD)
 600           CONTINUE
               CALL PLACED(IOUT,ID,J,I,IBUF,ROUT,NX,NY,3,BAD,NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'+++ERROR PLACING FIELD IN CRINTRP+++'
                  CALL FLUSH_STDOUT
               END IF
               
 400        CONTINUE
 350     CONTINUE
         
      ELSE IF (MEMUSE.EQ.2) THEN
C     
C     EDIT VOLUME AND REMAP VOLUME ARE BOTH IN MEMORY; TRANSFER ARRAY ELEMENTS
C     
         DO 200 I=1,MAXLCM
            LCMB(I)=LCMB2(I)
 200     CONTINUE
      END IF
      RETURN
      
      END
