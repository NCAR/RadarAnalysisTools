      SUBROUTINE COINTRP(IROT,ITRANS,INWGRD,XOR,YOR,ZOR,CSPN,NCXN,
     X     NPLNEW,ANGXAX,ANGUSR,WXYC,IXYC,RELMAX,LCMB2,ISPEC,VNYQ,
     X     IFLAT,RTOP,RBOT,ROUT,ITEMP,IBUF,MEMUSE,ICORD,LATLON,DE)
C
C     THIS SUBROUTINE IS THE DRIVER FOR THE INTERPOLATION OF DATA
C     FROM EITHER COPLANE OR ELEVATION ANGLE SPACE TO CARTESIAN OR
C     LON-LAT SPACE.
C
C-----One-dimensional interpolation in z-direction between elev or coplanes
C     ICORD: (1) COP (XYC) --> CRT (XYZ)
C            (2) ELE (XYE) --> CRT (XYZ)
C            (3) LLE       --> LLZ
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
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      DIMENSION CSPN(3,3),NCXN(3),WXYC(MAXP,MAXP,3)
      DIMENSION IXYC(MAXP,MAXP,3),LCMB2(1),ITEMP(1)
      DIMENSION IBUF(1),ROUT(1),RTOP(1),RBOT(1)
      COMMON /SCRATCH_GRD/ XGRD(MAXX,MAXY),YGRD(MAXX,MAXY)

      DATA EPS/0.001/
      INTEGER CVMGP

      LOGICAL LATLON

      character*8 tyme

      SF=1./FLOAT(ID(68))
      CF=1./FLOAT(ID(69))
      ORLAT=ID(33)+ID(34)/60.0+ID(35)/(3600.0 * ID(68))
      ORLON=ID(36)+ID(37)/60.0+ID(38)/(3600.0 * ID(68))
      ZRAD=ID(317)*.001 

      print *,'COINTRP: lat  ddmmss=',id(33),id(34),id(35)
      print *,'         lon  ddmmss=',id(36),id(37),id(38)
      print *,'  org lat/lon,angxax=',orlat,orlon,angxax
      print *,'  angusr,xor,yor,zor=',angusr,xor,yor,zor
      print *,'       zrad,iflat,de=',zrad,iflat,de
      print *,'         id(315-317)=',id(315),id(316),id(317)
      print *,'         id(321-322)=',id(321),id(322)

      ATR=ATAN(1.)/45.
      IDIM2=0
C
C     Normal cartesian:   (X,Y) represent distances in km
C     Longitude-latitude: (X,Y) represent degrees
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
      write(*,*)'Cointrp: '
      write(*,*)'   irot,itrans,latlon=',irot,itrans,latlon
      write(*,*)' '
      if(latlon)then
         write(*,*)'   old lon=',(csp(i,1),i=1,3),ncx(1)
         write(*,*)'   old lat=',(csp(i,2),i=1,3),ncx(2)
         write(*,*)'   old ele=',(csp(i,3),i=1,3),ncx(3)
         write(*,*)'   old ele=',(vallev(i),i=1,ncx(3))
      else
         write(*,*)'   old   x=',(csp(i,1),i=1,3),ncx(1)
         write(*,*)'   old   y=',(csp(i,2),i=1,3),ncx(2)
         write(*,*)'   old ele=',(csp(i,3),i=1,3),ncx(3)
         write(*,*)'   old ele=',(vallev(i),i=1,ncx(3))
      end if
      write(*,*)' '
      if(latlon)then
         write(*,*)'   new lon=',(cspn(i,1),i=1,3),ncxn(1)
         write(*,*)'   new lat=',(cspn(i,2),i=1,3),ncxn(2)
         write(*,*)'   new   z=',(cspn(i,3),i=1,3),ncxn(3)
      else
         write(*,*)'   new   x=',(cspn(i,1),i=1,3),ncxn(1)
         write(*,*)'   new   y=',(cspn(i,2),i=1,3),ncxn(2)
         write(*,*)'   new   z=',(cspn(i,3),i=1,3),ncxn(3)
      end if
      write(*,*)' '

      IF(ICORD.EQ.1)THEN
C     coplane --> x,y,z
         XORG=(ID(315)*SF + ID(321)*SF)/2.
         YORG=(ID(316)*SF + ID(322)*SF)/2.
      ELSE IF (ICORD.EQ.2 .OR. ICORD.EQ.3)THEN
C     x,y,e --> x,y,z  or  l,l,e --> l,l,z
         XORG=ID(315)*SF
         YORG=ID(316)*SF
      END IF
C
C     LOOP OVER ALL POINTS IN NEW COORD. SYS. AND FIND COORD. IN OLD SYSTEM
C     Longitude-latitude: (X,Y) represent degrees so convert to distance for 
C        height calculations using S2, the distance from the origin.  Save 
C        (XC,YC) deg for later calculations of weighting factors and indices.
C
      DO 100 I=1,NZ
         ZC=CSPN(1,3) + (I-1)*CSPN(3,3)
         IZLEV = I
         DO 120 J=1,NY
            YC=CSPN(1,2) + (J-1)*CSPN(3,2)
            DO 130 K=1,NX
               XC=CSPN(1,1) + (K-1)*CSPN(3,1)
               Z=ZC
               Y=YC
               X=XC

               IF (LATLON) THEN
C     ppi l,l,e --> l,l,z (convert grid lon-lat to distances in km)
                  IF(I.EQ.1)THEN
                     PLAT=Y
                     PLON=-X
                     CALL LL2XYDRV(PLAT,PLON,X,Y,ORLAT,ORLON,ANGXAX)
                     XGRD(K,J)=X
                     YGRD(K,J)=Y
                  ELSE
                     X=XGRD(K,J)
                     Y=YGRD(K,J)
                  END IF
               END IF

               IF (IROT.EQ.1) THEN
                  THETA=(ANGUSR-ANGXAX)*ATR
                  XP=X*COS(THETA) - Y*SIN(THETA)
                  YP=X*SIN(THETA) + Y*COS(THETA)
                  X=XP
                  Y=YP
               END IF
               IF (ITRANS.EQ.1) THEN
                  X=X+XOR
                  Y=Y+YOR
                  Z=Z+ZOR
               END IF
C     
C     CORRECT Z FOR HEIGHT OF RADAR AND CURVATURE OF EARTH
C     IFLAT: (0) CURVED EARTH - Use 4/3 Earth's radius
C            (1) FLAT EARTH   - No correction
C     
               Z=Z-ZRAD
               S2=(X-XORG)**2.0 + (Y-YORG)**2.0 
               IF (IFLAT.EQ.1) THEN
                  CCOR=0.0
               ELSE
                  CCOR=3.*S2/(4.*DE)
               END IF
               Z=Z-CCOR

               IF (ICORD.EQ.1)THEN
C
C     CONVERT XYZ COORDINATES TO COPLANE XYC
C
                  IF (Z.LE.0.0 .OR. X.LE.0.0) THEN
                     C=-1.0
                     XC=-99.0
                     YC=-99.0
                  ELSE 
                     C=ATAN2(Z,X)
                     XC=X*COS(C) + Z*SIN(C)
                     YC=Y
                     C=C/ATR
                  END IF
                  
               ELSE IF (ICORD.EQ.2 .OR. ICORD.EQ.3)THEN
C     
C     CONVERT XYZ COORDINATES TO ELEVATION XYE
C     
                  IF(S2.GE.0.0)THEN
                     H=SQRT(S2)
                  ELSE
                     H=0.0
                  END IF
                  IF (Z.EQ.0.0 .OR. H.EQ.0.0) THEN
                     E=0.0
                  ELSE 
                     E=ATAN2(Z,H)/ATR
                  END IF
                  
               END IF
C     
C     CALCULATE WEIGHTING FACTORS AND INDICES FOR BILINEAR INTERPOLATION
C
               WXYC(K,J,1)=0.0
               WXYC(K,J,2)=0.0
               WXYC(K,J,3)=0.0
               IXYC(K,J,1)=0
               IXYC(K,J,2)=0
               IXYC(K,J,3)=0
               RL=(XC-X1)*XDI+1.0+EPS
               RM=(YC-Y1)*YDI+1.0+EPS
               L=INT(RL)
               M=INT(RM)

               IF (ICORD.EQ.1) THEN
C
C     COPLANE --> CARTESIAN (Assumes coplane angles are equally spaced.)
C
                  RN=(C-Z1)*ZDI+1.0+EPS
                  N=INT(RN)
                  
               ELSE IF (ICORD.EQ.2 .OR. ICORD.EQ.3) THEN
C     
C     ELEVATION --> CARTESIAN (Assumes elevation angles are unequally spaced.)
C                   Find index (N) for VALLEV below the current level.
C                   RN - fractional distance of E above VALLEV(N)
C
                  N=0
                  IF(E.LT.VALLEV(1) .OR. E.GT.VALLEV(NCX(3)))GO TO 10
                  DO II=1,NCX(3)-1
                     IF(E.GE.VALLEV(II) .AND. E.LT.VALLEV(II+1))THEN
                        N=II
                        ED=VALLEV(II+1)-VALLEV(II)
                        IF (ED.GT.0.0) THEN
                           EDI=1./ED
                        ELSE 
                           EDI=1.0
                        END IF
                        RN=(E-VALLEV(N))*EDI
                        GO TO 10
                     END IF
                  END DO
 10               CONTINUE
               END IF
               
c               IF(L.LT.1 .OR. L.GT.NCX(1)) GOTO 130
c               IF(M.LT.1 .OR. M.GT.NCX(2)) GOTO 130
c               IF(N.LT.1 .OR. N.GT.NCX(3)) GOTO 130

               WXYC(K,J,1)=RL-FLOAT(L)
               WXYC(K,J,2)=RM-FLOAT(M)
               IF (ICORD.EQ.1)THEN
                  WXYC(K,J,3)=RN-FLOAT(N)
               ELSE IF (ICORD.EQ.2 .OR. ICORD.EQ.3)THEN
                  WXYC(K,J,3)=RN
               END IF
               IXYC(K,J,1)=L
               IXYC(K,J,2)=M
               IXYC(K,J,3)=N

c--------------debug (ljm)
c               if( (j .eq. 1  .and. k .eq. 1) .or.
c     +             (j .eq. NY .and. k .eq. NX).or.
c     +             (mod(j,1) .eq. 0 .and. mod(k,1) .eq. 0) ) then
c                  wx=wxyc(k,j,1)
c                  wy=wxyc(k,j,2)
c                  wz=wxyc(k,j,3)
c                  if(e.ge.vallev(1) .and. e.le.vallev(ncx(3)))then
c                     write(*,1771)k,j,i,xc,yc,x,y,h,z,e,wx,wy,wz
c 1771                format(' KJI=',3i4,' LL=',2f10.4,
c     +                    ' XYHZE=',5f10.4,3f8.4)
c                  end if
c               end if  
c--------------debug (ljm)

 130        CONTINUE
 120     CONTINUE
C
C     DO THE INTERPOLATION NOW TO THIS Z-LEVEL
C     
         call clock(tyme)
         print *,'Cotrp: i,z=',i,zc,' t=',tyme

C     NX,NY,NZ       --> TRPCO: NNX,NNY,NNZ - numbers of new grid points
C                  I -->        NLEV        - current new grid level
C     NCX(1), NCX(2) -->        NX,NY       - numbers of old grid points

         CALL TRPCO(WXYC,IXYC,NX,NY,NZ,CSPN,I,RELMAX,ICORD,LCMB2,ISPEC,
     X              VNYQ,RTOP,RBOT,ROUT,IBUF,NCX(1),NCX(2),ITEMP,IDIM2,
     X              MEMUSE)
 100  CONTINUE

C     
C     VOLUME HAS BEEN REMAPPED. REDEFINE EDIT VOLUME CHARACTERISTICS AND
C     TRANSFER VOLUME TO EDIT SPACE
C     
      ID(451)=(NPLNEW-1)/(WORDSZ/16.0)+1
      ID(452)=NCXN(3)*ID(451)
      ID(400)=ID(452)
      
      NF=ID(175)
      NFL=ID(175)
      DO 220 J=1,NF
         ITMP=MAPVID(J,2)
         DO 225 I=1,4
            NAMF(I,J)=NAMF(I,ITMP)
 225     CONTINUE
         SCLFLD(J)=SCLFLD(ITMP)
         ISETFL(J)=ISETFL(ITMP)
         MAPVID(J,1)=J
         MAPVID(J,2)=J
 220  CONTINUE
      
      DO 227 J=(NF+1),NFMAX
         MAPVID(J,1)=0
         MAPVID(J,2)=0
         SCLFLD(J)=0
         ISETFL(J)=0
         DO 245 I=1,4
            NAMF(I,J)=' '
 245     CONTINUE
 227  CONTINUE
      
      DO 250 I=1,NFMAX
         IRCP(I)=1+ID(400)*(I-1)
         ID(400+I)=IRCP(I)
 250  CONTINUE
      
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
