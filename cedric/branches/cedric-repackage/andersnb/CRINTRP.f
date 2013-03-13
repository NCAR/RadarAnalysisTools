      SUBROUTINE CRINTRP(IROT,ITRANS,INWGRD,XOR,YOR,ZOR,CSPN,NCXN,
     X     NPLNEW,ANGXAX,ANGUSR,WXYC,IXYC,RELMAX,LCMB2,
     X     ISPEC,VNYQ,RTOP,RBOT,ROUT,ITEMP,IBUF,IDIM2,
     X     MEMUSE)
C     
C     THIS SUBROUTINE IS THE DRIVER FOR THE INTERPOLATION OF DATA
C     FROM CARTESIAN SPACE TO CARTESIAN SPACE.
C     
C     IROT    -  FLAG INDICATING ROTATION
C     ITRANS  -  FLAG INDICATING TRANSLATION
C     INWGRD  -  FLAG INDICATING NEW GRID
C     XOR,YOR -  NEW ORIGIN SPECIFIED IN OLD COORDINATE SYSTEM
C     CSPN    -  NEW GRID SPECIFICATION
C     NCXN    -  NUMBER OF X,Y, AND Z GRID POINTS IN NEW GRID
C     NPLNEW  -  NUMBER OF X-Y GRID POINTS IN A Z PLANE OF NEW COORD. SYSTM.
C     ANGXAX  -  ANGLE OF NEW X-AXIS, RELATIVE TO TRUE NORTH
C     ANGUSR  -  ANGLE OF OLD X-AXIS, RELATIVE TO TRUE NORTH
C     WXYC    -  WILL STORE WEIGHTING INFORMATION FOR THE INTERP
C     IXYC    -  WILL STORE INFO FOR INTERPOLATION
C     RELMAX  -  MAX DISTANCE TO RELOCATE ANY POINT FOR CLOSEST POINT METHOD
C     IDIM2   -  0 ==> 3-D INTERP.  1 ==> 2-D INTERP.
C     MEMUSE  -  MEMORY USE FLAG FOR REMAP
C     
      INCLUDE 'CEDRIC.INC'
      PARAMETER (NFMAX=25,NID=510,MAXP=256)
      COMMON LCMB(1)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      DIMENSION CSPN(3,3),NCXN(3),WXYC(MAXP,MAXP,3),ITEMP(1)
      DIMENSION IXYC(MAXP,MAXP,3),LCMB2(1),IBUF(1),ROUT(1),RTOP(1)
      DIMENSION RBOT(1)
      DATA EPS/0.000001/
      DATA NBITS/16/
      INTEGER CVMGP
      SF=1./FLOAT(ID(68))
      CF=1./FLOAT(ID(69))
      ATR=ATAN(1.)/45.
      ICORD=1
      
      
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
      
C     
C     LOOP OVER ALL POINTS IN NEW COORD. SYSTEM AND FIND COORD. IN OLD SYSTEM
C     
      DO 100 I=1,NZ
         DO 120 J=1,NY
            DO 130 K=1,NX
               X=CSPN(1,1) + (K-1)*CSPN(3,1)
               Y=CSPN(1,2) + (J-1)*CSPN(3,2)
               Z=CSPN(1,3) + (I-1)*CSPN(3,3)
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
C     CALCULATE WEIGHTING FACTORS FOR BILINEAR INTERPOLATION
C     
               WXYC(K,J,1)=0.0
               WXYC(K,J,2)=0.0
               WXYC(K,J,3)=0.0
               IXYC(K,J,1)=0
               IXYC(K,J,2)=0
               IXYC(K,J,3)=0
               RL=(X-X1)*XDI+1.0+EPS
               L=INT(RL)
               IF(L.LT.1 .OR. L.GT.NCX(1)) GOTO 130
               RM=(Y-Y1)*YDI+1.0+EPS
               M=INT(RM)
               IF(M.LT.1 .OR. M.GT.NCX(2)) GOTO 130
               RN=(Z-Z1)*ZDI+1.0+EPS
               N=INT(RN)
               IF(N.LT.1 .OR. N.GT.NCX(3)) GOTO 130
               WXYC(K,J,1)=RL-FLOAT(L)
               WXYC(K,J,2)=RM-FLOAT(M)
               WXYC(K,J,3)=RN-FLOAT(N)
               IXYC(K,J,1)=L
               IXYC(K,J,2)=M
               IXYC(K,J,3)=N
 130        CONTINUE
 120     CONTINUE
C     
C     DO THE INTERPOLATION NOW TO THIS Z-LEVEL FOR ALL FIELDS
C     
         CALL TRPCO(WXYC,IXYC,NX,NY,NZ,CSPN,I,RELMAX,ICORD,LCMB2,ISPEC,
     X        VNYQ,RTOP,RBOT,ROUT,IBUF,NCX(1),NCX(2),ITEMP,IDIM2,
     X        MEMUSE)
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
     X              ID,NID,NX,NY,NZ,J,ITEMP,NWL,LCMB2,MEM,IVOL)
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
               CALL PLACED(IOUT,ID,NID,J,I,IBUF,ROUT,NX,NY,3,BAD,NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'+++ERROR PLACING FIELD IN CRINTRP+++'
                  STOP
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
