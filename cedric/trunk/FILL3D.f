      SUBROUTINE FILL3D(DATA,TEMP,IGRID,NOCT,RMNPTS,NX,NY,NZ,BAD,
     X                  IEDW,IOPT)
C  THIS ROUTINE PERFORMS 3-DIMENSIONAL DATA FILLING *OR* FILTERING 
C  OF   A CARTESIAN GRID USING A CONSTRAINED LOCAL LEAST-SQUARES DATA FILL.  
C
C  JOHN TUTTLE, NCAR/MMM - MARCH 1991.
C
C  DATA  -INPUT 3-DIMENSIONAL DATA SET TO BE FILLED.  ON OUTPUT DATA
C         CONTAINS THE FILLED DATA.
C  IGRID-  MAXIMUM NUMBER OF GRID POINTS TO SEARCH OUTWARD FROM A 
C         MISSING DATA LOCATION TO DETERMINE IF IT IS BOUNDED AND IF
C         RMNPTS HAS BEEN SATISFIED.
C  NOCT-  MINIMUM NUMBER OF OCTANTS OCCUPIED IN ORDER TO SATISFY THE SEARCH.
C  RMNPTS-MINIMUM NUMBER OF SURROUNDING POINTS REQUIRED
C  NX-    NUMBER OF X GRID POINTS
C  NY-    NUMBER OF Y GRID POINTS
C  NZ-    NUMBER OF Z GRID POINTS
C  BAD-   MISSING DATA FLAG
C  IEDW-  WINDOW OF DATA TO APPLY FILLING/FILTERING OVER
C  IOPT-  IOPT=0 ==> FILLING, IOPT=1 ==> FILTERING
C     

      DIMENSION DATA(NX,NY,NZ),TEMP(NX,NY,NZ),IOCT(8),IEDW(2,3)
      DATA EPS/0.0001/

      IF (IOPT.EQ.0) THEN
C
C     MAKE A COPY OF INPUT ARRAY
C
         DO 10 K=1,NZ
            DO 10 J=1,NY
               DO 10 I=1,NX
 10        TEMP(I,J,K)=DATA(I,J,K)

C LOOP OVER GRID POINTS. IF GRID POINT VALUE IS MISSING FILL IT, 
C OTHERWISE DO NOTHING.
      DO 30 K0=1,NZ
         DO 40 J0=1,NY
            DO 50 I0=1,NX
               IF(TEMP(I0,J0,K0).NE.BAD)GO TO 50
               DO 55 L=1,8
                  IOCT(L)=0
 55            CONTINUE
C     START SEARCHING OUTWARD FROM GRID POINT UNTIL THE NOCT AND RMNPTS
C     CONDITIONS ARE MET.
               DO 60 L=1,IGRID
                  K1=MAX0(1,K0-L)
                  K2=MIN0(NZ,K0+L)
                  J1=MAX0(1,J0-L)
                  J2=MIN0(NY,J0+L)
                  I1=MAX0(1,I0-L)
                  I2=MIN0(NX,I0+L)
C     INITIALIZE SUMMATION TERMS USED IN THE LEAST-SQUARES FIT
                  RNPTS=0.
                  SX=0.
                  SY=0.
                  SZ=0.
                  SX2=0.
                  SY2=0.
                  SZ2=0.
                  SXY=0.
                  SXZ=0.
                  SYZ=0.
                  SDX=0.
                  SDY=0.
                  SDZ=0.
                  SD=0.
                  DO 70 K=K1,K2
                     IZ=K-K0
                     DO 80 J=J1,J2
                        IY=J-J0
                        DO 90 I=I1,I2
                           IX=I-I0
                           IF(TEMP(I,J,K).EQ.BAD)GO TO 90
                           IF(IX.GE.0.AND.IY.GT.0.AND.IZ.LE.0)IOCT(1)=1
                           IF(IX.GT.0.AND.IY.LE.0.AND.IZ.LE.0)IOCT(2)=1
                           IF(IX.LE.0.AND.IY.LT.0.AND.IZ.LE.0)IOCT(3)=1
                           IF(IX.LT.0.AND.IY.GE.0.AND.IZ.LE.0)IOCT(4)=1
                           IF(IX.GE.0.AND.IY.GT.0.AND.IZ.GT.0)IOCT(5)=1
                           IF(IX.GT.0.AND.IY.LE.0.AND.IZ.GT.0)IOCT(6)=1
                           IF(IX.LE.0.AND.IY.LT.0.AND.IZ.GT.0)IOCT(7)=1
                           IF(IX.LT.0.AND.IY.GE.0.AND.IZ.GT.0)IOCT(8)=1
                           RNPTS=RNPTS+1.0
                           SX=SX+IX
                           SY=SY+IY
                           SZ=SZ+IZ
                           SX2=SX2+IX*IX
                           SY2=SY2+IY*IY
                           SZ2=SZ2+IZ*IZ
                           SXY=SXY+IX*IY
                           SXZ=SXZ+IX*IZ
                           SYZ=SYZ+IY*IZ
                           SD=SD+TEMP(I,J,K)
                           SDX=SDX+IX*TEMP(I,J,K)
                           SDY=SDY+IY*TEMP(I,J,K)
                           SDZ=SDZ+IZ*TEMP(I,J,K)
 90                     CONTINUE
 80                  CONTINUE
 70               CONTINUE
                  KQ=0
                  DO 75 K1=1,8
                     KQ=KQ+IOCT(K1)
 75               CONTINUE
                  IF(KQ.LT.NOCT)GO TO 60
                  IF(RNPTS.LT.RMNPTS)GO TO 60
                  T1=SX2*(SY2*SZ2-SYZ*SYZ)
                  T2=-SXY*(SXY*SZ2-SYZ*SXZ)
                  T3=SXZ*(SXY*SYZ-SXZ*SY2)
                  T4=SX*(SZ2*SY2-SYZ*SYZ)
                  T5=-SXY*(SY*SZ2-SZ*SYZ)
                  T6=SXZ*(SY*SYZ-SZ*SY2)
                  T7=SX*(SXY*SZ2-SYZ*SXZ)
                  T8=-SX2*(SY*SZ2-SZ*SYZ)
                  T9=SXZ*(SY*SXZ-SZ*SXY)
                  T10=SX*(SXY*SYZ-SY2*SXZ)
                  T11=-SX2*(SY*SYZ-SZ*SY2)
                  T12=SXY*(SY*SXZ-SZ*SXY)
                  DENOM=RNPTS*(T1+T2+T3)-SX*(T4+T5+T6)+SY*(T7+T8+T9)-
     +                 SZ*(T10+T11+T12)
                  IF(DENOM.LE.EPS)THEN
C                     PRINT 130,I0,J0,K0,DENOM
C 130                 FORMAT(1X,'DENOM LT EPS-I,J,K,DENOM=',3I6,2X,E12.4)
                     GO TO 60
                  END IF
                  RNUM=SD*(T1+T2+T3)-SDX*(T4+T5+T6)+SDY*(T7+T8+T9)-
     +                 SDZ*(T10+T11+T12)
                  DATA(I0,J0,K0)=RNUM/DENOM
                  GO TO 56
 60            CONTINUE  
 56            CONTINUE
 50         CONTINUE
 40      CONTINUE
 30   CONTINUE
      ELSE IF (IOPT.EQ.1) THEN
C
C     DO FILTERING
C


C LOOP OVER GRID POINTS. IF GRID POINT VALUE IS MISSING FILL IT, 
C OTHERWISE DO NOTHING.
      DO 300 K0=1,NZ
         DO 400 J0=1,NY
            DO 500 I0=1,NX
               IF (DATA(I0,J0,K0).EQ.BAD)GO TO 500
               DO 550 L=1,8
                  IOCT(L)=0
 550           CONTINUE
C     START SEARCHING OUTWARD FROM GRID POINT UNTIL THE NOCT AND RMNPTS
C     CONDITIONS ARE MET.
                  K1=MAX0(1,K0-IGRID)
                  K2=MIN0(NZ,K0+IGRID)
                  J1=MAX0(1,J0-IGRID)
                  J2=MIN0(NY,J0+IGRID)
                  I1=MAX0(1,I0-IGRID)
                  I2=MIN0(NX,I0+IGRID)
C     INITIALIZE SUMMATION TERMS USED IN THE LEAST-SQUARES FIT
                  RNPTS=0.
                  SX=0.
                  SY=0.
                  SZ=0.
                  SX2=0.
                  SY2=0.
                  SZ2=0.
                  SXY=0.
                  SXZ=0.
                  SYZ=0.
                  SDX=0.
                  SDY=0.
                  SDZ=0.
                  SD=0.
                  DO 700 K=K1,K2
                     IZ=K-K0
                     DO 800 J=J1,J2
                        IY=J-J0
                        DO 900 I=I1,I2
                           IX=I-I0
                           IF(TEMP(I,J,K).EQ.BAD)GO TO 900
                           IF(IX.GE.0.AND.IY.GT.0.AND.IZ.LE.0)IOCT(1)=1
                           IF(IX.GT.0.AND.IY.LE.0.AND.IZ.LE.0)IOCT(2)=1
                           IF(IX.LE.0.AND.IY.LT.0.AND.IZ.LE.0)IOCT(3)=1
                           IF(IX.LT.0.AND.IY.GE.0.AND.IZ.LE.0)IOCT(4)=1
                           IF(IX.GE.0.AND.IY.GT.0.AND.IZ.GT.0)IOCT(5)=1
                           IF(IX.GT.0.AND.IY.LE.0.AND.IZ.GT.0)IOCT(6)=1
                           IF(IX.LE.0.AND.IY.LT.0.AND.IZ.GT.0)IOCT(7)=1
                           IF(IX.LT.0.AND.IY.GE.0.AND.IZ.GT.0)IOCT(8)=1
                           RNPTS=RNPTS+1.0
                           SX=SX+IX
                           SY=SY+IY
                           SZ=SZ+IZ
                           SX2=SX2+IX*IX
                           SY2=SY2+IY*IY
                           SZ2=SZ2+IZ*IZ
                           SXY=SXY+IX*IY
                           SXZ=SXZ+IX*IZ
                           SYZ=SYZ+IY*IZ
                           SD=SD+TEMP(I,J,K)
                           SDX=SDX+IX*TEMP(I,J,K)
                           SDY=SDY+IY*TEMP(I,J,K)
                           SDZ=SDZ+IZ*TEMP(I,J,K)
 900                     CONTINUE
 800                  CONTINUE
 700               CONTINUE
                  KQ=0
                  DO 750 K1=1,8
                     KQ=KQ+IOCT(K1)
 750              CONTINUE
                  T1=SX2*(SY2*SZ2-SYZ*SYZ)
                  T2=-SXY*(SXY*SZ2-SYZ*SXZ)
                  T3=SXZ*(SXY*SYZ-SXZ*SY2)
                  T4=SX*(SZ2*SY2-SYZ*SYZ)
                  T5=-SXY*(SY*SZ2-SZ*SYZ)
                  T6=SXZ*(SY*SYZ-SZ*SY2)
                  T7=SX*(SXY*SZ2-SYZ*SXZ)
                  T8=-SX2*(SY*SZ2-SZ*SYZ)
                  T9=SXZ*(SY*SXZ-SZ*SXY)
                  T10=SX*(SXY*SYZ-SY2*SXZ)
                  T11=-SX2*(SY*SYZ-SZ*SY2)
                  T12=SXY*(SY*SXZ-SZ*SXY)
                  DENOM=RNPTS*(T1+T2+T3)-SX*(T4+T5+T6)+SY*(T7+T8+T9)-
     +                 SZ*(T10+T11+T12)
                  IF(DENOM.LE.EPS)THEN
C                     PRINT 130,I0,J0,K0,DENOM
C 130                 FORMAT(1X,'DENOM LT EPS-I,J,K,DENOM=',3I6,2X,E12.4)
                     GO TO 600
                  END IF
                  RNUM=SD*(T1+T2+T3)-SDX*(T4+T5+T6)+SDY*(T7+T8+T9)-
     +                 SDZ*(T10+T11+T12)
                  DATA(I0,J0,K0)=RNUM/DENOM
                  GO TO 560
 600              CONTINUE  
 560              CONTINUE
 500           CONTINUE
 400        CONTINUE
 300     CONTINUE
      END IF
      RETURN
      END
      
      
