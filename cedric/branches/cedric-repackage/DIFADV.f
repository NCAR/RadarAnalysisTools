      SUBROUTINE DIFADV(B,A,T,NX,NY,ITANAL,ITVOL,WDADV,WSADV,XD,YD,
     X                  ANGXAX,BAD,IVEL,BIJ,SCLADV,I4BAD,NST)
C
C        PERFORMS DIFFERENTIAL ADVECTION. DATA IS RELOCATED ON A POINT
C           BY POINT BASIS AND REMAPPED TO THE NEW LOCATION USING ANY
C           FOUR BOUNDING POINTS. AVERAGE (I,J) SHIFT IS CALCULATED
C           AND SAVED WHENEVER IVEL=1
C
C             B- OUTPUT ARRAY OF REMAPPED INFORMATION
C             A- INPUT ARRAY TO ADVECT
C             T- TIME ARRAY CORRESPONDING TO A
C            NX- NO. OF POINTS ALONG I
C            NY- NO. OF POINTS ALONG J
C        ITANAL- ANALYSIS TIME (HHMMSS)
C         ITVOL- STARTING TIME OF VOLUME (HHMMSS)
C         WDADV- DIRECTION OF STORM MOTION
C         WSADV- STORM SPEED (M/S)
C            XD- SPACING ALONG I
C            YD- SPACING ALONG J
C        ANGXAX- X AXIS ORIENTATION (0=NORTH)
C           BAD- MISSING DATA FLAG
C          IVEL- EXPLAINED ABOVE
C           BIJ- OUTPUT ARRAY FOR (I,J) SHIFTS
C        SCLADV- SCALE FACTOR FOR PACKING (I,J) SHIFTS
C         I4BAD- MISSING DATA FLAG FOR BIJ
C           NST- STATUS: =0, OK.   =1, CANNOT ADVECT
C
C
      DIMENSION A(NX,NY),B(NX,NY),T(NX,NY),BIJ(NX,NY),SI(4),SJ(4)
      DIMENSION IA(4),IT(4),WTA(4),WTT(4)
      EQUIVALENCE (GRAB,IJWRD)
      DATA MASK16/O'177777'/
C      DATA MASK16/65535/
      DATA ATR/0.0174533/
      DATA EPS/0.01/
C
C        SO LONG AS THE BOX DEFINED BY THE GOOD VALUES SURROUNDS A
C           TARGET LOCATION THE INTERPOLATION IS PERFORMED. 1,2,3 OR 4
C           ORIGINAL LOCATIONS AMY BE USED:
C           1- A SINGLE LOCATION IS WITHIN EPS OF THE TARGET
C           2- THE LOCATIONS ARE IN OPPOSITE QUADRANTS
C           3,4- THE BOX DEFINED BY THE LOCATIONS SURROUNDS THE TARGET
C

C
C        CHECK THAT SPACING IS SAME ALONG I AND J
C
      NST=0
      XDI=0.001/XD
      YDI=0.001/YD
      BAST=ICONSC(ITVOL)-ICONSC(ITANAL)
C
C        ADJUSTMENT FOR ROTATED COORDINATE SYSTEM
C
      ANGADJ=90.0-ANGXAX
      AR=(WDADV+ANGADJ)*ATR
      UAD=WSADV*SIN(AR)*XDI
      VAD=WSADV*COS(AR)*YDI
      RNX=NX
      RNY=NY
      NXM1=NX-1
      NYM1=NY-1
      NXY=NX*NY
      CALL CONFLD(B,NXY,BAD)
      IF(IVEL.NE.0) CALL CONFLD(BIJ,NXY,FLOAT(I4BAD))

      DO 50 J=1,NYM1
      JP1=J+1
      DO 49 I=1,NXM1
      IP1=I+1
C
C        EXAMINE VALUES TO ADVECT
C
         Q1=  1.E8
         Q2= -1.E8
         R1=  1.E8
         R2= -1.E8
         K=0
         ITCNT=0
         DO 20 II=I,IP1
         DO 19 JJ=J,JP1
            K=K+1
            IT(K)=0
            SI(K)=1.E8
C
C     SEE IF TIME FIELD IS MISSING; IF SO, CAN'T ADVECT POINT
C
            IF (T(II,JJ).EQ.BAD) THEN
               IT(K)=1
               IA(K)=1
               GOTO 19
            END IF
            TREL=BAST+T(II,JJ)
C
C           SI AND SJ CONTAIN THE NEW INDICES OF THE ADVECTED POINTS
C
            SI(K)=II+TREL*UAD
            SJ(K)=JJ+TREL*VAD
C
C           Q AND R ARE THE BOUNDARIES OF THE BOX DEFINED BY SI AND SJ
C
            Q1=AMIN1(Q1,SI(K))
            Q2=AMAX1(Q2,SI(K))
            R1=AMIN1(R1,SJ(K))
            R2=AMAX1(R2,SJ(K))
C
   19    CONTINUE
   20    CONTINUE
C
         I1=MAX1(Q1+0.99,1.0)
         I2=MIN1(Q2+EPS,RNX)
         J1=MAX1(R1+0.99,1.0)
         J2=MIN1(R2+EPS,RNY)
         IF(I2.LT.I1.OR.J2.LT.J1) GO TO 49
C
C           REMAP TO NEW LOCATIONS IN THE OUTPUT ARRAY
C
         DO 30 JC=J1,J2
         DO 29 IC=I1,I2
C
C           CHECK IF THIS LOCATION HAS ALREADY BEEN DONE
C
            IF(B(IC,JC).NE.BAD) GO TO 29
C
C           PERFORM THE REMAPPING
C
            WTASUM=0.0
            WTTSUM=0.0
            DO 25 K=1,4
               IF (IT(K).EQ.1) THEN
                  WTT(K)=0.0
               ELSE
                  WTT(K)=1.0/AMAX1(EPS,SQRT((SI(K)-IC)**2+
     X                 (SJ(K)-JC)**2))
                  WTTSUM=WTTSUM+WTT(K)
               END IF
   25       CONTINUE
            WTTFAC=1./WTTSUM
            IF(IVEL.EQ.0) GO TO 29
C
C           COMPUTE AND STORE (I,J) SHIFT
C
            IS=(FLOAT(IC)-((WTT(1)+WTT(2))*I+(WTT(3)+WTT(4))*IP1)
     X          * WTTFAC) * SCLADV
            JS=(FLOAT(JC)-((WTT(1)+WTT(3))*J+(WTT(2)+WTT(4))*JP1)
     X          * WTTFAC) * SCLADV
            IS=ICEDAND(IS,MASK16)
            JS=ICEDAND(JS,MASK16)
            IJWRD=ICEDOR(ICEDSHFT(JS,16),IS)
C            IJWRD=OR(LSHIFT(JS,16),IS)
            BIJ(IC,JC)=GRAB
   29    CONTINUE
   30    CONTINUE
C
C        EXAMINE VALUES TO ADVECT
C
         Q1=  1.E8
         Q2= -1.E8
         R1=  1.E8
         R2= -1.E8
         K=0
         ITCNT=0
         DO 22 II=I,IP1
            DO 23 JJ=J,JP1
            K=K+1
            IA(K)=0
            SI(K)=1.E8
C
C     SEE IF TIME FIELD IS MISSING; IF SO, CAN'T ADVECT POINT
C
            IF (T(II,JJ).EQ.BAD .OR. A(II,JJ).EQ.BAD) THEN
               IA(K)=1
               GOTO 23
            END IF
            TREL=BAST+T(II,JJ)
C
C           SI AND SJ CONTAIN THE NEW INDICES OF THE ADVECTED POINTS
C
            SI(K)=II+TREL*UAD
            SJ(K)=JJ+TREL*VAD
C
C           Q AND R ARE THE BOUNDARIES OF THE BOX DEFINED BY SI AND SJ
C
            Q1=AMIN1(Q1,SI(K))
            Q2=AMAX1(Q2,SI(K))
            R1=AMIN1(R1,SJ(K))
            R2=AMAX1(R2,SJ(K))
C
 23      CONTINUE
 22      CONTINUE
C
         I1=MAX1(Q1+0.99,1.0)
         I2=MIN1(Q2+EPS,RNX)
         J1=MAX1(R1+0.99,1.0)
         J2=MIN1(R2+EPS,RNY)
         IF(I2.LT.I1.OR.J2.LT.J1) GO TO 49
C
C           REMAP TO NEW LOCATIONS IN THE OUTPUT ARRAY
C
         DO 33 JC=J1,J2
         DO 39 IC=I1,I2
C
C           CHECK IF THIS LOCATION HAS ALREADY BEEN DONE
C
            IF(B(IC,JC).NE.BAD) GO TO 39
C
C           PERFORM THE REMAPPING
C
            WTASUM=0.0
            WTTSUM=0.0
            DO 35 K=1,4
               IF (IA(K).EQ.1) THEN
                  WTA(K)=0.0
               ELSE
                  WTA(K)=1.0/AMAX1(EPS,SQRT((SI(K)-IC)**2+
     X                 (SJ(K)-JC)**2))
                  WTASUM=WTASUM+WTA(K)
               END IF
               IF (IT(K).EQ.1 .OR. IA(K).EQ.1) THEN
                  WTT(K)=0.0
               ELSE
                  WTT(K)=1.0/AMAX1(EPS,SQRT((SI(K)-IC)**2+
     X                 (SJ(K)-JC)**2))
                  WTTSUM=WTTSUM+WTT(K)
               END IF
   35       CONTINUE
            IF (WTASUM.NE.0.0) THEN
               WTAFAC=1./WTASUM
               B(IC,JC)=(WTA(1)*A(  I,J)+WTA(2)*A(  I,JP1)+
     X              WTA(3)*A(IP1,J)+WTA(4)*A(IP1,JP1))*WTAFAC
            END IF
            WTTFAC=1./WTTSUM
            IF(IVEL.EQ.0) GO TO 39
C
C           COMPUTE AND STORE (I,J) SHIFT
C
            IS=(FLOAT(IC)-((WTT(1)+WTT(2))*I+(WTT(3)+WTT(4))*IP1)
     X          * WTTFAC) * SCLADV
            JS=(FLOAT(JC)-((WTT(1)+WTT(3))*J+(WTT(2)+WTT(4))*JP1)
     X          * WTTFAC) * SCLADV
            IS=ICEDAND(IS,MASK16)
            JS=ICEDAND(JS,MASK16)
            IJWRD=ICEDOR(ICEDSHFT(JS,16),IS)
C            IJWRD=OR(LSHIFT(JS,16),IS)
            BIJ(IC,JC)=GRAB
   39    CONTINUE
 33   CONTINUE
C
   49 CONTINUE
   50 CONTINUE
C
      RETURN
      END

