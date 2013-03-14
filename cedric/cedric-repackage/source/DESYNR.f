      SUBROUTINE DESYNR(OBUF,U,V,W,NX,NY,IBEG,IEND,JBEG,JEND,
     X                  Z,CSP,RLOC,CON,BAD,ICOR,CTEMP,ZAXNAM)
C
C        DERIVES A RADIAL VELOCITY VALUE FOR A GIVEN LOCATION FROM
C                U,V AND W FIELD VALUES.
C
      DIMENSION OBUF(NX,NY),U(NX,NY),V(NX,NY),W(NX,NY),CSP(3,3),RLOC(3)
      DIMENSION CON(3)
      CHARACTER*4 CTEMP,ZAXNAM
      ATR=ATAN(1.)/45.
      IF (ICOR.EQ.1) THEN
C
C     VECTORS ASSUMED TO BE IN A TRUE NORTH COORD. SYS.; ROTATE THEM IF NEC.
C
         ANGR=AMOD((450.-CON(1)),360.)*ATR
      ELSE
C
C     VECTORS ASSUMED TO BE IN AND ALONG CURRENT COORD. AXES; NO ROTATION
C
         ANGR=0.0
      END IF
      ASNF=SIN(ANGR)
      ACSF=COS(ANGR)
C
C        ACTIVATE THE NEXT TWO STMTS TO ALLOW RADAR POSN. TO BE IN ORIGINAL
C                 COORDINATES INSTEAD OF THE NEW TRANSLATED SYSTEM.
C
      XORTR=0.-RLOC(1)
      YORTR=0.-RLOC(2)
      XD=CSP(3,1)
      YD=CSP(3,2)
      IF(ZAXNAM.EQ.'E')THEN
         EL=ATR*Z
      ELSE
         ZR=Z-RLOC(3)
         ZRSQ=ZR*ZR
      END IF
C
C        LOOP OVER Y-LOCATIONS
C
      Y=CSP(1,2)+(JBEG-2)*YD
      DO 20 J=JBEG,JEND
         Y=Y+YD
         YTRM1=XORTR-ASNF*Y
         YTRM2=YORTR+ACSF*Y
C
C           LOOP OVER X-LOCATIONS
C
         X=CSP(1,1)+(IBEG-2)*XD
         DO 10 I=IBEG,IEND
            X=X+XD
            XR=ACSF*X+YTRM1
            YR=ASNF*X+YTRM2
C
C           CHECK FOR BAD DATA AND COMPUTE RADIAL VELOCITY
C
            OBUF(I,J)=BAD
            IF(ABS(XR).LT.0.1.AND.ABS(YR).LT.0.1) GO TO 10
            IF(U(I,J).EQ.BAD.OR.V(I,J).EQ.BAD) GO TO 10
            HRNG=SQRT(XR*XR+YR*YR)
            RNG=SQRT(XR*XR+YR*YR+ZRSQ)
            IF(ZAXNAM.EQ.'E')THEN
               ZR=HRNG*TAN(EL)
               ZR=ZR-RLOC(3)
               ZRSQ=ZR*ZR
            ELSE
               EL=ASIN(ZR/RNG)
            END IF
            AZ=ATAN2(XR,YR)
            IF (CTEMP.EQ.'COPL') THEN
               OBUF(I,J)=(U(I,J)*SIN(AZ)+V(I,J)*COS(AZ))
            ELSE
               OBUF(I,J)=(U(I,J)*SIN(AZ)+V(I,J)*COS(AZ))*COS(EL)
     X              + W(I,J)*SIN(EL)
            END IF
   10    CONTINUE
   20 CONTINUE
      RETURN
      END




