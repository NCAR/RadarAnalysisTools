      SUBROUTINE CRTMAP(JD,NJD,CSP,NCX,MAP,MAXAXIS,JST)
C
C        CALCULATES THE INDICES OF THE CARTESIAN SYSTEM DESCRIBED BY
C           THE ID HEADER WITH RESPECT TO THE EXISTING DATA STRUCTURE.
C               JST=0, EXACT MATCH
C                  =1, PARTIAL OVERLAP
C                  =2, NO OVERLAP
C           MODIFICATIONS MADE 12/83 TO ASSUME THE FOLLOWING:
C               IF AXIS SPACING IS >0, MORE THAN ONE PLANE OF DATA EXISTS
C                                  =0, ONLY ONE PLANE OF DATA EXISTS
C                                  <0, ALL FIELDS ARE AT THE SURFACE
C           MODS 7/87 TO ALLOW MAPPING FROM COARSE TO FINE IN Z W/MAP
C           MAP CONTAINS INDICES (ALONG EACH EDIT FIELD AXIS)
C               POINTING TO THE CORRESPONDING LOCATIONS IN THE INPUT
C               FILE. IF NO INPUT POSITION MATCHES AN EDIT FILE LOCATION
C               THE VALUE -1 IS INSERTED.
C
      DIMENSION CSP(3,3),NCX(3),MAP(MAXAXIS,3),ICSP(3,3),ISTAT(3)
      DIMENSION JD(NJD)
      SAVE
C
      SF=FLOAT(JD(68))
      METR=1000/JD(68)
      L=155
      DO 20 J=1,3
         IF(J.EQ.3) THEN
            SF=1000.0
            METR=1
         END IF
         ISTAT(J)=2
         L=L+5
         ICSP(1,J)=NINT(CSP(1,J)*SF)
         ICSP(2,J)=NINT(CSP(2,J)*SF)
         ICSP(3,J)=NINT(CSP(3,J)*1000.0)
         NP=NCX(J)
         CALL CONFLD(MAP(1,J),NP,-1)
         IF(JD(L+3).LT.0.OR.ICSP(3,J).LT.0) THEN
C
C                                     SURFACE DATA --MAPS TO FIRST LEVEL
C
            MAP(1,J)=1
            ISTAT(J)=1
C
         ELSE
C
C                                     GENERATE MAP TABLE
C
            JP=JD(L+2)
            KJ=1
            IZJ=JD(L)*METR
            DO 10 KP=1,NP
               IZP=(ICSP(1,J)*METR)+((KP-1)*ICSP(3,J))
    5          CONTINUE
                  IF(IZJ.EQ.IZP) THEN
C                                     MATCHUP AT THIS POSITION
                     MAP(KP,J)=KJ
                     ISTAT(J)=1
                  ELSE IF(IZJ.LT.IZP) THEN
C                                     INPUT POSITION LOWER
                     KJ=KJ+1
                     IF(KJ.GT.JP) GO TO 20
                     IZJ=IZJ+JD(L+3)
                     GO TO 5
                  END IF
   10       CONTINUE
         END IF
   20 CONTINUE
C
C                                     DETERMINE STATUS OF MAPPING
C
      JST=MAX0(ISTAT(1),ISTAT(2),ISTAT(3))
      IF(JST.NE.2) THEN
C                                     CHECK IF IDENTICAL
         L=155
         DO 25 J=1,3
            L=L+5
            IF(ICSP(1,J).NE.JD( L )) GO TO 30
            IF(ICSP(2,J).NE.JD(L+1)) GO TO 30
            IF(ICSP(3,J).NE.JD(L+3)) GO TO 30
   25    CONTINUE
         JST=0
   30    CONTINUE
      END IF
C
      RETURN
      END
