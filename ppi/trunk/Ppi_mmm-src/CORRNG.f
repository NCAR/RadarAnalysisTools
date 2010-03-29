c
c----------------------------------------------------------------------X
c
      SUBROUTINE CORRNG(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X                  NANG,TMP1,TMP2,MXR,MXA,MXF)
C
C  FUNCTION - CORRNG: F(OUT)=CROSS-CORRELATION[F(IN1),F(IN2)]
C             Cross-correlation along range
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C     TMP1   - TEMPORARY STORAGE ARRAY
C     NR     - (NUMBER OF RANGE GATES)
C     NR2    - (   "    "   "     "  )/2 = (MAXIMUM LAG)
c     NR4    - (   "    "   "     "  )/4 = (# REQUIRED FOR CORRELATION)
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)

      NR=MXGATE-MNGATE+1
      NR2=NR/2
      NR4=NR/4

C     LOOP OVER ALL BEAMS, CALCULATING AVERAGE VALUES OF EACH FIELD
C     AND THEIR CROSS-CORRELATION FUNCTION (-1 .LE. CORREL .LE. 1).
C
      DO 100 J=1,NANG
         CNT1=0.0
         CNT2=0.0
         AVG1=0.0
         AVG2=0.0

C        CALCULATE AVERAGE VALUES (AVG1,AVG2) FOR THIS BEAM
C        IF TOO FEW VALUES TO CORRELATE, BDVAL ENTIRE BEAM
C
         DO 40 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            IF(DATIN1.NE.BDVAL)THEN
               AVG1=AVG1+DATIN1
               CNT1=CNT1+1.0
            END IF
            IF(DATIN2.NE.BDVAL)THEN
               AVG2=AVG2+DATIN2
               CNT2=CNT2+1.0
            END IF
   40    CONTINUE
         IF(CNT1.GE.NR4.AND.CNT2.GE.NR4)THEN
            AVG1=AVG1/CNT1
            AVG2=AVG2/CNT2
         ELSE
            DO 50 L=1,MXR
               DAT(L,J,IOUT)=BDVAL
   50       CONTINUE
            GO TO 100
         END IF

C        CALCULATE CORRELATION FUNCTION: LAGS=(-NR2,NR2)
C        LG=ABS(L): SUM THE PRODUCT FOR I=(MNGATE,MXGATE-LG)
C                   LAGS .LT. 0 : [F1(I+LG)-AVG1]*[F2(I   )-AVG2)]
C                   LAGS .GE. 0 : [F1(I   )-AVG1]*[F2(I+LG)-AVG2)]
C
         DO 90 L=-NR2,NR2
            LL=L+NR2+1
            CNT=0.0
            DATSQ1=0.0
            DATSQ2=0.0
            DATOUT=0.0

C           SUM PRODUCT OVER RANGE GATES FOR LAGS=LG=IABS(L)
C
            LG=IABS(L)
            DO 80 I=MNGATE,MXGATE-LG
               IF(L.LT.0)THEN
                  DATIN1=DAT(I+LG,J,IIN1)
                  DATIN2=DAT(I   ,J,IIN2)
               ELSE
                  DATIN1=DAT(I   ,J,IIN1)
                  DATIN2=DAT(I+LG,J,IIN2)
               END IF
               IF(DATIN1.NE.BDVAL.AND.DATIN2.NE.BDVAL)THEN
                  CNT=CNT+1.0
                  DATIN1=DATIN1-AVG1
                  DATIN2=DATIN2-AVG2
                  DATSQ1=DATSQ1+DATIN1*DATIN1
                  DATSQ2=DATSQ2+DATIN2*DATIN2
                  DATOUT=DATOUT+DATIN1*DATIN2
               END IF
   80       CONTINUE

            IF(CNT.GE.NR4)THEN
               DATSQ1=SQRT(DATSQ1/CNT)
               DATSQ2=SQRT(DATSQ2/CNT)
               DENO=(NR-LG)*DATSQ1*DATSQ2
               IF(DENO.GT.0.0)THEN
                  TMP1(LL)=DATOUT/DENO
               ELSE
                  TMP1(LL)=BDVAL
               END IF
            ELSE
               TMP1(LL)=BDVAL
            END IF

   90    CONTINUE

C        MOVE THE CROSS-CORRELATION FUNCTION INTO
C        OUTPUT ARRAY AND GO TO THE NEXT BEAM
C
         DO 96 L=1,MXR
c            LL=MNGATE+L-1
            LL=L
            IF(L.LE.NR)THEN
               DAT(LL,J,IOUT)=TMP1(L)
            ELSE
               DAT(LL,J,IOUT)=BDVAL
            END IF
   96    CONTINUE
  100 CONTINUE
      RETURN
      END
