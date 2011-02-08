c
c----------------------------------------------------------------------X
c
      SUBROUTINE CORANG(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X                  NANG,TMP1,TMP2,MXR,MXA,MXF)
C
C  FUNCTION - CORANG: F(OUT)=CROSS-CORRELATION[F(IN1),F(IN2)]
C             Cross-correlation along angle
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C     TMP1   - TEMPORARY STORAGE ARRAY
C     NA     - (NUMBER OF ANGLES)
C     NA2    - (   "    "   "   )/2 = (MAXIMUM LAG)
c     NA4    - (   "    "   "   )/4 = (# REQUIRED FOR CORRELATION)
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)

      MNANG=1
      MXANG=NANG

      NA=MXANG-MNANG+1
      NA2=NA/2
      NA4=NA/4

C     LOOP OVER ALL RANGES, CALCULATING AVERAGE VALUES OF EACH FIELD
C     AND THEIR CROSS-CORRELATION FUNCTION (-1 .LE. CORREL .LE. 1).
C
      DO 100 I=1,MXGATE
         CNT1=0.0
         CNT2=0.0
         AVG1=0.0
         AVG2=0.0

C        CALCULATE AVERAGE VALUES (AVG1,AVG2) FOR THIS RANGE
C        IF TOO FEW VALUES TO CORRELATE, BDVAL ENTIRE RANGE
C
         DO 40 J=MNANG,MXANG
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
         IF(CNT1.GE.NA4.AND.CNT2.GE.NA4)THEN
            AVG1=AVG1/CNT1
            AVG2=AVG2/CNT2
         ELSE
            DO 50 L=1,MXA
               DAT(I,L,IOUT)=BDVAL
   50       CONTINUE
            GO TO 100
         END IF

C        CALCULATE CORRELATION FUNCTION: LAGS=(-NA2,NA2)
C        MG=ABS(M): SUM THE PRODUCT FOR J=(MNANG,MXANG-MG)
C                   LAGS .LT. 0 : [F1(J+MG)-AVG1]*[F2(J   )-AVG2)]
C                   LAGS .GE. 0 : [F1(J   )-AVG1]*[F2(J+MG)-AVG2)]
C
         DO 90 M=-NA2,NA2
            MM=M+NA2+1
            CNT=0.0
            DATSQ1=0.0
            DATSQ2=0.0
            DATOUT=0.0

C           SUM PRODUCT OVER RANGE GATES FOR LAGS=MG=IABS(M)
C
            MG=IABS(M)
            DO 80 J=MNANG,MXANG-MG
               IF(M.LT.0)THEN
                  DATIN1=DAT(I,J+MG,IIN1)
                  DATIN2=DAT(I,J   ,IIN2)
               ELSE
                  DATIN1=DAT(I,J   ,IIN1)
                  DATIN2=DAT(I,J+MG,IIN2)
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

            IF(CNT.GE.NA4)THEN
               DATSQ1=SQRT(DATSQ1/CNT)
               DATSQ2=SQRT(DATSQ2/CNT)
               DENO=(NA-MG)*DATSQ1*DATSQ2
               IF(DENO.GT.0.0)THEN
                  TMP1(MM)=DATOUT/DENO
               ELSE
                  TMP1(MM)=BDVAL
               END IF
            ELSE
               TMP1(MM)=BDVAL
            END IF

   90    CONTINUE

C        MOVE THE CROSS-CORRELATION FUNCTION INTO
C        OUTPUT ARRAY AND GO TO THE NEXT RANGE
C
         DO 96 M=1,MXA
            MM=MNANG+M-1
            IF(M.LE.NA)THEN
               DAT(I,MM,IOUT)=TMP1(M)
            ELSE
               DAT(I,MM,IOUT)=BDVAL
            END IF
   96    CONTINUE
  100 CONTINUE
      RETURN
      END
