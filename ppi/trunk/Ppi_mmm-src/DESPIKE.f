c
c----------------------------------------------------------------------X
c
      SUBROUTINE DESPIKE(DAT,IOUT,IIN1,IIN2,C1,C2,C3,C4,BDVAL,MNGATE,
     X     MXGATE,NGTS,NANG,TMP1,TMP2,MXR,MXA,MXF)
C
C  FUNCTION - COMPUTE THE UNBIASED VARIANCE OF A FIELD (IIN2) OVER A
C             LOCAL REGION WITH RANGE DIMENSIONS = 2*C1+1 GATES AND ANGLE
C             DIMENSIONS = 2*C2+1 BEAMS AND USE THIS VARIANCE OR THE MEAN
C             TO REMOVE DATA SPIKES IN IIN1.  FILL THE HOLES WITH A LEAST
C             SQUARES LINEAR FIT OF NON-BAD VALUES WITHIN THE LOCAL REGION.
C
C       C3   - (>0) DECIMATE ON MEAN, (<0) DECIMATE ON VARIANCE
C       C4   - (<0) DO NOT, (>0) DO FILL WITH LOCAL LEAST SQUARES FIT
C              ABS(C4) IS NUMBER OF POINTS REQUIRED FOR GOOD CENTRAL VALUE
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   FOR OUTPUT INTO F(IOUT)
C     IIN2   -  INPUT   "      "    "  CHECKING THE EXISTENCE OF SPIKES
C              GENERALLY, IIN2 WILL BE THE SAME AS IIN1.
C     TMP2   - TEMPORARY STORAGE ARRAY
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)
      DIMENSION AM(3),BM(3),CM(3),DM(3)
      DATA EPS/0.00001/

      IR=NINT(C1)
      JA=NINT(C2)
      DIFTST=C3
      VARTST=C3*C3
      IFLAG=NINT(C4)
      CNTMIN=ABS(C4)
      IF(CNTMIN.LT.2.0)CNTMIN=2.0

C     OUTER LOOP OVER ALL CENTRAL INDICES (II,JJ).  COMPUTE THE LOCAL
C     VARIANCE AND MEAN OF FIELD(IIN2) WITHIN REGION (2*IR+1,2*JA+1) 
C     SURROUNDING THE CENTRAL LOCATION.  STORE FIELD(IIN1) IN TMP2 
C     WHEN (C3<0) THE LOCAL VARIANCE .LE. VARTST OR (C3>0) THE
C     DIFFERENCE BETWEEN CENTRAL AND LOCAL MEAN .LE. DIFTST
C
      DO JJ=1,NANG
         DO II=MNGATE,MXGATE
            TMP2(II,JJ)=DAT(II,JJ,IIN1)
         END DO
      END DO

      DO 100 JJ=1,NANG
         J1=JJ-JA
         J2=JJ+JA
         IF(J1.LT.1)J1=1
         IF(J2.GT.NANG)J2=NANG

         DO 90 II=MNGATE,MXGATE
            I1=II-IR
            I2=II+IR
            IF(I1.LT.MNGATE)I1=MNGATE
            IF(I2.GT.MXGATE)I2=MXGATE
            CNT=0.0
            SUM=0.0
            SUMSQ=0.0


C     INNER LOOPS SURROUNDING CENTRAL LOCATION.  INCLUDE THE CENTRAL
C     VALUE WHEN COMPUTING THE VARIANCE, BUT EXCLUDE IT IN THE MEAN.
C     
            DO 80 J=J1,J2
               DO 70 I=I1,I2

                  IF(DAT(I,J,IIN2).EQ.BDVAL)GO TO 70
                  IF(C3.GE.0.0)THEN
                     IF(I.EQ.II .AND. J.EQ.JJ)GO TO 70
                  END IF
                  SUM=SUM+DAT(I,J,IIN2)
                  SUMSQ=SUMSQ+DAT(I,J,IIN2)*DAT(I,J,IIN2)
                  CNT=CNT+1.0


 70            CONTINUE
 80         CONTINUE

            IF(CNT.GE.CNTMIN)THEN
               DBAR=SUM/CNT
               DVAR=(SUMSQ-CNT*DBAR*DBAR)/(CNT-1.0)
               DIFF=ABS(DBAR-DAT(II,JJ,IIN2))

               IF(C3.GE.0.0)THEN
                  IF(DIFF.GT.DIFTST)TMP2(II,JJ)=BDVAL
               ELSE
                  IF(DVAR.GT.VARTST)TMP2(II,JJ)=BDVAL
               END IF
            ELSE
               TMP2(II,JJ)=BDVAL
            END IF
 90      CONTINUE
 100  CONTINUE

      IF(IFLAG.LT.0)THEN
         DO JJ=1,NANG
            DO II=MNGATE,MXGATE
               DAT(II,JJ,IOUT)=TMP2(II,JJ)
            END DO
         END DO
         RETURN
      END IF

C     OUTER LOOP OVER ALL CENTRAL INDICES (II,JJ).  COMPUTE THE LOCAL
C     LEAST-SQUARES, LINEAR FIT OF TMP2 WITHIN REGION (2*IR+1,2*JA+1) 
C     SURROUNDING THE CENTRAL LOCATION.  STORE THIS FIT IN FIELD(IOUT) 
C     WHEN CENTRAL VALUE IS MISSING, OTHERWISE TRANSFER FIELD(IIN1).
C     
      IR=IFLAG*NINT(C1)
      JA=IFLAG*NINT(C2)
      DO 120 JJ=1,NANG
         J1=JJ-JA
         J2=JJ+JA
         IF(J1.LT.1)J1=1
         IF(J2.GT.NANG)J2=NANG
         DO 110 II=MNGATE,MXGATE
            I1=II-IR
            I2=II+IR
            IF(I1.LT.MNGATE)I1=MNGATE
            IF(I2.GT.MXGATE)I2=MXGATE
            DAT(II,JJ,IOUT)=BDVAL
            IF(TMP2(II,JJ).NE.BDVAL)THEN
               DAT(II,JJ,IOUT)=TMP2(II,JJ)
            ELSE
               DO 102 L=1,3
                  AM(L)=0.0
                  BM(L)=0.0
                  CM(L)=0.0
                  DM(L)=0.0
 102           CONTINUE

C     INNER LOOP FOR LEAST-SQUARES FITTING OVER NON-BAD VALUES
C
               DO 106 I=I1,I2
                  IX=I-II
                  DO 104 J=J1,J2
                     IY=J-JJ
                     IF(TMP2(I,J).EQ.BDVAL) GO TO 104
                     AM(1)=AM(1)+1.0
                     AM(2)=AM(2)+IX
                     AM(3)=AM(3)+IY
                     BM(2)=BM(2)+IX*IX
                     BM(3)=BM(3)+IX*IY
                     CM(3)=CM(3)+IY*IY
                     DM(1)=DM(1)+TMP2(I,J)
                     DM(2)=DM(2)+IX*TMP2(I,J)
                     DM(3)=DM(3)+IY*TMP2(I,J)
 104              CONTINUE
 106           CONTINUE
               BM(1)=AM(2)
               CM(1)=AM(3)
               CM(2)=BM(3)
               T1=BM(2)*CM(3)-BM(3)*CM(2)
               T2=BM(1)*CM(3)-BM(3)*CM(1)
               T3=BM(1)*CM(2)-BM(2)*CM(1)
               DENO=AM(1)*T1-AM(2)*T2+AM(3)*T3
               IF(DENO.LE.EPS) GO TO 110
               ANUM=DM(1)*T1-DM(2)*T2+DM(3)*T3
               DAT(II,JJ,IOUT)=ANUM/DENO
            END IF
 110     CONTINUE
 120  CONTINUE
      RETURN
      END
