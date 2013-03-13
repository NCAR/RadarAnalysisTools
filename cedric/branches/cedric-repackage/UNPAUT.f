      SUBROUTINE UNPAUT(RBUF,OBUF,NX,NY,IBEG,IEND,JBEG,JEND,IADJ,
     X                  VNYQ,VPTS,SD,BAD,ITYP)
C
C        AUTOMATIC UNFOLDING OF DATA IN AN (X,Y) PLANE
C           ITYP=0, DECIMATE AND UNFOLD RESULT INTO RBUF
C               =1, DECIMATE ONLY RESULT INTO OBUF
C               =2, UNFOLD ONLY USING OBUF AS A REFERENCE, UPDATE TEMPLATE
C                   INTO OBUF.
C               =3, SAME AS (2) ONLY DATA FILL THE TEMPLATE FIELD AND
C                   RETURN WITH THE OUTPUT IN OBUF
C
C     August 29, 1997 (LJM) - changed DECILOC to have local standard deviation 
C                             in the output if the radius is .lt. zero
C
      DIMENSION RBUF(NX,NY),OBUF(NX,NY)
      DATA EPS/0.01/
      VAR=SD*SD
      IF(ITYP.EQ.3) GO TO 45
      IF(ITYP.EQ.2) GO TO 50
C
C        DECIMATE RBUF- RESULT INTO OBUF
C
            DO 84 J=JBEG,JEND
               J1=MAX0( 1,J-ABS(IADJ))
               J2=MIN0(NY,J+ABS(IADJ))
               DO 84 I=IBEG,IEND
                  I1=MAX0( 1,I-ABS(IADJ))
                  I2=MIN0(NX,I+ABS(IADJ))
                  IF(RBUF(I,J).EQ.BAD) GO TO 84
                  DIV=0.0
                  SUM=0.0
                  SUMSQ=0.0
                  OBUF(I,J)=BAD
                  DO 83 JC=J1,J2
                  DO 83 IC=I1,I2
                     IF(RBUF(IC,JC).EQ.BAD) GO TO 83
                     DIV=DIV+1.0
                     SUM=SUM+RBUF(IC,JC)
                     SUMSQ=SUMSQ+RBUF(IC,JC)**2
   83             CONTINUE
                  IF(DIV.LT.VPTS) GO TO 84
                  AVG=SUM/DIV
                  TEST=SUMSQ/DIV-AVG**2
C
C                 UNBIASED ESTIMATE OF VARIANCE
C
                  IF(DIV.GT.1.0)TEST=(TEST*DIV)/(DIV-1.0)
C+++++ DEBUG START
CDISABLED         IF(TEST.LE.VAR.AND.ABS(RBUF(I,J)-AVG).LE.SD)
C
C     If dimension of decimation region is negative, put the 
C     local standard deviation of input field into output.
C
                  IF(IADJ.LT.0)THEN
                     IF(TEST.GE.0.0)OBUF(I,J)=SQRT(TEST)
                  ELSE IF(ABS(RBUF(I,J)-AVG).LE.SD)THEN
                     OBUF(I,J)=RBUF(I,J)
                  END IF
C+++++ DEBUG END
   84       CONTINUE
C
C     RETURN IF DECIMATION ONLY IS TO BE PERFORMED
C
      IF(ITYP.EQ.1) RETURN
   45 CONTINUE
      VN2=VNYQ*2.0
C
C           PERFORM THE DATA FILL OPERATION ON DECIMATED ARRAY
C
            CALL EXTEND(OBUF,NX,NY,1,BAD)
            IF(ITYP.EQ.3) THEN
C
C             RESET OBUF TO RBUF OUTSIDE THE WINDOW
C
              DO 48 J=1,NY
              DO 47 I=1,NX
                IF(J.GE.JBEG.AND.J.LE.JEND.AND.
     X             I.GE.IBEG.AND.I.LE.IEND) GO TO 47
                OBUF(I,J)=RBUF(I,J)
   47         CONTINUE
   48         CONTINUE
            END IF
   50 CONTINUE
      VN2=VNYQ*2.0
C
C        PERFORM UNFOLDING USING OBUF AS A REFERENCE
C
C
C           COMPARE ESTIMATES AND UNFOLD IF WARRANTED
C
            DO 95 J=JBEG,JEND
            DO 95 I=IBEG,IEND
               VREF=OBUF(I,J)
               IF(VREF.EQ.BAD) GO TO 95
               VEL=RBUF(I,J)
               IF(VEL.EQ.BAD) GO TO 94
               DIF=VREF-VEL
               IF(ABS(DIF).LE.VNYQ) GO TO 94
               K=(DIF+SIGN(VNYQ-EPS,DIF))/VN2
               RBUF(I,J)=VEL+K*VN2
   94          CONTINUE
               OBUF(I,J)=RBUF(I,J)
   95       CONTINUE
      RETURN
      END
