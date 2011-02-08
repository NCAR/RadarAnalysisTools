c
c----------------------------------------------------------------------X
c
      SUBROUTINE RA_SHFT(DAT,IOUT,IIN1,C1,C2,BDVAL,MNGATE,MXGATE,NANG,
     X     GSPC,AVGI,TMP1,TMP2,MXR,MXA,MXF)
C
C  FUNCTION - ISHIFT: F(I,J,IOUT)=F(I-IS,J-JS,IIN1); I.E. SHIFT THE 
C             INPUT AN INTEGER NUMBER OF RANGES AND ANGLES.
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     GSPC   - GATE SPACING IN (KM)
C     AVGI   - AVERAGE ANGLE INCREMENT (DEG)
C     C1     - RANGE SHIFT (KM)
C     C2     - ANGLE   "   (DEG)
C              (C1,C2.GT.0) - SHIFT ORIGINAL DATA RIGHT
C              (C1,C2.LT.0) -   "      "       "  LEFT
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)

C     COMPUTE INTEGER SHIFTS
C
      IS=NINT(C1/GSPC)
      JS=NINT(C2/ABS(AVGI))

C     SHIFT INPUT DATA AND STORE TEMPORARILY
C
      DO 30 J=1,NANG
         JJ=J-JS
         IF(JJ.GE.1.AND.JJ.LE.NANG)THEN
            DO 10 I=MNGATE,MXGATE
               II=I-IS
               IF(II.GE.MNGATE.AND.II.LE.MXGATE)THEN
                  TMP2(I,J)=DAT(II,JJ,IIN1)
               ELSE
                  TMP2(I,J)=BDVAL
               END IF
 10         CONTINUE
         ELSE
            DO 20 I=MNGATE,MXGATE
               TMP2(I,J)=BDVAL
 20         CONTINUE
         END IF
 30   CONTINUE
      
C     TRANSFER SHIFTED DATA INTO OUTPUT ARRAY
C
      DO 50 J=1,NANG
         DO 40 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=TMP2(I,J)
 40      CONTINUE
 50   CONTINUE

      RETURN
      END
