c
c----------------------------------------------------------------------X
c
      SUBROUTINE NORMAL(DAT,IOUT,C1,C2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - GENERATE RANDOM NUMBER, NORMALLY DISTRIBUTED
C             WITH MEAN C1 AND STANDARD DEVIATION C2
C
C     IOUT   - OUTPUT FIELD NUMBER
C
      DIMENSION DAT(MXR,MXA,MXF)
      DATA IR1,IR2/ 3, 11 /


      IR1=IR1+2
      IF(C2.LE.0.0) C2=1.0
      PI2=ATAN(1.)*8.0

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            U1=RANF()
            U2=RANF()
            VX= SQRT(-2.0*ALOG(U1)) * COS(PI2*U2)
            DAT(I,J,IOUT)=C2*VX+C1
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
