c
c----------------------------------------------------------------------X
c
      SUBROUTINE RANDOM(DAT,IOUT,C1,C2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - GENERATE RANDOM NUMBERS, UNIFORMLY DISTRIBUTED (C1,C2)
C
C     IOUT   - OUTPUT FIELD NUMBER
C
      DIMENSION DAT(MXR,MXA,MXF)
      DATA IR1,IR2/ 3, 11 /


      IR1=IR1+2
      CON=C2-C1

      DO 100 J=1,MXA
         DO 90 I=1,MXR
            DAT(I,J,IOUT)=BDVAL
            VX=RANF()
            DAT(I,J,IOUT)=CON*VX+C1
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
