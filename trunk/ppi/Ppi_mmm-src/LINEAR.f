c
c----------------------------------------------------------------------X
c
      SUBROUTINE LINEAR(DAT,IOUT,IIN1,C1,C2,C3,BDVAL,MNGATE,MXGATE,
     X     MANG,TMP1,MXR,MXA,MXF)
C
C  FUNCTION - LINEAR: F(OUT)=C1*F(IN)+C2
C                     WITH RANGE SHIFT OF C3
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     C3     - INTEGER RANGE SHIFT 
C              (C3.GT.0 - SHIFT RIGHT, C3.LT.0 - SHIFT LEFT)
C
      DIMENSION DAT(MXR,MXA,MXF),TMP1(MXR)

      IS=NINT(C3)
      DO 100 J=1,MANG
         DO 80 I=MNGATE,MXGATE
            II=I+IS
            TMP1(II)=DAT(I,J,IIN1)
 80      CONTINUE
         DO 90 I=MNGATE,MXGATE
            DATIN1=TMP1(I)
            IF(DATIN1.NE.BDVAL)THEN
               DAT(I,J,IOUT)=C1*DATIN1+C2
            ELSE
               DAT(I,J,IOUT)=BDVAL
            END IF
 90      CONTINUE
 100  CONTINUE
      RETURN
      END

