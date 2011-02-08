c
c----------------------------------------------------------------------X
c
      SUBROUTINE ABSVAL(DAT,IOUT,IIN1,C1,C2,C3,BDVAL,MNGATE,MXGATE,
     X     MANG,TMP1,MXR,MXA,MXF)
C
C  FUNCTION - LINEAR: F(OUT)=C1*F(IN)+C2
C                     WITH RANGE SHIFT OF C3
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF),TMP1(MXR)
      
      DO 100 J=1,MANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            IF(DATIN1.NE.BDVAL)THEN
               DAT(I,J,IOUT)=ABS(DATIN1)
            ELSE
               DAT(I,J,IOUT)=BDVAL
            END IF
 90      CONTINUE
 100  CONTINUE

      RETURN
      END

