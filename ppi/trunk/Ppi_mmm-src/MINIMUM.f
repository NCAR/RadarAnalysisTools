c
c----------------------------------------------------------------------X
c
      SUBROUTINE MINIMUM(DAT,IOUT,IIN1,IIN2,BDVAL,MNGATE,MXGATE,MANG,
     X     MXR,MXA,MXF,NSCTP)
C
C  FUNCTION - MINIMUM: F(OUT)=AMIN1(F(IIN1),F(IIN2))
C     IF NSCTP = 'EITHER', F(OUT)= whichever is not missing
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -    "     "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)
      CHARACTER*8 NSCTP

      DO 100 J=1,MANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL.AND.DATIN2.NE.BDVAL)THEN
               DAT(I,J,IOUT)=AMIN1(DATIN1,DATIN2)
            END IF
            IF(NSCTP.EQ.'EITHER')THEN
               IF(DATIN1.NE.BDVAL.AND.DATIN2.EQ.BDVAL)THEN
                  DAT(I,J,IOUT)=DATIN1
               END IF
               IF(DATIN1.EQ.BDVAL.AND.DATIN2.NE.BDVAL)THEN
                  DAT(I,J,IOUT)=DATIN2
               END IF
            END IF
 90      CONTINUE
 100  CONTINUE
      
      RETURN
      END
      
