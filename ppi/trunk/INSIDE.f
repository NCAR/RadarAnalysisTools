c
c----------------------------------------------------------------------X
c
      SUBROUTINE INSIDE(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X                  NANG,MXR,MXA,MXF)
C
C  FUNCTION - INSIDE: F(OUT)=F(IIN1); IF F(IIN2) .GE. C1 .AND.
C                                        F(IIN2) .LE. C2
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -    "     "      "  (TESTING ON THIS FIELD)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,MXA
         DO 90 I=1,MXR
            IF(J.GT.NANG .OR.
     X        (I.LT.MNGATE. OR. I.GT.MXGATE))THEN
               DAT(I,J,IOUT)=BDVAL
               GO TO 90
            END IF
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN2.EQ.BDVAL)GO TO 90

C     When DATIN2 is azimuth angle, change logic of test
C        C1=ALFT and C2=ARHT facing the azimuthal sector
C     LJM (7/7/2014)
C
            IF((C2-C1) .LT. 0.0)THEN
               IF((DATIN2 .LT. C1) .AND. (DATIN2 .GT. C1))GO TO 90
            ELSE
               IF(DATIN2.GE.C1 .AND. DATIN2.LE.C2)THEN
                  DAT(I,J,IOUT)=DATIN1
               END IF
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
