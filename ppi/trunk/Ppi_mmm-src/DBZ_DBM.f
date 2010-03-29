c
c----------------------------------------------------------------------X
c
      SUBROUTINE DBZ_DBM(DAT,IOUT,IIN1,C1,BDVAL,RNG,MNGATE,MXGATE,
     X     NANG,MXR,MXA,MXF)
C
C  FUNCTION - DBZ_DBM: F(OUT)=DBZ-Rcon-20*Log(Rng)
C     Convert DBZ values to a received power
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     RNG       - Distance (km) to the range gates
C
      DIMENSION DAT(MXR,MXA,MXF),RNG(MXR,2)

      RADCON = C1
      DO 100 I=MNGATE,MXGATE
         IF(RNG(I,1).LE.0.0)THEN
            DO J=1,NANG
               DAT(I,J,IOUT)=BDVAL
            END DO
            GO TO 100
         END IF
         RNGCOR = 20.0*ALOG10(RNG(I,1))
         DO 90 J=1,NANG
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.EQ.BDVAL)GO TO 90
            DAT(I,J,IOUT)=DATIN1-RADCON-RNGCOR
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
