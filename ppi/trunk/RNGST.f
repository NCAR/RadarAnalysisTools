c
c----------------------------------------------------------------------X
c
      SUBROUTINE RNGST

      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'

      print *,'RNGST has been called: MXR,DROLD,R0=',MXR,DROLD,R0
      DO 80 I=1,MXR
         RNG(I,1)=(I-1)*DROLD+R0
         IF(RNG(I,1).LE.0.)RNG(I,1)=0.00001
         RCOR(I)=20.*ALOG10(RNG(I,1))
c         print *,'RNGST: i,rng=',i,rng(i,1)
 80   CONTINUE
      RETURN
      END
