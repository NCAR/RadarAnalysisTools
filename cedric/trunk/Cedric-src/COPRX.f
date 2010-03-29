c------------------------------------------------------------
c     If more than one call to COPRX is in the same routine,
c     Linux can give you a warning about disagreement between
c     parameter types.  
c     For example - SUBROUTINE SYNNER:
c        Call COPRX  if 1st parameter is a floating point.
c             COPRXI if 1st parameter is an integer.
c------------------------------------------------------------
c
      SUBROUTINE COPRX(RO,RI,N)
C
C        COPIES ONE ARRAY TO ANOTHER
C
C               RI- INPUT ARRAY
C               RO- OUTPUT ARRAY
C
      DIMENSION RI(1),RO(1)
      DO 10 I=1,N
         RO(I)=RI(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE COPRXI(RO,RI,N)
C
C        COPIES ONE ARRAY TO ANOTHER
C
C               RI- INPUT ARRAY
C               RO- OUTPUT ARRAY
C
      DIMENSION RI(1),RO(1)
      DO 10 I=1,N
         RO(I)=RI(I)
   10 CONTINUE
      RETURN
      END
