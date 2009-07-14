c------------------------------------------------------------
c     If more than one call to CONFLD is in the same routine,
c     Linux can give you a warning about disagreement between
c     parameter types.  
c     For example - SUBROUTINE PLTDRV:
c        Call CONFLD  if 1st parameter is an integer.
c             CONFLDR if 1st parameter is a floating point.
c------------------------------------------------------------
c
      SUBROUTINE CONFLD(RBUF,NPLANE,CON)
C
C        FILLS AN ARRAY WITH A CONSTANT VALUE (CON)
C
      DIMENSION RBUF(NPLANE)
      DO 10 I=1,NPLANE
      RBUF(I)=CON
   10 CONTINUE
      RETURN
      END

      SUBROUTINE CONFLDR(RBUF,NPLANE,CON)
C
C        FILLS AN ARRAY WITH A CONSTANT VALUE (CON)
C
      DIMENSION RBUF(NPLANE)
      DO 10 I=1,NPLANE
      RBUF(I)=CON
   10 CONTINUE
      RETURN
      END
