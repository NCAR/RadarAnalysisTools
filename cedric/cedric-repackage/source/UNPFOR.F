      SUBROUTINE UNPFOR(RBUF,NX,NY,IBEG,IEND,JBEG,JEND,
     X                  VREF,VNYQ,VMIN,VMAX,BAD,INOD)
C
C        FORCED UNFOLDING OF DATA IN AN (X,Y) PLANE
C
      DIMENSION RBUF(NX,NY)
      DATA EPS/0.01/
      VN2=VNYQ*2.0
            DO 87 J=JBEG,JEND
            DO 87 I=IBEG,IEND
               VEL=RBUF(I,J)
               IF(VEL.EQ.BAD) GO TO 87
               IF(INOD.EQ.1.AND.VEL.GE.VMIN.AND.VEL.LE.VMAX) GO TO 87
               IF(INOD.EQ.2.AND.(VEL.LT.VMIN.OR.VEL.GT.VMAX)) GO TO 87
               DIF=VREF-VEL
               IF(ABS(DIF).LE.VNYQ) GO TO 87
               K=(DIF+SIGN(VNYQ-EPS,DIF))/VN2
               RBUF(I,J)=VEL+K*VN2
   87       CONTINUE
      RETURN
      END
