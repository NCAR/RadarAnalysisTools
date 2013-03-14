C
C The subroutine ISGFXY.
C --- ---------- -------
C
      SUBROUTINE ISGFXY (X,Y,RMX,RMY)
C
C     MODIFIED 6/92 BY BILL ANDERSON FOR PROPER SCALING OF AXES
C
C This routine, given two coordinates of a point on the current slab,
C supplies the missing third coordinate and computes the fractional
C coordinates of the projection of that point on the plotter frame.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
      COMMON /AXSPAC/ XD,YD,ZD
C
C Arithmetic statement functions for scaling.
C
      SU(UTEMP) = UTEMP*XD
      SV(VTEMP) = VTEMP*YD
      SW(WTEMP) = WTEMP*ZD
C
      XX = X
      YY = Y
      IF (ISLBT) 10,20,30
   10 CALL ISTR32 (SU(U),SV(XX-1.),SW(YY-1.),RMX,RMY,DUM,2)
      GO TO  40
   20 CALL ISTR32 (SU(XX-1.),SV(V),SW(YY-1.),RMX,RMY,DUM,2)
      GO TO  40
   30 CALL ISTR32 (SU(XX-1.),SV(YY-1.),SW(W),RMX,RMY,DUM,2)
   40 RETURN
C
      END
