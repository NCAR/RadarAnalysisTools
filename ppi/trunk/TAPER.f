c
c----------------------------------------------------------------------X
c
      SUBROUTINE TAPER (T,N)
      DIMENSION       T(1)
      PI = 3.141592653589793
      M = N/10
      IF (M .LT. 4) M = 4
      MM = N-M+1
      FM = M
      DO 101 I=1,M
         F = I
         WT = .5*(1-COS(F*PI/FM))
         T(I) = WT*T(I)
  101 CONTINUE
      IS = N-M+1
      DO 102 I=MM,N
         F = N+1-I
         WT = .5*(1-COS(F*PI/FM))
         T(I) = T(I)*WT
  102 CONTINUE
      RETURN
      END
