c
c----------------------------------------------------------------------X
c
      FUNCTION DLINE(DUM)

      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      COMMON /DLINEC/X1,Y1,X2,Y2
      COMMON /TERPC/U1,V1,U2,V2,U,V

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL
      LOGICAL IP1,IP2,DLINE
      DLINE=.FALSE.
      IP1=.FALSE.
      IP2=.FALSE.
      IF(IBSCAN.NE.1)THEN
         IF(X1.LT.GXMIN(ITPOLD) .OR. X1.GT.GXMAX(ITPOLD).OR.
     +      Y1.LT.GYMIN(ITPOLD) .OR. Y1.GT.GYMAX(ITPOLD))
     +      IP1=.TRUE.
         IF(X2.LT.GXMIN(ITPOLD) .OR. X2.GT.GXMAX(ITPOLD).OR.
     +      Y2.LT.GYMIN(ITPOLD) .OR. Y2.GT.GYMAX(ITPOLD))
     +      IP2=.TRUE.
      END IF
      IF(IP1.AND.IP2) RETURN
      IF(IP1.OR.IP2) GO TO 20
   10 CALL LINED(X1,Y1,X2,Y2)
      DLINE=.TRUE.
      RETURN
   20 IF(IP2) GO TO 30
      U1=X2
      V1=Y2
      U2=X1
      V2=Y1
      GO TO 40
   30 U1=X1
      U2=X2
      V1=Y1
      V2=Y2
   40 CALL TERP(GXMIN(ITPOLD),GXMAX(ITPOLD),GYMIN(ITPOLD),
     +GYMAX(ITPOLD))
      X1=U1
      Y1=V1
      X2=U
      Y2=V
      GO TO 10
      END
