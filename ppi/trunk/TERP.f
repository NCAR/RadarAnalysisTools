c
c----------------------------------------------------------------------X
c
      SUBROUTINE TERP(XMIN,XMAX,YMIN,YMAX)
      COMMON /TERPC/U1,V1,U2,V2,U,V
      COMMON /TERPC1/ DU,DV
  100 I=1
      DU=U1-U2
      DV=V1-V2
      A=U2-XMIN
      B=U2-XMAX
      C=V2-YMIN
      D=V2-YMAX
      IF(A) 110,140,120
  110 I=I+1
      GO TO 140
  120 IF(B) 140,140,130
  130 I=I+2
  140 IF(C)150,200,160
  150 I=I+6
      GO TO 200
  160 IF(D)200,200,170
  170 I=I+3
  200 GO TO (900,210,220,230,240,250,260,270,280),I
  210 U=XMIN
      GO TO 300
  220 U=XMAX
      GO TO 300
  230 V=YMAX
      GO TO 350
  240 Z=F(YMAX)
      IF(Z.LT.XMIN)THEN
      GO TO 210
      ELSE
      GO TO 230
      END IF
  250 Z=F(YMAX)
      IF(Z.LE.XMAX)THEN
      GO TO 230
      ELSE
      GO TO 220
      END IF
  260 V=YMIN
      GO TO 350
  270 Z=F(YMIN)
      IF(Z.LT.XMIN)THEN
      GO TO 210
      ELSE
      GO TO 260
      END IF
  280 Z=F(YMIN)
      IF(Z.LE.XMAX)THEN
      GO TO 260
      ELSE
      GO TO 220
      END IF
  300 V=G(U)
      RETURN
  350 U=F(V)
      RETURN
  360 U=Z
      RETURN
  900 U=U2
      V=V2
      RETURN
      END
