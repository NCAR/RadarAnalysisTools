      SUBROUTINE STPOBD (ZMIN,ZMAX,NST)
C
      PARAMETER (MAXT=13)
C
      DIMENSION TAB(MAXT)
      DATA BIAS/0.499/
      DATA TAB/ 100.0, 50.0, 25.0, 20.0, 10.0,
     X            5.0,  2.5,  2.0,  1.0,  0.5,
     X           0.25,  0.1, 0.01/
C
      NST = 0
C
      IF (ZMIN.EQ.ZMAX) THEN
         ZMIN = NINT(ZMIN-0.5)
         ZMAX = NINT(ZMAX+0.5)
         GO TO 300
      END IF
C
      CRAN = ZMAX - ZMIN
C
C     KICK OUT OF ROUTINE & SEND OUT A FLAG INDICATING
C        ERROR IF OUT OF RANGE:
C
      IF (CRAN.GT.TAB(1)*10.0.OR.CRAN.LT.TAB(MAXT)*10.0) THEN
         NST = 1
         GO TO 300
      END IF
C
      DO 20 I = 1,MAXT
         WID = TAB(I)*10.0
         SMUL = 1./TAB(I)
         IF (CRAN.LE.WID) GO TO 20
         ZMIN = NINT(ZMIN*SMUL-BIAS)*TAB(I)
         ZMAX = NINT(ZMAX*SMUL+BIAS)*TAB(I)
         GO TO 300
20    CONTINUE
C
300   RETURN
      END
