c
c----------------------------------------------------------------------X
c
      SUBROUTINE AZLIM(XN,XX,YN,YX,AZN,AZX,IAZC)
C
C  FIND AZIMUTH LIMITS OF PLOT WINDOW
C     IF IAZC = .TRUE., THE GRID IS ENTIRELY SOUTH OF THE RADAR AND
C                       CROSSES AZM=180 DEG.
C     IF IAZC =  .TRUE., THEN ANGLES ARE LEFT IN THE RANGE    0 TO 360
C               .FALSE.,   "     "    "   PUT  "  "    "   -180 TO 180
C     SEE ROUTINES RDFF AND RDUF FOR
C                  IF(AZ.GT.180..AND..NOT.IAZC)AZ=AZ-360.
C
      DATA PI/3.1415926535898/
      LOGICAL IAZC
      IAZC=.FALSE.
      PX=XX*XN
      PY=YX*YN
      IF(PX.LT.0.0) GO TO 30
      IF(PY.LT.0.0) GO TO 20
      IF(XX.LE.0.0) GO TO 10
      IF(YN.GE.0.0) THEN
         AZX=ATAN2(XX,YN)
         AZN=ATAN2(XN,YX)
         GO TO 200
      ELSE
         AZX=ATAN2(XN,YN)
         AZN=ATAN2(XX,YX)
         GO TO 200
      END IF
   10 IF(YN.GE.0.0) THEN
         AZX=ATAN2(XX,YX)
         AZN=ATAN2(XN,YN)
         GO TO 200
      ELSE
         AZX=ATAN2(XN,YX)
         AZN=ATAN2(XX,YN)
         GO TO 200
      END IF
   20 IF(XX.LE.0.0) THEN
         AZX=ATAN2(XX,YX)
         AZN=ATAN2(XX,YN)
         IF(AZN.GT.0.0) AZN=-AZN
         GO TO 200
      ELSE
         AZX=ATAN2(XN,YN)
         AZN=ATAN2(XN,YX)
         GO TO 200
      END IF
   30 IF(PY.LT.0.0) THEN
         AZX=PI
         AZN=-PI
         GO TO 200
      ELSE IF(YN.GE.0.0) THEN
         AZX=ATAN2(XX,YN)
         AZN=ATAN2(XN,YN)
         GO TO 200
      ELSE
         AZX=ATAN2(XN,YX)+2.*PI
         AZN=ATAN2(XX,YX)
         IAZC=.TRUE.
         GO TO 200
      END IF
200   AZN=AZN*57.295779
      AZX=AZX*57.295779
      PRINT 333,AZN,AZX
333   FORMAT(1X,'AZLIM-AZN,AZX= ',2F7.1)
      RETURN
      END
