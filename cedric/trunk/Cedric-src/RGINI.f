      SUBROUTINE RGINI(XL,XR,YB,YT,PLWIND,A,NLOC,IAXORD)
C
C     Set plot window parameters:
C             (X,Y) - (horizontal,vertical) axes for plotting
C         XBEG,XEND - Beginning/ending indices for plot X-axis
C         XRANGE    - Beginning/ending coordinates for plot X-axis
C         YBEG,YEND - Beginning/ending indices for plot Y-axis
C         YRANGE    - Beginning/ending coordinates for plot Y-axis
C         DELRG(1)  - Width of plot X-axis
C         DELRG(2)  - Width of plot Y-axis
C         XL,XR     - Fractional boundaries for plot X-axis
C         YB,YT     - Fractional boundaries for plot Y-axis
C
      PARAMETER (MXVSCT=20)
      COMMON /OVRLAY/ IOLAY,NPOLAY(5,MXVSCT),ICOLAY(10)
      COMMON /AXSTRB/ AXSFAC(3)
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
C
      DIMENSION ARYD(2)
      DIMENSION  A(3,3),IAXORD(3)
      DIMENSION  NLOC(3),PLWIND(2,3)
      DATA ARYD/ 0.800, 0.734 /
      DATA BASE/ 0.0625 /
C
C
      I=MIN0(1,IOLAY)+1
      ABDELT=ARYD(I)
      IXAXIS = IAXORD(1)
      IYAXIS = IAXORD(2)
C
C  ABSOLUTE PLOT RANGE.
      XBEG = (PLWIND(1,IXAXIS)-A(1,IXAXIS))/A(3,IXAXIS) + 1.0
      XEND = (PLWIND(2,IXAXIS)-A(1,IXAXIS))/A(3,IXAXIS) + 1.0
      YBEG = (PLWIND(1,IYAXIS)-A(1,IYAXIS))/A(3,IYAXIS) + 1.0
      YEND = (PLWIND(2,IYAXIS)-A(1,IYAXIS))/A(3,IYAXIS) + 1.0
C
C  APPARENT PLOT RANGE
      XRANGE(1) = PLWIND(1,IXAXIS)
      XRANGE(2) = PLWIND(2,IXAXIS)
      YRANGE(1) = PLWIND(1,IYAXIS)
      YRANGE(2) = PLWIND(2,IYAXIS)
C
C  CALCULATE VIEWING SPACE FOR PLOT
C
      XL=BASE
      YT=BASE+ABDELT
      DELRG(1)=ABS(XRANGE(2)-XRANGE(1))
      DELRG(2)=ABS(YRANGE(2)-YRANGE(1))
      DXCRT=DELRG(1)*AXSFAC(IXAXIS)
      DYCRT=DELRG(2)*AXSFAC(IYAXIS)
      MX=DXCRT+0.005
      MY=DYCRT+0.005
      IF(MX-MY) 125,200,150
  125 CONTINUE
C  X-AXIS SHORTER THAN Y-AXIS
      XR=XL+ABDELT*(DXCRT/DYCRT)
      YB=BASE
      GO TO 250
  150 CONTINUE
C  X-AXIS LONGER THAN Y-AXIS
      XR=XL+ABDELT
      YB=YT-ABDELT*(DYCRT/DXCRT)
      GO TO 250
  200 CONTINUE
C  X-AXIS EQUAL TO Y-AXIS
      XR=XL+ABDELT
      YB=BASE
  250 CONTINUE
C
C        ADJUST COORDINATE AXIS FOR PROPER UNITS AND SCALING
C
      XRANGE(1)=XRANGE(1)*SCLAXS(IXAXIS,IUNAXS)
      XRANGE(2)=XRANGE(2)*SCLAXS(IXAXIS,IUNAXS)
       DELRG(1)= DELRG(1)*SCLAXS(IXAXIS,IUNAXS)
      YRANGE(1)=YRANGE(1)*SCLAXS(IYAXIS,IUNAXS)
      YRANGE(2)=YRANGE(2)*SCLAXS(IYAXIS,IUNAXS)
       DELRG(2)= DELRG(2)*SCLAXS(IYAXIS,IUNAXS)

c      print *,'RGINI: xbeg,xend,xrang=',xbeg,xend,xrange
c      print *,'RGINI: ybeg,yend,yrang=',ybeg,yend,yrange
c      print *,'RGINI:           delrg=',delrg
      RETURN
      END

