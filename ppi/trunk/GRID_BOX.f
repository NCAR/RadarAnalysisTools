c
c----------------------------------------------------------------------X
c
      SUBROUTINE GRID_BOX(XMID,YMID,XDIM,YDIM,XRD,YRD,ROTATE,ANGXAX,
     +     LAB6,CSIZ)
C
C     Draws a box centered at (XMID,YMID) and having 
C     dimensions (XDIM,YDIM).
C
      INCLUDE 'colors.inc'
      CHARACTER*3 ROTATE
      CHARACTER*6 LAB6
      DIMENSION XBOX(5),YBOX(5)
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/

C     Set the coordinates of the corners of the box
C
C          UL-----UR
C          |       |
C          |   +   |
C          |       |
C          LL-----LR
C
C          4-------3
C          |       |
C          |   +   |
C          |       |
C          1-------2
C
      IF(ROTATE.EQ.'NO')THEN

         XLL = XMID - 0.5*XDIM
         XLR = XMID + 0.5*XDIM
         XUR = XMID + 0.5*XDIM
         XUL = XMID - 0.5*XDIM

         YLL = YMID - 0.5*YDIM
         YLR = YMID - 0.5*YDIM
         YUR = YMID + 0.5*YDIM
         YUL = YMID + 0.5*YDIM

      ELSEIF(ROTATE.EQ.'YES')THEN

C     Rotate the box to grid point (xmid,ymid) azimuth angle
C
c         azimuth = todeg*atan2(xmid-xrd,ymid-yrd)
c         theta=(azimuth-angxax)*torad
         theta = atan2(ymid-yrd,xmid-xrd)
         sin_theta=sin(theta)
         cos_theta=cos(theta)

         x1 = - 0.5*xdim
         x2 = + 0.5*xdim
         x3 = + 0.5*xdim
         x4 = - 0.5*xdim

         y1 = - 0.5*ydim
         y2 = - 0.5*ydim
         y3 = + 0.5*ydim
         y4 = + 0.5*ydim

         XLL = xmid + x1*cos_theta - y1*sin_theta
         YLL = ymid + x1*sin_theta + y1*cos_theta
         XLR = xmid + x2*cos_theta - y2*sin_theta
         YLR = ymid + x2*sin_theta + y2*cos_theta
         XUR = xmid + x3*cos_theta - y3*sin_theta
         YUR = ymid + x3*sin_theta + y3*cos_theta
         XUL = xmid + x4*cos_theta - y4*sin_theta
         YUL = ymid + x4*sin_theta + y4*cos_theta

      ENDIF

      XBOX(1)=XLL
      XBOX(2)=XLR
      XBOX(3)=XUR
      XBOX(4)=XUL
      XBOX(5)=XLL
      YBOX(1)=YLL
      YBOX(2)=YLR
      YBOX(3)=YUR
      YBOX(4)=YUL
      YBOX(5)=YLL

      ICOLBX=IGRAY-1
      CALL FAREA(ICOLBX,XBOX,YBOX,5)

      CALL LINE(XBOX(1),YBOX(1),XBOX(2),YBOX(2))
      CALL LINE(XBOX(2),YBOX(2),XBOX(3),YBOX(3))
      CALL LINE(XBOX(3),YBOX(3),XBOX(4),YBOX(4))
      CALL LINE(XBOX(4),YBOX(4),XBOX(5),YBOX(5))

      CALL PLCHHQ (XMID,YMID,LAB6,CSIZ,0.0,0.0)
c      CALL LINE(XRD,YRD,XMID,YMID)

      RETURN
      END
