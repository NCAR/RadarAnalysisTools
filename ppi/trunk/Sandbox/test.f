      program test
c
c     Test computation of the azimuth angle through
c     the grid point (X,Y)
c
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/

      X=0.0
      Y=10.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
 11   format(' X,Y=',2F8.1,' Azmiuth=',F8.1)
      X=10.0
      Y=10.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      X=10.0
      Y=0.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      X=10.0
      Y=-10.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      X=0.0
      Y=-10.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      X=-10.0
      Y=-10.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      X=-10.0
      Y=0.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      X=-10.0
      Y=10.0
      AZIMUTH = TODEG*ATAN2(X,Y)
      IF(AZIMUTH.LT.0.0)AZIMUTH=AZIMUTH+360.0
      write(6,11)X,Y,AZIMUTH
      stop
      end
