c
c----------------------------------------------------------------------X
c
      SUBROUTINE CONVERT(X,Y)
      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      THE=ATAN2(X,Y)
      R=SQRT(X**2+Y**2)
      X=X0+R*SIN(THE-AZCOR)
      Y=Y0+R*COS(THE-AZCOR)
      RETURN
      END
