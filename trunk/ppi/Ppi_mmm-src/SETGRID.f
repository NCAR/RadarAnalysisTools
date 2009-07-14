c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETGRID(INDAT,RMNSW,RMXSW,DRSW,AMNSW,AMXSW,DASW,
     X     AZCLO_MX1,MXR,MXA,RNG,AZA,NRSW,NANG,SETGRD)
C
C     Set parameters for regular range-angle grid for swathing
C
C     RMNSW     - Minimum range (km)
C     RMXSW     - Maximum range (km)
C     DRSW      - Range spacing (km)
C     AMNSW     - Minimum angle (azimuth or elevation, deg)
C     AMXSW     - Maximum   "       "     "     "       "
C     DASW      - Angular spacing (deg)
C     AZCLO_MX1 - Maximum angle difference between actual scan angle
C                 and regular grid angle
C
      CHARACTER*8 INDAT(10)
      LOGICAL SETGRD
      DIMENSION RNG(MXR,2),AZA(MXA,2),NANG(2)

      WRITE(6,11)(INDAT(I),I=2,10)
 11   FORMAT(1X,'SETGRID: ',9A8)
      READ(INDAT,13)RMNSW,RMXSW,DRSW,AMNSW,AMXSW,DASW,AZCLO_MX1
 13   FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(DRSW.GT.0.0)THEN
         NRSW = 1 + NINT((RMXSW-RMNSW)/DRSW)
      ELSE
         WRITE(*,*)'***GATE SPACING MUST BE GREATER THAN ZERO***'
         STOP
      END IF
      IF(DASW.GT.0.0)THEN
         NANG(2) = 1 + NINT((AMXSW-AMNSW)/DASW)
      ELSE
         WRITE(*,*)'***ANGLE SPACING MUST BE GREATER THAN ZERO***'
         STOP
      END IF
      IF(RMXSW.LE.0.0)THEN
         WRITE(*,*)'***MAXIMUM RANGE MUST BE GREATER THAN ZERO***'
         STOP
      END IF
      IF(AMXSW.LE.0.0)THEN
         WRITE(*,*)'***MAXIMUM ANGLE MUST BE GREATER THAN ZERO***'
         STOP
      END IF
      IF(AMXSW.LE.AMNSW)THEN
         WRITE(*,*)'***ANGLE BOUNDS MUST BE MONOTONICALLY INCREASING***'
         STOP
      END IF

      IF(AZCLO_MX1.LE.0.0)AZCLO_MX1=1.0
      IF(NRSW.GT.MXR)NRSW=MXR
      IF(NANG(2).GT.MXA)NANG(2)=MXA
      WRITE(*,15)RMNSW,RMXSW,DRSW,NRSW
      WRITE(*,17)AMNSW,AMXSW,DASW,AZCLO_MX1,NANG(2)
 15   FORMAT(1X,'SETGRID: ',3F8.3,I8)
 17   FORMAT(1X,'         ',4F8.3,I8)

      DO J=1,NANG(2)
         AZA(J,2)=AMNSW+(J-1)*DASW
c         write(*,*)'j,az=',j,aza(j,2)
      END DO
      DO I=1,NRSW
         RNG(I,2)=RMNSW+DRSW*(I-1)
c         write(*,*)'i,rg=',i,rng(i,2)
      END DO
      SETGRD=.TRUE.

      RETURN
      END

