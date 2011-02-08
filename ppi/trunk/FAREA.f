c
c----------------------------------------------------------------------X
c
      SUBROUTINE FAREA(ICOLOR,XP,YP,IPTS)
C
C  GKS CALLS TO FILL A POLYGON WITH COLOR (INDEX=ICOLOR+1), WHERE
C     ONE IS ADDED TO COLOR INDEX TO ACCOUNT FOR COLOR TABLE INDICES
C     2-62, NOT 1-61.
C
      DIMENSION XP(*),YP(*)
      CHARACTER*2 LAB
      LOGICAL DEBUG
      DATA DEBUG/.FALSE./

      XP(IPTS)=XP(1)
      YP(IPTS)=YP(1)
      IF(ICOLOR.EQ.0)THEN
         CALL GSFACI (ICOLOR)
      ELSE
         CALL GSFACI (ICOLOR+1)
      END IF
      CALL GFA (IPTS,XP,YP)

      IF(.NOT.DEBUG)RETURN

C     For debugging: write index (ICOLOR) at (x,y) center of polygon
C
      XPBAR=0.0
      YPBAR=0.0
      DO I=1,IPTS
         XPBAR=XPBAR+XP(I)
         YPBAR=YPBAR+YP(I)
      END DO
      XPBAR=XPBAR/IPTS
      YPBAR=YPBAR/IPTS
      IF(ICOLOR.EQ.0)THEN
         WRITE(LAB,11)ICOLOR
         ICOLPL=1
      ELSE
         WRITE(LAB,11)ICOLOR+1
         ICOLPL=62-ICOLOR
         IF(ICOLPL.LT.0)ICOLPL=0
         IF(ABS(ICOLOR-ICOLPL).LT.10)ICOLPL=0
      END IF
 11   FORMAT(I2)

      CALL SFLUSH
C      CALL GSTXCI(ICOLPL)
      CALL GSPLCI(ICOLPL)
      CALL PLCHMQ(XPBAR,YPBAR,LAB, 5.0, 0.0, 0.0)
      CALL SFLUSH
      CALL GSPLCI(1)
C      CALL GSTXCI(1)
      RETURN

      END
