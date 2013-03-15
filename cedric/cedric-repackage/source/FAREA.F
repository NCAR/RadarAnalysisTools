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

      XP(IPTS)=XP(1)
      YP(IPTS)=YP(1)
      IF(ICOLOR.EQ.0)THEN
         CALL GSFACI (ICOLOR)
      ELSE
         CALL GSFACI (ICOLOR)
      END IF
      CALL GFA (IPTS,XP,YP)
      RETURN
      END
