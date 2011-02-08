      SUBROUTINE INITAREA

      INCLUDE 'areatime.inc'
      CHARACTER*8 INDAT(10)
      DATA DTR/0.01745329/

      NAREA=0
 10   READ(5,105)(INDAT(I),I=1,10)
 105  FORMAT(10A8)
c      WRITE(6,107)(INDAT(I),I=1,10)
c 107  FORMAT('Kardin=',10A8)
      IF(INDAT(1).EQ.'END     ')THEN
         PRINT *,'NAREA= ',NAREA
         RETURN
      END IF
      NAREA=NAREA+1
      IF(NAREA.GE.101)THEN
         PRINT *,
     +   '***WARNING-NUMBER OF AREA PLOT BOXES CANNOT EXCEED 100***'
         NAREA=100
      END IF
      READ(INDAT,20)X0,Y0,WIDTH(NAREA),RLEN,ANG,UBOX(NAREA),
     +   VBOX(NAREA),BOX,AREATYP(NAREA)
 20   FORMAT(/F8.1/F8.1/F8.1/F8.1/F8.1/F8.1/F8.1/F8.1/A8)
      IBOX=NINT(BOX)
      READ(5,105)(INDAT(I),I=1,10)
c      WRITE(6,107)(INDAT(I),I=1,10)
      NCLA(NAREA)=0
      IF(AREATYP(NAREA).EQ.'AREA    ')THEN
         DO 30 I=2,10
            IF(INDAT(I).NE.'        ')THEN
               NCLA(NAREA)=NCLA(NAREA)+1
               READ(INDAT(I),25)CL
               CLAREA(NCLA(NAREA),NAREA)=CL
 25            FORMAT(F8.1)

            END IF
 30      CONTINUE
      ELSE
         NCLA(NAREA)=1
      END IF
      IRNGPLT(NAREA)=0
      DX=0.5*RLEN*SIN(ANG*DTR)
      DY=0.5*RLEN*COS(ANG*DTR)
      X1INT(NAREA)=X0+DX
      Y1INT(NAREA)=Y0+DY
      X2INT(NAREA)=X0-DX
      Y2INT(NAREA)=Y0-DY
      IF(WIDTH(NAREA).LT.0.)THEN

         WIDTH(NAREA)=-WIDTH(NAREA)
         READ(INDAT(2),25)BOX
         NBOX=NINT(BOX)
         IF(NBOX.LT.0)THEN
            IRNGPLT(NAREA)=1
            READ(INDAT(3),25)DAT1
            MAXPLOT=NINT(DAT1)
            READ(INDAT(4),25)DAT1
            NCOLAREA=NINT(DAT1)
         END IF

         DO 50 I=2,ABS(NBOX)
            DIST=(I-1)*WIDTH(NAREA)
            NAREA=NAREA+1
            WIDTH(NAREA)=WIDTH(NAREA-1)
            IRNGPLT(NAREA)=IRNGPLT(NAREA-1)
            UBOX(NAREA)=UBOX(NAREA-1)
            VBOX(NAREA)=VBOX(NAREA-1)
            AREATYP(NAREA)=AREATYP(NAREA-1)
            NCLA(NAREA)=NCLA(NAREA-1)
            DXX=DIST*COS(-ANG*DTR)
            DYY=DIST*SIN(-ANG*DTR)
            X=X0+DXX
            Y=Y0+DYY
            X1INT(NAREA)=X+DX
            X2INT(NAREA)=X-DX
            Y1INT(NAREA)=Y+DY
            Y2INT(NAREA)=Y-DY
 50      CONTINUE
      END IF
      GO TO 10
      END
