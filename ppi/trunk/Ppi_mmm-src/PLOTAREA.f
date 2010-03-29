      SUBROUTINE PLOTAREA(IDATE)

      INCLUDE 'areatime.inc'

      DIMENSION X1(2),X2(2),Y1(2),Y2(2)

      CHARACTER*38 LAB
      CHARACTER*2 LAB2
      CHARACTER*3 LAB3
      CHARACTER*4 LAB1
      CHARACTER*6 IFMT,LAB6
      CHARACTER*8 LAB8
      CHARACTER*20 LAB20
      CHARACTER*15 LAB15

      DATA YTOP,YSPACE/0.9,0.04/
      DATA ISTEP/15/
      DATA X1/0.1,0.1/
      DATA X2/0.9,0.9/
      DATA Y1/0.52,0.1/
      DATA Y2/0.9,0.48/
      DATA LOGSCL/0/
      DATA YMIN/0./



      IF(NA.LE.1)RETURN



      DO 25 K=1,NAREA

      IF(IRNGPLT(K).EQ.1)GO TO 25
      ARMX=0.

      IF(AREATYP(K).EQ.'AREA    '.OR.AREATYP(K).EQ.'INTEGRAL')THEN

      DO 50 I=1,NA

 50      IF(STAREA(1,I,K).GT.ARMX)ARMX=STAREA(1,I,K)
      IF(ARMX.LE.150.)THEN
         YMAX=10*INT(0.1*ARMX)+10.
      ELSE
         YMAX=(INT(ARMX*0.01)+1)*100.
      END IF
      IF(TMIN(NA,K).LE.150.)THEN
         XMAX=10*INT(0.1*TMIN(NA,K))+10
      ELSE
         XMAX=(INT(TMIN(NA,K)*0.01)+1)*100.
      END IF
      CALL MAJMIN(0.,YMAX,IFMT,MJY,MNY,IDIG)
      CALL MAJMIN(0.,XMAX,IFMT,MJX,MNX,IDIG)
      CALL LABMOD('(F4.0)','(F5.0)',4,5,14,14,8,8,0)
      CALL SET(0.1,0.9,0.1,0.9,0.,XMAX,0.,YMAX,1)
      CALL GRIDAL(0,0,MJY,MNY,0,1,5,0.,0.)


      DO 100 I=1,NA
         DO 100 J=1,NCLA(K)
            IF(AREATYP(K).EQ.'INTEGRAL')THEN
               WRITE(99,101)IDATE,TMAREA(I,K),FXAREA(I,K),STAREA(J,I,K)
            ELSE 
               WRITE(99,102)IDATE,TMAREA(I,K),FXAREA(I,K),CLAREA(J,K),
     +            STAREA(J,I,K)
            END IF
 100    IF(I.NE.NA)CALL LINE(TMIN(I,K),STAREA(J,I,K),TMIN(I+1,K),
     +       STAREA(J,I+1,K))
 101  FORMAT(1X,I8,F8.0,F8.1,F8.0)
 102  FORMAT(1X,I8,F8.0,2F8.1,F8.0)
      ITM1=NINT(TMAREA(1,K))
      ITM2=NINT(TMAREA(NA,K))
      IHR1=ITM1/10000
      IMN1=(ITM1-IHR1*10000)/100
      IMIN=IHR1*60+IMN1
      NTIME=XMAX
      ITIME=IMIN
      CALL SET(0.1,0.9,0.,1.,0.,XMAX,0.,1.,1)
      DO 130 I=1,NTIME
         IF(MOD(ITIME,15).EQ.0)THEN
            XPOS=FLOAT(I+1)
            CALL LINE(XPOS,0.1,XPOS,0.11)
            CALL LINE(XPOS,0.9,XPOS,0.89)
         END IF
         IF(MOD(ITIME,60).EQ.0)THEN
            XPOS=FLOAT(I+1)
            CALL LINE(XPOS,0.1,XPOS,0.12)
            CALL LINE(XPOS,0.9,XPOS,0.88)
            IHR=ITIME/60
            IF(IHR.GE.24)IHR=IHR-24
            WRITE(LAB2,135)IHR
            CALL PLCHMQ(XPOS,0.075,LAB2,12.,0.,0.)
         END IF
 135     FORMAT(I2)
         ITIME=ITIME+1
 130  CONTINUE
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHMQ(0.5,0.05,'TIME',-0.75,0.,0.)
      IF(AREATYP(K).NE.'INTEGRAL')THEN
         CALL PLCHMQ(0.03,0.5,'AREA (KM**2)',-0.75,90.,0.)
      ELSE
         CALL PLCHMQ(0.03,0.5,'AREA INTEGRAL (UNITS*KM**2)',
     +     -0.75,90.,0.)
      END IF

      WRITE(LAB,103)IDATE,ITM1,ITM2
103   FORMAT(I7,2X,I7,' TO ',I7)
      CALL PLCHMQ(0.15,.95,LAB,12.0,0.,-1.)
      WRITE(LAB20,105)X1INT(K),Y1INT(K)
      CALL PLCHMQ(0.75,0.97,LAB20,10.,0.,-1.)
      WRITE(LAB20,106)X2INT(K),Y2INT(K)
      CALL PLCHMQ(0.75,0.945,LAB20,10.,0.,-1.)
      WRITE(LAB15,108)WIDTH(K)
      CALL PLCHMQ(0.75,0.92,LAB15,10.,0.,-1.)      
      WRITE(LAB8,107)NAMAREA
      CALL PLCHMQ(0.7,0.95,LAB8,12.,0.,0.)
 107  FORMAT(A8)
 105  FORMAT('X1,Y1= ',2F6.1)
 106  FORMAT('X2,Y2= ',2F6.1)
 108  FORMAT('WIDTH= ',F6.1)
      YC=YTOP
      IF(AREATYP(K).NE.'INTEGRAL')THEN
         DO 120 I=1,NCLA(K)
            WRITE(LAB1,115)CLAREA(I,K)
            CALL PLCHMQ(0.91,YC,LAB1,-0.6,0.,-1.)
            YC=YC-YSPACE
 120     CONTINUE
      END IF
 115  FORMAT(F4.0)

      ELSE IF(AREATYP(K).EQ.'AVERAGE ')THEN
         
         DO 150 J=1,1
            ARMN=999.
            ARMX=-999.
            DO 160 I=1,NA
               IF(STAREA(J,I,K).GT.ARMX)ARMX=STAREA(J,I,K)
               IF(STAREA(J,I,K).LT.ARMN)ARMN=STAREA(J,I,K)
 160        CONTINUE

            YMIN=5.*INT(0.2*ARMN)-5.
            YMAX=5.*INT(0.2*ARMX)+5.
            IF(TMIN(NA,K).LE.150.)THEN
              XMAX=10*INT(0.1*TMIN(NA,K))+10
            ELSE
              XMAX=(INT(TMIN(NA,K)*0.01)+1)*100.
            END IF
            CALL MAJMIN(YMIN,YMAX,IFMT,MJY,MNY,IDIG)
            CALL MAJMIN(0.,XMAX,IFMT,MJX,MNX,IDIG)
            CALL LABMOD('(F4.0)','(F5.0)',4,5,14,14,8,8,0)
            CALL SET(X1(J),X2(J),Y1(J),Y2(J),0.,XMAX,YMIN,YMAX,1)
            CALL GRIDAL(0,0,MJY,MNY,0,1,5,0.,0.)


            DO 200 I=1,NA-1

 200           CALL LINE(TMIN(I,K),STAREA(J,I,K),TMIN(I+1,K),
     +         STAREA(J,I+1,K))
            ITM1=NINT(TMAREA(1,K))
            ITM2=NINT(TMAREA(NA,K))
            IHR1=ITM1/10000
            IMN1=(ITM1-IHR1*10000)/100
            IMIN=IHR1*60+IMN1
            NTIME=XMAX
            ITIME=IMIN
            CALL SET(0.1,0.9,0.,1.,0.,XMAX,0.,1.,1)
            DO 230 I=1,NTIME
               IF(MOD(ITIME,15).EQ.0)THEN
                 XPOS=FLOAT(I+1)
                 CALL LINE(XPOS,Y1(J),XPOS,Y1(J)+0.01)
                 CALL LINE(XPOS,Y2(J),XPOS,Y2(J)-0.01)
               END IF
               IF(MOD(ITIME,60).EQ.0)THEN
                 XPOS=FLOAT(I+1)
                 CALL LINE(XPOS,Y1(J),XPOS,Y1(J)+0.02)
                 CALL LINE(XPOS,Y2(J),XPOS,Y2(J)-0.02)
                 IHR=ITIME/60
                 IF(IHR.GE.24)IHR=IHR-24
                 WRITE(LAB2,135)IHR
                CALL PLCHMQ(XPOS,0.48,LAB2,12.,0.,0.)
              END IF

              ITIME=ITIME+1
 230       CONTINUE
           CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
           CALL PLCHMQ(0.5,0.44,'TIME',-0.75,0.,0.)
           YM=0.5*(Y1(J)+Y2(J))
           IF(J.EQ.1)THEN
              CALL PLCHMQ(0.03,YM,'AVERAGE',
     +          -0.75,90.,0.)
           ELSE IF(J.EQ.2)THEN
              CALL PLCHMQ(0.03,YM,'AZIMUTH',
     +          -0.75,90.,0.)
           END IF              
           WRITE(LAB,103)IDATE,ITM1,ITM2

           CALL PLCHMQ(0.15,.95,LAB,12.0,0.,-1.)
           WRITE(LAB20,105)X1INT(K),Y1INT(K)
           CALL PLCHMQ(0.75,0.97,LAB20,10.,0.,-1.)
           WRITE(LAB20,106)X2INT(K),Y2INT(K)
           CALL PLCHMQ(0.75,0.945,LAB20,10.,0.,-1.)
           WRITE(LAB15,108)WIDTH(K)
           CALL PLCHMQ(0.75,0.92,LAB15,10.,0.,-1.)      
           WRITE(LAB8,107)NAMAREA
           CALL PLCHMQ(0.7,0.95,LAB8,12.,0.,0.)
 150  CONTINUE

      END IF

      CALL FRAME
 25   CONTINUE



      IST=0
      DO 270 I=1,NAREA
         IF(IRNGPLT(I).EQ.1)THEN
            IST=I
            GO TO 275
         END IF
 270  CONTINUE
 275  IF(IST.EQ.0)RETURN

      DO 285 KK=1,1

      ITM2=NINT(TMAREA(1,1))
      ICOL=1
      IBEG=1
      IEND=MIN0(MAXPLOT,NA)

      RMAX=RNGBOX(NAREA)
      RMIN=RNGBOX(IST)
      XMAX=10.*INT(0.1*RMAX)+10.
      XMIN=10.*INT(0.1*RMIN)-10.
      IF((RMAX-RMIN).LT.20.)THEN
         XMAX=INT(RMAX)+1.
         XMIN=INT(RMIN)-1.
      END IF
      CALL LABMOD('(F4.0)','(F5.0)',4,5,14,14,8,8,0)
      ARMX=0.
      ARMN=10000.

      DO 300 K=IST,NAREA
         DO 300 I=1,NA
            IF(STAREA(1,I,K).EQ.-999.)GO TO 300
            IF(STAREA(1,I,K).GT.ARMX)ARMX=ABS(STAREA(1,I,K))
            IF(STAREA(1,I,K).LT.ARMN)ARMN=STAREA(1,I,K)
 300  CONTINUE

      IF(ARMX.LE.150.)THEN
         YMAX=10*INT(0.1*ARMX)+10.
         IF(ARMX.LE.30.)YMAX=10*INT(0.1*ARMX)+5.
         IF(ARMX.LE.15.)YMAX=INT(ARMX)+1.
      ELSE
         YMAX=(INT(ARMX*0.01)+1)*100.
      END IF
      IF(ARMN.GE.0.)THEN
         YMIN=0.
      ELSE
         YMIN=INT(ARMN)-1.
      END IF

 290  IF(KK.EQ.2.AND.MAXPLOT.GT.1)THEN
         ARMX=0.
         ARMN=10000.
         DO 305 K=IST,NAREA
         DO 305 I=IBEG,IEND
            IF(STAREA(1,I,K).LT.ARMN)ARMN=STAREA(1,I,K)
 305        IF(STAREA(1,I,K).GT.ARMX)ARMX=STAREA(1,I,K)
         IF(ARMX.LE.150.)THEN
            YMAX=10*INT(0.1*ARMX)+10.
            IF(ARMX.LE.30.)YMAX=10*INT(0.1*ARMX)+5.
            IF(ARMX.LE.15.)YMAX=INT(ARMX)+1.
         ELSE
            YMAX=(INT(ARMX*0.01)+1)*100.
         END IF
         IF(ARMN.GE.0.)THEN
            YMIN=0.
         ELSE
            YMIN=INT(ARMN)-1.
         END IF
      END IF
      DELX=0.93/FLOAT(NCOLAREA)
      XLEF=0.06+(ICOL-1)*DELX
      XRIG=XLEF+DELX

      CALL MAJMIN(YMIN,YMAX,IFMT,MJY,MNY,IDIG)
      CALL MAJMIN(XMIN,XMAX,IFMT,MJX,MNX,IDIG)
      CALL SET(XLEF+0.04,XRIG,0.06,0.96,XMIN,XMAX,YMIN,YMAX,1)

      IF(MAXPLOT.GT.1)THEN
         CALL GRIDAL(MJX,MNX,0,0,1,0,5,0.,0.)
      ELSE
         CALL PERIML(MJX,MNX,MJY,MNY)
      END IF
      DELMJX=(XMAX-XMIN)/FLOAT(MJX)
      CALL DASHDB(O'170360')
      DO 310 II=1,MJX-1
         X=XMIN+II*DELMJX
         CALL LINED(X,YMIN,X,YMAX)
 310  CONTINUE
      DELY=0.90/FLOAT(MAXPLOT)
      NPLTS=1
      YTOP=0.96
      YBOT=YTOP-DELY
      DO 350 I=IBEG,IEND

         IF(MAXPLOT.GT.1)THEN
         CALL SET(XLEF+0.04,XRIG,YBOT,YTOP,XMIN,XMAX,YMIN,YMAX,1)

         CALL LINE(XMIN,YMIN,XMAX,YMIN)

         WRITE(98,405)IDATE,TMAREA(I,1),FXAREA(I,1),RNGBOX(IST),
     +        RNGBOX(NAREA),NAREA-IST+1
         WRITE(98,406)(STAREA(1,I,J1),J1=IST,NAREA)
 405     FORMAT(I10,4F10.1,I10)
 406     FORMAT(10F7.1)
         END IF



         DO 360 J=IST,NAREA-1
            IF(STAREA(1,I,J).EQ.-999..OR.STAREA(1,I,J+1).EQ.-999.)
     +        GO TO 360
            CALL LINE(RNGBOX(J),STAREA(1,I,J),RNGBOX(J+1),
     +       STAREA(1,I,J+1))
 360     CONTINUE
         IF(MAXPLOT.GT.1)THEN

            CALL SET(0.,1.,YBOT,YTOP,0.,1.,YMIN,YMAX,1)
            DELMJY=(YMAX-YMIN)/FLOAT(MJY)

            DO 365 I1=1,MJY
               Y=(I1-1)*DELMJY+YMIN
               CALL LINE(XLEF+0.04,Y,XLEF+0.05,Y)
               CALL LINE(XRIG,Y,XRIG-0.01,Y)
               IF((I1.EQ.1.OR.I1.EQ.MJY).AND.(I.EQ.IBEG.OR.I.EQ.
     +         IEND))THEN
                  ILAB=NINT(Y)
                  WRITE(LAB3,370)ILAB
                  CALL PLCHMQ(XLEF+0.02,Y,LAB3,10.,0.,0.)
               END IF
 365        CONTINUE
         END IF
         CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
         ITM1=NINT(TMAREA(I,1))
         WRITE(LAB6,375)ITM1
 370     FORMAT(I3)
 375     FORMAT(I6)
         CALL PLCHMQ(XRIG-0.06,YTOP-0.015,LAB6,9.,0.,-1.)
         NPLTS=NPLTS+1
         YTOP=YTOP-DELY
         YBOT=YBOT-DELY
 350  CONTINUE
      ICOL=ICOL+1
      YTOP=0.96
      YBOT=YTOP-DELY
      NPLTS=1
      IF(ICOL.GT.NCOLAREA.OR.IEND.GE.NA)THEN
         ICOL=1
         CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
         WRITE(LAB,103)IDATE,ITM2,ITM1
         CALL PLCHMQ(0.5,0.98,LAB,12.,0.,0.)
         CALL PLCHMQ(0.55,0.020,'RANGE (KM)',12.,0.,0.)
         IF(AREATYP(1).EQ.'AREA    ')THEN
            CALL PLCHMQ(0.03,0.5,'AREA (KM**2)',-0.75,90.,0.)
         ELSE IF(AREATYP(1).EQ.'AVERAGE ')THEN
            CALL PLCHMQ(0.03,0.5,'AREA AVERAGE',-0.75,90.,0.)
         ELSE
            CALL PLCHMQ(0.03,0.5,'AREA INTEGRAL (UNITS*KM**2)',
     +        -0.75,90.,0. )
         END IF
         WRITE(LAB8,107)NAMAREA
         CALL PLCHMQ(0.8,0.98,LAB8,12.,0.,0.)
         CALL FRAME
         ITM2=NINT(TMAREA(IBEG+MAXPLOT,1))
      END IF
      IBEG=IBEG+MAXPLOT
      IEND=IBEG+MAXPLOT-1
      IF(IBEG.GT.NA.AND.KK.EQ.2)RETURN
      IF(IBEG.GT.NA)GO TO 285
      IEND=MIN0(IEND,NA)
            
      GO TO 290

 285  CONTINUE        



      RETURN
      END

