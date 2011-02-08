c
c----------------------------------------------------------------------X
c
      SUBROUTINE TOPO(DAT,IOUT,BDVAL,MNGATE,MXGATE,NANG,AZA,ELA,
     X                 XRD,YRD,R0,DROLD,AZROT,ITPOLD,NIN1,OLAT,
     X                 OLON,ITOPUN,MXR,MXA,MXF)
C
C  FUNCTION - TOPO: F(OUT)=TOPOGRAPHIC HEIGHT  AS
C                           SEEN BY THE SCANNING RADAR.  HEIGHTS (M)
C                           ARE READ IN FROM UNIT ITOPUN AND THEN
C                           INTERPOLATED TO (HRNG,AZIM) FOR CONTOURING.
C
C     OUTPUT TOPOGRAPHY IS IN KM, WITH SEA LEVEL SET TO BAD (-100.) SO
C     A HEIGHT OF 0.0 KM CAN BE RELIABLY CONTOURED.  


C
C     ITOPUN  - INPUT UNIT FOR TOPOGRAPHY 
C     AZA,ELA - AZIMUTH AND ELEVATION ANGLES OF THE SCANNING RADAR
C     XRD,YRD - TRUE (X,Y) COORDINATES        "  "     "       "
C     R0,DROLD- RANGE (KM) AND GATE SPACING   "  "     "       "
C     AZROT   - ROTATION ANGLE OF SCAN (SEE RDFF AND RDUF)
C
C     IOUT   - OUTPUT FIELD NUMBER
C
      PARAMETER(MX=270,MY=310,MXM=MX+12,MYM=MY+12)
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DIMENSION ZGG(MXM,MYM),AZROT(8)
      CHARACTER*8 NIN1
      LOGICAL INTOPO
      DATA INTOPO/.FALSE./
      DATA RE,TORAD,TODEG/17000.0,0.017453293,57.29577951/
      DATA IORG,JORG/221,190/

      DATA FLOOR,EPS/-100.0,1.0E-03/
C
C     LINEAR INTERPOLATION FORMULA FOR FUNCTION AT (X1.LE.X.LE.X2)
C
      FINT(X1,F1,X2,F2,X)=F1+(F2-F1)*(X-X1)/(X2-X1)
C
c      IF(ITPOLD.GE.3 .AND. ITPOLD.LE.7)THEN
c         WRITE(6,5)
c    5    FORMAT(1X,'*** WARNING: TOPOGRAPHY NOT ALLOWED ***')
c         DO 8 J=1,NANG
c         DO 8 I=MNGATE,MXGATE
c    8    DAT(I,J,IOUT)=BDVAL
c         RETURN
c      END IF





      IF(INTOPO)GO TO 50
C     BUFFER IN TOPOGRAPHY VALUES
C
      DO 10 I=1,MXM
      DO 10 J=1,MYM
   10 ZGG(I,J)=BDVAL

      READ(ITOPUN,15)TXD,TYD,RMX,RMY
      MMX=NINT(RMX)
      MMY=NINT(RMY)

      print *,'TOPO: txd,tyd,rmx,rmy=',txd,tyd,rmx,rmy
      DO 20 J=1,MMY
         DO 20 I=1,MMX
            READ(ITOPUN,16,END=40)X,Y,ZGG(I,J)
            IF(ZGG(I,J).LE.0.)ZGG(I,J)=-100.
            IF(NIN1.EQ.'LL      ')THEN
               PLAT=X
               PLON=Y
               CALL LL2XYDRV(PLAT,PLON,X,Y,OLAT,OLON,90.)
            ELSE
               X=X-OLAT
               Y=Y-OLON
            END IF
  
            IF(I.EQ.1.AND.J.EQ.1)THEN
               TX1=X
               TY1=Y
            END IF
            IF(I.EQ.MMX.AND.J.EQ.MMY)THEN
               TX2=X
               TY2=Y
            END IF
 20   CONTINUE
 15   FORMAT(4F12.4)
 16   FORMAT(3F12.5)

 40   INTOPO=.TRUE.
   50 CONTINUE
 
      IF(ITPOLD.NE.3)THEN


      DO 100 J=1,NANG
         SINA=SIN((AZA(J,1)-AZROT(ITPOLD))*TORAD)
         COSA=COS((AZA(J,1)-AZROT(ITPOLD))*TORAD)
         COSE=COS(ELA(J,1)*TORAD)
         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            ZGRND=FLOOR
            HRNG=COSE*(R0+(I-1)*DROLD)
            X=XRD+HRNG*SINA
            Y=YRD+HRNG*COSA


            IF((X.GE.TX1.AND.X.LE.TX2).AND.
     +         (Y.GE.TY1.AND.Y.LE.TY2))THEN
               IX=1.0001+(X-TX1)/TXD
               IY=1.0001+(Y-TY1)/TYD
               XE=TX1+IX*TXD
               XW=XE-TXD
               YN=TY1+IY*TYD
               YS=YN-TYD
               F1=ZGG(IX  ,IY  )
               F2=ZGG(IX+1,IY  )
               F3=ZGG(IX  ,IY+1)
               F4=ZGG(IX+1,IY+1)
               IF(F1.NE.BDVAL .AND. F2.NE.BDVAL .AND.
     +            F3.NE.BDVAL .AND. F4.NE.BDVAL)THEN
                  F12=FINT(XW,F1,XE,F2,X)
                  F34=FINT(XW,F3,XE,F4,X)
                  ZGRND=FINT(YS,F12,YN,F34,Y)
               END IF
            END IF

            IF(ZGRND.LE.EPS)THEN
               DAT(I,J,IOUT)=FLOOR
            ELSE
               DAT(I,J,IOUT)=ZGRND
            END IF
   90    CONTINUE
  100 CONTINUE

C FOR RHI SCANS FILL OUTPUT DATA ARRAY WITH THE HEIGHT OF THE BEAM
C ABOVE THE GROUND.  

      ELSE


      DO 200 J=1,NANG
         SINA=SIN((ELA(J,1))*TORAD)
         COSA=COS((ELA(J,1))*TORAD)
         COSE=COS(AZA(J,1)*TORAD)
         TANEL=TAN(AZA(J,1)*TORAD)
         DO 190 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            ZGRND=FLOOR
            HRNG=COSE*(R0+(I-1)*DROLD)
            HGT=HRNG*TANEL+HRNG*HRNG/RE
            X=XRD+HRNG*SINA
            Y=YRD+HRNG*COSA


               IX=1.0001+(X-TX1)/TXD
               IY=1.0001+(Y-TY1)/TYD
               XE=TX1+IX*TXD
               XW=XE-TXD
               YN=TY1+IY*TYD
               YS=YN-TYD
               F1=ZGG(IX  ,IY  )
               F2=ZGG(IX+1,IY  )
               F3=ZGG(IX  ,IY+1)
               F4=ZGG(IX+1,IY+1)
               IF(F1.NE.BDVAL .AND. F2.NE.BDVAL .AND.
     +            F3.NE.BDVAL .AND. F4.NE.BDVAL)THEN
                  F12=FINT(XW,F1,XE,F2,X)
                  F34=FINT(XW,F3,XE,F4,X)
                  ZGRND=FINT(YS,F12,YN,F34,Y)
                  DAT(I,J,IOUT)=HGT-ZGRND
               END IF
               




  190    CONTINUE
  200 CONTINUE

      END IF

      RETURN
      END
