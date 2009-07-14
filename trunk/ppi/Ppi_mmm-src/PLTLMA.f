c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTLMA(XLMA,YLMA,ZLMA,TLMA,HLMA,AZLMA,ILMA,JLMA,MXLM,
     X     DTLMA,XMN,XMX,YMN,YMX,IFTIME,ITIME,IGRPLT,BGFLAG,OLAT,OLON,
     X     ANGXAX,CLMA,FXOLD,H0,ITPOLD,DZLMA,NAMFLD,LMA_COLR)
C
C  Plot LMA lightning channel positions overlaid on CONTOUR or COLOR plots, 
c     only if the channel is within the plot space (XMN,XMX,YMN,YMX) and 
C     middle,time of the radar scan.
C     Color fill a box and plot the channel position (*) inside it.
C
C     XMN,XMX,YMX,YMX - (X,Y) BOUNDS OF THE PLOT DOMAIN
C     ICOLBX    - COLOR INDICES FOR THE BOX AND CHARACTERS
C     ILMA      - Number of strikes read in (See GETLMA).
C     JLMA      - (0) don't plot channel location
C                 (1) do    plot channel location
C     DTLMA     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTLMA .gt. 0: Plot +/- DTLMA from central time (RMSEC)
C                 DTLMA .le. 0: Plot within DTLMA seconds outside the
C                               radar scan time interval
C     DZLMA     - Height interval for plotting strikes.  Plot points within
C                 a height window relative to the current radar scan.
C                 DZLMA .gt. 0: Plot +/- DZLMA from radar scan height
C                 DZLMA .le. 0: Plot within any height interval
C     IFTIME    - Beginning time of scan
C     ITIME     - Ending time of scan
C
C     XLMA,YLMA - (X,Y) position (km) of lightning channel relative to origin
C     TLMA      - Time of the strike (sec)
C     ZLMA      - Height (km) of the lightning channel
C     HLMA      - Horizontal range (km) of the lightning channel
C     AZLMA     - Azimuth angle (deg) of the lightning channel
C     LMA_COLR  - WHI (white), BLK (black), GRY (gray), RED (red),
C                 GRN (green), BLU (blue), CYA (cyan), MAG (magenta),
C                 YEL (yellow), BMG (blue-magenta), SBL (sky-blue),
C                 ORN (orange), FGR (forest-green)
C     X0,Y0,H0  - Radar position (km,km,km)
C     FXOLD     - Fixed angle (elevation or azimuth) of scan being plotted
C     ITPOLD    - Scan mode [PPI(1), COP(2), RHI(3), or SUR(8)]
C
      INCLUDE 'colors.inc'
      CHARACTER BGFLAG*1,NAMFLD*8,LMA_COLR*4

      COMMON/COTABLE/ICOL(100)
      COMMON /DASHPAT/IDPAT(5,3),JDPAT,LWSTART,LWINC

      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DIMENSION TLMA(MXLM)
      DIMENSION HLMA(MXLM),AZLMA(MXLM)
      DIMENSION FXP(5),FYP(5)
      DATA XSIZ/4.0/
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/

c-----print *,'Pltlma: fxold,itpold,h0=',fxold,itpold,h0
c-----print *,'Pltlma:       jlma,ilma=',jlma,ilma
c-----print *,'Pltlma:           dt,dz=',dtlma,dzlma

      TANEL=TAN(TORAD*FXOLD)

C     Find beginning, middle, and ending seconds for the current scan.
C     Don't plot if the CG strike time TLMA is completely 
C     outside the plotting time segment [SECMN to SECMX]. 
C
      IHRB= IFTIME/10000
      IMNB=(IFTIME-IHRB*10000)/100
      ISCB= IFTIME-IHRB*10000-IMNB*100
      IBSEC=IHRB*3600+IMNB*60+ISCB

      IHRE= ITIME/10000
      IMNE=(ITIME-IHRE*10000)/100
      ISCE= ITIME-IHRE*10000-IMNE*100
      IESEC=IHRE*3600+IMNE*60+ISCE

      IMSEC=0.5*(IBSEC+IESEC)
      IHRM= IMSEC/3600.0
      IMNM= (IMSEC-3600.0*IHRM)/60.0
      ISCM= IMSEC-3600.0*IHRM-60.0*IMNM
      RMSEC=FLOAT(IHRM*3600+IMNM*60+ISCM)
      IMTIME=IHRM*10000+IMNM*100+ISCM

C     Plot +/- DTLMA from central time (RMSEC) of current scan,
C     i.e. SECMN < RMSEC < SECMX
C
      IF(DTLMA.GT.0.0)THEN
         SECMN=RMSEC-DTLMA
         SECMX=RMSEC+DTLMA
      ELSE
         SECMN=FLOAT(IBSEC)-ABS(DTLMA)
         SECMX=FLOAT(IESEC)+ABS(DTLMA)
      END IF

c      write(6,1770)iftime,imtime,itime,ibsec,imsec,iesec,
c     +     secmn,rmsec,secmx
c 1770 format(1x,'PltLMA: times=',6i8,3f8.1)
c      write(6,1770)iftime,imtime,itime,secmn,rmsec,secmx
c 1770 format(1x,'PltLMA: times=',3i8,3f8.1)

C     CHANGE LINE COLORS AND WIDTHS, CHANGE TO THE COMPLEX 
C     CHARACTER SET, AND TURN ON COLOR AREA FILL
C        IGRPLT - (0) NOT GRAYTONES, (1) GRAYTONES
C        
C
      ICOLR = IWHITE
      IF(LMA_COLR .EQ. 'WHI')THEN
         ICOLR = IWHITE
      ELSE IF(LMA_COLR .EQ. 'BLK')THEN
         ICOLR = IBLACK
      ELSE IF(LMA_COLR .EQ. 'GRY')THEN
         ICOLR = IGRAY
      ELSE IF(LMA_COLR .EQ. 'RED')THEN
         ICOLR = IRED
      ELSE IF(LMA_COLR .EQ. 'GRN')THEN
         ICOLR = IGREEN
      ELSE IF(LMA_COLR .EQ. 'BLU')THEN
         ICOLR = IBLUE
      ELSE IF(LMA_COLR .EQ. 'CYA')THEN
         ICOLR = ICYAN
      ELSE IF(LMA_COLR .EQ. 'MAG')THEN
         ICOLR = IMAGENTA
      ELSE IF(LMA_COLR .EQ. 'YEL')THEN
         ICOLR = IYELLOW
      ELSE IF(LMA_COLR .EQ. 'BMG')THEN
         ICOLR = IBMAGENTA
      ELSE IF(LMA_COLR .EQ. 'SBL')THEN
         ICOLR = ISBLUE
      ELSE IF(LMA_COLR .EQ. 'ORN')THEN
         ICOLR = IORANGE
      ELSE IF(LMA_COLR .EQ. 'FGR')THEN
         ICOLR = IFGREEN
      END IF

      IF(CLMA.LE.0.0)THEN
         CSIZ=XSIZ
      ELSE
         CSIZ=CLMA
      END IF
      CALL GETUSV('LW',ILW)
      JLW=1500
      CALL SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,JLW)
      CALL PCSETI ('CD',0)
      CALL GSFAIS(1)

      IF(IGRPLT.NE.1)THEN
         IF(BGFLAG.EQ.'W')THEN
            ICOLBX=IBLACK
         ELSE
            ICOLBX=IWHITE
         END IF
      ELSE
         ICOLBX=IBLACK
      END IF

C     PLOT RED SYMBOL (*) INSIDE COLORED BOX OR SYMBOL ONLY
C

      IF(JLMA.EQ.2)THEN

C        USE FRACT COORD TO PLOT SYMBOL (*) INSIDE COLORED BOXES
C
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         CALL SET(FL,FR,FB,FT,FL,FR,FB,FT,LLL)
         FDX=FR-FL
         FDY=FT-FB
         UDX=UR-UL
         UDY=UT-UB

C        CONVERT SIZE OF A BLANK CHARACTER (CSIZ) TO FRACTIONS OF 1024.
C        KK - LENGTH OF CHARACTER STRING (x) TO BE PLOTTED
C
         WSIZ=CSIZ/1024.
         HSIZ=2.0*WSIZ
         KK=1

         DO 20 I=1,ILMA
            IF(ITPOLD.EQ.3)THEN
               X1=HLMA(I)
               Y1=ZLMA(I)
               T1=TLMA(I)
               AZ=AZLMA(I)
               DIFF=ABS(AZ-FXOLD)
            ELSE
               X1=XLMA(I)
               Y1=YLMA(I)
               Z1=ZLMA(I)
               T1=TLMA(I)
               HDISTSQ=X1*X1+Y1*Y1
               HDIST=SQRT(HDISTSQ)
               ZRAD=H0+HDIST*TANEL+0.5*HDISTSQ*REI
               DIFF=ABS(ZRAD-Z1)
            END IF
            IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 18
            IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 18
            IF(T1.LT.SECMN .OR. T1.GT.SECMX)GO TO 18
            IF(DZLMA.GT.0.0 .AND. DIFF.GT.DZLMA)GO TO 18

C           GET THE BOUNDS OF THE BOX AROUND A CHARACTER STRING,
C           FILL IT WITH COLOR (INDEX=ICOLBX) AND WRITE STRING.
C
            FX1=FL+(X1-UL)*FDX/UDX
            FY1=FB+(Y1-UB)*FDY/UDY
            FXL=FX1-1.25*WSIZ
            FXR=FX1+(FLOAT(KK)+0.25)*WSIZ
            FYB=FY1-0.5*HSIZ
            FYT=FY1+0.5*HSIZ
            FXP(1)=FXL
            FXP(2)=FXR
            FXP(3)=FXR
            FXP(4)=FXL
            FYP(1)=FYB
            FYP(2)=FYB
            FYP(3)=FYT
            FYP(4)=FYT
            CALL FAREA(ICOLBX-1,FXP,FYP,5)
   18       CONTINUE
   20    CONTINUE
         CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)

      END IF

C     Change text color to color index ICOLR
C     
      CALL GSPLCI (ICOLR)
      DO 30 I=1,ILMA
         IF(ITPOLD.EQ.3)THEN
            X1=HLMA(I)
            Y1=ZLMA(I)
            T1=TLMA(I)
            AZ=AZLMA(I)
            DIFF=ABS(AZ-FXOLD)
         ELSE
            X1=XLMA(I)
            Y1=YLMA(I)
            Z1=ZLMA(I)
            T1=TLMA(I)
            HDISTSQ=X1*X1+Y1*Y1
            HDIST=SQRT(HDISTSQ)
            ZRAD=H0+HDIST*TANEL+0.5*HDISTSQ*REI
            DIFF=ABS(ZRAD-Z1)
         END IF

         IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 28
         IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 28
         IF(T1.LT.SECMN .OR. T1.GT.SECMX)GO TO 28
         IF(DZLMA.GT.0.0 .AND. DIFF.GT.DZLMA)GO TO 28

         TL=TLMA(I)
         XL=XLMA(I)
         YL=YLMA(I)
         ZL=ZLMA(I)
         HL=HLMA(I)
         AL=AZLMA(I)

         ISEC=INT(TLMA(I))
         FRSEC=TLMA(I)-ISEC
         IHR=INT(ISEC/3600.0)
         IMN=INT((ISEC-FLOAT(IHR*3600))/60.0)
         ISC=INT((ISEC-FLOAT(IHR*3600)-FLOAT(IMN*60)))
         IHMS=IHR*10000+IMN*100+ISC
         write(6,1771)namfld,ihms,frsec,xl,yl,zl,hl,al
 1771    format(6X,'PltLMA: ',a5,3x,'hms=',i6,3x,'sec=',f16.9,
     X        3x,'xyz=',3f10.3,3x,'ha=',2f10.3)
         
         CALL PLCHMQ (X1,Y1,'*',CSIZ,0.0,0.0)
         
 28      CONTINUE
 30   CONTINUE

C     Restore line widths and color and text color
C
      CALL SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,ILW)
      CALL GSPLCI(1)
      RETURN
      END

