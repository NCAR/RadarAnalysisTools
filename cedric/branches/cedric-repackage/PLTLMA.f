c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTLMA(XLMA,YLMA,ZLMA,TLMA,ILMA,JLMA,MXLM,DTLMA,
     X     XMN,XMX,YMN,YMX,ZMN,ZMX,IBTIME,IETIME,OLAT,OLON,ANGXAX,
     X     CLMA,DZLMA,ZLEV)
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
C     IBTIME    - Beginning time of the volume
C     IETIME    - Ending time of volume
C
C     XLMA,YLMA - (X,Y) position (km) of lightning channel relative to origin
C     TLMA      - Time of the strike (sec)
C     ZLMA      - Height (km) of the lightning channel
C
      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DOUBLE PRECISION TLMA(MXLM)
      DOUBLE PRECISION SEC,FRSEC
      DATA XSIZ/4.0/

C     Find beginning, middle, and ending seconds for the current scan.
C     Don't plot if the CG strike time TLMA is completely 
C     outside the plotting time segment [SECMN to SECMX]. 
C
      IHRB= IBTIME/10000
      IMNB=(IBTIME-IHRB*10000)/100
      ISCB= IBTIME-IHRB*10000-IMNB*100
      IBSEC=IHRB*3600+IMNB*60+ISCB

      IHRE= IETIME/10000
      IMNE=(IETIME-IHRE*10000)/100
      ISCE= IETIME-IHRE*10000-IMNE*100
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
      IF(DTLMA.LE.0.0)THEN
         SECMN=FLOAT(IBSEC)-ABS(DTLMA)
         SECMX=FLOAT(IESEC)+ABS(DTLMA)
      ELSE
         SECMN=RMSEC-DTLMA
         SECMX=RMSEC+DTLMA
      END IF

      IF(CLMA.LE.0.0)THEN
         CSIZ=XSIZ
      ELSE
         CSIZ=CLMA
      END IF

C     Change to RED text color
C     
c      CALL GSPLCI (IRED)
      DO 30 I=1,ILMA
         X1=XLMA(I)
         Y1=YLMA(I)
         Z1=ZLMA(I)
         T1=TLMA(I)
         DIFF=ABS(ZLEV-Z1)

         IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 28
         IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 28
         IF(T1.LT.SECMN .OR. T1.GT.SECMX)GO TO 28
         IF(DZLMA.GT.0.0 .AND. DIFF.GT.DZLMA)GO TO 28

         TL=TLMA(I)
         XL=XLMA(I)
         YL=YLMA(I)
         ZL=ZLMA(I)

         ISEC=INT(TLMA(I))
         FRSEC=TLMA(I)-ISEC
         IHR=INT(ISEC/3600.0)
         IMN=INT((ISEC-FLOAT(IHR*3600))/60.0)
         ISC=INT((ISEC-FLOAT(IHR*3600)-FLOAT(IMN*60)))
         IHMS=IHR*10000+IMN*100+ISC
         CALL PLCHMQ (X1,Y1,'X',CSIZ,0.0,0.0)
         
 28      CONTINUE
 30   CONTINUE

C     Restore text color
C
c      CALL GSPLCI(1)
      RETURN
      END

