c     
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTNLD(XNLD,YNLD,PNLD,TNLD,HNLD,AZNLD,INLD,JNLD,MXNL,
     X     DTNLD,XMN,XMX,YMN,YMX,IFTIME,ITIME,IGRPLT,BGFLAG,OLAT,OLON,
     X     ANGXAX,CNLD,FXOLD,H0,ITPOLD,DZNLD,NAMFLD,IFLD,ITIME1)
C
C  Plot NLD CG lightning positions overlaid on CONTOUR or COLOR plots, only
c     if the strike is within the plot space (XMN,XMX,YMN,YMX) and middle
C     time of the radar scan.
C     Color fill a box and plot the polarity of the strike inside it.
C
C     XMN,XMX,YMX,YMX - (X,Y) BOUNDS OF THE PLOT DOMAIN
C     ICOLBX    - COLOR INDICES FOR THE BOX AND CHARACTERS
C     INLD      - Number of strikes read in (See GETNLD).
C     JNLD      - (1) CG polarity, (2) CG polarity in colored box
C     DTNLD     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTNLD .gt. 0: Plot +/- DTNLD from central time (RMSEC)
C                 DTNLD .le. 0: Plot within DTNLD seconds outside the
C                               radar scan time interval
C     IFTIME    - Beginning time of scan
C     ITIME1    - Beginning time of the swath
C     ITIME     - Ending time of scan or swath
C
C     XNLD,YNLD - (X,Y) position (km) of CG strike relative to origin
C     TNLD      - Time of the strike (sec)
C     HNLD      - Horizontal range (km) of the CG strike
C     AZNLD     - Azimuth angle (deg) of the CG strike
C     PNLD      - Polarity of ground strike and E-field strength (Kv/m)
C     X0,Y0,H0  - Radar position (km,km,km)
C     FXOLD     - Fixed angle (elevation or azimuth) of scan being plotted
C     ITPOLD    - Scan mode [PPI(1), COP(2), RHI(3), or SUR(8)]
C
      INCLUDE 'colors.inc'
      CHARACTER BGFLAG*1,TLAB*8,NAMFLD*8

      COMMON/COTABLE/ICOL(100)
      COMMON /DASHPAT/IDPAT(5,3),JDPAT,LWSTART,LWINC

      DIMENSION XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL)
      DIMENSION HNLD(MXNL),AZNLD(MXNL)
      DIMENSION FXP(5),FYP(5)
      DATA TORAD,TODEG,RNG/0.017453293,57.29577951,111.137/
      DATA XSIZ/18.0/
      SAVE IHRB, IHRE

c-----print *,'Pltnld: fxold,itpold,h0=',fxold,itpold,h0
c-----print *,'Pltnld:       jnld,inld=',jnld,inld
c-----print *,'Pltnld:      dt,dz=',dtnld,dznld
c-----print *,'Pltnld: name,times=',namfld,itime1,iftime,itime

C     Find beginning, middle, and ending seconds for the current scan.
C     Don't plot if the CG strike time TNLD is completely 
C     outside the plotting time segment [SECMN to SECMX]. 
C
      ITIME1_OLD=ITIME1
      IFTIME_OLD=IFTIME
      ITIME_OLD=ITIME
      IF(IFLD.LE.-1)THEN      
         IHRB= ITIME1/10000
         IMNB=(ITIME1-IHRB*10000)/100
         ISCB= ITIME1-IHRB*10000-IMNB*100
      ELSE
         IHRB= IFTIME/10000
         IMNB=(IFTIME-IHRB*10000)/100
         ISCB= IFTIME-IHRB*10000-IMNB*100
      END IF
      IHRE= ITIME/10000
      IMNE=(ITIME-IHRE*10000)/100
      ISCE= ITIME-IHRE*10000-IMNE*100
c-----print *,'        old times=',itime1_old,iftime_old,itime_old
c-----print *,'        cur times=',itime1,iftime,itime

      IBSEC=IHRB*3600+IMNB*60+ISCB
      IESEC=IHRE*3600+IMNE*60+ISCE

C     Handle times crossing midnight
C
      IF(IFLD.LE.-1)THEN
         IF(ITIME1_OLD.GT.ITIME1)THEN
            IBSEC=IBSEC+24*3600
         END IF
      ELSE
         IF(IFTIME_OLD.GT.IFTIME)THEN
            IBSEC=IBSEC+24*3600
         END IF
      END IF
      IF(ITIME_OLD.GT.ITIME)THEN
         IESEC=IESEC+24*3600
      END IF
      IF(IBSEC.GT.IESEC)IESEC=IESEC+24*3600
         
      IMSEC=0.5*(IBSEC+IESEC)
      IHRM= IMSEC/3600.0
      IMNM= (IMSEC-3600.0*IHRM)/60.0
      ISCM= IMSEC-3600.0*IHRM-60.0*IMNM
      RMSEC=FLOAT(IHRM*3600+IMNM*60+ISCM)
      IMTIME=IHRM*10000+IMNM*100+ISCM

C     Plot +/- DTNLD from central time (RMSEC) of current scan,
C     i.e. SECMN < RMSEC < SECMX
C
      IF(DTNLD.GT.0.0)THEN
         SECMN=RMSEC-DTNLD
         SECMX=RMSEC+DTNLD
      ELSE
         SECMN=FLOAT(IBSEC)-ABS(DTNLD)
         SECMX=FLOAT(IESEC)+ABS(DTNLD)
      END IF

c      write(6,1770)iftime,imtime,itime,ibsec,imsec,iesec,
c     +     secmn,rmsec,secmx
c 1770 format(1x,'Pltnld: times=',6i8,3f8.1)
c      if(ifld.le.-1)then      
c         write(6,1770)itime1,imtime,itime,secmn,rmsec,secmx
c      else
c         write(6,1770)iftime,imtime,itime,secmn,rmsec,secmx
c      end if
c 1770 format(9x,'all times=',3i8,3f8.1)

C     CHANGE LINE COLORS AND WIDTHS, CHANGE TO THE COMPLEX 
C     CHARACTER SET, AND TURN ON COLOR AREA FILL
C        IGRPLT - (0) NOT GRAYTONES, (1) GRAYTONES
C        
C
      IF(CNLD.LE.0.0)THEN
         CSIZ=XSIZ
      ELSE
         CSIZ=CNLD
      END IF
      IF(IFLD.LE.-1)CSIZ=CSIZ*12.0/XSIZ
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

C     JLND = 2: PLOT COLORED BOX FOR SYMBOL.
C
      IF(JNLD.EQ.2)THEN
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         CALL SET(FL,FR,FB,FT,FL,FR,FB,FT,LLL)
         FDX=FR-FL
         FDY=FT-FB
         UDX=UR-UL
         UDY=UT-UB

C        CONVERT SIZE OF A BLANK CHARACTER (CSIZ) TO FRACTIONS OF 1024.
C        KK - LENGTH OF CHARACTER STRING TO BE PLOTTED
C
         WSIZ=CSIZ/1024.
         HSIZ=2.0*WSIZ
         KK=1

         DO 20 I=1,INLD
            IF(ITPOLD.EQ.3)THEN
               X1=HNLD(I)
               Y1=H0
               T1=TNLD(I)
               AZ=AZNLD(I)
               DIFF=ABS(AZ-FXOLD)
               IF(DIFF.GT.DZNLD)GO TO 18
            ELSE
               X1=XNLD(I)
               Y1=YNLD(I)
               T1=TNLD(I)
            END IF
            POLAR=PNLD(I)
            IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 18
            IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 18
            IF(T1.LT.SECMN .OR. T1.GT.SECMX)GO TO 18

C           Get the bounds of the box around a character
C           string, and fill it with color (Index=ICOLBX).
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

C     JLND = 1: PLOT SYMBOL [polarity = + (X), - (O)]
C     Change text color:
C        background (0) for symbol in a box
C        foreground (1) if only a symbol
C
c      IF(JLND.EQ.2)THEN
c         CALL GSPLCI (0)
c      ELSE
c         CALL GSPLCI (1)
c      END IF
      CALL GETUSV('LW',ILW)
      JLW=1800
      CALL SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,JLW)
      CALL GSPLCI (1)

      DO 30 I=1,INLD
         IF(ITPOLD.EQ.3)THEN
            X1=HNLD(I)
            Y1=H0
            T1=TNLD(I)
            AZ=AZNLD(I)
            DIFF=ABS(AZ-FXOLD)
            IF(DIFF.GT.DZNLD)GO TO 28
         ELSE
            X1=XNLD(I)
            Y1=YNLD(I)
            T1=TNLD(I)
         END IF
         POLAR=PNLD(I)
         IF(X1.LT.XMN .OR. X1.GT.XMX)GO TO 28
         IF(Y1.LT.YMN .OR. Y1.GT.YMX)GO TO 28
         IF(T1.LT.SECMN .OR. T1.GT.SECMX)GO TO 28
         TN=TNLD(I)
         XN=XNLD(I)
         YN=YNLD(I)
         ZN=H0
         HN=HNLD(I)
         AN=AZNLD(I)
         IPN=INT(PNLD(I))

         IHR=INT(TNLD(I)/3600.0)
         IMN=INT((TNLD(I)-FLOAT(IHR*3600))/60.0)
         ISC=INT((TNLD(I)-FLOAT(IHR*3600)-FLOAT(IMN*60)))
         IHMS=IHR*10000+IMN*100+ISC
         write(6,1771)namfld,ihms,int(tn),xn,yn,zn,hn,an,ipn
 1771    format(6X,'PltNLD: ',a5,3x,'hms=',i6,3x,'sec=',i6,
     X        13x,'xyz=',3f10.3,3x,'ha=',2f10.3,3x,'p=',i6)
         
         IF(POLAR.GT.0.0)THEN
            WRITE(TLAB,23)IHMS
 23         FORMAT('X-',I6.6)
c            CALL PLCHMQ (X1,Y1,TLAB,CSIZ,0.0,-1.0)
            CALL PLCHMQ (X1,Y1,'X',CSIZ,0.0,0.0)
         ELSE
            WRITE(TLAB,25)IHMS
 25         FORMAT('O-',I6.6)
c            CALL PLCHMQ (X1,Y1,TLAB,CSIZ,0.0,-1.0)
            CALL PLCHMQ (X1,Y1,'0',CSIZ,0.0,0.0)
         END IF
         
 28      CONTINUE
 30   CONTINUE
      
C     Restore line widths and color and text color
C     
      CALL SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,ILW)
      CALL GSPLCI(1)
      RETURN
      END

