c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTSCAT(NAS,SRMN,SRMX,SAMN,SAMX,SZMN,SZMX,X0,Y0,H0,
     X     ISKP,JSKP,IXNAM,XFMN,XFMX,IYNAM,YFMN,YFMX,SLOP,YCEPT,RLAG,
     X     ALAG,NSMX,PLTSW,NFRAME,ASCT,ASCTCLR,XDAT,YDAT,NDAT,NDMX,
     X     MSKP,NSKP,SMATCH,NFXSCT,NSWPAVG,PLTEOV,BGFLAG,SCTCOLR,
     X     LABLS)
C
C  PLOT ALL SCATTERGRAMS FOR THE CURRENT SWEEP
C
C     NAS       - NUMBER OF SCATTERGRAMS TO BE PLOTTED
C     SRMN,SRMX - MIN AND MAX HORIZ RANGE (KM) FOR ACCUMULATING SCATTERGRAM
C     SAMN,SAMX -  "   "   "  ANGLE (DEG)       "        "          "
C     SZMN,SZMX -  "   "   "  HEIGHT (KM)       "        "          "
C     ISKP,JSKP - SKIPPING FACTORS FOR RANGE AND ANGLE
C     JSKPR     - SKIPPING FACTOR FOR ANGLE CORRESPONDING TO RANGE SKIPPING.
C     SMATCH    - Match angular skipping factor to range skipping factor
C     SCTCOLR   - Color to be used for reference lines
C     IXNAM     - NAME OF THE FIELD TO BE PLOTTED ALONG THE ABSCISSA (X)
C     XFMN,XFMX - MINIMUM AND MAXIMUM VALUE OF THE ABSCISSA FIELD
C     IYNAM     - NAME OF THE FIELD TO BE PLOTTED ALONG THE ORDINATE (Y)
C     YFMN,YFMX - MINIMUM AND MAXIMUM VALUE OF THE ORDINATE FIELD
C     SLOP,YCEPT- SLOPE AND INTERCEPT OF LINE TO BE DRAWN ON SCATTER PLOT
C     RLAG,ALAG - LAG ORDINATE (Y) FIELD THESE AMOUNTS IN RANGE AND ANGLE
C                 BEFORE PLOTTING
C     NANG      - NUMBER OF ANGLES IN THE CURRENT SWEEP
C
C  Always accumulate scattergrams, but only plot if after each sweep (ASCT)
C     or end-of-volume.
C
C     ASCT      - ( TRUE)-Draw accumulated scattergrams after NSWPAVG sweeps, 
C                 (FALSE)-Do not draw accumulated scattergram after sweeps
C     PLTEOV    - ( TRUE)-Draw accumulated scattergrams after end-of-volume
C                 (FALSE)-Do not draw accumulated scattergrams after end-of-volume
C     ASCTCLR   - ( TRUE)-Clear arrays for accumulated scattergrams
C                 (FALSE)-Do not clear arrays
C     MSKP,NSKP - (RANGE,ANGLE) SKIPPING FACTORS
C     NDSKP     - (RANGE,ANGLE) SKIPPING FACTOR FOR STORING SCATTER POINTS
C                 NDSKP=MSKP+NSKP
C     NDAT      - STORAGE ARRAY FOR NUMBER OF ACCUMULATED SCATTER POINTS
C                 FOR THE Nth PAIR OF FIELDS BEING SCATTERED.
C     XDAT,YDAT - STORAGE ARRAYS FOR ACCUMULATING NTH PAIR OF SCATTER POINTS
C                 100*(XDAT,YDAT) to store and 0.01*(XDAT,YDAT) to plot
C     NDMX      - Number of accumulated scatter points (X,Y) pairs
C     NSMX      - Number of scatter plots
C     HEIGHT (MSL) = RADAR HEIGHT + Z ABOVE RADAR + CURVATURE CORRECTION
C                    Z ABOVE RADAR = SLANT RANGE * SIN(ELEV)
C                    CURV CORRECT  = HOR. RANGE * HOR. RANGE OVER 4/3 EARTH
C                                    DIAMETER
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'colors.inc'
      INCLUDE 'swth.inc'

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*8 AVNAM
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER LABF*8
      CHARACTER*4 SMATCH

      LOGICAL PLTSW,ASCT,ASCTCLR,PLTEOV
      CHARACTER BGFLAG*1,SCTCOLR*2,LABLS*3

      LOGICAL ADIABCRV

      CHARACTER*8 IXNAM(NSMX),IYNAM(NSMX),BLANK
      CHARACTER*4 IRTYPE
      DIMENSION XFMN(NSMX),XFMX(NSMX)
      DIMENSION YFMN(NSMX),YFMX(NSMX)
      DIMENSION SLOP(NSMX),YCEPT(NSMX)
      DIMENSION RLAG(NSMX),ALAG(NSMX)
      INTEGER*2 XDAT(NDMX,NSMX),YDAT(NDMX,NSMX)
      DIMENSION NDAT(NSMX)
      DIMENSION XBOX(5),YBOX(5)

c      DATA XRT,YTP,SIDE,DYP/0.970,0.940,0.84,0.02/
      DATA XRT,YTP,SIDE,DYP/0.90,0.94,0.80,0.02/
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA BLANK/'        '/

      CHARACTER*4 AC_STRING
      LOGICAL FINDX,FINDY,FINDSTRING,FILLBOX
      DATA AC_STRING/'fssp'/
      DATA FILLBOX/.FALSE./
      DATA LWSTART,LWINC/3000,1000/

      PARAMETER (NAMX=500,NCMX=10)
      COMMON/LWC/PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),B1(NCMX),
     X     B2(NCMX),B3(NCMX),HMAX(NCMX),NAL(NCMX),PALT(NAMX,NCMX),
     X     ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX),
     X     ADBCON(NAMX,NCMX),NC
      CHARACTER LABP*40,LABQ*40,LABL*40,LABN*1

C     Common blocks for NLDN and LMA positional information
C
C        XNLD,YNLD - (X,Y) position (km) of CG strike relative to origin
C        HNLD      - Horizontal range (km) of the CG strike
C        AZNLD     - Azimuth angle (deg) of the CG strike
C        PNLD      - Polarity of ground strike and E-field strength (Kv/m)
C        TNLD      - Time of the strike (sec)
C        INLD      - Number of strikes read in (See GETNLD).
C        DTNLD     - Time interval for plotting strikes.  Plot points within
C                    a time window relative to the current radar scan.
C                    DTNLD .gt. 0: Plot +/- DTNLD from central time (RMSEC)
C                    DTNLD .le. 0: Plot within DTNLD seconds outside the
C                               radar scan time interval
C
C        XLMA,YLMA - (X,Y) position of landmark relative to origin
C        ZLMA      - Height of LMA lightning channel
C        HLMA      - Horizontal range (km) of lightning channel
C        AZLMA     - Azimuth angle (deg) of lightning channel
C        TLMA      - Time of the strike
C        ILMA      - Number of strikes read in (See GETLMA).
C        DTLMA     - Time interval for plotting strikes.  Plot points within
C                    a time window relative to the current radar scan.
C                    DTLMA .gt. 0: Plot +/- DTLMA from central time (RMSEC)
C                    DTLMA .le. 0: Plot within DTLMA seconds outside the
C                                  radar scan time interval
C
      PARAMETER (MXNL=100000,MXLM=1000000)      
      COMMON/NLDN/XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL),
     X            HNLD(MXNL),AZNLD(MXNL),INLD,DTNLD
      COMMON/LMA/XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM),TLMA(MXLM),
     X           HLMA(MXLM),AZLMA(MXLM),ILMA,DTLMA
      DATA XSIZ/18.0/

C     Correlation and regression curves:
C        XVAR,YVAR - (Independent, Dependent) variables
C        NXY       - Number of (XVAR,YVAR) pairs
C        NCF       - Number of coefficients in least-squares polynomial fit
C        COEF      - Array of coefficients in least-squares polynomial fit
C        COEF(1)   - Constant coefficient of the polynomial
C        TX        - Workspace array, dimensioned at least NXY.
C
      DIMENSION XVAR(NDMX),YVAR(NDMX),TX(NDMX),COEF(10)
      DATA JTOT/20/

C     SWATH times and fixed angles are generated in PPI_MMM.
C        IFTIME,ITIME  - Beginning, Ending times of scan (data.inc)
C        ITIME1,ITIME2 - Beginning, Ending times of the swath (swth.inc)
C        FXANG1,FXANG2 - Beginning, Ending fixed angles for swath (swth.inc)
C
c-----debug (ljm)
c      print *,'PLTSCAT: itim1,iftim,itim,itim2=',
c     +     itime1,iftime,itime,itime2
c      print *,'PLTSCAT:    fxang1,fxang,fxang2=',fxang1,fxang,fxang2
c      print *,'PLTSCAT:    asct,plteov,asctclr=',asct,plteov,asctclr
c-----debug (ljm)

C     Impose the additional constraint that data must
C     fall inside the SUR, PPI, or RHI plot windows.
C        SUR: GXMN-GXMX are east-west   (X) bounds
C             GYMN-GYMX are north-south (Y) bounds
C        PPI: GXMN-GXMX are east-west   (X) bounds
C             GYMN-GYMX are north-south (Y) bounds
C        RHI: GXMN-GXMX are radar range bounds
C             GYMN-GYMX are radar height bounds
C
      GXMN=GXMIN(ITPOLD)
      GXMX=GXMAX(ITPOLD)
      GYMN=GYMIN(ITPOLD)
      GYMX=GYMAX(ITPOLD)

      IRTYPE='SCAT'
      X2=XRT
      X1=XRT-SIDE
      Y2=YTP
      Y1=YTP-SIDE

      CALL SFLUSH
      IF(BGFLAG.EQ.'W')THEN
         CALL GSCR(1,0,1.,1.,1.)
         CALL GSCR(1,1,0.,0.,0.)
      ELSE
         CALL GSCR(1,0,0.,0.,0.)
         CALL GSCR(1,1,1.,1.,1.)
      END IF
      CALL GSPLCI(1)
      CALL GSTXCI(1)

C     Loop over all pairs of scatter plot fields.
C     
      N=1
 10   CONTINUE
      ITOT=1

      FINDX=FINDSTRING(AC_STRING,IXNAM(N),4)
      FINDY=FINDSTRING(AC_STRING,IYNAM(N),4)    
      IF(FINDX .OR. FINDY)THEN
         NDSKP=1
      ELSE
         NDSKP=MSKP+NSKP
      END IF

      IFLX=IFIND(IXNAM(N),NAMFLD,NFLDS)
      IFLY=IFIND(IYNAM(N),NAMFLD,NFLDS)
      IF(IFLX*IFLY.EQ.0)GO TO 20
      DDX=XFMX(N)-XFMN(N)
      DDY=YFMX(N)-YFMN(N)
      DELX=0.01*DDX
      DELY=0.01*DDY
      IF(DDX.LE.0.0.OR.DDY.LE.0.0)GO TO 20
      IF(IFLD(IFLX).LT.0 .AND. IFLD(IFLY).LT.0)THEN
         PLTSW=.TRUE.
         ISW=2
         R1=RMNSW
         DRR=DRSW
      ELSE
         PLTSW=.FALSE.
         ISW=1
         R1=R0
         DRR=DROLD
      END IF
      
C     Compute indices for range window. (SRMN,SRMX) are horizontal ranges.
C     Match angular skipping factor (JSKPR) to range skipping (ISKP)
C        only if SMATCH='YES'.
C        DELA - actual average angular increment between beams.
C        DA   - desired angular increment corresponding to the range
C               increment (ISKP*DRR).  DA decreases as range increases.
C
      RMX=SQRT(SRMX*SRMX+SZMX*SZMX)
      IRB=1.001+(SRMN-R1)/DRR
      IRE=1.001+( RMX-R1)/DRR
      IF(IRB.LT.1)IRB=1
      IF(IRE.GT.MXR)IRE=MXR
      AZBEG=AZA(1,ISW)
      AZEND=AZA(NANG(ISW),ISW)
      IF(AZBEG.LT.0.0)AZBEG=AZBEG+360.0
      IF(AZEND.LT.0.0)AZEND=AZEND+360.0
      DELA=ABS(AZEND-AZBEG)/NANG(ISW)

c      write(*,*)'sct: isw,na,dela=',isw,nang(isw),dela
c      write(*,*)'sct: fxold,itpold=',fxold,itpold
c      write(6,1770)irb,ire,srmn,srmx,rmx,szmn,szmx,r1,drr
c 1770 format(' sct: irbe,srmn-mx,rmx,szmn-mxmx=',2i8,7f8.3)
      
C     SCATTER THE NTH PAIR OF FIELDS
C     
      CALL MAJMIN(XFMN(N),XFMX(N),IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(YFMN(N),YFMX(N),IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XFMN(N),XFMX(N),YFMN(N),YFMX(N),1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL PERIML(MJRX,MNRX,MJRY,MNRY)
      
      ILAG=NINT(RLAG(N))
      JLAG=NINT(ALAG(N))

c-----print *,'PLTSCAT: mscan,nfxsct,nswpavg=',mscan,nfxsct,nswpavg

      IF(ASCT .OR. PLTEOV)THEN

C        DRAW SCAN-VOLUME ACCUMULATED SCATTER PLOTS
C
         PLTSW=.TRUE.
         ITOT=NDAT(N)
         IF(NDAT(N).GE.JTOT)THEN
            DO ND=1,NDAT(N)
               DATX=0.01*XDAT(ND,N)
               DATY=0.01*YDAT(ND,N)
               CALL PLCHMQ(DATX,DATY,'+',6.0,0.0,0.0)
C               CALL POINT(DATX,DATY)
               XVAR(ND)=DATX
               YVAR(ND)=DATY
            END DO

C           Calculate correlation coefficient (CCF) and 
C           standard error (STDERR) from regression analysis.
C
            CCF=CORR(ITOT,XVAR,YVAR,STDERR)
            print *,'PLTSCAT: xnam,ynam=',ixnam(n),iynam(n)
            print *,'         itot,ccf,stderr=',itot,ccf,stderr

C           Perform an ICCF-order curve fit.
C              1st - two coefficients [y = c(1) + c(2)*x]
C              2nd - two coefficients [y = c(1) + c(2)*x + c(3)*x^2]
C
            ICCF=1
            NCF=ICCF+1
            CALL LSTSQR(NCF,ITOT,XVAR,YVAR,COEF,TX)
            print *,'         itot,coef=',itot,coef(1),coef(2)

         END IF

      ELSE

C        DRAW PER-SCAN SCATTER PLOTS
C
C     When doing INTEGR, AVRAGE, OR STATS, compute outputs through 
C     current sweep from the accumulator arrays,
C
C        IF(IFLD(IFLX).EQ.-2.OR.IFLD(IFLX).EQ.-3)THEN
C           IFL=IFLX
C           CALL INTEGR
C        END IF
C        IF(IFLD(IFLY).EQ.-2.OR.IFLD(IFLY).EQ.-3)THEN
C           IFL=IFLY
C           CALL INTEGR
C        END IF
C
         AVNAM=IXNAM(N)
         IF(IFLD(IFLX).EQ.-5 .OR. 
     X      IFLD(IFLX).EQ.-6 .OR.
     X      AVNAM(5:8).EQ.'mean' .OR. 
     X      AVNAM(5:8).EQ.'sdev')THEN
            IFL=IFLX
            CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF
         AVNAM=IYNAM(N)
         IF(IFLD(IFLY).EQ.-5 .OR.
     X      IFLD(IFLY).EQ.-6 .OR.
     X      AVNAM(5:8).EQ.'mean' .OR. 
     X      AVNAM(5:8).EQ.'sdev')THEN
            IFL=IFLY
            CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF

C     ABS(SLOP)=999.9: Plot NLDN and LMA positional information
C
         IF(ABS(SLOP(N)).EQ.999.9)GO TO 205

C        Loop over ranges and change JSKPR factor to maintain
C        linear skip in angular direction nearly same as range
C        skipping when SMATCH='YES'.
C
         DO 200 I=IRB,IRE,ISKP

            SRNG=R1+(I-1)*DRR
            IF(SRNG.LE.0.001)GO TO 200
            DA=TODEG*(ISKP*DRR)/SRNG
            IF(DELA.NE.0)THEN
               JSKPR=NINT(ABS(DA/DELA))
            ELSE
               JSKPR=1
            END IF
            IF(SMATCH.EQ.'YES')THEN
               JSKPR=MAX0(JSKPR,JSKP)
            ELSE
               JSKPR=JSKP
            END IF
C            write(*,*)'i,r,da,jskpr=',i,srng,da,jskpr

C           Loop over all changing angles at the current range.
C     
            DO 100 J=1,NANG(ISW),JSKPR

C           Check if changing angle is within angle window.  AZA contains
C           the angle that is changing during the scan at a fixed angle.  
C           If ITPOLD=3 (rhi), AZA is the elevation angle; otherwise AZA 
C           is the azimuth angle.
C     
               HANG=AZA(J,ISW)
               IF(HANG.LT.0.0)THEN
                  TANG=HANG+360.0
               ELSE
                  TANG=HANG
               END IF

C     Only plot data inside the PLTSCAT angle window
C
               IF(TANG.LT.SAMN.OR.TANG.GT.SAMX)GO TO 100

C              RHI scan:
C
               IF(ITPOLD.EQ.3)THEN
                  IF(ISW.EQ.2)THEN
                     AZ=FXOLD-AZROT(ITPOLD)
                  ELSE
                     AZ=ELA(J,ISW)
                  END IF
                  EL=AZA(J,ISW)

C              All other scans:
C
               ELSE
                  AZ=AZA(J,ISW)-AZROT(ITPOLD)
                  IF(ISW.EQ.2)THEN
                     EL=FXOLD
                  ELSE
                     EL=ELA(J,ISW)
                  END IF
                  IF(AZ.LT.0.0)AZ=AZ+360.0
               END IF
               SINA=SIN(AZ*TORAD)
               COSA=COS(AZ*TORAD)
               SINE=SIN(EL*TORAD)
               COSE=COS(EL*TORAD)

               HRNG=SRNG*COSE
               X=X0+HRNG*SINA
               Y=Y0+HRNG*COSA
               Z=H0+SINE*SRNG+HRNG*HRNG/RE
c               write(6,1771)j,i,el,srng,cose,hrng,z
c 1771          format('sct:j,i,el,srng,cose,hrng,z=',2i8,5f8.3)

C     Only plot data inside the PLTSCAT range-height window
C
               IF(Z.LT.SZMN.OR.Z.GT.SZMX)GO TO 100
               IF(HRNG.LT.SRMN.OR.HRNG.GT.SRMX)GO TO 100

C     Impose the additional constraint that data must
C     fall inside the SUR, PPI, or RHI plot windows.
C
               IF(ITPOLD.EQ.3)THEN

C                 RHI scan:
C
                  IF(HRNG.LT.GXMN.OR.HRNG.GT.GXMX)GO TO 100
                  IF(Z.LT.GYMN.OR.Z.GT.GYMX)GO TO 100
               ELSE

C                 All other scans:
C
                  IF(X.LT.GXMN.OR.X.GT.GXMX)GO TO 100
                  IF(Y.LT.GYMN.OR.Y.GT.GYMX)GO TO 100
               END IF
               
               DATX=DAT(I,J,IFLX)
               DATY=DAT(I+ILAG,J+JLAG,IFLY)
               IF(DATX.EQ.BDVAL .OR. DATY.EQ.BDVAL)GO TO 100
               IF(DATX.LE.XFMN(N) .OR. DATX.GE.XFMX(N))GO TO 100
               IF(DATY.LE.YFMN(N) .OR. DATY.GE.YFMX(N))GO TO 100

               CALL PLCHMQ(DATX,DATY,'+',6.0,0.0,0.0)

C              Store data pair from every NDSKPth (range, angle) location,
C              where NDSKP=MSKP+NSKP with MSKP (range) and NSKP (angle).
C
               IF(MOD(ITOT,NDSKP).EQ.0)THEN
                  NDAT(N)=NDAT(N)+1
                  IF(NDAT(N).LE.NDMX)THEN
                     XDAT(NDAT(N),N)=NINT(100.0*DATX)
                     YDAT(NDAT(N),N)=NINT(100.0*DATY)
                  ELSE
                     NDAT(N)=NDMX
                  END IF
                  XVAR(ITOT)=DATX
                  YVAR(ITOT)=DATY
                  ITOT=ITOT+1
                  IF(ITOT.GE.NDMX)ITOT=NDMX
               END IF

 100        CONTINUE
 200     CONTINUE


C        Calculate the correlation coefficient (CCF) and
C        standard error (STDERR) from regression analysis.
C     
         IF(ITOT.GE.JTOT)THEN
            CCF=CORR(ITOT,XVAR,YVAR,STDERR)
            print *,'PLTSCAT: xnam,ynam=',ixnam(n),iynam(n)
            print *,'         itot,ccf,stderr=',itot,ccf,stderr
         
C           Perform an ICCF-order curve fit.
C              1st - two coefficients [y = c(1) + c(2)*x]
C              2nd - two coefficients [y = c(1) + c(2)*x + c(3)*x^2]
C     
            ICCF=1
            NCF=ICCF+1
            CALL LSTSQR(NCF,ITOT,XVAR,YVAR,COEF,TX)
            print *,'         itot,coef=',itot,coef(1),coef(2)
         END IF

 205     CONTINUE

C     Plot NLDN (SLOP=999.9) and LMA (SLOP=-999.9) positional information
C
         IF(CNLD.LE.0.0)THEN
            CSIZ=0.5*XSIZ
         ELSE
            CSIZ=CNLD
         END IF

C     Find beginning, middle, and ending seconds for the current scan.
C     Don't plot if the CG strike time TNLD is completely 
C     outside the plotting time segment [SECMN to SECMX]. 
C
         IFTIME_OLD=IFTIME
         ITIME_OLD=ITIME
         IHRB= IFTIME/10000
         IMNB=(IFTIME-IHRB*10000)/100
         ISCB= IFTIME-IHRB*10000-IMNB*100
         IHRE= ITIME/10000
         IMNE=(ITIME-IHRE*10000)/100
         ISCE= ITIME-IHRE*10000-IMNE*100
         IBSEC=IHRB*3600+IMNB*60+ISCB
         IESEC=IHRE*3600+IMNE*60+ISCE
         
C     Handle times crossing midnight
C     
         IF(IFTIME_OLD.GT.IFTIME)THEN
            IBSEC=IBSEC+24*3600
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

C     Plot NLDN positional information
C
         IF(IXNAM(N).EQ.'X'.AND.IYNAM(N).EQ.'Y')THEN
            print *,'PLTSCAT NLDN: inld,dtnld=',inld,dtnld
            print *,'            iftime,itime=',iftime,itime
            print *,'                 xmn,xmx=',xfmn(n),xfmx(n)
            print *,'                 ymn,ymx=',yfmn(n),yfmx(n)

C           Plot +/- DTNLD from central or
C           beg/end time of current scan.
C
            IF(DTNLD.GT.0.0)THEN
               SECMN=RMSEC-DTNLD
               SECMX=RMSEC+DTNLD
            ELSE
               SECMN=FLOAT(IBSEC)-ABS(DTNLD)
               SECMX=FLOAT(IESEC)+ABS(DTNLD)
            END IF

            ITOT=0
            DO 210 I=1,INLD
               XL=XNLD(I)
               YL=YNLD(I)
               TL=TNLD(I)
               POLAR=PNLD(I)
               IF(XL.LE.GXMN .OR. XL.GE.GXMX)GO TO 210
               IF(YL.LE.GYMN .OR. YL.GE.GYMX)GO TO 210
               IF(TL.LT.SECMN .OR. TL.GT.SECMX)GO TO 210
               IF(XL.LE.XFMN(N) .OR. XL.GE.XFMX(N))GO TO 210
               IF(YL.LE.YFMN(N) .OR. YL.GE.YFMX(N))GO TO 210
               IF(POLAR.GT.0.0)THEN
                  CALL PLCHMQ (XL,YL,'X',CSIZ,0.0,0.0)
               ELSE
                  CALL PLCHMQ (XL,YL,'0',CSIZ,0.0,0.0)
               END IF
               ITOT=ITOT+1
 210        CONTINUE
         END IF

C     Plot LMA positional information
C
         IF(IXNAM(N).EQ.'Hrng'.AND.IYNAM(N).EQ.'Hght')THEN
            print *,'PLTSCAT - plot LMA: ilma=',ilma,dtlma
            print *,'            iftime,itime=',iftime,itime
            print *,'                 xmn,xmx=',xfmn(n),xfmx(n)
            print *,'                 ymn,ymx=',yfmn(n),yfmx(n)

C           Plot +/- DTLMA from central or
C           beg/end time of current scan.
C
            IF(DTLMA.GT.0.0)THEN
               SECMN=RMSEC-DTLMA
               SECMX=RMSEC+DTLMA
            ELSE
               SECMN=FLOAT(IBSEC)-ABS(DTLMA)
               SECMX=FLOAT(IESEC)+ABS(DTLMA)
            END IF

            ITOT=0
            DO 220 I=1,ILMA
               XL=HLMA(I) 
               YL=ZLMA(I)
               TL=TLMA(I)
               IF(XL.LE.GXMN .OR. XL.GE.GXMX)GO TO 220
               IF(YL.LE.GYMN .OR. YL.GE.GYMX)GO TO 220
               IF(TL.LT.SECMN .OR. TL.GT.SECMX)GO TO 220
               IF(XL.LE.XFMN(N) .OR. XL.GE.XFMX(N))GO TO 220
               IF(YL.LE.YFMN(N) .OR. YL.GE.YFMX(N))GO TO 220
               CALL PLCHMQ (XL,YL,'*',CSIZ,0.0,0.0)
               ITOT=ITOT+1
 220        CONTINUE

C     Plot NLDN positional information
C
            print *,'PLTSCAT NLDN: inld,dtnld=',inld,dtnld
            print *,'            iftime,itime=',iftime,itime
            print *,'                 xmn,xmx=',xfmn(n),xfmx(n)
            print *,'                 ymn,ymx=',yfmn(n),yfmx(n)

C           Plot +/- DTNLD from central or
C           beg/end time of current scan.
C
            IF(DTNLD.GT.0.0)THEN
               SECMN=RMSEC-DTNLD
               SECMX=RMSEC+DTNLD
            ELSE
               SECMN=FLOAT(IBSEC)-ABS(DTNLD)
               SECMX=FLOAT(IESEC)+ABS(DTNLD)
            END IF

            DO 230 I=1,INLD
               XL=XNLD(I)
               YL=YNLD(I)
               HL=HNLD(I)
               ZL=YFMN(N)+0.5
               TL=TNLD(I)
               POLAR=PNLD(I)
               IF(XL.LE.GXMN .OR. XL.GE.GXMX)GO TO 230
               IF(YL.LE.GYMN .OR. YL.GE.GYMX)GO TO 230
               IF(TL.LT.SECMN .OR. TL.GT.SECMX)GO TO 230
               IF(HL.LE.XFMN(N) .OR. HL.GE.XFMX(N))GO TO 230
               IF(POLAR.GT.0.0)THEN
                  CALL PLCHMQ (HL,ZL,'X',2.0*CSIZ,0.0,0.0)
               ELSE
                  CALL PLCHMQ (HL,ZL,'0',2.0*CSIZ,0.0,0.0)
               END IF
 230        CONTINUE
         END IF

C     When doing INTEGR, AVRAGE, OR STATS, restore accumulators.
C     
C        IF(IFLD(IFLX).EQ.-2.OR.IFLD(IFLX).EQ.-3)THEN
C           IFL=IFLX
C           CALL UNINTEGR
C        END IF
C        IF(IFLD(IFLY).EQ.-2.OR.IFLD(IFLY).EQ.-3)THEN
C           IFL=IFLY
C           CALL UNINTEGR
C        END IF
C
         AVNAM=IXNAM(N)
         IF(IFLD(IFLX).EQ.-5 .OR.
     X      IFLD(IFLX).EQ.-6 .OR.
     X      AVNAM(5:8).EQ.'mean' .OR.
     X      AVNAM(5:8).EQ.'sdev')THEN
            IFL=IFLX
            CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF
         AVNAM=IYNAM(N)
         IF(IFLD(IFLY).EQ.-5 .OR.
     X      IFLD(IFLY).EQ.-6 .OR.
     X      AVNAM(5:8).EQ.'mean' .OR. 
     X      AVNAM(5:8).EQ.'sdev')THEN
            IFL=IFLY
            CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF
         
c         write(*,*)'regsct: ',ixnam(n),iynam(n),n,ndat(n),itot
      END IF

C     NOW ADD ALL REFERENCE LINES with color SCTCOLR.
C        DRAW 1:1 SOLID LINE
C     
 20   CONTINUE
      CALL SFLUSH
      IF(SCTCOLR.EQ.'WW')THEN
         CALL GSPLCI (1)
      ELSE IF(SCTCOLR.EQ.'BB')THEN
         CALL GSPLCI (IBLACK)
      ELSE IF(SCTCOLR.EQ.'GG')THEN
         CALL GSPLCI (IGRAY)
      ELSE IF(SCTCOLR.EQ.'ww')THEN
         CALL GSPLCI (IWHITE)
      ELSE IF(SCTCOLR.EQ.'rr')THEN
         CALL GSPLCI (IRED)
      ELSE IF(SCTCOLR.EQ.'gg')THEN
         CALL GSPLCI (IGREEN)
      ELSE IF(SCTCOLR.EQ.'bb')THEN
         CALL GSPLCI (IBLUE)
      ELSE IF(SCTCOLR.EQ.'cy')THEN
         CALL GSPLCI (ICYAN)
      ELSE IF(SCTCOLR.EQ.'mg')THEN
         CALL GSPLCI (IMAGENTA)
      ELSE IF(SCTCOLR.EQ.'yy')THEN
         CALL GSPLCI (IYELLOW)
      END IF
      CALL SETUSV('LW',1250)

      IF(SLOP(N) .EQ. 1.0 .AND. YCEPT(N) .EQ. 0.0 .OR.
     +   SLOP(N) .EQ. 0.0 .AND. YCEPT(N) .EQ. 0.0)THEN
         XX1=AMAX1(XFMN(N),YFMN(N))
         YY1=AMIN1(XFMX(N),YFMX(N))
         CALL LINE (XX1,XX1,YY1,YY1)
      END IF

      IF(SLOP(N).EQ.-99.9)THEN

C        Draw horizontal dashed line for reference
C
         XX1=XFMN(N)
         YY1=YCEPT(N)
         XX2=XFMX(N)
         YY2=YCEPT(N)
         CALL DASHDB (O'170360')
         CALL LINED(XX1,YY1,XX2,YY2)

      ELSE IF(SLOP(N).EQ.99.9)THEN

C        Draw vertical dashed line for reference
C
         XX1=YCEPT(N)
         YY1=YFMN(N)
         XX2=YCEPT(N)
         YY2=YFMX(N)
         CALL DASHDB (O'170360')
         CALL LINED(XX1,YY1,XX2,YY2)

      ELSE IF(SLOP(N).EQ.-1000.0)THEN

C        Draw dashed line for linear regression curve
C        (1st-order polynomial fit: Y=SLOPE*X+YINCEPT)
C        SLOPE = COEF(2), YINCEPT = XMN*SLOPE
C
         CALL SFLUSH
         CALL GSPLCI (IRED)
         CALL SETUSV('LW',1500)
         YINCEPT = COEF(1)
         SLOPE = COEF(2)
         XMN=XFMN(N)
         YMN=SLOPE*XMN+YINCEPT
         IF(YMN.LT.YFMN(N))THEN
            XX1=(YFMN(N)-YINCEPT)/SLOPE
            YY1=YFMN(N)
         ELSE IF(YMN.GT.YFMX(N))THEN
            XX1=(YFMX(N)-YINCEPT)/SLOPE
            YY1=YFMX(N)
         ELSE
            XX1=XFMN(N)
            YY1=YMN
         END IF
         XMX=XFMX(N)
         YMX=SLOPE*XMX+YINCEPT
         IF(YMX.LT.YFMN(N))THEN
            XX2=(YFMN(N)-YINCEPT)/SLOPE
            YY2=YFMN(N)
         ELSE IF(YMX.GT.YFMX(N))THEN
            XX2=(YFMX(N)-YINCEPT)/SLOPE
            YY2=YFMX(N)
         ELSE
            XX2=XFMX(N)
            YY2=YMX
         END IF
         CALL DASHDB (O'170360')
         CALL LINED(XX1,YY1,XX2,YY2)
         CALL SFLUSH
         CALL GSPLCI(1)
         CALL GSTXCI(1)

      ELSE IF(SLOP(N).NE.0.0)THEN

C        Draw dashed line for Y=SLOP*X+YCEPT
C
         XMN=XFMN(N)
         YMN=SLOP(N)*XMN+YCEPT(N)
         IF(YMN.LT.YFMN(N))THEN
            XX1=(YFMN(N)-YCEPT(N))/SLOP(N)
            YY1=YFMN(N)
         ELSE IF(YMN.GT.YFMX(N))THEN
            XX1=(YFMX(N)-YCEPT(N))/SLOP(N)
            YY1=YFMX(N)
         ELSE
            XX1=XFMN(N)
            YY1=YMN
         END IF
         XMX=XFMX(N)
         YMX=SLOP(N)*XMX+YCEPT(N)
         IF(YMX.LT.YFMN(N))THEN
            XX2=(YFMN(N)-YCEPT(N))/SLOP(N)
            YY2=YFMN(N)
         ELSE IF(YMX.GT.YFMX(N))THEN
            XX2=(YFMX(N)-YCEPT(N))/SLOP(N)
            YY2=YFMX(N)
         ELSE
            XX2=XFMX(N)
            YY2=YMX
         END IF
         IF(SLOP(N).EQ.1.0.AND.YCEPT(N).EQ.0.0)THEN
            CALL LINE(XX1,YY1,XX2,YY2)
         ELSE
            CALL DASHDB (O'170360')
            CALL LINED(XX1,YY1,XX2,YY2)
         END IF

      END IF
      
      IF(N.LT.NAS)THEN
         IF(IXNAM(N+1).EQ.IXNAM(N).AND.IYNAM(N+1).EQ.IYNAM(N))THEN
            N=N+1
            XFMN(N)=XFMN(N-1)
            XFMX(N)=XFMX(N-1)
            YFMN(N)=YFMN(N-1)
            YFMX(N)=YFMX(N-1)
            GO TO 20
         END IF
      END IF

C     ADD ADIABATIC CON, LWC or DBZ
C
      IF((IXNAM(N)(1:3).EQ.'CON' .AND. IYNAM(N).EQ.'HGHT').OR.
     +   (IXNAM(N)(1:2).EQ.'CN'  .AND. IYNAM(N).EQ.'HGHT').OR.
     +   (IXNAM(N)(1:3).EQ.'LWC' .AND. IYNAM(N).EQ.'HGHT').OR.
     +   (IXNAM(N)(1:2).EQ.'LW'  .AND. IYNAM(N).EQ.'HGHT').OR.
     +   (IXNAM(N)(1:3).EQ.'DBZ' .AND. IYNAM(N).EQ.'HGHT').OR.
     +   (IXNAM(N)(1:2).EQ.'DZ'  .AND. IYNAM(N).EQ.'HGHT'))THEN

         ADIABCRV=.TRUE.

         DO 40 NCRV=1,NC

            K=1+MOD(NCRV-1,3)
            KLW=LWSTART-(K-1)*LWINC
            CALL SETUSV('LW',KLW)

c            IF(NCRV.EQ.1)THEN
c               CALL SFLUSH
c               CALL GSPLCI (IRED)
c               CALL SETUSV('LW',1500)
c            ELSE
c               CALL SFLUSH
c               CALL GSPLCI (IGREEN)
c               CALL SETUSV('LW',1200)
c            END IF

            DO 30 L=1,NAL(NCRV)-1
               YP1=PALT(L,NCRV)
               YP2=PALT(L+1,NCRV)
               IF(IXNAM(N)(1:3).EQ.'CON'.OR.
     +            IXNAM(N)(1:2).EQ.'CN')THEN
                  XP1=ADBCON(L,NCRV)
                  XP2=ADBCON(L+1,NCRV)
               ELSE IF(IXNAM(N)(1:3).EQ.'LWC'.OR.
     +                 IXNAM(N)(1:2).EQ.'LW')THEN
                  XP1=ADBLWC(L,NCRV)
                  XP2=ADBLWC(L+1,NCRV)
               ELSE IF(IXNAM(N)(1:3).EQ.'DBZ'.OR.
     +                 IXNAM(N)(1:2).EQ.'DZ')THEN
                  XP1=ADBDBZ(L,NCRV)
                  XP2=ADBDBZ(L+1,NCRV)
               END IF
               YPQ=AMIN1(YFMX(N)-0.125*DDY,PALT(NAL(NCRV),NCRV))
               IF(YPQ.GE.YP1.AND.YPQ.LE.YP2)THEN
                  YP=0.5*(YP1+YP2)-DELY
                  XP=0.5*(XP1+XP2)+DELX
               END IF
               IF(XP1.LT.XFMN(N) .AND. XP2.GT.XFMN(N))XP1=XFMN(N)
               IF(XP1.LT.XFMX(N) .AND. XP2.GT.XFMX(N))XP2=XFMX(N)
               IF(XP1.LT.XFMN(N) .AND. XP2.LT.XFMX(N))GO TO 30
               IF(XP1.GT.XFMX(N) .AND. XP2.GT.XFMX(N))GO TO 30
               IF(YP1.LT.YFMN(N))GO TO 30
               IF((YP2+0.2).GE.YFMX(N))GO TO 32
               CALL LINE(XP1,YP1,XP2,YP2)
 30         CONTINUE

 32         LL=L

            IF(IXNAM(N)(1:3).EQ.'CON'.OR.
     +         IXNAM(N)(1:2).EQ.'CN')THEN
               XP2=ADBCON(LL,NCRV)
            ELSE IF(IXNAM(N)(1:3).EQ.'LWC'.OR.
     +              IXNAM(N)(1:2).EQ.'LW')THEN
               XP2=ADBLWC(LL,NCRV)
            ELSE IF(IXNAM(N)(1:3).EQ.'DBZ'.OR.
     +              IXNAM(N)(1:2).EQ.'DZ')THEN
               XP2=ADBDBZ(LL,NCRV)
            END IF

            YP2=PALT(LL,NCRV)
            FX=CUFX(XP2)
            FY=CUFY(YP2)+0.02
            XP=CFUX(FX)
            YP=CFUY(FY)
            WRITE(LABN,33)NCRV
 33         FORMAT(I1)
            CALL PLCHMQ(XP,YP,LABN,10.0,0.0,0.0)

 40      CONTINUE

      ELSE

         ADIABCRV=.FALSE.

      END IF

C     FLUSH THE CURRENT PLOT (AFTER ALL ADDITIONAL LINES)
C
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SETUSV('LW',1000)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

      IF(ADIABCRV)THEN

C        Fill a box with background color (Index=0)
C
         XUL=XRT-SIDE+0.03
         YUL=YTP-0.03
         XLR=XUL+0.36
         YLR=YUL-DYP*(NC+1.5)
         XBOX(1)=XUL
         YBOX(1)=YUL
         XBOX(2)=XLR
         YBOX(2)=YUL
         XBOX(3)=XLR
         YBOX(3)=YLR
         XBOX(4)=XUL
         YBOX(4)=YLR
         XBOX(5)=XUL
         YBOX(5)=YUL

         IF(FILLBOX)THEN
            CALL SFLUSH
            CALL GSFACI (0)
            CALL GFA (5,XBOX,YBOX)
         END IF

         CALL LINE(XUL,YUL,XLR,YUL)
         CALL LINE(XLR,YUL,XLR,YLR)
         CALL LINE(XLR,YLR,XUL,YLR)
         CALL LINE(XUL,YLR,XUL,YUL)

         WRITE(LABL,41)
 41      FORMAT('   Z(km)  p(mb)  T(C)  N(#/cc)')
         XP=XUL
         YP=YUL-0.7*DYP
         CALL PLCHMQ(XP,YP,LABL,12.0,0.0,-1.0)

         DO NCRV=1,NC
            WRITE(LABP,43)NCRV,ZBASE(NCRV),PBASE(NCRV),
     X           TBASE(NCRV),NINT(B3(NCRV))
 43         FORMAT(I2,F5.1,F8.1,F6.1,I7)
            YP=YP-DYP
            CALL PLCHMQ(XP,YP,LABP,12.0,0.0,-1.0)
c         WRITE(LABQ,45)B1(NCRV),B2(NCRV),NINT(B3(NCRV))
c 45      FORMAT('(a1,a2,a3)=(',2F5.2,I5,')')
c            YP=YP-0.02
c            CALL PLCHMQ (XP,YP,LABQ,12.0,0.0,-1.0)
         END DO
      END IF

      WRITE(LABF,201)IXNAM(N)
 201  FORMAT(A8)
      XP=XRT-0.5*SIDE
      YP=YTP-SIDE-0.055
      CALL PLCHMQ (XP, YP, LABF, 12.0, 0.0, 0.0)
      WRITE(LABF,203)IYNAM(N)
 203  FORMAT(A8)
      XP=XRT-SIDE-0.075
      YP=YTP-0.5*SIDE
      CALL PLCHMQ (XP, YP, LABF, 12.0, 90.0, 0.0)

      FBT=YTP-SIDE
      PTOT=FLOAT(ITOT)
      C0=COEF(1)
      C1=COEF(2)
      CALL LABEL2(IRTYPE,PTOT,PBAR,PSTD,SRMN,SRMX,SAMN,SAMX,
     X     SZMN,SZMX,BDUM,ILAG,JLAG,PLTSW,NFRAME,FBT,PMIN,PMAX,
     X     CCF,STDERR,C0,C1,LABLS)

C     INCREMENT COUNTER AND CONTINUE IF MORE SCATTER PLOTS
C
      N=N+1

      IF(N.GT.NAS)THEN

C        CLEAR COUNT ACCUMULATORS IF DOING VOLUME-ACCUMULATED PLOTS
C
         IF(ASCT .AND. ASCTCLR)THEN
            DO 300 N=1,NAS
               NDAT(N)=0
 300        CONTINUE
         END IF

         RETURN
      END IF

      GO TO 10
      
      END
