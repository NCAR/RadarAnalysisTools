c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTHIST(NAH,IHNAM,HRMN,HRMX,HAMN,HAMX,HZMN,HZMX,X0,Y0,
     X     H0,IHKP,JHKP,FMN,FMX,FBIN,FREF,PMN,PMX,HTYP,HFIT,PLTSW,
     X     NFRAME,AHST,AHSTCLR,HMATCH,NFXHST,NSWPAVG,PLTEOV,BGFLAG,
     X     HSTCOLR,LABLS)
C
C  PLOT ALL HISTOGRAMS FOR THE CURRENT SWEEP
C
C     NAH       - NUMBER OF HISTOGRAMS TO BE PLOTTED
C     IHNAM     - NAMES   "   FIELDS    "  "    "
C     HRMN,HRMX - MIN AND MAX HORZ RANGE (KM) FOR ACCUMULATING HISTOGRAM
C     HAMN,HAMX -  "   "   "  ANGLE (DEG)      "        "          "
C     HZMN,HZMX -  "   "   "  HEIGHT (KM)      "        "          "
C     IHKP,JHKP - SKIPPING FACTORS FOR RANGE AND ANGLE
C     JHKPR     - SKIPPING FACTOR FOR ANGLE CORRESPONDING TO RANGE SKIPPING.
C     HMATCH    - Match angular skipping factor to range skipping factor
C     HSTCOLR   - Color to be used for reference lines
C     FMN,FMX   - MINIMUM AND MAXIMUM FIELD VALUES FOR PLOT ABSCISSA (X)
C     FREF      - ABSCISSA (X) REFERENCE LINE
C     FBIN      - BIN WIDTH FOR ACCUMULATION OF PERCENT OCCURRENCE
C     PMN,PMX   - MINIMUM AND MAXIMUM PERCENT OCCURRENCE FOR PLOT ORDINATE (Y)
C     HTYP      - TYPE OF SCALING FOR ORDINATE (LINEAR OR LOGARITHMIC)
C     HFIT      - TYPE OF FIT TO DISTRIBUTION
C     NANG      - NUMBER OF ANGLES IN THE CURRENT SWEEP
C
C     NBIN      - NUMBER OF HISTOGRAM BINS
C     BIN       - ARRAY OF CENTERS OF HISTOGRAM BIN
C     PBIN      - ARRAY OF NUMBER OF FIELD VALUES FALLING IN EACH BIN FOR
C                 A SINGLE SWEEP.
C
C  Always accumulate histograms, but only plot if after each sweep (AHST)
C     or end-of-volume.
C
C     AHST      - ( TRUE)-Draw accumulated histograms after NSWPAVG sweeps, 
C                 (FALSE)-Do not draw accumulated histogram after sweeps
C     PLTEOV    - ( TRUE)-Draw accumulated histograms after end-of-volume
C                 (FALSE)-Do not draw accumulated histograms after end-of-volume
C     AHSTCLR   - ( TRUE)-Clear arrays for accumulated histograms
C                 (FALSE)-Do not clear arrays
C     ABIN      - ARRAY OF NUMBER OF FIELD VALUES FALLING IN EACH BIN FOR
C                 SEVERAL SWEEPS.
C     HEIGHT (MSL) = RADAR HEIGHT + Z ABOVE RADAR + CURVATURE CORRECTION
C                    Z ABOVE RADAR = SLANT RANGE * SIN(ELEV)
C                    CURV CORRECT  = HOR. RANGE * HOR. RANGE OVER 4/3 EARTH
C                                    DIAMETER
C
C     IFTIME,ITIME  - Beginning, Ending times of scan (data.inc)
C     ITIME1,ITIME2 - Beginning, Ending times of the swath (swth.inc)
C     FXANG1,FXANG2 - Beginning, Ending fixed angles for swath (swth.inc)
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'colors.inc'
      INCLUDE 'swth.inc'

c     PARAMETER (NBMX=1001,NHMX=25,NBHMX=NBMX*NHMX)
      PARAMETER (NBMX=501,NHMX=50,NBHMX=NBMX*NHMX)
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*4 IRTYPE
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*8 AVNAM
      CHARACTER*4 HMATCH

      LOGICAL PLTSW,AHST,AHSTCLR,PLTEOV
      CHARACTER BGFLAG*1,HSTCOLR*2,LABLS*3

      CHARACTER*8 IHNAM(NHMX)
      CHARACTER*4 HTYP(NHMX),HFIT(NHMX)
      DIMENSION FMN(NHMX),FMX(NHMX),FBIN(NHMX),FREF(NHMX,2)
      DIMENSION PMN(NHMX),PMX(NHMX)
      DIMENSION BIN(NBMX),PBIN(NBMX),ABIN(NBMX,NHMX)
      DIMENSION ATOT(NHMX),ASUM(NHMX),ASUMSQ(NHMX)
      DIMENSION AMIN(NHMX),AMAX(NHMX)
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA SQRT_2PI/2.506628275/
      DATA EPS/0.001/
      DATA ATOT,ASUM,ASUMSQ/NHMX*0.0,NHMX*0.0,NHMX*0.0/
      DATA ABIN/NBHMX*0.0/
      DATA XRT,YTP,SIDE/0.970,0.940,0.84/

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

C     SWATH times and fixed angles are generated in PPI_MMM.
C        IFTIME,ITIME  - Beginning, Ending times of scan (data.inc)
C        ITIME1,ITIME2 - Beginning, Ending times of the swath (swth.inc)
C        FXANG1,FXANG2 - Beginning, Ending fixed angles for swath (swth.inc)
C
c-----debug (ljm)
c      print *,'PLTHIST: itim1,iftim,itim,itim2=',
c     +     itime1,iftime,itime,itime2
c      print *,'PLTHIST:    fxang1,fxang,fxang2=',fxang1,fxang,fxang2
c      print *,'PLTHIST:    ahst,plteov,ahstclr=',ahst,plteov,ahstclr
c      print *,'hst; nah,nflds=',nah,nflds
c      print *,'hst:     ihnam=',(ihnam(n),n=1,nah)
c      print *,'hst:    namfld=',(namfld(n),n=1,nflds)
c-----debug (ljm)
C     Loop over all histogram fields, extract field for accumulation
C
      DO 300 N=1,NAH
         IFL=IFIND(IHNAM(N),NAMFLD,NFLDS)
         print *,'PLTHIST: n,namfld=',n,ihnam(n)
         IF(IFLD(IFL).LT.0)THEN
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
         ITYP=1
         IF(HTYP(N).EQ.'LOG ')ITYP=2

         PTOT=0.0
         PBAR=0.0
         PSTD=0.0
         PSUM=0.0
         PSUMSQ=0.0
         PMIN=1000.0
         PMAX=-1000.0

         NBIN=1.001+(FMX(N)-FMN(N))/FBIN(N)
         IF(NBIN.GT.NBMX)THEN
            WRITE(6,11)NBIN,NBMX
 11         FORMAT(1X,'*** WARNING: NBIN=',I5,
     X           ' EXCEEDS MAX ALLOWED, RESET TO ',I5,' ***')
            NBIN=NBMX
         END IF
         DO 20 NB=1,NBIN
            BIN(NB)=FMN(N)+(NB-1)*FBIN(N)
            PBIN(NB)=0.0
 20      CONTINUE
         BINC=0.5*FBIN(N)
         BMIN=BIN(1)-BINC
         BMAX=BIN(NBIN)+BINC
         print *,' hst: bmn,bmx,bin,n=',bmin,bmax,binc,nbin

C        Compute indices for range window. (HRMN,HRMX) are horizontal ranges.
C        Match angular skipping factor (JHKPR) to range skipping (IHKP)
C        only if HMATCH='YES'.
C           DELA - actual average angular increment between beams.
C           DA   - desired angular increment corresponding to the range
C                  increment (IHKP*DRR).  DA decreases as range increases.
C
         RMX=SQRT(HRMX*HRMX+HZMX*HZMX)
         IRB=1.001+(HRMN-R1)/DRR
         IRE=1.001+( RMX-R1)/DRR
         IF(IRB.LT.1)IRB=1
         IF(IRE.GT.MXR)IRE=MXR
         AZBEG=AZA(1,ISW)
         AZEND=AZA(NANG(ISW),ISW)
         IF(AZBEG.LT.0.0)AZBEG=AZBEG+360.0
         IF(AZEND.LT.0.0)AZEND=AZEND+360.0
         DELA=ABS(AZEND-AZBEG)/NANG(ISW)

c--------debug (ljm)
c         print *,'hst: n,ihnam,ifl=',n,' ',ihnam(n),ifl
c         print *,'hst: isw,na,dela=',isw,nang(isw),dela
c         print *,'hst: fxold,itpold=',fxold,itpold
c         print *,'hst:   bins-mn-mx=',fmn(n),fmx(n),fbin(n)
c         write(6,1770)irb,ire,hrmn,hrmx,rmx,hzmn,hzmx,r1,drr
c 1770    format(' hst: irbe,hrmn-mx,rmx,hzmn-mxmx=',2i8,7f8.3)
c--------debug (ljm)

C     When doing INTEGR, AVRAGE, OR STATS, compute outputs through 
C     current sweep from the accumulator arrays,
C        Older code: IF(IFLD(IFL).EQ.-2.OR.IFLD(IFL).EQ.-3)CALL INTEGR
C
         AVNAM=IHNAM(N)
         IF(IFLD(IFL).EQ.-5 .OR.
     X      IFLD(IFL).EQ.-6 .OR.
     X      AVNAM(5:8).EQ.'mean' .OR. 
     X      AVNAM(5:8).EQ.'sdev')THEN
            CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF

C     Don't continue accumulation, only plot at end-of-volume
C
         IF(PLTEOV)GO TO 200

C        Loop over ranges and change JHKPR factor to maintain
C        linear skip in angular direction nearly same as range
C        skipping when HMATCH='YES'.
C
c--------print *,'hst: irb,ire,ihkp=',irb,ire,ihkp
         DO 150 I=IRB,IRE,IHKP

            SRNG=R1+(I-1)*DRR
            IF(SRNG.LE.0.001)GO TO 150
            DA=TODEG*(IHKP*DRR)/SRNG
            IF(DELA.NE.0)THEN
               JHKPR=NINT(ABS(DA/DELA))
            ELSE
               JHKPR=1
            END IF
            IF(HMATCH.EQ.'YES')THEN
               JHKPR=MAX0(JHKPR,JHKP)
            ELSE
               JHKPR=JHKP
            END IF
c-----------print *,'i,r,da,jhkpr=',i,srng,da,jhkpr

C           Loop over all changing angles at the current range
C           and accumulate total and appropriate bin count.
C
            DO 100 J=1,NANG(ISW),JHKPR

               DATIJ=DAT(I,J,IFL)
               IF(DATIJ.EQ.BDVAL)GO TO 100
               IF(DATIJ .LT. BMIN .OR. DATIJ .GT. BMAX)GO TO 100

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

C     Only plot data inside the PLTHIST angle window
C
               IF(TANG .LT. HAMN .OR. TANG .GT. HAMX)GO TO 100
               
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
c--------------debug (ljm)
c               write(6,1771)j,i,ela(j,isw),srng,cose,hrng,z
c 1771          format('hst:j,i,el,srng,cose,hrng,z=',2i8,5f8.3)
c--------------debug (ljm)

C     Only plot data inside the PLTHIST range-height window
C
               IF(Z.LT.HZMN.OR.Z.GT.HZMX)GO TO 100
               IF(HRNG.LT.HRMN.OR.HRNG.GT.HRMX)GO TO 100

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

               IB=1.001+(DATIJ-BMIN)/FBIN(N)

C              Single-sweep histogram
C
               PTOT=PTOT+1.0
               PSUM=PSUM+DATIJ
               PSUMSQ=PSUMSQ+DATIJ*DATIJ
               PBIN(IB)=PBIN(IB)+1.0
               IF(DATIJ.LT.PMIN)PMIN=DATIJ
               IF(DATIJ.GT.PMAX)PMAX=DATIJ

C              Accumulate histogram for future plotting
C
               ATOT(N)=ATOT(N)+1.0
               ASUM(N)=ASUM(N)+DATIJ
               ASUMSQ(N)=ASUMSQ(N)+DATIJ*DATIJ
               ABIN(IB,N)=ABIN(IB,N)+1.0
               IF(DATIJ.LT.AMIN(N))AMIN(N)=DATIJ
               IF(DATIJ.GT.AMAX(N))AMAX(N)=DATIJ

 100        CONTINUE
 150     CONTINUE
         
C     When doing INTEGR, AVRAGE, OR STATS, restore accumulators.
C     Older code: IF(IFLD(IFL).EQ.-2.OR.IFLD(IFL).EQ.-3)CALL UNINTEGR
C     
         AVNAM=IHNAM(N)
         IF(IFLD(IFL).EQ.-5 .OR.
     X      IFLD(IFL).EQ.-6 .OR.
     X      AVNAM(5:8).EQ.'mean' .OR. 
     X      AVNAM(5:8).EQ.'sdev')THEN
            CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF
         
C     Always draw per-scan histogram
C     
c-----debug (ljm)
c         print *,'PLTRHST: mscan,nfxhst,nswpavg=',
c     +        mscan,nfxhst,nswpavg
c         print *,'PLTRHST:     n,nbin,ptot,atot=',
c     +        n,nbin,ptot,atot(n)
c         print *,'PLTRHST: ihnam,fmn,fmx,pmn,pmx,htyp=',
c     +        ihnam(n),fmn(n),fmx(n),pmn(n),pmx(n),htyp(n)
c-----debug (ljm)
         CALL SETHIST(IHNAM,FMN,FMX,PMN,PMX,HTYP,HFIT,FREF,N,NHMX,
     X        BGFLAG)

         IF(PTOT .GT. 2.0)THEN
            PBAR=PSUM/PTOT
            PVAR=(PSUMSQ-PTOT*PBAR*PBAR)/(PTOT-1.0)
            IF(PVAR.GE.0.0)PSTD=SQRT(PVAR)

C           Compute amplitude at mean bin location (PBAR)
C           for scaling the Gaussian distribution.
C           Add reference lines at Mean and Mean +/- Sigma.
C
            IF(HFIT(N).EQ.'FIT ')THEN
               NBAR=NINT(1.0+(PBAR-FMN(N))/FBIN(N))
               PBIN_MX=100.0*PBIN(NBAR)/PTOT
               print *,'         nbar,pbin_mx=',nbar,pbin_mx
               DO M=-1,1
                  XP=PBAR+FLOAT(M)*PSTD
                  IF(ITYP.EQ.2)THEN
                     YP=0.7*PMX(N)
                  ELSE
                     YP=0.7*PMX(N)
                  END IF
                  CALL DASHDB (O'170360')
                  CALL LINED(XP,PMN(N),XP,YP)
                  YP=1.05*YP
                  IF(M.EQ.0)CALL PLCHMQ(XP,YP,'<-M->',10.0,0.0,0.0)
               END DO
            END IF
            DO NB=2,NBIN-1
               PLTBIN=100.0*PBIN(NB)/PTOT
               IF(PLTBIN.GT.PMN(N))THEN
                  FXL=BIN(NB)-BINC
                  FXR=BIN(NB)+BINC
                  CALL LINE(FXL,PMN(N),FXL,PLTBIN)
                  CALL LINE(FXL,PLTBIN,FXR,PLTBIN)
                  CALL LINE(FXR,PLTBIN,FXR,PMN(N))
               ELSE
                  CALL PLCHMQ(BIN(NB),PMN(N),'*',10.0,0.0,0.0)
               END IF

C              Generate a Gaussian distribution with 
C              sample mean and standard deviation
C
               IF(HFIT(N).EQ.'FIT ')THEN
                  ARG1=((BIN(NB)-PBAR)**2)/(2.0*PVAR)
                  ARG2=1.0
c                  ARG2=1.0-(BIN(NB)-PBAR)**2/(2.0*PVAR)
                  GAUSS=PBIN_MX*ARG2*EXP(-ARG1)
                  IF(GAUSS.GE.PMN(N).AND.GAUSS.LE.PMX(N))THEN
                     CALL PLCHMQ(BIN(NB),GAUSS,'+',10.0,0.0,0.0)
                  END IF
               END IF
            END DO
         END IF
         IRTYPE='HIST'
         FBT=YTP-SIDE
         CALL LABEL2(IRTYPE,PTOT,PBAR,PSTD,HRMN,HRMX,HAMN,HAMX,
     X        HZMN,HZMX,FBIN(N),IDUM,JDUM,PLTSW,NFRAME,FBT,PMIN,
     X        PMAX,CCF,STDERR,C0,C1,LABLS)
      
 200     CONTINUE

         IF(AHST .OR. PLTEOV)THEN
         
C     May also draw accumulated histogram
C     
c-----debug (ljm)
c            print *,'PLTAHST: mscan,nfxhst,nswpavg=',
c     +           mscan,nfxhst,nswpavg
c            print *,'PLTAHST:     n,nbin,ptot,atot=',
c     +           n,nbin,ptot,atot(n)
c            print *,'PLTAHST: ihnam,fmn,fmx,pmn,pmx,htyp=',
c     +        ihnam(n),fmn(n),fmx(n),pmn(n),pmx(n),htyp(n)
c-----debug (ljm)
            CALL SETHIST(IHNAM,FMN,FMX,PMN,PMX,HTYP,HFIT,FREF,N,NHMX,
     X           BGFLAG)
            
            PTOT=ATOT(N)
            IF(PTOT.GT.2.0)THEN
               PSUM=ASUM(N)
               PSUMSQ=ASUMSQ(N)
               PMIN=AMIN(N)
               PMAX=AMAX(N)
            ELSE
               PBAR=0.0
               PSTD=0.0
               PSUM=0.0
               PSUMSQ=0.0
               PMIN=1000.0
               PMAX=-1000.0
            END IF
         
C           Label the average and standard deviation
C
            IF(PTOT .GT. 2.0)THEN
               PBAR=PSUM/PTOT
               PVAR=(PSUMSQ-PTOT*PBAR*PBAR)/(PTOT-1.0)
               IF(PVAR.GE.0.0)PSTD=SQRT(PVAR)

C              Compute amplitude at mean bin location (PBAR)
C              for scaling the Gaussian distribution.
C              Add reference lines at Mean and Mean +/- Sigma
C
               IF(HFIT(N).EQ.'FIT ')THEN
                  NBAR=NINT(1.0+(PBAR-FMN(N))/FBIN(N))
                  PBIN_MX=100.0*PBIN(NBAR)/PTOT
                  print *,'         nbar,pbin_mx=',nbar,pbin_mx
                  DO M=-1,1
                     XP=PBAR+FLOAT(M)*PSTD
                     IF(ITYP.EQ.2)THEN
                        YP=0.8*PMX(N)
                     ELSE
                        YP=0.8*PMX(N)
                     END IF
                     CALL DASHDB (O'170360')
                     CALL LINED(XP,PMN(N),XP,YP)
                     YP=1.05*YP
                     IF(M.EQ.0)CALL PLCHMQ(XP,YP,'<-M->',10.0,0.0,0.0)
                  END DO
               END IF
               DO NB=2,NBIN-1
                  PLTBIN=100.0*ABIN(NB,N)/PTOT
                  IF(PLTBIN.GT.PMN(N))THEN
                     FXL=BIN(NB)-BINC
                     FXR=BIN(NB)+BINC
                     CALL LINE(FXL,PMN(N),FXL,PLTBIN)
                     CALL LINE(FXL,PLTBIN,FXR,PLTBIN)
                     CALL LINE(FXR,PLTBIN,FXR,PMN(N))
                  ELSE
                     CALL PLCHMQ(BIN(NB),PMN(N),'*',10.0,0.0,0.0)
                  END IF

C                 Generate a Gaussian distribution with 
C                 sample mean and standard deviation
C
                  IF(HFIT(N).EQ.'FIT ')THEN
                     ARG1=((BIN(NB)-PBAR)**2)/(2.0*PVAR)
                     ARG2=1.0
c                     ARG2=1.0-(BIN(NB)-PBAR)**2/(2.0*PVAR)
                     GAUSS=PBIN_MX*ARG2*EXP(-ARG1)
                     IF(GAUSS.GE.PMN(N).AND.GAUSS.LE.PMX(N))THEN
                        CALL PLCHMQ(BIN(NB),GAUSS,'+',10.0,0.0,0.0)
                        print *,'NB,BIN(NB),GAUSS=',nb,bin(nb),gauss
                     END IF
                  END IF
               END DO
               IF(AHSTCLR)THEN
                  ATOT(N)=0.0
                  ASUM(N)=0.0
                  ASUMSQ(N)=0.0
                  DO NB=1,NBIN
                     ABIN(NB,N)=0.0
                  END DO
               END IF
            END IF
            IRTYPE='HIST'
            FBT=YTP-SIDE
            ISW=1
            PLTSW=.TRUE.
            CALL LABEL2(IRTYPE,PTOT,PBAR,PSTD,HRMN,HRMX,HAMN,HAMX,
     X           HZMN,HZMX,FBIN(N),IDUM,JDUM,PLTSW,NFRAME,FBT,PMIN,
     X           PMAX,CCF,STDERR,C0,C1,LABLS)
         END IF

 300  CONTINUE

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETHIST(IHNAM,FMN,FMX,PMN,PMX,HTYP,HFIT,FREF,N,NHMX,
     X     BGFLAG)
C
C     Set up histogram plot for this field
C
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER LABF*8,LABP*18,LABH*20
      CHARACTER IFMT*4,IFMT1*20
      DATA LABP/'PERCENT OCCURRENCE'/
      DATA XRT,YTP,SIDE/0.970,0.940,0.84/
      DATA EPS/0.001/

      CHARACTER*8 IHNAM(NHMX)
      CHARACTER*4 HTYP(NHMX),HFIT(NHMX)
      DIMENSION FMN(NHMX),FMX(NHMX),FREF(NHMX,2)
      DIMENSION PMN(NHMX),PMX(NHMX)
      CHARACTER*1 BGFLAG

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
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      WRITE(LABF,201)IHNAM(N)
 201  FORMAT(A8)

      XP=XRT-0.5*SIDE
      YP=YTP-SIDE-0.055
      CALL PLCHMQ (XP, YP, LABF, 12.0, 0.0, 0.0)
      XP=XRT-SIDE-0.075
      YP=YTP-0.5*SIDE
      CALL PLCHMQ (XP, YP, LABP, 12.0, 90.0, 0.0)
      DDX=FMX(N)-FMN(N)
      DDY=PMX(N)-PMN(N)
      CALL MAJMIN(FMN(N),FMX(N),IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(PMN(N),PMX(N),IFMTY,MJRY,MNRY,IPLY)
      ITYP=1
      IF(HTYP(N).EQ.'LOG ')THEN
         ITYP=2
         MJRY=1
         MNRY=8
      END IF

      X2=XRT
      X1=XRT-SIDE
      Y2=YTP
      Y1=YTP-SIDE
      CALL SET(X1,X2,Y1,Y2,FMN(N),FMX(N),PMN(N),PMX(N),ITYP)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL PERIML(MJRX,MNRX,MJRY,MNRY)

      DO 210 L=1,2
         IF(FREF(N,L).NE.99.9)THEN
            XP=FREF(N,L)
            IF(ITYP.EQ.2)THEN
               YP=(0.85-L*0.1)*PMX(N)
            ELSE
               YP=(1.0-L*0.1)*PMX(N)
            END IF
            CALL DASHDB (O'170360')
            CALL LINED(XP,PMN(N),XP,YP)
            AFREF=FREF(N,L)
            IFREF=INT(FREF(N,L))
            IF(FREF(N,L).EQ.0.0)THEN
               IDIGITS=1
            ELSE
               IDIGITS=1.001+ALOG10(ABS(FREF(N,L)))
               IF(FREF(N,L).LT.0.0)IDIGITS=IDIGITS+1
            END IF
            IF(ABS(NINT(AFREF)-AFREF).LT.EPS)THEN
               IF(IDIGITS.GT.9)IDIGITS=9
               WRITE(IFMT,203)IDIGITS
 203           FORMAT('I',I1)
               WRITE(IFMT1,205)IFMT
 205           FORMAT('("--[",',A4,',"]")')
               WRITE(LABH,IFMT1)IFREF
            ELSE
               IDEC=0.999+ABS(ALOG10(ABS(AFREF-IFREF)))
               IDIGITS=IDIGITS+IDEC+1
               IF(IDIGITS.GT.9)IDIGITS=9
               WRITE(IFMT,207)IDIGITS,IDEC
 207           FORMAT('F',I1,'.',I1)
               WRITE(IFMT1,205)IFMT
               WRITE(LABH,IFMT1)AFREF
            END IF
            CALL PLCHMQ(XP,YP,LABH,10.0,0.0,-1.0)
         END IF
 210  CONTINUE
      
      RETURN
      END

