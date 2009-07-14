c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTSPEC(NSP,IPNAM,PRMN,PRMX,PAMN,PAMX,IPKP,JPKP,PTYP,
     X                   SPAVG,FRQMN,FRQMX,PEXMN,PEXMX,FRQAX,AMPAX,
     X                   DTREND,PTAVG,FLDMN,FLDMX,FLDRF,PLTSW,NFRAME)
C
C  PLOT ALL SPECTROGRAMS FOR THE CURRENT SWEEP
C
C     NSP       - NUMBER OF SPECTROGRAMS TO BE COMPUTED AND PLOTTED
C     IPNAM     - NAMES   "   FIELDS      "  "    "      "     "
C     PRMN,PRMX - MINIMUM AND MAXIMUM RANGE  (KM) FOR COMPUTING SPECTRA
C     PAMN,PAMX -    "     "     "    ANGLE (DEG)  "      "        "
C     IPKP,JPKP - SKIPPING FACTORS FOR RANGE AND ANGLE
C     PTYP      - TYPE OF SPECTRA ('RNGE' OR 'ANGL')
C     SPAVG     - NUMBER OF SPECTRA TO AVERAGE (.LT. 0.0, AVG WITHOUT PLOTTING
C                 INDIVIDUAL SPECTRA, ONLY PLOT THE AVERAGED SPECTRA)
C     FRQMN,MX  - MINIMUM AND MAXIMUM FREQUENCIES FOR PLOT ABSCISSA (X)
C     PEXMN,MX  -    "     "     "     EXPONENTS   "    "  ORDINATE (Y)
C     FRQAX     - TYPE OF FREQUENCY AXIS ('LOGF' OR 'LINF')
C     AMPAX     -   "   " AMPLITUDE   "  ('LOGA' OR 'LINA')
C     DTREND    -   "   " DETRENDING ('NONE', 'MEAN', 'LINR', 'QUAD')
C     PTAVG     - NUMBER OF SPECTRAL POINTS TO AVERAGE
C     FLDMN,MX  - MINIMUM AND MAXIMUM VALUES OF THE FIELD FOR PLOTTING
C     FLDRF     - DASHED LINE FOR REFERENCE IN FIELD PLOTS
C
C     NANG      - NUMBER OF ANGLES IN THE CURRENT SWEEP
C
C
C     INPUT PARAMETERS FOR ROUTINE SPAL:
C           NOTE: A NORMALLY DISTRIBUTED [(MEAN,STD)=(0.0,0.2)]
C                 IS USED WHERE ORIGINAL DATA IS MISSING (BDVAL)
C
C             T - COPY OF REAL DATA AT CONSTANT INTERVAL TO BE TRANSFORMED
C             M - ACTUAL NUMBER OF INPUT DATA POINTS
C            MM -   "       "    " OUTPUT SPECTRAL POINTS
C             P - POWER SPECTRAL DENSITY ESTIMATES
C           DUM - SCRATCH ARRAY
C            MA - NUMBER OF SPECTRAL POINTS TO BE AVERAGED
C                 (MA=0, 5 PTS AVERAGED)
C           IDT - TYPE OF DETRENDING
C                 0 - NO DETRENDING      2 - LINEAR DETRENDING
C                 1 - REMOVAL OF MEAN    3 - QUADRATIC DETRENDING
C            SR - SAMPLE RATE (INTERVAL BETWEEN POINTS IN T)
C            MT - DIMENSION OF T, 2**K+4; WHERE K IS THE SMALLEST INTEGER
C                 THAT WILL MAKE 2**K .GE. N, THE NUMBER OF INPUT POINTS.
C                 FOR EXAMPLE, WITH MXR=768 INPUT POINTS, K=10 AND THE
C                 NUMBER OF POINTS TRANSFORMED BECOMES 2**10=1024.
C
C     OUTPUT PARAMETERS FROM ROUTINE SPAL:
C
C            FF - FUNDAMENTAL FREQUENCY PRESENT IN OUTPUT SPECTRUM (1/(N*SR))
C             F - FREQUENCIES ASSOCIATED WITH SPECTRAL ESTIMATES
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'colors.inc'
      PARAMETER (NBMX=201,NPMX=25)
      PARAMETER (MT=1028,MT2=MT/2)
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)

      CHARACTER*6 IFMT,IFMTX,IFMTY
      CHARACTER LABF*25,LABP*16,LABL*27
      CHARACTER LABT*8,LABR*11,LABA*11,LABG*34
      CHARACTER*4 PTYP,FRQAX(NPMX),AMPAX(NPMX),DTREND(NPMX)
      CHARACTER*3 FILT(6)
      CHARACTER*8 IPNAM(NPMX)
      LOGICAL COLRFIL,PLTSW

      DIMENSION FRQMN(NPMX),FRQMX(NPMX)
      DIMENSION PEXMN(NPMX),PEXMX(NPMX),PTAVG(NPMX)
      DIMENSION FLDMN(NPMX),FLDMX(NPMX),FLDRF(NPMX)
      DIMENSION PAVG(MXR,NPMX)
      DIMENSION T(MT),PWR(MT2),DUM(MT2),F(MT2)

      DATA XRT,YTP1,YTP2/0.960,0.940,0.71/
      DATA SIDEX,SIDEY1,SIDEY2/0.84,0.17,0.61/
      DATA LABP/'SPECTRAL DENSITY'/
      DATA LABF/'FREQUENCY (CYCLES PER KM)'/
      DATA LABR/'RANGE (KM) '/
      DATA LABA/'ANGLE (DEG)'/
      DATA IFMT,IPL,MJR,MNR/'(F8.5)',8,1,0/
      DATA MA,IDT/3,1/
      DATA IR1,IR2/ 3, 11 /
      DATA G1,G2/0.05,1.0/
      DATA FILT/'UNI','TRI','CRE','QUA','EXP','LSQ'/

      IF(PTYP.EQ.'ANGL')THEN
         PRINT *,'***WARNING - Angular spectra not yet implemented***'
         PRINT *,'****** These will actually be range spectra *******'
      END IF
      JAVG=0

      IR1=IR1+2
      PI2=ATAN(1.)*8.0

      COLRFIL=.FALSE.
      PLTSW=.FALSE.
      IRB=1.001+(PRMN-R0)/DROLD
      IRE=1.001+(PRMX-R0)/DROLD
      IF(IRB.LT.1)IRB=1
      IF(IRE.GT.MXR)IRE=MXR
      SR=DROLD
      M=IRE-IRB+1
      KAVG=IABS(NINT(SPAVG))

      CALL SFLUSH
      CALL GSCR(1,0,0.,0.,0.)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSPLCI(1)
      CALL GSTXCI(1)

C     LOOP OVER ALL ANGLES IN THE CURRENT SWEEP
C
      DO 200 J=1,NANG(1),JPKP
         PANG=AZA(J,1)
         IF(PANG.LT.0.0)THEN
            TANG=PANG+360.0
         ELSE
            TANG=PANG
         END IF
         IF(TANG.LT.PAMN.OR.TANG.GT.PAMX)GO TO 110
         JAVG=JAVG+1
         IF(JAVG.EQ.1)THEN
            ITM1=ITM(J,ISW)
            ITM2=ITM(J,ISW)
            AZ1=PANG
            AZ2=PANG
         END IF
         ITM2=ITM(J,ISW)
         AZ2=PANG

C        LOOP OVER ALL FIELDS IN THE CURRENT BEAM
C        EXTRACT M VALUES OF FIELD FOR SPECTRAL COMPUTATION AND PLOTTING;
C        REPLACE MISSING DATA WITH GAUSSIAN RANDOM NOISE WITH MEAN=RAVG
C        AND STD=0.1*(FLDMX-FLDMN).
C
         DO 100 N=1,NSP
            IFL=IFIND(IPNAM(N),NAMFLD,MXF)
            RSUM=0.0
            RCNT=0.0
            DO I=1,M
               II=IRB+I-1
               IF(DAT(II,J,IFL).NE.BDVAL)THEN
                  RSUM=RSUM+DAT(II,J,IFL)
                  RCNT=RCNT+1.0
               END IF
            END DO
            IF(RCNT.GT.0.0)THEN
               RAVG=RSUM/RCNT
            ELSE
               RAVG=0.0
            END IF
            FLD_DEL=FLDMX(N)-FLDMN(N)
            IF(FLD_DEL.NE.0.0)THEN
               G2=G1*FLD_DEL
            END IF
            DO I=1,M
               II=IRB+I-1
               IF(DAT(II,J,IFL).NE.BDVAL)THEN
                  T(I)=DAT(II,J,IFL)
               ELSE
                  U1=RANF()
                  U2=RANF()
                  VX= SQRT(-2.0*ALOG(U1)) * COS(PI2*U2)
                  RND=G2*VX+RAVG
                  T(I)=RND
               END IF
            END DO

C     Do Zero-padding beyond last true sample until power-of-two samples reached
C
            DO I=M+1,MT
               T(I)=0.0
            END DO

C           IF BOTH FLDMN(N) AND FLDMX(N) EQUAL 0.0, DO NOT PLOT THE CURRENT FIELD
C
            IF(FLDMN(N).EQ.0.0 .AND. FLDMX(N).EQ.0.0)GO TO 28
            X2=XRT
            X1=XRT-SIDEX
            Y2=YTP1
            Y1=YTP1-SIDEY1
            CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
            XP=XRT-0.5*SIDEX
            YP=YTP1-SIDEY1-0.04
            CALL PLCHMQ (XP, YP, LABR, 10.0, 0.0, 0.0)
            WRITE(LABT,25)IPNAM(N)
   25       FORMAT(A8)
            XP=XRT+0.02
            YP=YTP1-0.5*SIDEY1
            CALL PLCHMQ (XP, YP, LABT, 12.0,270.0, 0.0)
            CALL MAJMIN(PRMN,PRMX,IFMTX,MJRX,MNRX,IPLX)
            CALL MAJMIN(FLDMN(N),FLDMX(N),IFMTY,MJRY,MNRY,IPLY)
            CALL SET(X1,X2,Y1,Y2,PRMN,PRMX,FLDMN(N),FLDMX(N),1)
            CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
            CALL TICK4(20,10,20,10)
            CALL PERIML(MJRX,MNRX,MJRY,MNRY)
            IF(FLDRF(N).GT.FLDMN(N).AND.FLDRF(N).LT.FLDMX(N))THEN
c               CALL DASHDB (O'170360')
c               CALL LINED (PRMN,FLDRF(N),PRMX,FLDRF(N))
               CALL LINE (PRMN,FLDRF(N),PRMX,FLDRF(N))
            END IF
            IF(RAVG.GT.FLDMN(N).AND.RAVG.LT.FLDMX(N))THEN
               CALL DASHDB (O'170360')
               CALL LINED (PRMN,RAVG,PRMX,RAVG)
            END IF
            DO 26 I=1,M-1
               II=IRB+I-1
               R1=R0+(II-1)*DROLD
               R2=R1+DROLD
               T1=T(I)
               T2=T(I+1)
               IF(R1.LT.PRMN.OR.R1.GT.PRMX)GO TO 26
               IF(R2.LT.PRMN.OR.R2.GT.PRMX)GO TO 26
               IF(T1.LT.FLDMN(N).OR.T1.GT.FLDMX(N))GO TO 26
               IF(T2.LT.FLDMN(N).OR.T2.GT.FLDMX(N))GO TO 26
               CALL LINE(R1,T1,R2,T2)
   26       CONTINUE

C           COMPUTE SPECTRUM OF CURRENT FIELD
C
   28       CONTINUE
            IDT=1
            IF(DTREND(N).EQ.'NONE')THEN
               IDT=0
            ELSE IF(DTREND(N).EQ.'MEAN')THEN
               IDT=1
            ELSE IF(DTREND(N).EQ.'LINR')THEN
               IDT=2
            ELSE IF(DTREND(N).EQ.'QUAD')THEN
               IDT=3
            END IF
            MA=NINT(PTAVG(N))
            CALL SPAL(T,M,PWR,DUM,MA,IDT,SR,FF)
            FNYQ=1.0/(2.0*SR)
            MM=NINT(FNYQ/FF)
            FMIN=FF
            FMAX=MM*FF
            FMN=FRQMN(N)
            FMX=FRQMX(N)
            IF(AMPAX(N).EQ.'LOGA')THEN
               PMN=10.0**PEXMN(N)
               PMX=10.0**PEXMX(N)
            ELSE
               PMN=PEXMN(N)
               PMX=PEXMX(N)
            END IF

C           NORMALIZE THE POWER SPECTRUM TO UNIT AREA; I.E. POWER
C           SPECTRAL DENSITY CONVERTED TO A PROBABILITY DENSITY FUNCTION.
C
            PTOTAL=0.0
            DO 30 L=1,MM
               PTOTAL=PTOTAL+PWR(L)
               F(L)=FMIN+(L-1)*FF
   30       CONTINUE
            IF(PTOTAL.EQ.0.0)PTOTAL=1.0
            PMIN=PWR(1)/PTOTAL
            PMAX=PWR(1)/PTOTAL
            DO 40 L=1,MM
               PWR(L)=PWR(L)/PTOTAL
               IF(PWR(L).LT.PMN)PWR(L)=PMN
               IF(PWR(L).LT.PMIN)PMIN=PWR(L)
               IF(PWR(L).GT.PMAX)PMAX=PWR(L)
               PAVG(L,N)=PAVG(L,N)+PWR(L)
   40       CONTINUE

C           IF(SPAVG .LT. 0.O) DO NOT PLOT THE CURRENT FIELD SPECTRUM
C
            IF(SPAVG .LT. 0.0)GO TO 100
            X2=XRT
            X1=XRT-SIDEX
            Y2=YTP2
            Y1=YTP2-SIDEY2
            CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
            XP=XRT-0.5*SIDEX
            YP=YTP2-SIDEY2-0.05
            CALL PLCHMQ (XP, YP, LABF, 12.0, 0.0, 0.0)
            WRITE(LABL,51)LABP,IPNAM(N)
   51       FORMAT(A16,' (',A8,')')
            XP=XRT+0.02
            YP=YTP2-0.5*SIDEY2
            CALL PLCHMQ (XP, YP, LABL, 12.0,270.0, 0.0)
            XP=XRT-SIDEX
            YP=YTP2-SIDEY2-0.075
            IF(IGATE(IFL).LE.1) THEN
               CALL PLCHMQ(XP,YP,'NOT SMOOTHED   ',10.0,0.0,-1.0)
            ELSE
               IFLTYP=IFILTER(IFL)
               IF(FLSPAC(IFL).EQ.'RADR')THEN
                  WRITE(LABG,105)FLSPAC(IFL),FILT(IFLTYP),DXX(IFL),
     +                           DYY(IFL)
  105             FORMAT(A4,'-',A3,':',F5.2,' GTS BY',F5.2,' BMS')
                  CALL PLCHMQ(XP,YP,LABG,10.0,0.0,-1.0)
               ELSE IF(FLSPAC(IFL).EQ.'CART')THEN
                  WRITE(LABG,107)FLSPAC(IFL),FILT(IFLTYP),DXX(IFL),
     +                           DYY(IFL)
  107             FORMAT(A4,'-',A3,':',F5.2,' KM BY',F5.2,' KM')
                  CALL PLCHMQ(XP,YP,LABG,10.0,0.0,-1.0)
               ELSE
                  WRITE(LABG,109)FLSPAC(IFL),FILT(IFLTYP),IGATE(IFL)
  109             FORMAT(A4,'-',A3,':',I3,' GTS')
                  CALL PLCHMQ(XP,YP,LABG,10.0, 0.0,-1.0)
               END IF
            END IF
            IF(FRQAX(N).EQ.'LOGF')THEN
               IPLX=IPL
               IFMTX=IFMT
               MJRX=MJR
               MNRX=MNR
            ELSE
               CALL MAJMIN(FMN,FMX,IFMTX,MJRX,MNRX,IPLX)
            END IF
            IF(AMPAX(N).EQ.'LOGA')THEN
               IPLY=IPL
               IFMTY=IFMT
               MJRY=MJR
               MNRY=MNR
            ELSE
               CALL MAJMIN(PMN,PMX,IFMTY,MJRY,MNRY,IPLY)
            END IF
            IF(FRQAX(N).EQ.'LINF'.AND.AMPAX(N).EQ.'LINA')THEN
               LL=1
            ELSE IF(FRQAX(N).EQ.'LINF'.AND.AMPAX(N).EQ.'LOGA')THEN
               LL=2
            ELSE IF(FRQAX(N).EQ.'LOGF'.AND.AMPAX(N).EQ.'LINA')THEN
               LL=3
            ELSE IF(FRQAX(N).EQ.'LOGF'.AND.AMPAX(N).EQ.'LOGA')THEN
               LL=4
            END IF

            CALL SET(X1,X2,Y1,Y2,FMN,FMX,PMN,PMX,LL)
            CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
            CALL TICK4(20,10,20,10)
            CALL PERIML(MJRX,MNRX,MJRY,MNRY)
            FSUM=0.0
            PSUM=0.0
            FTOT=0.0
            DO 60 L=1,MM-1
               F1=F(L)
               F2=F(L+1)
               P1=PWR(L)
               P2=PWR(L+1)
               IF(P1.LE.0.0.OR.P2.LE.0.0)GO TO 60
               FSUM=FSUM+F1*P1
               PSUM=PSUM+P1
               FTOT=FTOT+1.0
               IF(F1.LT.FMN.OR.F1.GT.FMX)GO TO 60
               IF(F2.LT.FMN.OR.F2.GT.FMX)GO TO 60
               IF(P1.LT.PMN.OR.P1.GT.PMX)GO TO 60
               IF(P2.LT.PMN.OR.P2.GT.PMX)GO TO 60
               CALL LINE(F1,P1,F2,P2)
   60       CONTINUE
            IF(FTOT.GT.5.AND.PSUM.GT.0.0)THEN
               FBAR=FSUM/PSUM
            ELSE
               FBAR=0.0
            END IF
            CALL DASHDB (O'170360')
            CALL LINED (FMIN,PWR( 1),FMIN,PMN)
            CALL LINED (FMAX,PWR(MM),FMAX,PMN)
            CALL LINED (FMIN,PMX,FMIN,PMN)
            CALL LINED (FMAX,0.1*PMX,FMAX,PMN)
            ITM1=-999
            CALL LABEL3(PTYP,FTOT,FBAR,PRMN,PRMX,PAMN,PAMX,J,MA,FF,
     X                  FNYQ,DTREND(N),XRT,YTP2,SIDEY2,PLTSW,NFRAME,
     X                  ITM1,ITM2)
  100    CONTINUE
  110    IF(ABS(SPAVG).LE.0.0)GO TO 200

         IF(JAVG.GE.KAVG.OR.J.GE.NANG(1))THEN
C
C           PLOT THE AVERAGES OF (JAVG=KAVG) SPECTRA FOR EACH FIELD:
C
            X2=XRT
            X1=XRT-SIDEX
            Y2=YTP2
            Y1=YTP2-SIDEY2
            DO 150 N=1,NSP
               IFL=IFIND(IPNAM(N),NAMFLD,MXF)
               CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
               XP=XRT-0.30
               YP=YTP2+0.055
               WRITE(LABL,125)JAVG
  125          FORMAT('AVERAGE OF ',I4,' SPECTRA')
               CALL PLCHMQ( XP, YP, LABL, 12.0, 0.0, -1.0)
               IF(AZ1.LT.0.0)AZ1=AZ1+360.0
               IF(AZ2.LT.0.0)AZ2=AZ2+360.0
               WRITE(LABL,127)AZ1,AZ2
  127          FORMAT('ANGLES=',F6.2,' TO ',F6.2)
               YP=YP-0.02
               CALL PLCHMQ( XP, YP, LABL, 12.0, 0.0, -1.0)
               XP=XRT-0.5*SIDEX
               YP=YTP2-SIDEY2-0.05
               CALL PLCHMQ (XP, YP, LABF, 12.0, 0.0, 0.0)
               WRITE(LABL,51)LABP,IPNAM(N)
               XP=XRT+0.02
               YP=YTP2-0.5*SIDEY2
               CALL PLCHMQ (XP, YP, LABL, 12.0,270.0, 0.0)
               XP=XRT-SIDEX
               YP=YTP2-SIDEY2-0.075
               IF(IGATE(IFL).LE.1) THEN
                  CALL PLCHMQ(XP,YP,'NOT SMOOTHED   ',10.0,0.0,-1.0)
               ELSE
                  IFLTYP=IFILTER(IFL)
                  IF(FLSPAC(IFL).EQ.'RADR')THEN
                     WRITE(LABG,105)FLSPAC(IFL),FILT(IFLTYP),DXX(IFL),
     +                              DYY(IFL)
                     CALL PLCHMQ(XP,YP,LABG,10.0,0.0,-1.0)
                  ELSE IF(FLSPAC(IFL).EQ.'CART')THEN
                     WRITE(LABG,107)FLSPAC(IFL),FILT(IFLTYP),DXX(IFL),
     +                              DYY(IFL)
                     CALL PLCHMQ(XP,YP,LABG,10.0,0.0,-1.0)
                  ELSE
                     WRITE(LABG,109)FLSPAC(IFL),FILT(IFLTYP),IGATE(IFL)
                     CALL PLCHMQ(XP,YP,LABG,10.0, 0.0,-1.0)
                  END IF
               END IF
               IF(FRQAX(N).EQ.'LOGF')THEN
                  IPLX=IPL
                  IFMTX=IFMT
                  MJRX=MJR
                  MNRX=MNR
               ELSE
                  CALL MAJMIN(FMN,FMX,IFMTX,MJRX,MNRX,IPLX)
               END IF
               IF(AMPAX(N).EQ.'LOGA')THEN
                  IPLY=IPL
                  IFMTY=IFMT
                  MJRY=MJR
                  MNRY=MNR
               ELSE
                  CALL MAJMIN(PMN,PMX,IFMTY,MJRY,MNRY,IPLY)
               END IF
               IF(FRQAX(N).EQ.'LINF'.AND.AMPAX(N).EQ.'LINA')THEN
                  LL=1
               ELSE IF(FRQAX(N).EQ.'LINF'.AND.AMPAX(N).EQ.'LOGA')THEN
                  LL=2
               ELSE IF(FRQAX(N).EQ.'LOGF'.AND.AMPAX(N).EQ.'LINA')THEN
                  LL=3
               ELSE IF(FRQAX(N).EQ.'LOGF'.AND.AMPAX(N).EQ.'LOGA')THEN
                  LL=4
               END IF

               CALL SET(X1,X2,Y1,Y2,FMN,FMX,PMN,PMX,LL)
               CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
               CALL TICK4(20,10,20,10)
               CALL PERIML(MJRX,MNRX,MJRY,MNRY)
               FSUM=0.0
               PSUM=0.0
               FTOT=0.0
               PLFT=PAVG( 1,N)/JAVG
               PRHT=PAVG(MM,N)/JAVG
               IF(PLFT.LT.PMN)PLFT=PMN
               IF(PRHT.LT.PMN)PRHT=PMN
               DO 140 L=1,MM-1
                  F1=F(L)
                  F2=F(L+1)
                  P1=PAVG(L,N)/JAVG
                  P2=PAVG(L+1,N)/JAVG
                  PAVG(L,N)=0.0
                  IF(P1.LE.0.0.OR.P2.LE.0.0)GO TO 140
                  FSUM=FSUM+F1*P1
                  PSUM=PSUM+P1
                  FTOT=FTOT+1.0
                  IF(F1.LT.FMN.OR.F1.GT.FMX)GO TO 140
                  IF(F2.LT.FMN.OR.F2.GT.FMX)GO TO 140
                  IF(P1.LT.PMN.OR.P1.GT.PMX)GO TO 140
                  IF(P2.LT.PMN.OR.P2.GT.PMX)GO TO 140
                  CALL LINE(F1,P1,F2,P2)
  140          CONTINUE
               PAVG(MM,N)=0.0
               IF(FTOT.GT.5.AND.PSUM.GT.0.0)THEN
                  FBAR=FSUM/PSUM
               ELSE
                  FBAR=0.0
               END IF
               CALL DASHDB (O'170360')
               CALL LINED (FMIN,PLFT,FMIN,PMN)
               CALL LINED (FMAX,PRHT,FMAX,PMN)
               CALL LINED (FMIN,PMX,FMIN,PMN)
               CALL LINED (FMAX,0.1*PMX,FMAX,PMN)
               CALL LABEL3(PTYP,FTOT,FBAR,PRMN,PRMX,PAMN,PAMX,J,MA,FF,
     X                     FNYQ,DTREND(N),XRT,YTP2,SIDEY2,PLTSW,NFRAME,
     X                     ITM1,ITM2)
  150       CONTINUE
            JAVG=0
         END IF
  200 CONTINUE
      RETURN
      END
