c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTVAD(NVD,JVD,ZMNVD,ZMXVD,ISKPVD,WFILT,XMNVD,XMXVD,
     X     XSCLVD,XREFVD,TYPVD,H0,PLTSW,NFRAME,LABLS,BGFLAG)
C
C  PLOT ALL VAD SCATTERGRAMS FOR THE CURRENT SWEEP
C     VAD OUTPUT FIELDS ARE FILTERED WITH A TRIANGULAR FILTER APPLIED
C     OVER (2*WFILT+1) RANGE GATES (EQUIVALENT TO VERTICAL FILTERING)
C
C  VAD MEAN AND VARIANCE OUTPUT QUANTITIES STORED IN COMMON/VADWINDS/:
C
C     NAMVD   - LIST OF OUTPUT FIELDS FROM VAD WIND ANALYSES
C     NAMINVD -  "    "  INPUT    "    TO   "    "     "
C     NAMPLVD -  "    " OUTPUT    "   FROM  "    "     "     TO BE PLOTTED
C     VADTYPE - TYPE OF PLOT (VAD or COV)
C     VTYPE   - Type of analysis (FOUR or LSQR)
C     MVD     - TOTAL NUMBER OF VAD ANALYSES (MXVD=10)
C     JVD     -   "      "    "  "      "    TO BE PLOTTED
C     M       - LOOP INDEX FOR CURRENT VAD PLOTTING [NAMPLVD(M)]
C     IVD     - INDEX OF VAD ANALYSIS OUTPUT (NAMVD) CORRESPONDING TO NAMPLVD
C     U0,V0   - HORIZONTAL WINDS       FOR THE ITH RANGE GATE
C     SPD     -     "      SPEED        "   "   "    "     "
C     DIR     -     "      WIND DIREC   "   "   "    "     "
C     CON     -     "      CONVERGENCE  "   "   "    "     "
C     WVD     - VERTICAL WINDS (CON)    "   "   "    "     "
C     STR     - STRETCHING DEFORMATION  "   "   "    "     "
C     SHR     - SHEARING        "       "   "   "    "     "
C     ERR     - RMS DIFFERENCE BETWEEN FOURIER FIT AND INPUT RADIAL VEL
C     VARUVW  - SUM OF U, V, AND W VARIANCES (SEE FUNC = VADCOV)
C     COV_UV  - COVARIANCE(UV) (SEE FUNC = VADCOV)
C     COV_UW  - COVARIANCE(UW) (        "        )
C     COV_VW  - COVARIANCE(VW) (        "        )
C
C     ZMNVD,ZMXVD - MINIMUM AND MAXIMUM HEIGHT (KM) TO BE PLOTTED
C     ISKPVD      - HEIGHT SKIPPING FACTOR
C     U_VD        - AMOUNT TO SUBTRACT FROM U COMPONENT
C     V_VD        -    "    "     "      "  V     "
C     AZMVD       - AZIMUTH ANGLE OF +U-COMPONENT OF WINDS
C     WFILT       - WIDTH OF VERTICAL FILTER (KM)
C     XMNVD,XMXVD - MINIMUM AND MAXIMUM VALUE OF THE ABSCISSA FIELD
C     XSCLVD      - SCALING FACTOR (PLOT VALUE*XSCL)
C     XREFVD      - REFERENCE VALUE
C     TYPVD       - TYPE OF PLOT: 'SCAT', 'LINE', OR 'BOTH'
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'colors.inc'
      INCLUDE 'vadwinds.inc'
      PARAMETER (NVDMX=6)

      COMMON/SCRATCH/TMP1(MXR),TMP2(MXR,MXA)
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER LABX*7,LABY*7,LABZ*6,LABL*100,LABH*3,LABLS*3
      CHARACTER*1 BGFLAG
      CHARACTER*4 TYPVD(NVDMX,MXVD)
      LOGICAL PLTSW,FRST

      CHARACTER*7 IXNAM(NVDMX),JXNAM(NVDMX)
      DIMENSION XRT(NVDMX),YTP(NVDMX)
      DIMENSION XMNVD(NVDMX,MXVD),XMXVD(NVDMX,MXVD)
      DIMENSION XSCLVD(NVDMX,MXVD),XREFVD(NVDMX,MXVD)
      DIMENSION ZMNVD(MXVD),ZMXVD(MXVD),ISKPVD(MXVD)
      DIMENSION WFILT(MXVD)
      DATA IXNAM/'   U   ','   V   ','  SPD  ','  DIR  ','  CON  ',
     +           '  ERR  '/
      DATA JXNAM/'   U   ','   V   ','VAR_UVW','COV(UV)','COV(UW)',
     +           'COV(VW)'/
      DATA LABZ/'Z (KM)'/
      DATA XRT/0.32,0.54,0.76,0.98,0.32,0.54/
      DATA YTP/0.92,0.92,0.92,0.92,0.50,0.50/
      DATA SIDEX,SIDEY/0.175,0.35/
      DATA XRTU,YTPV,SIDE/0.98,0.50,0.35/
      DATA XOFF1,XOFF2,YOFF/0.0625,0.06,0.0425/
      DATA RE,TORAD/17000.0,0.017453293/

      M=1
 10   CONTINUE
      IVD=IFIND(NAMPLVD(M),NAMVD,MXVD)
      IF(IVD.EQ.0)THEN
         WRITE(6,11)
 11      FORMAT(1X,'*** NO SUCH VAD ANALYSIS IS AVAILABLE ***')
         M=M+1
         IF(M.LE.JVD)GO TO 10
         RETURN
      END IF

C     CHECK THE TYPE OF FIELD (IFLD) FOR THE VAD FIELD INDEX [IFLVAD(IVD)]
C
      IF(IFLD(IFLVAD(IVD)).LT.0)THEN
         ISW=2
         PLTSW=.TRUE.
         GSPC=DRSW
      ELSE
         ISW=1
         PLTSW=.FALSE.
         GSPC=DROLD
      END IF
c      write(*,*)"pltvad: m=",m," ",namplvd(m),naminvd(ivd),namvd(ivd),
c     +     iflvad(ivd),ifld(iflvad(ivd))

      CALL GSPLCI(1)
      CALL GSTXCI(1)

      WRITE(LABL,21)NAMPLVD(M),NAMINVD(IVD)
 21   FORMAT('VAD-OUT: ',A8,4X,'VAD-IN: ',A8)
c      WRITE(LABL,21)AZMVD(M),U_VD(M),V_VD(M),MSCAN,NAMINVD(IVD),
c     +     NAMPLVD(M)
c 21   FORMAT('AZ(U)=',F5.1,' DG [US,VS]=[',F5.1,',',F5.1,']',
c     +     4X,'SCAN:',I3,4X,'IN: ',A8,4X,'OUT: ',A8)
      J2=NINT(WFILT(M))
      J1=-J2

C     SET RANGE INDICES (SAME AS HEIGHT)
C
      IRB=MAX0(MNGATE,1)
      IRE=MIN0(MXGATE,MXG,MXR)
      SINE=SIN(TORAD*FXOLD)
      COSE=COS(TORAD*FXOLD)
      SRMN=R0+(IRB-1)*GSPC
      SRMX=R0+(IRE-1)*GSPC
      SAMN=AZA(1,ISW)
      SAMX=AZA(NANG(ISW),1)
      IF(SAMN.LT.0.0)SAMN=SAMN+360.0
      IF(SAMX.LT.0.0)SAMX=SAMX+360.0
      PTOT=WFILT(M)

C     PUT DIRECTIONS INSIDE INTERVAL [XMNVD(4,M), XMXVD(4,M)]
C
      IF(VADTYPE(M).EQ.'VAD')THEN
         DO I=IRB,IRE
            IF(DIR(I,IVD).NE.BDVAL)THEN
               IF(DIR(I,IVD).GT.XMXVD(4,M))THEN
                  DIR(I,IVD)=DIR(I,IVD)-360.0
               ELSE IF (DIR(I,IVD).LT.XMNVD(4,M))THEN
                  DIR(I,IVD)=DIR(I,IVD)+360.0
               END IF
            END IF
         END DO
      END IF

C     LOOP OVER ALL PAIRS OF FIELDS TO PLOT: (X,Y)=(VAD OUT,HEIGHT)
C     DO SCATTER, LINE OR BOTH PLOTS FOR EACH VAD OUTPUT FIELD.
C
      DO 300 N=1,NVDMX
         X2=XRT(N)
         X1=XRT(N)-SIDEX
         Y2=YTP(N)
         Y1=YTP(N)-SIDEY
         CALL MAJMIN(XMNVD(N,M),XMXVD(N,M),IFMTX,MJRX,MNRX,IPLX)
         CALL MAJMIN(  ZMNVD(M),  ZMXVD(M),IFMTY,MJRY,MNRY,IPLY)
         IPLX=0
         IPLY=0
         CALL SET(X1,X2,Y1,Y2,XMNVD(N,M),XMXVD(N,M),
     +        ZMNVD(M),ZMXVD(M),1)
         CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
         IF(N.EQ.1.OR.N.EQ.5)THEN
            CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)
         ELSE
            CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,0,5,X1,Y1)
         END IF

         CALL DASHDB (O'104210')
         X1=XSCLVD(N,M)*XREFVD(N,M)
         Y1=ZMNVD(M)
         Y2=ZMXVD(M)
         IF(X1.GT.XMNVD(N,M).AND.X1.LT.XMXVD(N,M))THEN
            CALL LINED(X1,Y1,X1,Y2)
         END IF

C        LOOP OVER ALL RANGES WITHIN CURRENT BEAM
C
         KK=0
         DO 100 I=IRB,IRE,ISKPVD(M)
            SRNG=R0+(I-1)*GSPC
            HRNG=SRNG*COSE
            Z=H0+SINE*SRNG+HRNG*HRNG/RE

            IF(VADTYPE(M).EQ.'VAD')THEN

               IF (N.EQ.1)THEN
                  VADOUT=U0(I,IVD)
               ELSE IF (N.EQ.2)THEN
                  VADOUT=V0(I,IVD)
               ELSE IF (N.EQ.3)THEN
                  VADOUT=SPD(I,IVD)
               ELSE IF (N.EQ.4)THEN
                  VADOUT=DIR(I,IVD)
               ELSE IF (N.EQ.5)THEN
                  VADOUT=CON(I,IVD)
               ELSE IF (N.EQ.6)THEN
                  VADOUT=ERR(I,IVD)
               ELSE IF (N.EQ.7)THEN
                  VADOUT=WVD(I,IVD)
               ELSE IF (N.EQ.8)THEN
                  VADOUT=STR(I,IVD)
               ELSE IF (N.EQ.9)THEN
                  VADOUT=SHR(I,IVD)
               END IF

            ELSE IF(VADTYPE(M).EQ.'COV')THEN

               IF (N.EQ.1)THEN
                  VADOUT=U0(I,IVD)
               ELSE IF (N.EQ.2)THEN
                  VADOUT=V0(I,IVD)
               ELSE IF (N.EQ.3)THEN
                  VADOUT=VARUVW(I,IVD)
               ELSE IF (N.EQ.4)THEN
                  VADOUT=COV_UV(I,IVD)
               ELSE IF (N.EQ.5)THEN
                  VADOUT=COV_UW(I,IVD)
               ELSE IF (N.EQ.6)THEN
                  VADOUT=COV_VW(I,IVD)
               ELSE 
                  VADOUT=BDVAL
               END IF

            END IF

C           PLOT ORIGINAL VALUES AS Xs regardless of plottype
C
            IF(Z.LT.ZMNVD(M) .OR. Z.GT.ZMXVD(M))GO TO 100
            IF(VADOUT.EQ.BDVAL)GO TO 100
            DATX=XSCLVD(N,M)*VADOUT
            IF(DATX.LT.XMNVD(N,M))DATX=XMNVD(N,M)
            IF(DATX.GT.XMXVD(N,M))DATX=XMXVD(N,M)
            CALL PLCHMQ(DATX,Z,'X',2.,0.,0.)

C           FILTER VALUES AS PASS THROUGH RANGE GATES
C
            IF(WFILT(M).GT.0.0)THEN
               IF(I.GE.(IRB+J2) .AND. I.LE.(IRE-J2))THEN
                  KK=KK+1
                  TMP2(KK,1)=BDVAL
                  TMP2(KK,2)=Z
                  WTSUM=0.0
                  CNT=0.0
                  SUMWT=0.0
                  DO 90 J=J1,J2
                     JJ=I+J
                     WT=(WFILT(M)+1.0)-ABS(FLOAT(J))

                     IF(VADTYPE(M).EQ.'VAD')THEN

                        IF (N.EQ.1)THEN
                           VADOUT=U0(JJ,IVD)
                        ELSE IF (N.EQ.2)THEN
                           VADOUT=V0(JJ,IVD)
                        ELSE IF (N.EQ.3)THEN
                           VADOUT=SPD(JJ,IVD)
                        ELSE IF (N.EQ.4)THEN
                           VADOUT=DIR(JJ,IVD)
                        ELSE IF (N.EQ.5)THEN
                           VADOUT=CON(JJ,IVD)
                        ELSE IF (N.EQ.6)THEN
                           VADOUT=ERR(JJ,IVD)
                        ELSE IF (N.EQ.7)THEN
                           VADOUT=WVD(JJ,IVD)
                        ELSE IF (N.EQ.8)THEN
                           VADOUT=STR(JJ,IVD)
                        ELSE IF (N.EQ.9)THEN
                           VADOUT=SHR(JJ,IVD)
                        END IF

                     ELSE IF(VADTYPE(M).EQ.'COV')THEN

                        IF (N.EQ.1)THEN
                           VADOUT=U0(JJ,IVD)
                        ELSE IF (N.EQ.2)THEN
                           VADOUT=V0(JJ,IVD)
                        ELSE IF (N.EQ.3)THEN
                           VADOUT=VARUVW(JJ,IVD)
                        ELSE IF (N.EQ.4)THEN
                           VADOUT=COV_UV(JJ,IVD)
                        ELSE IF (N.EQ.5)THEN
                           VADOUT=COV_UW(JJ,IVD)
                        ELSE IF (N.EQ.6)THEN
                           VADOUT=COV_VW(JJ,IVD)
                        ELSE 
                           VADOUT=BDVAL
                        END IF

                     END IF

                     IF(VADOUT.EQ.BDVAL)GO TO 90
                     WTSUM=WTSUM+WT*VADOUT
                     SUMWT=SUMWT+WT
                     CNT=CNT+1.0
   90             CONTINUE
                  IF(CNT.GE.3.0.AND.SUMWT.GT.0.0)TMP2(KK,1)=WTSUM/SUMWT
               END IF
            END IF
  100    CONTINUE

C        PLOT FILTERED VALUES
C
         IF( WFILT(M).GT.0.0 .AND. KK.GT.1 .AND.
     +      (TYPVD(N,M).EQ.'LINE' .OR. TYPVD(N,M).EQ.'BOTH'))THEN
            DO 110 K=2,KK
               X1=TMP2(K,1)
               X2=TMP2(K-1,1)
               Y1=TMP2(K,2)
               Y2=TMP2(K-1,2)
               IF(X1.EQ.BDVAL.OR.X2.EQ.BDVAL)GO TO 110
               IF(X1.LT.XMNVD(N,M) .OR. X1.GT.XMXVD(N,M))GO TO 110
               IF(Y1.LT.ZMNVD(M)   .OR. Y1.GT.ZMXVD(M)  )GO TO 110
               IF(X2.LT.XMNVD(N,M) .OR. X2.GT.XMXVD(N,M))GO TO 110
               IF(Y2.LT.ZMNVD(M)   .OR. Y2.GT.ZMXVD(M)  )GO TO 110
               CALL LINE(X1,Y1,X2,Y2)
  110       CONTINUE
         END IF
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

         IF(VADTYPE(M).EQ.'VAD')THEN
            WRITE(LABX,201)IXNAM(N)
         ELSE IF(VADTYPE(M).EQ.'COV')THEN
            WRITE(LABX,201)JXNAM(N)
         END IF
  201    FORMAT(A7)

         XP=XRT(N)-0.5*SIDEX
         YP=YTP(N)-SIDEY-YOFF
         CALL PLCHMQ (XP, YP, LABX, 12.0, 0.0, 0.0)
         IF(N.EQ.1.OR.N.EQ.5)THEN
            XP=XRT(N)-SIDEX-XOFF1
            YP=YTP(N)-0.5*SIDEY
            CALL PLCHMQ (XP, YP, LABZ, 12.0, 90.0, 0.0)
         END IF
         IF(N.EQ.1)THEN
            XP=XRT(N)-SIDEX
            YP=YTP(N)+0.015
            CALL PLCHMQ (XP, YP, LABL, 10.0, 0.0, -1.0)
         END IF
  300 CONTINUE

      FBT=YTP(6)-SIDEY
C      CALL LABEL2(VADTYPE(M),PTOT,PBAR,PSTD,SRMN,SRMX,SAMN,SAMX,
C     X            ZMNVD(M),ZMXVD(M),BDUM,IDUM,JDUM,PLTSW,NFRAME,FBT,
C     X            PMIN,PMAX,BGFLAG)

C     PLOT (U0,V0) HODOGRAPH: (X,Y)=(U0,V0)
C     DO SCATTER, LINE OR BOTH PLOTS.
C     
      X2=XRTU
      X1=XRTU-SIDE
      Y2=YTPV
      Y1=YTPV-SIDE
      CALL MAJMIN(XMNVD(1,M),XMXVD(1,M),IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(XMNVD(2,M),XMXVD(2,M),IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XMNVD(1,M),XMXVD(1,M),
     +     XMNVD(2,M),XMXVD(2,M),1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)
      
      CALL DASHDB (O'104210')
      X1=XMNVD(1,M)
      X2=XMXVD(1,M)
      Y1=XSCLVD(2,M)*XREFVD(2,M)
      IF(Y1.GT.XMNVD(2,M).AND.Y1.LT.XMXVD(2,M))THEN
         CALL LINED(X1,Y1,X2,Y1)
      END IF
      X1=XSCLVD(1,M)*XREFVD(1,M)
      Y1=XMNVD(2,M)
      Y2=XMXVD(2,M)
      IF(X1.GT.XMNVD(1,M).AND.X1.LT.XMXVD(1,M))THEN
         CALL LINED(X1,Y1,X1,Y2)
      END IF
      
C     LOOP OVER ALL RANGES WITHIN CURRENT BEAM
C     
      CALL DASHDB (O'111111')
      KK=0
      DO 400 I=IRB,IRE,ISKPVD(M)
         SRNG=R0+(I-1)*GSPC
         HRNG=SRNG*COSE
         Z=H0+SINE*SRNG+HRNG*HRNG/RE
         IZ=NINT(10.0*Z)
         
         U1=U0(I,IVD)
         V1=V0(I,IVD)
         
C     PLOT ORIGINAL VALUES
C     
         IF(U1.EQ.BDVAL.OR.V1.EQ.BDVAL)GO TO 400
         IF(U1.LT.XMNVD(1,M).OR.U1.GT.XMXVD(1,M))GO TO 400
         IF(V1.LT.XMNVD(2,M).OR.V1.GT.XMXVD(2,M))GO TO 400
         WRITE(LABH,315)IZ
 315     FORMAT(I2)
         IF(I.EQ.IRB)THEN
            CALL PLCHMQ(U1,V1,'S',10.0,0.,1.0)
         ELSE
            CALL PLCHMQ(U1,V1,'X',2.,0.,0.)
         END IF
c         IF(MOD(IZ,5).EQ.0)THEN
c            CALL PLCHMQ(U1,V1,LABH,6.0,0.,1.0)
c         ELSE
c            CALL PLCHMQ(U1,V1,'X',1.,0.,0.)
c         END IF
c         IF(I.LT.IRE)THEN
c            U2=U0(I+1,IVD)
c            V2=V0(I+1,IVD)
c            IF(U2.EQ.BDVAL.OR.V2.EQ.BDVAL)GO TO 400
c            IF(U2.LT.XMNVD(1,M).OR.U2.GT.XMXVD(1,M))GO TO 400
c            IF(V2.LT.XMNVD(2,M).OR.V2.GT.XMXVD(2,M))GO TO 400
c            CALL LINED(U1,V1,U2,V2)
c         END IF
         
C     FILTER VALUES AS PASS THROUGH RANGE GATES
C     
         IF(WFILT(M).GT.0.0)THEN
            IF(I.GE.(IRB+J2) .AND. I.LE.(IRE-J2))THEN
               KK=KK+1
               TMP2(KK,1)=BDVAL
               TMP2(KK,2)=BDVAL
               WTSUM1=0.0
               CNT1=0.0
               SUMWT1=0.0
               WTSUM2=0.0
               CNT2=0.0
               SUMWT2=0.0
               DO 390 J=J1,J2
                  JJ=I+J
                  WT=(WFILT(M)+1.0)-ABS(FLOAT(J))
                  VAD1=U0(JJ,IVD)
                  VAD2=V0(JJ,IVD)
                  IF(VAD1.EQ.BDVAL.OR.VAD2.EQ.BDVAL)GO TO 390
                  WTSUM1=WTSUM1+WT*VAD1
                  SUMWT1=SUMWT1+WT
                  CNT1=CNT1+1.0
                  WTSUM2=WTSUM2+WT*VAD2
                  SUMWT2=SUMWT2+WT
                  CNT2=CNT2+1.0
 390           CONTINUE
               IF(CNT1.GE.3.0.AND.SUMWT1.GT.0.0)TMP2(KK,1)=WTSUM1/SUMWT1
               IF(CNT2.GE.3.0.AND.SUMWT2.GT.0.0)TMP2(KK,2)=WTSUM2/SUMWT2
            END IF
         END IF
 400  CONTINUE
      
C     PLOT FILTERED VALUES
C     
      FRST=.TRUE.
      IF( WFILT(M).GT.0.0 .AND. KK.GT.1 .AND.
     +     (TYPVD(1,M).EQ.'LINE' .OR. TYPVD(1,M).EQ.'BOTH'))THEN
         DO 410 K=2,KK
            U1=TMP2(K,1)
            U2=TMP2(K-1,1)
            V1=TMP2(K,2)
            V2=TMP2(K-1,2)
            IF(U1.EQ.BDVAL.OR.V1.EQ.BDVAL)GO TO 410
            IF(U2.EQ.BDVAL.OR.V2.EQ.BDVAL)GO TO 410
            IF(U1.LT.XMNVD(1,M) .OR. U1.GT.XMXVD(1,M))GO TO 410
            IF(V1.LT.XMNVD(2,M) .OR. V1.GT.XMXVD(2,M))GO TO 410
            IF(U2.LT.XMNVD(1,M) .OR. U2.GT.XMXVD(1,M))GO TO 410
            IF(V2.LT.XMNVD(2,M) .OR. V2.GT.XMXVD(2,M))GO TO 410
            CALL LINE(U1,V1,U2,V2)
            IF(FRST)THEN
               CALL PLCHMQ(U2,V2,'X',12.0,0.,0.0)
               FRST=.FALSE.
            END IF
 410     CONTINUE
      END IF
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      
      WRITE(LABX,411)IXNAM(1)
      WRITE(LABY,411)IXNAM(2)
 411  FORMAT(A7)
      
      XP=XRTU-0.5*SIDE
      YP=YTPV-SIDE-YOFF
      CALL PLCHMQ (XP, YP, LABX, 12.0, 0.0, 0.0)

      XP=XRTU-SIDE-XOFF2
      YP=YTPV-0.5*SIDE
      CALL PLCHMQ (XP, YP, LABY, 12.0, 90.0, 0.0)
      
      FBT=YTPV-SIDE
      CALL LABEL2(VADTYPE(M),PTOT,PBAR,PSTD,SRMN,SRMX,SAMN,SAMX,
     X     ZMNVD(M),ZMXVD(M),BDUM,IDUM,JDUM,PLTSW,NFRAME,FBT,
     X     PMIN,PMAX,CCF,STDERR,CO,C1,LABLS,BGFLAG)

      M=M+1
      IF(M.LE.JVD)GO TO 10

      RETURN
      END

