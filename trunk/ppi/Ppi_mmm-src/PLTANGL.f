c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTANGL(NAMFLD,NFLDS,PAMN,PAMX,PRMN,PRMX,RSKIP,IANAM,
     X                   AFMN,AFMX,AREF,APROC,ACNT,AGAP,AERR,APRNT,NAP,
     X                   COLRFIL,PLTSW,NFRAME,H0,IFLD)
C
C  PLOT SEVERAL RANGES AS A FUNCTION OF ANGLE WITHIN THE CURRENT SWEEP
C
C     NAP       - NUMBER OF FIELDS PER FRAME
C     IANAM     - NAMES   "    "
C     AFMN,AFMX - MINIMUM AND MAXIMUM FIELD VALUES FOR PLOT BOUNDARIES
C     AREF      - DASHED REFERENCE LINE ON PLOTS
C     APROC     - NAME FOR SPECIAL PROCESSING (E.G., VAD)
C     APRNT     - PRINT THIS FIELD IF APRNT='PRNT'
C     ACNT      - MINIMUM NUMBER OF GOOD DATA POINTS FOR VAD ANALYSIS
C     AGAP      - MAXIMUM ALLOWED AZIMUTH GAP         "   "      "
C     AERR      -    "       "    RMS DIFFERENCE BETWEEN INPUT AND VAD WINDS
C     IRTYPE    - TYPE OF PLOT (RNGE OR ANGL)
C     PAMN,PAMX - MINIMUM AND MAXIMUM ANGLES (DEG) FOR PLOT BOUNDARIES
C     PRMN,PRMX -    "     "     "    RANGES (KM) TO BE PLOTTED
C     RSKIP     - RANGE SKIPPING FACTOR WITHIN THE SWEEP
C     NANG      - NUMBER OF ANGLES IN THE CURRENT SWEEP
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'colors.inc'
      PARAMETER (MXK=7)

      CHARACTER*8 NAMFLD(MXF),IANAM(MXF),APROC(MXF),APRNT(MXF)
      CHARACTER*4 IRTYPE
      CHARACTER*120 LABF
      DIMENSION AFMN(MXF),AFMX(MXF),AREF(MXF),ACNT(MXF),AGAP(MXF),
     +    AERR(MXF)
      DIMENSION AFYB(MXF),AFYT(MXF),IFLD(MXF)
      DIMENSION FLD(MXR)
      DIMENSION A(MXK),B(MXK)
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA XRT,YTP,SIDE/0.970,0.930,0.84/
      DATA KFIT/2/
      CHARACTER*6 IFMTX,IFMTY
      LOGICAL COLRFIL,PLTSW

      CHARACTER*8 AVNAM
      DATA AVNAM/'??????? '/

      IRTYPE='ANGL'
      COLRFIL=.FALSE.
      CALL SFLUSH
      CALL GSCR(1,0,0.,0.,0.)
      CALL GSCR(1,1,1.,1.,1.)
      CNTMN=15.0
      TGAP=75.0
      ERRMX=50.0

      IRB=1.001+(PRMN-R0)/DROLD
      IRE=1.001+(PRMX-R0)/DROLD
      IF(IRB.LT.1)IRB=1
      IF(IRE.GT.MXR)IRE=MXR
      SINE=SIN(TORAD*FXOLD)
      COSE=COS(TORAD*FXOLD)
      X2=XRT
      X1=XRT-SIDE
      DDX=PAMX-PAMN
      CALL MAJMIN(PAMN,PAMX,IFMTX,MJRX,MNRX,IPLX)
      ISKIP=RSKIP
      DY=SIDE/NAP

C        COMPUTE ANY AVERAGES THROUGH CURRENT SWEEP
C
      DO 10 N=1,NAP
         AFYT(N)=YTP-(N-1)*DY
         AFYB(N)=YTP-N*DY+0.02

         IFL=IFIND(IANAM(N),NAMFLD,NFLDS)
         IF(IFLD(IFL).LT.0)THEN
            ISW=2
            PLTSW=.TRUE.
         ELSE
            ISW=1
            PLTSW=.FALSE.
         END IF
C         write(*,*)'plta:',n,' ',ianam(n),namfld(ifl),ifl,ifld(ifl)
         IF(IFLD(IFL).EQ.-5.OR.IFLD(IFL).EQ.-6)THEN
            CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF

   10 CONTINUE

C     LOOP OVER ALL RANGES IN THE CURRENT SWEEP
C
c      DO 200 I=IRB,IRE,ISKIP
      DO 200 I=1,IRE,ISKIP

         RNGE=R0+(I-1)*DROLD
         ZMSL=H0+RNGE*SINE

C        LOOP OVER ALL FIELDS IN THE CURRENT RANGE
C
         DO 100 N=1,NAP
            IF(ACNT(N).NE.0.0)CNTMN=ACNT(N)
            IF(AGAP(N).NE.0.0)TGAP=AGAP(N)
            IF(AERR(N).NE.0.0)ERRMX=AERR(N)
            Y2=AFYT(N)
            Y1=AFYB(N)
            DDY=AFMX(N)-AFMN(N)
            CALL SFLUSH
            CALL GSPLCI(1)
            CALL GSTXCI(1)
            CALL MAJMIN(AFMN(N),AFMX(N),IFMTY,MJRY,MNRY,IPLY)
            CALL SET(X1,X2,Y1,Y2,PAMN,PAMX,AFMN(N),AFMX(N),1)
            CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
            CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,0,1,5,X1,Y1)
            IF(N.EQ.NAP)CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,-1,5,X1,Y1)

C           EXTRACT FIELD FOR PLOTTING AS A FUNCTION OF ANGLE
C
            IFL=IFIND(IANAM(N),NAMFLD,NFLDS)
            IF(IFLD(IFL).LT.0)THEN
               ISW=2
               PLTSW=.TRUE.
            ELSE
               ISW=1
               PLTSW=.FALSE.
            END IF
            MANG=NANG(ISW)-1

            IF(AREF(N).GT.AFMN(N).AND.AREF(N).LT.AFMX(N))THEN
C               CALL SFLUSH
C               CALL GSPLCI(IMAGENTA)
C               CALL GSTXCI(IMAGENTA)
               CALL DASHDB (O'052525')
               CALL LINED (PAMN, AREF(N), PAMX, AREF(N))
C               CALL SFLUSH
C               CALL GSPLCI(1)
C               CALL GSTXCI(1)
            END IF
            IF(APROC(N)(1:3).EQ.'VAD')THEN
               READ(APROC(N)(4:4),13)KFIT
 13            FORMAT(I1)
               IF(KFIT.EQ.0)KFIT=2
               A0=0.0
               DO K=1,MXK
                  A(K)=0.0
                  B(K)=0.0
               END DO
               CNT=0.0
               GAPMX=-999.0
               GAPMN=999.0
               ALFT=BDVAL
            END IF
            IF(APRNT(N)(1:4).EQ.'PRNT')THEN
               READ(APRNT(N)(5:8),14)IMOD,JMOD
 14            FORMAT(2I2)
               IF(IMOD.EQ.0)IMOD=9999
               IF(JMOD.EQ.0)JMOD=9999
            END IF
            DO 20 J=1,MANG
               ANG1=AZA(J,ISW)
               ANG2=AZA(J+1,ISW)
               IF(ANG1.LT.0.0)ANG1=ANG1+360.0
               IF(ANG2.LT.0.0)ANG2=ANG2+360.0
               FLD1=DAT(I,J,IFL)
               FLD2=DAT(I,J+1,IFL)
               IF(APRNT(N)(1:4).EQ.'PRNT')THEN
                  IF(MOD(I,IMOD).EQ.0 .AND. MOD(J,JMOD).EQ.0)THEN
                     WRITE(6,17)I,J,IANAM(N),RNGE,AZA(J,ISW),ELA(J,ISW),
     +                          DAT(I,J,IFL)
 17                  FORMAT(1X,'IJ=',2I5,' NAME=',A8,' RAE=',3F8.2,
     +                         ' DAT=',F8.2)
                  END IF
               END IF
               IF(FLD1.EQ.BDVAL)GO TO 20
               IF(FLD2.EQ.BDVAL)GO TO 20
               IF(ABS(ANG2-ANG1).GT.180.0)GO TO 20
               IF(ANG1.GE.PAMN    .AND. ANG1.LE.PAMX   .AND.
     +            ANG2.GE.PAMN    .AND. ANG2.LE.PAMX   .AND.
     +            FLD1.GE.AFMN(N) .AND. FLD1.LE.AFMX(N).AND.
     +            FLD2.GE.AFMN(N) .AND. FLD2.LE.AFMX(N))
C     +            CALL LINE(ANG1,FLD1,ANG2,FLD2)
     +            CALL PLCHMQ(ANG1,FLD1,'+',6.0,0.0,0.0)
               IF(APROC(N)(1:3).EQ.'VAD')THEN
                  ANGR1=TORAD*ANG1
                  A0=A0+FLD1
                  DO K=1,KFIT
                     A(K)=A(K)+FLD1*COS(ANGR1*K)
                     B(K)=B(K)+FLD1*SIN(ANGR1*K)
                  END DO
                  CNT=CNT+1.0
                  IF(ALFT.EQ.BDVAL)THEN
                     ALFT=ANG1
                  ELSE
                     GAP=ABS(ANG1-ALFT)
                     ALFT=ANG1
                     IF(GAP.GT.180.0)GAP=ABS(GAP-360.0)
                     IF(GAP.LT.GAPMN)GAPMN=GAP
                     IF(GAP.GT.GAPMX)GAPMX=GAP
                  END IF
               END IF
 20         CONTINUE

C           ADD VAD WINDS
C
            IF(APROC(N)(1:3).EQ.'VAD' .AND. RNGE.GT.0.0)THEN

               CALL SFLUSH
               IF(APROC(N)(5:6).EQ.'rr')THEN
                  CALL GSPLCI(IRED)
                  CALL GSTXCI(IRED)
               ELSE IF(APROC(N)(5:6).EQ.'gg')THEN
                  CALL GSPLCI(IGREEN)
                  CALL GSTXCI(IGREEN)
               ELSE IF(APROC(N)(5:6).EQ.'bb')THEN
                  CALL GSPLCI(IBLUE)
                  CALL GSTXCI(IBLUE)
               ELSE IF(APROC(N)(5:6).EQ.'cy')THEN
                  CALL GSPLCI(ICYAN)
                  CALL GSTXCI(ICYAN)
               ELSE IF(APROC(N)(5:6).EQ.'mg')THEN
                  CALL GSPLCI(IMAGENTA)
                  CALL GSTXCI(IMAGENTA)
               ELSE IF(APROC(N)(5:6).EQ.'yy')THEN
                  CALL GSPLCI(IYELLOW)
                  CALL GSTXCI(IYELLOW)
               ELSE
                  CALL GSPLCI(1)
                  CALL GSTXCI(1)
               END IF

               IF(CNT.GT.CNTMN .AND. GAPMX.LE.TGAP)THEN
                  A0=A0/CNT
                  DO K=1,KFIT
                     A(K)=2.0*A(K)/CNT
                     B(K)=2.0*B(K)/CNT
                  END DO
                  U0=B(1)/COSE
                  V0=A(1)/COSE
                  IF(V0.EQ.0.0.AND.U0.EQ.0.0)THEN
                     DIR=180.0
                  ELSE IF(V0.EQ.0.0.AND.U0.GT.0.0)THEN
                     DIR=90.0
                  ELSE IF(V0.EQ.0.0.AND.U0.LT.0.0)THEN
                     DIR=270.0
                  ELSE
                     DIR=TODEG*ATAN2(U0,V0)
                     IF(DIR.LT.0.0)DIR=DIR+360.0
                  END IF
                  DIR=DIR+180.0
                  IF(DIR.GE.360.0)DIR=DIR-360.0
                  SPD=SQRT(U0*U0+V0*V0)
                  DIV= 2.0*A0/(RNGE*COSE*COSE)
                  STR=-2.0*A(2)/(RNGE*COSE*COSE)
                  SHR= 2.0*B(2)/(RNGE*COSE*COSE)
                  SUMSQDIF=0.0
                  CNTDIF=0.0
                  DO 30 J=1,MANG
                     ANG1=AZA(J,ISW)
                     ANG2=AZA(J+1,ISW)
                     IF(ANG1.LT.0.0)ANG1=ANG1+360.0
                     IF(ANG2.LT.0.0)ANG2=ANG2+360.0
                     IF(ABS(ANG2-ANG1).GT.180.0)GO TO 30
                     ANGR1=TORAD*ANG1
                     ANGR2=TORAD*ANG2
                     FLD1=A0
                     FLD2=A0
                     DO K=1,KFIT 
                        FLD1=FLD1+A(K)*COS(ANGR1*K)+B(K)*SIN(ANGR1*K)
                        FLD2=FLD2+A(K)*COS(ANGR2*K)+B(K)*SIN(ANGR2*K)
                     END DO
                     IF(ANG1.GE.PAMN    .AND. ANG1.LE.PAMX   .AND.
     +                  ANG2.GE.PAMN    .AND. ANG2.LE.PAMX   .AND.
     +                  FLD1.GE.AFMN(N) .AND. FLD1.LE.AFMX(N).AND.
     +                  FLD2.GE.AFMN(N) .AND. FLD2.LE.AFMX(N))THEN
                        CALL LINE(ANG1,FLD1,ANG2,FLD2)
                     END IF
                     VR_INP=DAT(I,J,IFL)
                     VR_VAD=FLD1
                     IF(VR_INP.NE.BDVAL)THEN
                        CNTDIF=CNTDIF+1.0
                        SUMSQDIF=SUMSQDIF+(VR_INP-VR_VAD)**2
                     END IF
   30             CONTINUE
                  CALL DASHDB (O'052525')
                  CALL LINED (DIR,AFMN(N),DIR,AFMX(N))
                  RDIR=DIR+180.0
                  IF(RDIR.GT.360.0)RDIR=RDIR-360.0
                  CALL LINED (RDIR,AFMN(N),RDIR,AFMX(N))
C                  CALL LINE (PAMN, A0, PAMX, A0)

               END IF

               IF(CNTDIF.GT.CNTMN)THEN
                  RMSDIF=SQRT(SUMSQDIF/CNTDIF)
                  ERR=RMSDIF
               END IF
               CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
               CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
               IF(CNT.GT.CNTMN .AND. GAPMX.LE.TGAP)THEN
                  WRITE(LABF,31)NINT(U0),NINT(V0),NINT(SPD),NINT(DIR),
     +                 1000*NINT(DIV),1000*NINT(STR),1000*NINT(SHR),
     +                 NINT(CNT),NINT(GAPMX),NINT(ERR)
 31               FORMAT('[U,V,SPD,DIR,DIV,STR,SHR]=[',6(I3,','),I3,']',
     +                   '--[CNT,GAP,ERR]=[',2(I3,','),I3,']')
               ELSE
                  IF(ABS(GAPMX).GT.360.0)GAPMX=360.0
                  WRITE(LABF,33)NINT(CNT),NINT(GAPMX),NINT(ERR)
 33               FORMAT('[CNT,GAP,ERR]=[',2(I3,','),I3,']')
               END IF
               XP=X1
               YP=Y2+0.01
               CALL PLCHMQ (XP,YP,LABF,10.0,0.0,-1.0)
               CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)                  

               CALL SFLUSH
               CALL GSPLCI(1)
               CALL GSTXCI(1)
            END IF

 100     CONTINUE

         CALL LABEL1(IRTYPE,AFYB,AFYT,IANAM,NAP,I,NFRAME,
     x               XRT,YTP,SIDE)
 200  CONTINUE

C     RESTORE ANY AVERAGES
C
      DO 220 N=1,NAP
         IFL=IFIND(IANAM(N),NAMFLD,NFLDS)
         IF(IFLD(IFL).LT.0)THEN
            ISW=2
            PLTSW=.TRUE.
         ELSE
            ISW=1
            PLTSW=.FALSE.
         END IF
C         write(*,*)'plta:',n,' ',ianam(n),namfld(ifl),ifl,ifld(ifl)
         IF(IFLD(IFL).EQ.-5.OR.IFLD(IFL).EQ.-6)THEN
            CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,IRB,IRE,NANG,
     X           NAMFLD,IFLD,IFL,AVNAM)
         END IF
 220  CONTINUE

      RETURN
      END
