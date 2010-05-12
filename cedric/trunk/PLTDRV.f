      SUBROUTINE PLTDRV(KRD,IBUF,RBUF,WBUF,IFLASH,MXFLSH,IPR,
     X     XSTA,YSTA,ZSTA,NNET,SMRK,NMRK,NET,IMRK,LATLON)
C
C     ESTABLISHES PARAMETERIZATION AND GENERATES PLOTTED DISPLAYS
C
C     Note:  Added trap to prevent station plots when the fixed axis is not
C            Z (IFIXAX=3); otherwise, PLTSTA was being called with VALLEV(L)
C            where L exceeded MAXZLEV.  LJM 5/10/1999.
C     September 2, 1999 (LJM) - pick up correlation slope from input.  
C            If ISLOPE = (-1) a negative slope 1:1 line is drawn in SCTDSP
C                        (+1) " positive   "    "    "   "   "    "    "
C     Plot types: [IPLTYP(1-10) and ITYP from IFINDC]
C        (1) VEctors, (2) SCatter, (3) COntour, (4) ALphanumeric, 
C        (5) DIgital, (6) OVerlay, (7) AIrtrck, (8) STaloc, 
C        (9) NLdloc, (10) LMaloc
C     
C     Coordinate arrays:
C
C        (XMIN,XMAX,XDEL) = (CSP(1,1),CSP(2,1),CSP(3,1),NCX(1)]
C        (YMIN,YMAX,YDEL) = (CSP(1,2),CSP(2,2),CSP(3,2),NCX(2)]
C        (ZMIN,ZMAX,ZDEL) = (CSP(1,3),CSP(2,3),CSP(3,3),NCX(3)]
C
C        (XMIN,XMAX) = (PWIND(1,1),PWIND(2,1)]
C        (YMIN,YMAX) = (PWIND(1,2),PWIND(2,2)]
C        (ZMIN,ZMAX) = (PWIND(1,3),PWIND(2,3)]
C
      PARAMETER (NFMAX=25,NID=510,MAXZLEV=128)
      PARAMETER (MXVSCT=20,MXPLTS=3*MXVSCT+3*NFMAX)
      PARAMETER (NDSYM=27,MXL=20000)
      PARAMETER (MAXLEV=61,NPLTYPS=10)
      COMMON /GUI/ IFATAL,IGUISTAT
      COMMON /SYMTAB/ ISYM(NDSYM,2)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /AIRTRCK/ XACT(MXL),YACT(MXL),ZACT(MXL),BEGACT,DELACT,
     X                 NACT,DTAC,UACT(MXL),VACT(MXL),WACT(MXL),IACTWND,
     X                 AIRTHK
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /CONTUR/ CLB(MAXLEV,NFMAX),NLB(NFMAX),ICSPCS(10,NFMAX)
      COMMON /DSPECS/ IWIND(2,3),PWIND(2,3),NCWORD(3),IFLDW(NFMAX),
     X                SCLFAC(NFMAX),NSYMCD(2,NFMAX),WIDCD(NFMAX),
     X                ZREFCD(NFMAX),THKLIN(NFMAX)
      COMMON /VCTSCT/ IFLVEC(2,MXVSCT),IFLSCT(2,MXVSCT),
     X                VECTIN(6,MXVSCT),SCATIN(6,MXVSCT)
      COMMON /OVRLAY/ IOLAY,NPOLAY(5,MXVSCT),ICOLAY(10)
      COMMON /AXSTRB/ AXSFAC(3)
      COMMON /PLTORD/ IPLOTTP(MXPLTS),IPLTFLD(MXPLTS),NPLTS,IPLTFLG,
     X     IPLTCUR

C     ARRAYS/VARIABLES FOR STATION PLOTTING
      PARAMETER(MXK=1000,MXNET=20)
      DIMENSION XSTA(MXK),YSTA(MXK),ZSTA(MXK),NNET(MXNET)
      CHARACTER*6 SMRK(MXNET)
      CHARACTER*7 NMRK(MXK)
      LOGICAL   LATLON
      DIMENSION MAXSYM(2)

C     Common blocks for NLDN and LMA datasets
C        MXNL   - NLD CG lightning strikes (100000)
C        MXLM   - LMA lightning channel (1000000)
C
      PARAMETER (MXNL=100000,MXLM=1000000)
      CHARACTER*8 NLDOPS,LMAOPS
      COMMON/NLDN/XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL),
     X     INLD,DTNLD,DZNLD,THKNLD

      DOUBLE PRECISION TLMA
      COMMON/LMA/XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM),TLMA(MXLM),
     X     ILMA,DTLMA,DZLMA

      DIMENSION IBUF(1),RBUF(65537),IFLASH(MXFLSH),
     X          IFWAD(NFMAX,2),SCLAD(NFMAX,2),
     X          ISPAD(NFMAX,2),NDGDSP(NFMAX),NPAD(2),WBUF(1),
     X          IVSOTB(MXVSCT,3)
      DIMENSION ISLOPE(MXVSCT)
      INTEGER OVLYFLG(MXVSCT)
      CHARACTER*(*) KRD(10)
      CHARACTER*1 ISYM,LF,IASCHK,IWOP,IDSHP(5),LETTRD
      CHARACTER*1 TCHAR1,TCHAR2,IVPTYP,IAROW,IDOTBL,ISLASH
      CHARACTER*1 AST1,AST2,SCATCHAR(MXVSCT)
      CHARACTER*2 NAME(4,2),IPLTYP(NPLTYPS),IPLIN
      CHARACTER*3 ITEST
      CHARACTER*8 IUSPAC,IZSFAC,NCOLORS(10),NCCLRS(6),ITIT(5),ICOLOR
      CHARACTER*8 NAMIN,ICOL1,ICOL2,ISKP,AIROPS,CMARKS
      DATA NCOLORS / 'WHITE  ', 'GREY   ', 'PURPLE ', 'BLUE   ', 
     +               'GREEN  ', 'YELLOW ', 'BROWN  ', 'MAGENTA', 
     +               'RED     ', 'BLACK' /
      DATA NCCLRS/ 'WHITE','BLACK','GREY','BLUE','YELLOW','GREYS'/
      DATA IPLTYP/'VE','SC','CO','AL','DI','OV','AI','ST','NL','LM'/
      DATA IDSHP/'S','D','L','N','M'/
      DATA AXSFAC/1.0,1.0,1.0/
      DATA LETTRD/'D'/
      DATA MAXSYM/ 26, 26 /
      DATA EPS/0.001/

C     Default line thickness (THKMAP*ILW, see CEDRIC.f) for political maps
C
      DATA THKMAP/1.0/

C     Convert Cedric volume header times (HHMMSS) to seconds:
C        BSEC, TMV, ESEC - beginning, middle, ending seconds
C        BTIME, ETIME - both are set to the middle time (TMV)
C
      BSEC=ID(119)*3600. + ID(120)*60. + ID(121)
      ESEC=ID(125)*3600. + ID(126)*60. + ID(127)
      IF (ESEC.LT.BSEC) ESEC = ESEC + 3600.*24
      TMV=(BSEC+ESEC)/2.
      BTIME=TMV
      IF (BTIME.LT.0) BTIME = BTIME + 3600.*24
      ETIME=TMV
      IF (ETIME.LT.BTIME) ETIME = ETIME + 3600.*24
      PRINT *,'    Beg: hhmmss=',id(119),id(120),id(121),' sec=',bsec
      PRINT *,'    End: hhmmss=',id(125),id(126),id(127),' sec=',esec
C
C        INITIALIZE PLOTTED DISPLAY PARAMETERIZATION
C
      THKMAP =AIRTHK
      IF(AIRTHK.LE.0.0)THEN
         THKMAP=1.0
      ELSE
         THKMAP=AIRTHK
      END IF

      NPLTS  =0
      IPLTFLG=1
      IGREY  =0
      NVECTP =0
      NSCATP =0
      NOLAYP =0

C     Plotting flags: (0) not defined, (1) defined
C
      IAIR   =0
      ISTA   =0
      JNLD   =0
      JLMA   =0

      IAIRDEF=0
      ISTADEF=0
      INLDDEF=0
      ILMADEF=0

      LABFLG =9

      CALL CONFLDR(THKLIN,NFMAX,1.0)
c      print *,'PLTDRV: krd=',krd
c      print *,'PLTDRV:   iskp=',iskp
c      print *,'PLTDRV:     lf=',lf
c      print *,'PLTDRV: iaschk=',iaschk
c      print *,'PLTDRV: izsfac=',izsfac
c      print *,'PLTDRV:   rskp=',rskp
c      print *,'PLTDRV:   itit=',itit
c      print *,'PLTDRV:   iwop=',iwop
      READ (KRD,100)ISKP,LF,IASCHK,IZSFAC,RSKP,ITIT,IWOP
 100  FORMAT(/A8/2A1,A6/F8.0/A8/A8/A8/A8/A8/A1)
c      print *,'PLTDRV:   iskp=',iskp
c      print *,'PLTDRV:     lf=',lf
c      print *,'PLTDRV: iaschk=',iaschk
c      print *,'PLTDRV: izsfac=',izsfac
c      print *,'PLTDRV:   rskp=',rskp
c      print *,'PLTDRV:   itit=',itit
c      print *,'PLTDRV:   iwop=',iwop

      LSKP=MAX1(RSKP,1.0)
C
C     UNPACK SKIPPING FACTORS, IF PRESENT
C
      READ(ISKP,23)AST1,AST2
 23   FORMAT(2X,A1,2X,A1)
      IF (AST1.NE.'*' .OR. AST2.NE.'*') THEN
         ISKPI=1
         ISKPJ=1
         ISKPK=1
      ELSE
         READ(ISKP,33)ISKPX,ISKPY,ISKPZ
 33      FORMAT(I2,1X,I2,1X,I2)
         IF (ISKPX.LE.0) ISKPX=1
         IF (ISKPY.LE.0) ISKPY=1
         IF (ISKPZ.LE.0) ISKPZ=1
         IF (LF.EQ.'Z') THEN
            ISKPI=ISKPX
            ISKPJ=ISKPY
            ISKPK=ISKPZ
         ELSE IF (LF.EQ.'Y') THEN
            ISKPI=ISKPX
            ISKPJ=ISKPZ
            ISKPK=ISKPY
         ELSE
            ISKPI=ISKPY
            ISKPJ=ISKPZ
            ISKPK=ISKPX
         END IF
         LSKP=ISKPK
C
C     OUTPUT SKIPPING INFORMATION
C
         WRITE(*,43)ISKPX,ISKPY,ISKPZ
 43      FORMAT(/,5X,'SKIPPING FACTORS APPLIED TO GRID FOR GRAPHICS...',
     X          /,5X,'IN X-DIRECTION:',I2,
     X          /,5X,'IN Y-DIRECTION:',I2,
     X          /,5X,'IN Z-DIRECTION:',I2,/)
      END IF
            
            
      CALL WINSET(IWIND,PWIND,IWOP)
      print *,'PWIND(i,1)=',(pwind(i,1),i=1,2)
      print *,'PWIND(i,2)=',(pwind(i,2),i=1,2)
      print *,'PWIND(i,3)=',(pwind(i,3),i=1,2)
      CALL SETAXS(NCWORD,LF,IPR)
      AXSFAC(3)=1.0
      IF(NCWORD(3).NE.3.AND.IASCHK.EQ.'*') THEN
C
C        SET Z-AXIS STRETCHING FACTOR
C
         READ (IZSFAC,118)ZSFAC
  118    FORMAT(F6.0)
         IF(ZSFAC.GE.0.5.AND.ZSFAC.LE.25.0) AXSFAC(3)=ZSFAC
         WRITE(IPR,119) AXSFAC(3)
  119    FORMAT(' PLOTTED Z-AXIS STRETCH FACTOR ',F6.2/)
      END IF
C
C        ESTABLISH FIELDS, PLOT TYPES AND CONTOUR LEVELS
C
      CALL CONFLD(IVSOTB,3*MXVSCT,0.0)
      CALL CONFLD(IFLDW,NFMAX,0.0)
      CALL CONFLD(IFWAD(1,1),NFMAX,0.0)
      CALL CONFLD(IFWAD(1,2),NFMAX,0.0)
C
C     INITIALIZE VECTOR SCALING FACTOR TO A BOGUS VALUE
C
      OMS=-999.9
    5 CONTINUE
C
C        READ ANOTHER FIELD CARD WITHIN GRAPHICS STACK
C
      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      READ (KRD,101)ITEST,IPLIN,TCHAR1
C  101 FORMAT(A3,5X,A2,5X,A1)
 101  FORMAT(A3/A2,5X,A1)

      IF(ITEST.EQ.'END') GO TO 50

      IF(IPLIN.EQ.'LA') THEN
C
C     ADJUST LABELING
C
         READ(KRD(3),727)FLGLAB
 727     FORMAT(F8.0)
         LABFLG=FLGLAB
         IF (LABFLG.LT.0 .OR. LABFLG.GT.9) LABFLG=9
         GOTO 5
      END IF

C     Set ITYP - number for type of plot IPLTYP
C         VE, SC, CO, AL, DI, OV, AI, ST, NL, LM
C
      ITYP=IFINDC(IPLIN,IPLTYP,NPLTYPS,0)
      IF(ITYP.EQ.0) THEN
         CALL CEDERX(540,0)
         GO TO 5
      END IF
      IDEFMD=1
      IF(TCHAR1.EQ.LETTRD) THEN
C
C        PLOT DEFINITION ONLY- NO PLOT PRODUCED
C
         IDEFMD=-1
         WRITE(IPR,107)
  107    FORMAT(6X,'...PLOT WILL BE DEFINED BUT NOT GENERATED.')
         IF(ITYP.EQ.2.OR.ITYP.EQ.6) GO TO 5
      END IF

C     Branch according to plot type (IPLTYP) index (ITYP)
C            VE,SC,CO,AL,DI,OV,AI,ST,NL,LM
C
      GO TO (10,20,30,31,36,37,38,40,44,48), ITYP

   10 CONTINUE
C
C        VECTOR PLOT  - Set IVSOTB
C
      IF(NVECTP.GT.MXVSCT) THEN
         CALL CEDERX(541,0)
         GO TO 5
      END IF
      NVECTP=NVECTP+1
      NPLTS =NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS) =1
      IPLTFLD(NPLTS)=NVECTP
      READ (KRD,102)((NAME(I,J),I=1,4),J=1,2),IVPTYP,
     X                   (VECTIN(I,NVECTP),I=1,3),IAROW,
     X                    VECTIN(5,NVECTP),ICOLOR
C  102 FORMAT(16X,8A2,A1,7X,3F8.0,A1,7X,F8.0)
 102  FORMAT(//4A2/4A2/A1/F8.0/F8.0/F8.0/A1,3X,F4.2/A8)
      IFLVEC(1,NVECTP)=LOCFLD(NAME(1,1),NAMF,4,NFMAX,4)
      IFLVEC(2,NVECTP)=LOCFLD(NAME(1,2),NAMF,4,NFMAX,4)
      VECTIN(6,NVECTP)=MAX(LOCFLD(ICOLOR,NCOLORS,1,10,1),1)
      IF(IFLVEC(1,NVECTP).EQ.0.OR.IFLVEC(2,NVECTP).EQ.0) THEN
         NVECTP=NVECTP-1
         NPLTS =NPLTS-1
         CALL CEDERX(507,0)
         GO TO 5
      END IF
      I=VECTIN(1,NVECTP)
      IF(IVPTYP.EQ.'S') THEN
C
C        STREAMLINE PLOT
C
      IF(I.LE.0) I=2
      VECTIN(1,NVECTP)=I
      VECTIN(2,NVECTP)=0.0
      ELSE
C
C        VECTOR PLOT
C
      IF(I.LT.-2.OR.I.EQ.0) I = -1
      VECTIN(1,NVECTP)=I
      IF(VECTIN(2,NVECTP).LE.0.0) VECTIN(2,NVECTP)=2.0
      IF(VECTIN(3,NVECTP).LE.0.0) VECTIN(3,NVECTP)=1.0
      OMSV=VECTIN(3,NVECTP)/VECTIN(2,NVECTP)
      VECTIN(4,NVECTP)=1.0
      IF(IAROW.EQ.'P')VECTIN(4,NVECTP)=2.0
      VECTIN(5,NVECTP)=BNDVAL(VECTIN(5,NVECTP),.01,5.,1.,0)
      END IF
      IVSOTB(NVECTP,2)=IDEFMD
      GO TO 5

   20 CONTINUE
C
C        SCATTER PLOT - Set flag (IVSOTB)
C
      IF(NSCATP.GE.MXVSCT) THEN
         CALL CEDERX(541,0)
         GO TO 5
      END IF
      NSCATP=NSCATP+1
      NPLTS =NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS) =2
      IPLTFLD(NPLTS)=NSCATP
      READ (KRD,103)((NAME(I,J),I=1,4),
     X       SCATIN(J*2-1,NSCATP),SCATIN(J*2,NSCATP),J=1,2),
     X       SCATIN(5,NSCATP),ROVLYFLG,SCATCHAR(NSCATP),ICOLOR
 103  FORMAT(//4A2/F8.0/F8.0/4A2/F8.0/F8.0/F4.0,F4.0/A1,A7)
      IFLSCT(1,NSCATP)=LOCFLD(NAME(1,1),NAMF,4,NFMAX,4)
      IFLSCT(2,NSCATP)=LOCFLD(NAME(1,2),NAMF,4,NFMAX,4)
      SCATIN(6,NSCATP)=MAX(LOCFLD(ICOLOR,NCOLORS,1,9,1),1)
      IF(IFLSCT(1,NSCATP).EQ.0.OR.IFLSCT(2,NSCATP).EQ.0) THEN
         NSCATP=NSCATP-1
         NPLTS =NPLTS-1
         CALL CEDERX(507,1)
         GO TO 5
      END IF
      IF(ABS(SCATIN(2,NSCATP)-SCATIN(1,NSCATP)).LE.EPS.OR.
     X   ABS(SCATIN(4,NSCATP)-SCATIN(3,NSCATP)).LE.EPS) THEN
         SCATIN(1,NSCATP)= -50.
         SCATIN(2,NSCATP)=  50.
         SCATIN(3,NSCATP)= -50.
         SCATIN(4,NSCATP)=  50.
      END IF
      ISLOPE(NSCATP)=SCATIN(5,NSCATP)
      SCATIN(5,NSCATP)=MAX1(ABS(SCATIN(5,NSCATP)),0.0)
      OVLYFLG(NSCATP)=INT(ROVLYFLG)
      IVSOTB(NSCATP,1)=IDEFMD
      GO TO 5

   30 CONTINUE
C
C        CONTOUR PLOT - Set flag (IPLTCUR)
C
      IPLTCUR=3
      CALL SETCNT(NAMF,IFLDW,IDEFMD,SCLFAC,THKLIN,KRD,NST)
      GO TO 5

   31 CONTINUE
C
C        ALPHA PLOT - Set flag (IPLTCUR)
C
      READ (KRD,104)NAMIN,SCL,IUSPAC
C  104 FORMAT(16X,A8,F8.0,32X,A8)
 104  FORMAT(//A8/F8.0/////A8)
      IF(SCL.EQ.0.0) SCL=1.0
      READ (IUSPAC,111)IDOTBL,ISLASH
  111 FORMAT(2A1)
      IMUL=1
      IF(ISLASH.EQ.'/') THEN
         READ (IUSPAC,112)SPAC
  112    FORMAT(2X,F6.0)
         IF(IDOTBL.EQ.'B') IMUL=65536
      ELSE
         READ (IUSPAC,114)SPAC
  114    FORMAT(F8.0)
      END IF
      I=SPAC
      IF(I.LT.-2.OR.I.EQ.0) I=-1
      I=I*IMUL
      IPLTCUR=4
      CALL FLDSET(NAMIN,NAMF,IFWAD(1,1),IDEFMD,NST)
      IF(NST.NE.0) THEN
         GO TO 5
      END IF
      CALL FLDSETR(NAMIN,NAMF,SCLAD(1,1),SCL,NST)
      IF (NST.NE.0) THEN
         GOTO 5
      END IF
      CALL FLDSET(NAMIN,NAMF,ISPAD(1,1),  I,NST)
      IF (NST.NE.0) THEN
         GOTO 5
      END IF
      CALL SETDCD(-1,IBUF,0,NAMF,ISYM,NDSYM,MAXSYM,KRD,NST)
      IF (NST.NE.0) THEN
         IGUISTAT=-1
      END IF
      GO TO 5

   36 CONTINUE
C
C        DIGITAL PLOT - Set flag (IPLTCUR)
C
      READ (KRD,106)NAMIN,SCL,RDIG,IUSPAC
C  106 FORMAT(16X,A8,2F8.0,A8)
 106  FORMAT(//A8/F8.0/F8.0/A8)
      IF(SCL.EQ.0.0) SCL=1.0
      READ (IUSPAC,111)IDOTBL,ISLASH
      IMUL=1
      IF(ISLASH.EQ.'/') THEN
         READ (IUSPAC,112)SPAC
         IF(IDOTBL.EQ.'B') IMUL=65536
      ELSE
         READ (IUSPAC,114)SPAC
      END IF
      I=SPAC
      IF(I.LT.-2.OR.I.EQ.0) I=-1
      I=I*IMUL
      NDIG=RDIG
      IF(NDIG.LE.0.OR.NDIG.GT.8) NDIG=3
      IPLTCUR=5
      CALL FLDSET(NAMIN,NAMF,IFWAD(1,2),IDEFMD,NST)
      IF(NST.NE.0) THEN
         GO TO 5
      END IF
      CALL FLDSETR(NAMIN,NAMF,SCLAD(1,2),SCL,NST)
      IF (NST.NE.0) THEN
         GOTO 5
      END IF
      CALL FLDSET(NAMIN,NAMF,ISPAD(1,2),  I,NST)
      IF (NST.NE.0) THEN
         GOTO 5
      END IF
      CALL FLDSET(NAMIN,NAMF,NDGDSP,NDIG,NST)
      IF (NST.NE.0) THEN
         IGUISTAT=-1
      END IF
      GO TO 5

   37 CONTINUE
C
C        OVERLAY PLOT - Set flag (IVSOTB)
C
      IF(NOLAYP.GE.MXVSCT) THEN
         CALL CEDERX(541,0)
         GO TO 5
      END IF
      NOLAYP=NOLAYP+1
      NPLTS =NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS) =6
      IPLTFLD(NPLTS)=NOLAYP
      READ (KRD,108)((NAME(I,J),I=1,4),J=1,2),IPLIN,TCHAR1,TCHAR2,
     X              ICOL1,ICOL2
C  108 FORMAT(16X,8A2,A2,6X,A1,7X,A1)
 108  FORMAT(//4A2/4A2/A2/A1/A1/A8/A8)
      NPOLAY(1,NOLAYP)=LOCFLD(NAME(1,1),NAMF,4,NFMAX,4)
      NPOLAY(2,NOLAYP)=LOCFLD(NAME(1,2),NAMF,4,NFMAX,4)
      IF(NPOLAY(1,NOLAYP).EQ.0.OR.NPOLAY(2,NOLAYP).EQ.0) THEN
         NOLAYP=NOLAYP-1
         NPLTS =NPLTS-1
         CALL CEDERX(507,0)
         GO TO 5
      END IF
      NPOLAY(3,NOLAYP)=IFINDC(IPLIN,IPLTYP(4),2,0)
      NPOLAY(4,NOLAYP)=MAX0(1,IFINDC(TCHAR1,IDSHP,5,0)) -7
      NPOLAY(5,NOLAYP)=IFINDC(TCHAR2,IDSHP,5,0) -7
      IF(NPOLAY(5,NOLAYP).LT.-6) NPOLAY(5,NOLAYP)= -6
      IOCOL1=LOCFLD(ICOL1,NCCLRS,1,6,1)
      IOCOL2=LOCFLD(ICOL2,NCCLRS,1,6,1)
      IF (IOCOL1.EQ.6) IOCOL1=3
      IF (IOCOL2.EQ.6) IOCOL2=3

      IF (IOCOL1.LT.1) IOCOL1=1
      IF (IOCOL2.LT.1) IOCOL2=1
C
C     ENCODE THE REQUESTED DASHED PATTERN AND THE COLOR INTO ONE NUMBER.
C     NUMBER IS DECODED IN SUBROUTINE CONTHALF.
C
      NPOLAY(4,NOLAYP)=ABS(NPOLAY(4,NOLAYP))
      NPOLAY(5,NOLAYP)=ABS(NPOLAY(5,NOLAYP))
      NPOLAY(4,NOLAYP)=-((NPOLAY(4,NOLAYP)**NPOLAY(4,NOLAYP))+IOCOL1)
      NPOLAY(5,NOLAYP)=-((NPOLAY(5,NOLAYP)**NPOLAY(5,NOLAYP))+IOCOL2)
      IVSOTB(NOLAYP,3)=IDEFMD
      GO TO 5

 38   CONTINUE
C
C        AIRCRAFT TRACK PLOT - Set flag (IAIR)
C
      IAIR=1
      READ (KRD,138) AIROPS,CMARKS,REFVMS,REFVKM,VFREQ,ICOLOR
 138  FORMAT(////A8/A8/F8.0/F8.0/F8.0/A8)
      READ(CMARKS,123)TMAJOR,TMINOR
 123  FORMAT(F4.0,F4.0)
      IF (TMAJOR.LT.DELACT .AND. TMAJOR.NE.0.0) TMAJOR=DELACT*2.
      IF (TMINOR.LT.DELACT .AND. TMINOR.NE.0.0) TMINOR=DELACT
      IF (INT((TMAJOR/DELACT)+EPS).NE.(INT(TMAJOR*100)/INT(DELACT*100)))
     X     THEN
         WRITE(*,117)TMAJOR,DELACT
 117     FORMAT(/,5X,'+++MAJOR TICK MARKS MUST OCCUR AT INTEGER',
     X        ' MULTIPLES OF DATA RESOLUTION.+++',/5X,'   MAJOR=',F4.0,
     X        ' DATA RESOLUTION=',F4.0)
         IGUISTAT=-1
         GOTO 5
      END IF
      IF (INT((TMINOR/DELACT)+EPS).NE.(INT(TMINOR*100)/INT(DELACT*100)))
     X     THEN
         WRITE(*,120)TMINOR,DELACT
 120     FORMAT(/,5X,'+++MINOR TICK MARKS MUST OCCUR AT INTEGER',
     X        ' MULTIPLES OF DATA RESOLUTION.+++',/5X,'   MINOR=',F4.0,
     X        ' DATA RESOLUTION=',F4.0)
         IGUISTAT=-1
         GOTO 5
      END IF
      IF (VFREQ.LT.DELACT .AND. VFREQ.NE.0.0) VFREQ=DELACT
      IF (REAL(INT(VFREQ/DELACT)).NE.(VFREQ/DELACT)) THEN
         WRITE(*,122)VFREQ,DELACT
 122     FORMAT(/,5X,'+++VECTOR PLOTTING FREQUENCY MUST BE AN INTEGER',
     X        ' MULTIPLE OF DATA RESOLUTION.+++',/5X,'   VFREQ=',F4.0,
     X        ' DATA RESOLUTION=',F4.0)
         IGUISTAT=-1
         GOTO 5
      END IF
      IF (REFVMS.NE.0) REFV=REFVKM/REFVMS
      IF (REFV.LE.0.0) REFV=0.5
      NPLTS=NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS)=7
      IAIRDEF=IDEFMD
      IAIRCOL=MAX(LOCFLD(ICOLOR,NCOLORS,1,10,1),1)
      IBEGCNT=1
      IENDCNT=NACT
      GOTO 5

 40   CONTINUE
C
C        STATION LOCATION PLOT  - Set flag (ISTA)
C
      ISTA=1
      READ (KRD,41)SYMMRK
 41   FORMAT(//F8.0)
      IF (SYMMRK.GT.9.0 .OR. SYMMRK.LT.1.0) SYMMRK=3.0
      NPLTS=NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS)=8
      ISTADEF=IDEFMD
      GOTO 5

 44   CONTINUE
C
C        NLDN LOCATION PLOT  - Set flag (JNLD)
C
      READ (KRD,45)DTNLD,NLDOPS
 45   FORMAT(///F8.0/A8)
      READ(NLDOPS,46)JNLD
 46   FORMAT(I1)
      print *,'DTNLD,JNLD=',DTNLD,JNLD
      IF (JNLD.GT.1) JNLD=1
      NPLTS=NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS)=9
      INLDDEF=IDEFMD
      GOTO 5
 
 48   CONTINUE
C
C        LMA LOCATION PLOT - Set flag (JLMA)
C
      READ (KRD,49)DTLMA,LMAOPS
 49   FORMAT(///F8.0/A8)
      READ(LMAOPS,46)JLMA
      print *,'DTLMA,JLMA=',DTLMA,JLMA
      IF (JLMA.GT.1) JLMA=1
      NPLTS=NPLTS+1
      IF (NPLTS.GT.MXPLTS) THEN
         CALL CEDERX(541,1)
         NPLTS=NPLTS-1
         GOTO 5
      END IF
      IPLOTTP(NPLTS)=10
      ILMADEF=IDEFMD
      GOTO 5

 50   CONTINUE

C     *****COMPLETED READ OF THE GRAPHICS STACK*****
C
C        CHECK THAT OVERLAY PLOTS ARE COMPLETELY DEFINED
C
      IF(NOLAYP.NE.0) THEN
         DO 70 I=1,NOLAYP
            IV1=NPOLAY(1,I)
            IV2=NPOLAY(2,I)
            ITYP=NPOLAY(3,I)
            IF(IFLDW(IV1).EQ.0) THEN
               IVSOTB(I,3)=-1
            ELSE IF(ITYP.EQ.0.AND.IFLDW(IV2).EQ.0) THEN
               IVSOTB(I,3)=-1
            ELSE IF(ITYP.GT.0) THEN
               IF(IFWAD(IV2,ITYP).EQ.0) THEN
                  IVSOTB(I,3)=-1
               END IF
            END IF
            IF(IVSOTB(I,3).EQ.-1) THEN
               WRITE(IPR,109) I
  109          FORMAT(5X,'+++  OVERLAY PLOT NO. ',I2,' CANNOT BE ',
     X                    'GENERATED-  COMPONENT PLOTS HAVE NOT BEEN ',
     X                    'SUFFICIENTLY DEFINED  +++')
               CALL CONFLD(NPOLAY(1,I),5,0.0)
            END IF
   70    CONTINUE
      END IF
C
C        PLOTS TO BE PRODUCED
C
      I1=KNTOCR(IVSOTB(1,1),NSCATP,1)
      I2=KNTOCR(IVSOTB(1,2),NVECTP,1)
      I3=KNTOCR(IFLDW,NFMAX,1)
      I4=KNTOCR(IFWAD(1,1),NFMAX,1)
      I5=KNTOCR(IFWAD(1,2),NFMAX,1)
      I6=MAX(IAIRDEF,0)
      I7=MAX(ISTADEF,0)
      I8=MAX(INLDDEF,0)
      I9=MAX(ILMADEF,0)
      NOLYGD=KNTOCR(IVSOTB(1,3),NOLAYP,1)
C
C        PLOTS DEFINED
C
      CALL PLNFLD(IFIXAX,L1,L2,NFDSP)
      NPAD(1)=I4+KNTOCR(IFWAD(1,1),NFMAX,-1)
      NPAD(2)=I5+KNTOCR(IFWAD(1,2),NFMAX,-1)
      IF(IFIXAX.NE.3)THEN
         IF(ISTA.NE.0)THEN
            WRITE(IPR,75)
 75         FORMAT(/,' +++ WARNING - ',
     X      'STATION PLOTS NOT ALLOWED FOR THIS FIXED AXIS +++ ')
            ISTA=0
            I7=0
         END IF
      END IF
C
C        PROCEED WITH DISPLAYS
C
      WRITE(IPR,105) NSCATP,I1,NVECTP,I2,NFDSP,I3,NPAD(1),I4,
     X               NPAD(2),I5,NOLAYP,NOLYGD,IAIR,I6,ISTA,I7,
     X               JNLD,I8,JLMA,I9
  105 FORMAT(/1X,I2,' SCATTER PLOTS DEFINED, ',I2,' PLOTS GENERATED'
     X       /1X,I2,' VECTOR  PLOTS DEFINED, ',I2,' PLOTS GENERATED'
     X       /1X,I2,' CONTOUR PLOTS DEFINED, ',I2,' PLOTS GENERATED'
     X       /1X,I2,' ALPHA   PLOTS DEFINED, ',I2,' PLOTS GENERATED'
     X       /1X,I2,' DIGITAL PLOTS DEFINED, ',I2,' PLOTS GENERATED'
     X       /1X,I2,' OVERLAY PLOTS DEFINED, ',I2,' PLOTS GENERATED',
     X       /1X,I2,' AIRTRCK PLOTS DEFINED, ',I2,' PLOTS GENERATED',
     X       /1X,I2,' STALOC  PLOTS DEFINED, ',I2,' PLOTS GENERATED',
     X       /1X,I2,' NLDLOC  PLOTS DEFINED, ',I2,' PLOTS GENERATED',
     X       /1X,I2,' LMALOC  PLOTS DEFINED, ',I2,' PLOTS GENERATED'/)

      IOTHER = IAIR+ISTA+JNLD+JLMA
      IF(NSCATP+NVECTP+NFDSP+NPAD(1)+NPAD(2)+IOTHER.EQ.0)GO TO 91

C     *****DO INDIVIDUAL PLOTS***** Outer by grid level; inner by plots.
C
      IVOBG=IVBGEN(NOLAYP)
      DO 85 L=L1,L2,LSKP
         DO 89 NP=1,NPLTS
            IOLAY=0

c            print *,'NP,IPLOTTP(NP)=',np,iplottp(np)
C           Branch by plot type
C                  VE, SC, CO, AL, DI, OV, AI, ST, NL, LM
            GOTO (110,222,333,444,555,666,710,720,730,740)IPLOTTP(NP)
C
C        VEctor PLOT - create FLASH buffer #1
C
 110        I=IPLTFLD(NP)
            CALL NEXACT(IFIXAX,L,IFLVEC(1,I),RLEV,NAME,2)
            CALL VECDSP(IBUF,RBUF,WBUF,L,RLEV,PWIND,NCWORD,
     X           IFLVEC(1,I),ITIT,VECTIN(1,I),IFLASH,MXFLSH,
     X           IVSOTB(I,2),NST,INT(VECTIN(6,I)),ISKPI,ISKPJ,
     X           LABFLG)
            GOTO 89
C
C     AIrcraft TRACK PLOT - create FLASH buffer #3
C
 710        CONTINUE
            IF (LF.EQ.'Y') THEN
               IF (IACTWND.NE.3 .AND. IACTWND.NE.4) THEN
                  IWND=0
               ELSE
                  IWND=1
               END IF
               CALL PLTACT(XACT,ZACT,YACT,BEGACT,DELACT,IBEGCNT,
     X              IENDCNT,((L-1)*CSP(3,2)+CSP(1,2)),AIROPS,
     X              IAIRDEF,PWIND,NCWORD,ITIT,UACT,WACT,IWND,REFV,
     X              IAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
            ELSE IF (LF.EQ.'X') THEN
               IF (IACTWND.NE.2 .AND. IACTWND.NE.4) THEN
                  IWND=0
               ELSE
                  IWND=1
               END IF
               CALL PLTACT(YACT,ZACT,XACT,BEGACT,DELACT,IBEGCNT,
     X              IENDCNT,((L-1)*CSP(3,1)+CSP(1,1)),AIROPS,
     X              IAIRDEF,PWIND,NCWORD,ITIT,VACT,WACT,IWND,REFV,
     X              IAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
            ELSE
               IF (IACTWND.NE.1 .AND. IACTWND.NE.4) THEN
                  IWND=0
               ELSE
                  IWND=1
               END IF
               CALL PLTACT(XACT,YACT,ZACT,BEGACT,DELACT,IBEGCNT,
     X              IENDCNT,VALLEV(L),AIROPS,
     X              IAIRDEF,PWIND,NCWORD,ITIT,UACT,VACT,IWND,REFV,
     X              IAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
            END IF
            GOTO 89
C
C     STATION LOCATION PLOT - create FLASH buffer #4
C                           - added ISTA test (ljm 5/10/1999)
C
 720        CONTINUE
            IF(ISTA.EQ.1)THEN
               CALL PLTSTA(NMRK,XSTA,YSTA,ZSTA,IMRK,NET,NNET,SMRK,
     X              SYMMRK,ISTADEF,LABFLG,ITIT,PWIND,NCWORD,VALLEV(L))
            END IF
            GOTO 89
C
C     NLDN LOCATION PLOT - create FLASH buffer #5
C
 730        CONTINUE
            print *,'REGULAR-NLDN: jnld,inld=',jnld,inld
            IF(JNLD.EQ.1.AND.INLD.GE.1)THEN
c               ZLEV=CSP(1,3)+(L-1)*CSP(3,3)
               ZLEV=VALLEV(L)
               IBTIME=BSEC
               IETIME=ESEC
               CALL PLTNLD(XNLD,YNLD,PNLD,TNLD,INLD,JNLD,MXNL,DTNLD,
     X              IBTIME,IETIME,OLAT,OLON,ANGXAX,CNLD,DZNLD,ZLEV,
     X              INLDDEF,THKNLD,LABFLG,ITIT,PWIND,NCWORD)
            END IF
            GOTO 89
C
C     LMA LOCATION PLOT  - create FLASH buffer #6
C
 740        CONTINUE
            IF(JLMA.EQ.1.AND.ILMA.GE.1)THEN
               ZLEV=CSP(1,3)+(L-1)*CSP(3,3)
               XMN=PWIND(1,1)
               XMX=PWIND(2,1)
               YMN=PWIND(1,2)
               YMX=PWIND(2,2)
               ZMN=PWIND(1,3)
               ZMX=PWIND(2,3)
               IBTIME=BTIME
               IETIME=ETIME
               CALL PLTLMA(XLMA,YLMA,ZLMA,TLMA,ILMA,JLMA,MXLM,DTLMA,
     X              XMN,XMX,YMN,YMX,ZMN,ZMX,IBTIME,IETIME,OLAT,OLON,
     X              ANGXAX,CLMA,DZLMA,ZLEV)
            END IF
            GOTO 89
C
C        SCATTER PLOTS 
C
 222        I=IPLTFLD(NP)
            IF(IVSOTB(I,1).NE.1) GO TO 89
            CALL NEXACT(IFIXAX,L,IFLSCT(1,I),RLEV,NAME,2)
            CALL SCTDSP(IBUF,RBUF,L,RLEV,IWIND,NCWORD,IFLSCT(1,I),
     X           ITIT,SCATIN(1,I),IFIXAX,NST,SCATCHAR(I),
     X           INT(SCATIN(6,I)),ISKPI,ISKPJ,LABFLG,ISLOPE(I),
     X           OVLYFLG(I))
            GOTO 89
C
C        CONTOUR PLOTS - also add requested overlays (P5 - options)
C
 333        IF(NFDSP.EQ.0) GO TO 89
            JFLD=IPLTFLD(NP)
            IV=MAPVID(JFLD,2)
            IF(IFLDW(IV).LT.1) GO TO 89
C
C     CREATE NEW COLOR TABLE FOR GREY SHADES
C
            IF (IFLDW(IV).EQ.2) CALL DFCLRS(1)
            IF (NVECTP.NE.0) THEN
               CALL NEXACT(IFIXAX,L,IFLVEC(1,NVECTP),RLEV,NAME,2)
C
C     SWITCH BLACK AND WHITE VECTOR COLORS, IF NECESSARY
C
               IF (IFLDW(IV).EQ.2) THEN
                  NVECCOL=INT(VECTIN(6,NVECTP))
                  IF (NVECCOL.EQ.1) THEN
                     NTVECCOL=10
                  ELSE IF (NVECCOL.EQ.10) THEN
                     NTVECCOL=1
                  ELSE
                     NTVECCOL=NVECCOL
                  END IF
               ELSE
                  NTVECCOL=INT(VECTIN(6,NVECTP))
               END IF
               CALL VECDSP(IBUF,RBUF,WBUF,L,RLEV,PWIND,NCWORD,
     X              IFLVEC(1,NVECTP),ITIT,VECTIN(1,NVECTP),
     X              IFLASH,MXFLSH,-1,NST,NTVECCOL,ISKPI,ISKPJ,
     X              LABFLG)
            END IF
            IF (IAIR.EQ.1 .AND. (((ICSPCS(7,IV).EQ.2 .OR.
     X           ICSPCS(7,IV).EQ.3 .OR. ICSPCS(7,IV).EQ.6 .OR.
     X           ICSPCS(7,IV).EQ.7).AND.IOLAY.EQ.0))) THEN
               IF (IFLDW(IV).EQ.2) THEN
                  IF (IAIRCOL.EQ.1) THEN
                     ITAIRCOL=10
                  ELSE IF (IAIRCOL.EQ.10) THEN
                     ITAIRCOL=1
                  ELSE
                     ITAIRCOL=IAIRCOL
                  END IF
               ELSE
                  ITAIRCOL=IAIRCOL
               END IF
               IF (LF.EQ.'Y') THEN
                  IF (IACTWND.NE.3 .AND. IACTWND.NE.4) THEN
                     IWND=0
                  ELSE
                     IWND=1
                  END IF
                  CALL PLTACT(XACT,ZACT,YACT,BEGACT,DELACT,IBEGCNT,
     X                 IENDCNT,((L-1)*CSP(3,2)+CSP(1,2)),AIROPS,
     X                 -1,PWIND,NCWORD,ITIT,UACT,WACT,IWND,REFV,
     X                 ITAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
               ELSE IF (LF.EQ.'X') THEN
                  IF (IACTWND.NE.2 .AND. IACTWND.NE.4) THEN
                     IWND=0
                  ELSE
                     IWND=1
                  END IF
                  CALL PLTACT(YACT,ZACT,XACT,BEGACT,DELACT,IBEGCNT,
     X                 IENDCNT,((L-1)*CSP(3,1)+CSP(1,1)),AIROPS,
     X                 -1,PWIND,NCWORD,ITIT,VACT,WACT,IWND,REFV,
     X                 ITAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
               ELSE
                  IF (IACTWND.NE.1 .AND. IACTWND.NE.4) THEN
                     IWND=0
                  ELSE
                     IWND=1
                  END IF
                  CALL PLTACT(XACT,YACT,ZACT,BEGACT,DELACT,IBEGCNT,
     X                 IENDCNT,VALLEV(L),AIROPS,
     X                 -1,PWIND,NCWORD,ITIT,UACT,VACT,IWND,REFV,
     X                 ITAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
               END IF
            END IF   
            CALL NEXACT(IFIXAX,L,IV,RLEV,NAME,1)
            CALL FETCHD(IN,ID,L,JFLD,IBUF,RBUF,N1,N2,
     X           IFIXAX,BAD,RLEV,NST)
            IF (IAIR.EQ.1 .AND. NVECTP.NE.0 .AND. ((ICSPCS(7,IV).EQ.3 
     X           .OR. ICSPCS(7,IV).EQ.7).AND. IOLAY.EQ.0)
     X           .AND. OMSV.NE.REFV .AND. AIROPS(4:4).EQ.'1') THEN
C
C     VECTOR AND AIRCRAFT OVERLAYS DON'T HAVE SAME REF. VECTORS!!
C
               WRITE(*,77)
 77            FORMAT(/,5X,' +++REFERENCE VECTORS FOR VECTOR PLOT',
     X              ' AND AIRTRCK PLOT ARE NOT THE SAME+++'/,5X,
     X              '+++AIRTRCK PLOT WILL NOT BE OVERLAYED+++')
               IAIRP=0
            ELSE
               IAIRP=IAIR
            END IF
C
C     PLOT STATION LOCATIONS, IF REQUESTED
C
            IF (ISTA.EQ.1 .AND. (ICSPCS(7,IV).EQ.4 .OR. 
     X           ICSPCS(7,IV).EQ.5 .OR. ICSPCS(7,IV).EQ.6 .OR. 
     X           ICSPCS(7,IV).EQ.7)) THEN
               CALL PLTSTA(NMRK,XSTA,YSTA,ZSTA,IMRK,NET,NNET,SMRK,
     X              SYMMRK,-1,LABFLG,ITIT,PWIND,NCWORD,VALLEV(L)) 
            END IF
C
C     PLOT NLDN LOCATIONS, IF REQUESTED
C          Pass (INLDDEF = -1) into PLTNLD to prevent NLDN labeling
C
               IF(JNLD.EQ.1 .AND. I.EQ.1)THEN
                  IOLAY=1
                  ZLEV=VALLEV(L)
                  IBTIME=BSEC
                  IETIME=ESEC
                  INLDDEF=-1
                  CALL PLTNLD(XNLD,YNLD,PNLD,TNLD,INLD,JNLD,MXNL,DTNLD,
     X                 IBTIME,IETIME,OLAT,OLON,ANGXAX,CNLD,DZNLD,ZLEV,
     X                 INLDDEF,THKNLD,LABFLG,ITIT,PWIND,NCWORD)
               END IF

            CALL PLOTCH(ID,RBUF,N1,N2,PWIND,BAD,RLEV,NAME,SCLFAC(IV),
     X           ITIT,IV,CSP,NCX,NCWORD,NVECTP,WBUF,IGREY,ISKPI,ISKPJ,
     X           IAIRP,ISTA,LABFLG,THKLIN(IV),LATLON,THKMAP)

C
C     RETURN COLOR TABLE TO STATE BEFORE GREY SHADES IF NECESSARY
C
            IF (IFLDW(IV).EQ.2) CALL DFCLRS(0)
            GOTO 89
C
C        ALphanumeric PLOTS 
C
 444        K=1
            JFLD=IPLTFLD(NP)
            IV=MAPVID(JFLD,2)
            IF(IFWAD(IV,K).NE.1) GO TO 89
            CALL NEXACT(IFIXAX,L,IV,RLEV,NAME,1)
            CALL FETCHD(IN,ID,L,JFLD,IBUF,RBUF,
     X           N1,N2,IFIXAX,BAD,RLEV,NST)
            CALL PLOTAD(ID,RBUF,N1,N2,PWIND,BAD,RLEV,NAME,
     X           SCLAD(IV,K),ISPAD(IV,K),NDGDSP(IV),
     X           NSYMCD(1,IV),WIDCD(IV),ZREFCD(IV),
     X           CSP,NCX,NCWORD,ITIT,K,ISKPI,ISKPJ,LABFLG)
            GOTO 89

C
C        DIgital PLOTS 
C
 555        K=2
            JFLD=IPLTFLD(NP)
            IV=MAPVID(JFLD,2)
            IF(IFWAD(IV,K).NE.1) GO TO 89
            CALL NEXACT(IFIXAX,L,IV,RLEV,NAME,1)
            CALL FETCHD(IN,ID,L,JFLD,IBUF,RBUF,
     X           N1,N2,IFIXAX,BAD,RLEV,NST)
            CALL PLOTAD(ID,RBUF,N1,N2,PWIND,BAD,RLEV,NAME,
     X           SCLAD(IV,K),ISPAD(IV,K),NDGDSP(IV),
     X           NSYMCD(1,IV),WIDCD(IV),ZREFCD(IV),
     X           CSP,NCX,NCWORD,ITIT,K,ISKPI,ISKPJ,LABFLG)
            GOTO 89
C
C        OVerlay PLOTS - also add requested overlays (P5 - options)
C           Any overlays are created here since OVERLAY plots are
C           scaled to be smaller than regular CONTOUR plots.
C
 666        K=IPLTFLD(NP)
            IF(IVSOTB(K,3).NE.1) GO TO 89
            DO 83 I=1,2
               IOLAY=I
               IV=NPOLAY(I,K)
               IF (I.EQ.1) THEN
                  IOT=ABS(IFLDW(IV))
               ELSE
                  IT=NPOLAY(1,K)
                  IOT=ABS(IFLDW(IT))
                  IF (IOT.EQ.2) IOT=3
               END IF
               CALL OVSETP(ITYP,K)
C
C     IF HALFTONE PLOTS ARE INVOLVED, REVERSE FOREGROUND/BACKGROUND COLORS
C
               IGREY=0
               IF (IOT.EQ.2) THEN
                  CALL  DFCLRS(1)
                  IGREY=1
               ELSE IF (IOT.EQ.3) THEN
                  IGREY=1
               END IF
C
C        GENERATE VECTOR BACKGROUND IF REQUIRED
C
               IF(IVOBG.NE.0.AND.NVECTP.NE.0 .AND. I.EQ.1) THEN
                  IOLAY=1
                  CALL NEXACT(IFIXAX,L,IFLVEC(1,NVECTP),RLEV,NAME,2)
C
C     SWITCH BLACK AND WHITE VECTOR COLORS, IF NECESSARY
C
                  IF (IGREY.EQ.1) THEN
                     NVECCOL=INT(VECTIN(6,NVECTP))
                     IF (NVECCOL.EQ.1) THEN
                        NTVECCOL=10
                     ELSE IF (NVECCOL.EQ.10) THEN
                        NTVECCOL=1
                     ELSE
                        NTVECCOL=NVECCOL
                     END IF
                  ELSE
                     NTVECCOL=INT(VECTIN(6,NVECTP))
                  END IF
                  CALL VECDSP(IBUF,RBUF,WBUF,L,RLEV,PWIND,NCWORD,
     X                 IFLVEC(1,NVECTP),ITIT,VECTIN(1,NVECTP),
     X                 IFLASH,MXFLSH,-1,NST,NTVECCOL,ISKPI,ISKPJ,
     X                 LABFLG)
               END IF
C
C              Add aircraft track to OVERLAY plot
C
               IF (IAIR.EQ.1 .AND. I.EQ.1) THEN
                  IOLAY=1
                  IF (IGREY.EQ.1) THEN
                     IF (IAIRCOL.EQ.1) THEN
                        ITAIRCOL=10
                     ELSE IF (IAIRCOL.EQ.10) THEN
                        ITAIRCOL=1
                     ELSE
                        ITAIRCOL=IAIRCOL
                     END IF
                  ELSE
                     ITAIRCOL=IAIRCOL
                  END IF
                  IF (LF.EQ.'Y') THEN
                     IF (IACTWND.NE.3 .AND. IACTWND.NE.4) THEN
                        IWND=0
                     ELSE
                        IWND=1
                     END IF
                     CALL PLTACT(XACT,ZACT,YACT,BEGACT,DELACT,IBEGCNT,
     X                    IENDCNT,((L-1)*CSP(3,2)+CSP(1,2)),AIROPS,
     X                    -1,PWIND,NCWORD,ITIT,UACT,WACT,IWND,REFV,
     X                    ITAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
                  ELSE IF (LF.EQ.'X') THEN
                     IF (IACTWND.NE.2 .AND. IACTWND.NE.4) THEN
                        IWND=0
                     ELSE
                        IWND=1
                     END IF
                     CALL PLTACT(YACT,ZACT,XACT,BEGACT,DELACT,IBEGCNT,
     X                    IENDCNT,((L-1)*CSP(3,1)+CSP(1,1)),AIROPS,
     X                    -1,PWIND,NCWORD,ITIT,VACT,WACT,IWND,REFV,
     X                    ITAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
                  ELSE
                     IF (IACTWND.NE.1 .AND. IACTWND.NE.4) THEN
                        IWND=0
                     ELSE
                        IWND=1
                     END IF
                     CALL PLTACT(XACT,YACT,ZACT,BEGACT,DELACT,IBEGCNT,
     X                    IENDCNT,VALLEV(L),AIROPS,
     X                    -1,PWIND,NCWORD,ITIT,UACT,VACT,IWND,REFV,
     X                    ITAIRCOL,LABFLG,TMAJOR,TMINOR,VFREQ,AIRTHK)
                  END IF
               END IF   
C
C              Add station locations to OVERLAY plot
C
               IF (ISTA.EQ.1 .AND. I.EQ.1) THEN
                  IOLAY=1
                  CALL PLTSTA(NMRK,XSTA,YSTA,ZSTA,IMRK,NET,NNET,SMRK,
     X                 SYMMRK,-1,LABFLG,ITIT,PWIND,NCWORD,
     X                 VALLEV(L))
               END IF
C
C              Add NLDN CG locations to OVERLAY plot
C              Pass (INLDDEF = -1) into PLTNLD to prevent NLDN labeling
C
c               print *,'OVERLAY-NLDN: jnld,inld=',jnld,inld
               IF(JNLD.EQ.1 .AND. I.EQ.1)THEN
                  IOLAY=1
c                  ZLEV=CSP(1,3)+(L-1)*CSP(3,3)
                  ZLEV=VALLEV(L)
                  IBTIME=BSEC
                  IETIME=ESEC
                  INLDDEF=-1
                  CALL PLTNLD(XNLD,YNLD,PNLD,TNLD,INLD,JNLD,MXNL,DTNLD,
     X                 IBTIME,IETIME,OLAT,OLON,ANGXAX,CNLD,DZNLD,ZLEV,
     X                 INLDDEF,THKNLD,LABFLG,ITIT,PWIND,NCWORD)
               END IF

               JFLD=MAPVID(IV,1)
               CALL NEXACT(IFIXAX,L,IV,RLEV,NAME,1)
               CALL FETCHD(IN,ID,L,JFLD,IBUF,RBUF,N1,N2,
     X              IFIXAX,BAD,RLEV,NST)
               IF(ITYP.EQ.0) THEN
C
C              CONTOUR PLOT TO BE PRODUCED
C
                  CALL PLOTCH(ID,RBUF,N1,N2,PWIND,BAD,RLEV,NAME,
     X                 SCLFAC(IV),ITIT,IV,CSP,NCX,NCWORD,NVECTP,
     X                 WBUF,IGREY,ISKPI,ISKPJ,IAIR,ISTA,LABFLG,
     X                 THKLIN(IV),LATLON,THKMAP)
               ELSE
C
C              ALPHA/DIGITAL PLOT TO BE PRODUCED
C
                  CALL PLOTAD(ID,RBUF,N1,N2,PWIND,BAD,RLEV,NAME,
     X                 SCLAD(IV,ITYP),ISPAD(IV,ITYP),NDGDSP(IV),
     X                 NSYMCD(1,IV),WIDCD(IV),ZREFCD(IV),
     X                 CSP,NCX,NCWORD,ITIT,ITYP,ISKPI,ISKPJ,LABFLG)
               END IF
 83         CONTINUE
C
C     IF HALFTONE PLOTS WERE INVOLVED, UNDO REVERSE OF FORE/BACKGROUND COLORS
C
            IF (IOT.EQ.3) CALL DFCLRS(0)
            IGREY=0
 89      CONTINUE
 85   CONTINUE

 90   CONTINUE
      IPLTFLG=0
      RETURN

 91   CONTINUE
      CALL CEDERX(543,0)
      IPLTFLG=0
      RETURN
      END


