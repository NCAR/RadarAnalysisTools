      SUBROUTINE STATS(KRD,IBUF,RBUF,IPR)
C     
C     GENERATES STATISTICAL DISPLAYS
C     
C     VARIABLE FDORD IS PRESENT TO KEEP THE ORDER OF PLOTTED SINGLE FIELD 
C     HISTOGRAMS THE SAME AS THE ORDER SPECIFIED BY THE USER. THIS ORDER
C     IS IMPORTANT FOR OVERLAYING.
C
C     August 8, 1997 (LJM) - changed to have coordinates printed to 3 decimals
C     
      
      INCLUDE 'CEDRIC.INC'
      PARAMETER (NDSYM=27,NWRK=415)
      PARAMETER (MAXBIN=1003)
      COMMON /SYMTAB/ ISYM(NDSYM,2)
      CHARACTER*1 ISYM,IDEST,LF,IPVB(3),LAX(3),IWOP,IC
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,NAME(4)
      CHARACTER*3 ITEST,ICOM
      COMMON /DSPECS/ IWIND(2,3),PWIND(2,3),NCWORD(3),IFLDW(NFMAX),
     X     SCLFAC(NFMAX),NSYMCD(2,NFMAX),WIDCD(NFMAX),
     X     ZREFCD(NFMAX),THKLIN(NFMAX)
      COMMON /HSTOPT/ IMED,ICOL,TPER,OOPT,IMEDA(NFMAX),ICOLA(NFMAX),
     X     TPERA(NFMAX),OOPTA(NFMAX),FDORD(NFMAX),ICUR,SYMCD(2,NFMAX)
      CHARACTER*8 IMED,ICOL
      COMMON /HSTBUF/ IBINS(MAXBIN),KBINS(MAXBIN)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      DIMENSION IBUF(1),RBUF(1),IDSEND(2),MAXSYM(2),CLASS(MAXBIN),
     X     SPAC(2),ISBIN(MAXBIN,MAXZLEV),
     X     KSBIN(MAXBIN),WRK(NWRK),WINDOW(4),
     X     NPPTSO(MAXZLEV)
      INTEGER DCOL(8)
      CHARACTER*(*) KRD(10)
      CHARACTER*32  TITLE
      CHARACTER*8   OOPT,OOPTA,NAMIN
      REAL MED
      DATA MAXSYM/ 50, 50 /
      DATA IPVB/'P','V','B'/
c      DATA LAX/'X', 'Y', 'Z' /
      DATA LAX/'L', 'L', 'Z' /
      DATA SPAC/1.0,-1.5/
      DATA WINDOW/0.,1.,0.,.85/
      
      
      STOP1=2.0
      RLEVO=-99.
      FAVGO=-99.
      FSTDO=-99.
      NPO=-99
      ICUR=1
      STOP=3.0-2.0
      
C     
C     DETERMINE OPERATION AND PARAMETERIZATION
C     
      READ (KRD,100)ICOM,IDEST,LF,RSKP,IWOP
C     100 FORMAT(A3,5X,A1,7X,A1,7X,F8.0,40X,A1)
 100  FORMAT(A3/A1/A1/F8.0//////A1)
      LSKP=MAX1(RSKP,1.0)
      CALL WINSET(IWIND,PWIND,IWOP)
      CALL SETAXS(NCWORD,LF,IPR)
      L3=IFINDC(LF,LAX,3,0)
      IF (L3.LE.0) L3=3
      CALL SELDEV(IDSEND,N,IDEST)
      CALL CONFLD(IFLDW,NFMAX,0)
      CALL CONFLDR(FDORD,NFMAX,0.0)
      CALL CONFLD(DCOL,8,1)
      IF(ICOM.EQ.'STA') THEN
C     
C     STATISTICS
C     
         IOPT=1
         READ (KRD(6),110)IC
 110     FORMAT(A1)
         CALL FLDSET(KRD(5),NAMF,IFLDW,1,NST)
         IF(NST.NE.0) GO TO 90
         CALL FLDSETR(KRD(5),NAMF,SCLFAC,1.0,NST)
C     
      ELSE
C     
         IOPT=2
         READ (KRD,111)IC,TITLE(1:8),TITLE(9:16),TITLE(17:24),
     X        TITLE(25:32)
 111     FORMAT(////A1/A8/A8/A8/A8)
C     
 10      CONTINUE
C     
C     HISTOGRAM- FIELD CARD LOOP
C     
         CALL KARDIN(KRD)
         CALL COMCHK(IPR,KRD)
         READ (KRD,101)ITEST,NAMIN,SCL,FTMP,FTMP,FTMP,IMED,ICOL,
     X        TPER,OOPT
 101     FORMAT(A3/A8/F8.0/F8.0/F8.0/F8.0/A8/A8/F8.0/A8)
         
         IF(ITEST.EQ.'END') THEN
C     
C     FILL FDORD WITH FIELD LOCATIONS NOT LOADED IN SUB SETDCD
C     
            DO 13 I=1,NFL
               DO 17 J=1,NFL
                  IF (FDORD(J).EQ.I) GOTO 13
 17            CONTINUE
               FDORD(ICUR)=I
               ICUR=ICUR+1
 13         CONTINUE
            GOTO 15
         END IF
         IF (ITEST.EQ.'*') GOTO 10
         IF(SCL.EQ.0.0) SCL=1.0
         CALL FLDSET(NAMIN,NAMF,IFLDW,1,NST)
         IF(NST.NE.0) GO TO 10
         CALL FLDSETR(NAMIN,NAMF,SCLFAC,SCL,NST)
         CALL SETDCD(0,IDSEND,N,NAMF,ISYM,NDSYM,MAXSYM,KRD,NST)
         GO TO 10
C     
      END IF
C     
 15   CONTINUE
C     
C     GENERATE STATISTICS AND SEND TO APPROPRIATE DEVICE
C     
      ITPD=LOCATE(IC,IPVB,3)
      IF(ITPD.LE.0) ITPD=3
      I1=NCWORD(1)
      I2=NCWORD(2)
      I3=NCWORD(3)
      CALL PLNFLD(IFIXAX,L1,L2,NFDSP)
      IF ((L2-L1).GT.MAXZLEV .AND. IOPT.EQ.2) THEN
         WRITE(6,540)
 540     FORMAT(/,5X,'+++TOO MANY LEVELS FOR HISTO COMMAND.+++')
         L2=L1+(MAXZLEV-1)
      END IF
      IF(NFDSP.EQ.0) GO TO 91
      IF(IOPT.NE.1) GO TO 35
      DO 18 M=1,N
         IOUT=IDSEND(M)
         WRITE(IOUT,104) IWIND(1,I1),IWIND(2,I1),PWIND(1,I1),
     X        PWIND(2,I1),LABAXS(I1,IUNAXS),IWIND(1,I2),IWIND(2,I2),
     X        PWIND(1,I2),PWIND(2,I2),LABAXS(I2,IUNAXS)
         WRITE(99,104) IWIND(1,I1),IWIND(2,I1),PWIND(1,I1),
     X        PWIND(2,I1),LABAXS(I1,IUNAXS),IWIND(1,I2),IWIND(2,I2),
     X        PWIND(1,I2),PWIND(2,I2),LABAXS(I2,IUNAXS)
 104     FORMAT(/8X,'STATISTICS ARE FROM (',I3,' TO ',I3,')   (',F8.3,
     X        ' TO ',F8.3,1X,A4,') ALONG I'/19X,'         (',I3,' TO ',
     X        I3,')   (',F8.3,' TO ',F8.3,1X,A4,') ALONG J')
         IF(ITPD.GT.1) THEN
            WRITE(IOUT,106) IWIND(1,I3),IWIND(2,I3),
     X           PWIND(1,I3),PWIND(2,I3),LABAXS(I3,IUNAXS)
            WRITE(99,106) IWIND(1,I3),IWIND(2,I3),
     X           PWIND(1,I3),PWIND(2,I3),LABAXS(I3,IUNAXS)
 106        FORMAT(19X,'         (',I3,' TO ',I3,')   (',F8.3,' TO ',
     X           F8.3,1X,A4,') ALONG K')
         END IF
         WRITE(IOUT,107)
         WRITE(99,107)
 107     FORMAT(/)
 18   CONTINUE
C     
C     GENERATE STATISTICS
C     
      DO 30 I=1,NFL
         IV=MAPVID(I,2)
         IF(IFLDW(IV).EQ.0) GO TO 30
C     
C     LOOP OVER REQUESTED FIELDS
C     
         CALL STATZI
         DO 19 M=1,N
            CALL STINT(NAMF(1,IV),IFIXAX,AXNAM(IFIXAX),IDSEND(M))
 19      CONTINUE
         DO 25 L=L1,L2
            IGO=MOD(L-L1,LSKP)
            IF(IGO.NE.0.AND.ITPD.EQ.1) GO TO 25
C     
C     LOOP OVER REQUESTED LEVELS
C     
            CALL FETCHD(IN,ID,L,I,IBUF,RBUF,N1,N2,
     X           IFIXAX,BAD,RLEV,NST)
            IF(NST.NE.0) GO TO 90
c     RLEV=CSP(1,IFIXAX)+(L-1)*CSP(3,IFIXAX)
            RLEV=RLEV*SCLAXS(IFIXAX,IUNAXS)
            CALL STRID(RBUF,N1,N2,IWIND,NCWORD,BAD,FAVG,FSTD,NP,
     X           M1,M2,M3,M4,FMN,FMX,1)
            IF(IGO.EQ.0.AND.ITPD.NE.2) THEN
               DO 23 M=1,N
                  IOUT=IDSEND(M)
                  WRITE(IOUT,103) RLEV,FAVG,FSTD,NP,M1,M2,M3,M4,FMN,FMX
                  WRITE(99,103) RLEV,FAVG,FSTD,NP,M1,M2,M3,M4,FMN,FMX
 103              FORMAT(8X,F8.3,F10.2,F8.2,I7,1X,4I4,3X,2F8.2)
 23            CONTINUE
            END IF
C     
 25      CONTINUE
C     
         IF(ITPD.NE.1) THEN
            CALL STRID(RBUF,N1,N2,IWIND,NCWORD,BAD,FAVG,FSTD,NP,
     X           M1,M2,M3,M4,FMN,FMX,2)
            DO 28 M=1,N
               IOUT=IDSEND(M)
               WRITE(IOUT,108) FAVG,FSTD,NP,M1,M2,M3,M4,FMN,FMX
               WRITE(99,108) FAVG,FSTD,NP,M1,M2,M3,M4,FMN,FMX
 108           FORMAT(9X,' VOLUME',F10.2,F8.2,I7,1X,4I4,3X,2F8.2)
 28         CONTINUE
         END IF
 30   CONTINUE
      GO TO 90
 35   CONTINUE
#if CEDRIC_USE_NCARG
C     
C     GENERATE HISTOGRAM DISPLAYS
C     
      DO 45 KJ=1,NFL
C     
         I=FDORD(KJ)
         IV=MAPVID(I,2)
         IF(IFLDW(IV).EQ.0) GO TO 45
C     
         HMIN=SYMCD(1,IV)
         HMAX=SYMCD(2,IV)
         HINC=WIDCD(IV)
         TPER=TPERA(IV)
         
         NBINS=(HMAX-HMIN)/HINC + 1.00001
C     
C     FILL THE CLASS ARRAY WITH MIDPOINT VALUES OF HISTOGRAM BARS
C     
         DO 27 IJ=1,NBINS
            VAL=HMIN+HINC*(IJ-1)
            CLASS(IJ)=VAL
 27      CONTINUE
         
C     
C     ZERO THE ACCUMULATORS AND DO SOME INITIALIZATIONS
C     
         CALL STATZI
         CALL HSTINT
C     
C     CHECK TOP PERCENT VALUE
C     
         IF (TPERA(IV).LE.0.0) TPERA(IV)=100.0
C     
C     CHECK OVERLAY OPTION
C     
         DCOL(1)=ICOLA(IV)
C     
         DO 42 L=L1,L2
            IGO=MOD(L-L1,LSKP)
            IF(IGO.NE.0.AND.ITPD.EQ.1) GO TO 42
            CALL NEXACT(IFIXAX,L,IV,RLEV,NAME,1)
C     
C     PRODUCE A HISTOGRAM FOR THIS FIELD
C     
            IF (L.EQ.L1) NVPTS=0
            CALL FETCHD(IN,ID,L,I,IBUF,RBUF,N1,N2,
     X           IFIXAX,BAD,RLEV,NST)
            CALL STRID(RBUF,N1,N2,IWIND,NCWORD,BAD,FAVG,FSTD,NP,
     X           M1,M2,M3,M4,FMN,FMX,1)
            CALL HSTGRM(RBUF,N1,N2,HMIN,HMAX,WIDCD(IV),SCLFAC(IV),
     X           BAD,NCWORD,IWIND,1,NPPTS,NVPTS)
            IF(IGO.EQ.0.AND.ITPD.NE.2) THEN
               IOUT=IDSEND(M)
               
C     
C     DRAW THE HISTOGRAM
C     
               IF (OOPTA(IV).EQ.'LOG') THEN
                  IYAXIS=1
               ELSE
                  IYAXIS=0
               END IF
               IF (IMEDA(IV).EQ.1) CALL MEDHST(MED,1,NPPTS,HMIN,HINC)
               IF (MED.EQ.-999.0 .AND. IMEDA(IV).EQ.1) THEN
                  IMEDA(IV)=0
                  CALL PLTHIST(IBINS,NBINS,HMIN,(HMIN+HINC*(NBINS-1)),
     .                 NPPTS,TPERA(IV),HMIN,HINC,ICOLA(IV),IYAXIS,MED,
     .                 IMEDA(IV))
                  IMEDA(IV)=1
               ELSE
                  CALL PLTHIST(IBINS,NBINS,HMIN,(HMIN+HINC*(NBINS-1)),
     .                 NPPTS,TPERA(IV),HMIN,HINC,ICOLA(IV),IYAXIS,MED,
     .                 IMEDA(IV))
               END IF
C     
C     LABEL THE HISTOGRAM
C     
               CALL HSTLBL(KJ,RLEV,FAVG,FSTD,NP,M1,M2,M3,
     X              M4,FMN,FMX,L3,1,NBINS,MED)
               CALL MYFRAME
               
            END IF
 42      CONTINUE
         IF(ITPD.NE.1) THEN
            CALL STRID(RBUF,N1,N2,IWIND,NCWORD,BAD,FAVG,FSTD,NP,
     X           M1,M2,M3,M4,FMN,FMX,2)
C     IOUT=IDSEND(M)
C     CALL STINT(NAMF(1,IV),IFIXAX,LAX(IFIXAX),IOUT)
C     WRITE(IOUT,108) FAVG,FSTD,NP,M1,M2,M3,M4,FMN,FMX
C     CALL HSTLST(IOUT,HMIN,HMAX,WIDCD(IV),SCLFAC(IV),1)
C     
C     NOW PLOT THE VOLUME INFORMATION
C     
C     
C     DRAW THE HISTOGRAM
C     
            IF (IMEDA(IV).EQ.1)CALL MEDHST(MED,2,NVPTS,HMIN,HINC)
               IF (OOPTA(IV).EQ.'LOG') THEN
                  IYAXIS=1
               ELSE
                  IYAXIS=0
               END IF
            IF (MED.EQ.-999.0 .AND. IMEDA(IV).EQ.1) THEN
               IMEDA(IV)=0
                  CALL PLTHIST(KBINS,NBINS,HMIN,(HMIN+HINC*(NBINS-1)),
     .                 NVPTS,TPERA(IV),HMIN,HINC,ICOLA(IV),IYAXIS,MED,
     .              IMEDA(IV))
               IMEDA(IV)=1
            ELSE
                  CALL PLTHIST(KBINS,NBINS,HMIN,(HMIN+HINC*(NBINS-1)),
     .              NVPTS,TPERA(IV),HMIN,HINC,ICOLA(IV),IYAXIS,MED,
     .              IMEDA(IV))
            END IF
C     
C     LABEL THE HISTOGRAM
C     
            CALL HSTLBL(KJ,RLEV,FAVG,FSTD,NP,M1,M2,M3,
     X           M4,FMN,FMX,L3,2,NBINS,MED)
            CALL MYFRAME
            
         END IF
C     
 45   CONTINUE
#endif
 90   CONTINUE
      RETURN
 91   CONTINUE
      CALL CEDERX(539,0)
      RETURN
      END
