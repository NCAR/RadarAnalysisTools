      SUBROUTINE SYNDRV(KRD,IBUF,MAXBSZ,IPR,NST,ICORD,GFIELD)
C
C        DRIVER FOR THE INTERACTIVE VERSION OF MULTIPLE DOPPLER RADAR
C               SYNTHESIS. OUTPUT IS DISPOSED TO THE EDIT FILE UNIT.
C
C     MAXTRF - Maximum number of fields that can be transferred within
C              the SYNTHES command (8).
C     MXFS   - Maximum number of fields allowed during synthesis,
C              including those transferred (10).  MXFS = MAXTRF + 2.
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXRAD=14,MAXTRF=8,MXFS=MAXTRF+2,MXADV=MXRAD+1)
      PARAMETER (NMFMAX=8)
      COMMON /ARBRNE/ IABFLG(MXRAD),NFAZEL(2,MXRAD),SCAZEL(2,MXRAD)
      CHARACTER*8 NFAZEL
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /SETWND/ ISETW(2,3),PSETW(2,3),ISETFL(NFMAX)
      COMMON /SYNBLK/ LSYN(MXRAD),NAMRAD(MXRAD),INVOLD(MXRAD),
     X                NFLDIN(MXFS,MXRAD,2),NRQF,IRADTB(MXRAD),NRADS
      CHARACTER*8 NAMRAD,INVOLD,NFLDIN
      COMMON /ADVECT/ ITVOLM(MXADV),NFLTIM(MXADV),TADV(MXADV),
     X                ISHAD(MXADV),JSHAD(MXADV),IADTYP(MXADV)
      CHARACTER*8 NFLTIM
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      DIMENSION IBUF(MAXBSZ),IDSAV(NID,MXRAD),DTEST(3),
     X          IOFTAB(4,NMFMAX),IBTIME(3),
     X          LASTLV(MXRAD),ISCL(NMFMAX),MAP(MAXAXIS,3,MXRAD),
     X          ITETB(10)
      COMMON /IOTYPE/ ICDF
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),IDIMID(1,MXCDF),
     X     IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,ICDUNT(MXCDF),IUSWRP
      CHARACTER*8 CIBUF(10),IVOLNM
      CHARACTER*(*) KRD(10)
      CHARACTER*1 IBL,IADTAB(2),CIBL,ITWO,IREW,IRTRAN,ITEST
      CHARACTER*2 IARB(3),IACTD(2),ITEQ,IWEQ,NAMFLD(4),CTEMP3(3)
      CHARACTER*2 CSTEMP(2)
      CHARACTER*3 CTEMP5,CTEMP6,CTEMP7
      CHARACTER*4 CTEMP1
      CHARACTER*8 CTEMP,IENCD1,NONAME,IDFTIM
      CHARACTER*9 COORD(3)
      CHARACTER ITCHAR(3)*12
      CHARACTER*8 GFIELD(NFMAX)
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      LOGICAL ICHECK,KEEPW,KEEPPE
      INTEGER COORDSYS
      DATA IADTAB/'U','D'/
      DATA IARB/'AI','RB','RN'/
      DATA ITCHAR/'          NO','     UNIFORM','DIFFERENTIAL'/
      DATA COORD/'CARTESIAN', 'COPLANE','LONLATELE'/
      DATA IACTD/'DE','  '/
      DATA IOFTAB/'U ',' ',' ',' ',
     X            'V ',' ',' ',' ',
     X            'W ',' ',' ',' ',
     X            'CT',' ',' ',' ',
     X            'US','TD',' ',' ',
     X            'VS','TD',' ',' ',
     X            'WS','TD',' ',' ',
     X            'MP','E ',' ',' '/
      DATA ITETB/'EW','U ',' ',' ',64,
     X           'EW','V ',' ',' ',64 /
      DATA NBKI/3200/
      DATA IENCD1,NONAME,IDFTIM/'   1.000','NONE','T=0.0   '/
      DATA ITEQ,IWEQ,IBL/'T=','W=','  '/
      DATA CIBL/' '/
      DATA NRQF/2/
      DATA ISCL/64,64,64,1,64,64,64,64/
C
C        INITIALIZATION -READ IN HEADER CARD INFO
C

      READ (KRD,100)IVOLNM,RNRADS,DTEST,ITWO,TANAL,WDADV,WSADV
C  100 FORMAT(8X,A8,4F8.0,A1,7X,3F8.0)
 100  FORMAT(/A8/F8.0/F8.0/F8.0/F8.0/A1/F8.0/F8.0/F8.0)
      NRADS=RNRADS
      IF (NRADS.GT.2 .AND. ICORD.EQ.2) THEN
         CALL CEDERX(579,1)
         RETURN
      END IF
      ITANAL=TANAL
      IADV=0
      ISYNFLG=1
      IF(WSADV.GT.0.0) IADV=1
      ITEQN=0
      IF(ITWO.EQ.'Y') ITEQN=1
      IF(DTEST(1).LE.0.0) DTEST(1)=1.0
      IF(DTEST(2).LE.0.0) DTEST(2)=100.0
      IF(DTEST(3).LE.0.0) DTEST(2)=100.0
      IF(NRADS.LT.2.OR.NRADS.GT.MXRAD) THEN
         CALL CEDERX(526,1)
         RETURN
      END IF
      KFLD=NMFMAX
      NPLIN=0
      MXKP=0
C
C     READ INFO FOR INDIVIDUAL RADARS.  The user can only specify
C     the name (A8) of the input radial velocity as NFLDIN(1,I,1).
C     The rest of the synthesis parameters stored in NFLDIN are done
C     so by SYNDRV and SYNNER.  They're stored as CHARACTER*8, but
C     extracted by WRITE(CTEMP,format)NFLDIN then READ(CTEMP,format)
C     into such as WEIGHT which is a floating point.
C
C        NFLDIN(1,I,1) - Name of input radial velocity for Ith radar
C        NFLDIN(1,I,2) - Set to IBL ('  ')
C        NFLDIN(2,I,1) - Set to IWEQ ('W=')
C        NFLDIN(2,I,2) - Set to IENDC1 ('   1.000') ==> WEIGHT(N) = 1.0
C        NFLDIN(other,I,other) - Other synthesis parameters
C
      IFILE=0
      DO 20 I=1,NRADS
         CALL KARDIN(KRD)
         CALL COMCHK(IPR,KRD)
         READ (KRD,101)RUNIT,INVOLD(I),RBTIM,RETIM,
     X                      IREW,NFLDIN(1,I,1),IRTRAN
C  101    FORMAT(8X,F8.0,A8,2F8.0,A1,7X,A8,16X,A1)
 101     FORMAT(/F8.0/A8/F8.0/F8.0/A1/A8///A1)

C
C        KRD(8) IS ADVECTION METHOD (or AZIMUTH   field if Airborne radar)
C        KRD(9) IS TIME INFO        (or ELEVATION   "    "    "       "  )
C
         LSYN(I)=RUNIT
         IF(INVOLD(I).EQ.IBL) INVOLD(I)='NEXT'
         IBTIM=RBTIM
         IETIM=240000
         IF(RETIM.GT.0.0) IETIM=RETIM
C
C           POSITION INPUT TAPE ON REQUESTED VOLUME AND SUMMARIZE
C
         CALL SETVOL(LSYN(I),INVOLD(I),IBTIM,IETIM,IPR,LASTLV(I),
     X               IREW,IDSAV(1,I),ISTAT,GFIELD)
         IF (ISTAT.NE.0) RETURN
C
C     SET AXIS LABELS, IF REQUESTED
C
         WRITE(CSTEMP(1),25) IDSAV(16,I)
 25       FORMAT(A2)
         WRITE(CSTEMP(2),25) IDSAV(17,I)

         IF (ICORD.EQ.0) THEN
         CALL SAVCRDSY(CSTEMP)
         CALL SAXISLBL(AXNAM,CTEMP5,CTEMP6,CTEMP7,COORDSYS)
             READ(CTEMP5,133)LABAXS(1,1)
             READ(CTEMP6,133)LABAXS(2,1)
             READ(CTEMP7,133)LABAXS(3,1)
 133         FORMAT(A3)
         END IF


         WRITE(IPR,107) I
  107    FORMAT(//' VOLUME SUMMARY FOR RADAR',I2,' ...')
         CALL IMHSUM(IPR,IDSAV(1,I))
         WRITE (CTEMP,150)NFLDIN(1,I,1)
         READ (CTEMP,106)NAMFLD
  106    FORMAT(4A2)
         IF(LOCFLDID(NAMFLD,IDSAV(176,I),5,IDSAV(175,I),4).EQ.0) THEN
            CALL CEDERX(530,1)
            RETURN
         END IF
C
C     CHECK THAT COORDINATE SYSTEMS OF DATA AGREE
C
         IF (ICORD.EQ.0 .AND. I.GT.1) THEN
            IF (IDSAV(16,I).NE.IDSAV(16,I-1)) THEN
               CALL CEDERX(574,1)
               RETURN
            END IF
         END IF
         WRITE (CTEMP,105)(IDSAV(L,I),L=13,15)
         READ (CTEMP,500)NAMRAD(I)
 500     FORMAT(A6)
  105    FORMAT(3A2)
         IABFLG(I)=0
 113     FORMAT(A2)
         WRITE(CTEMP3(1),113)IDSAV(312,I)
         WRITE(CTEMP3(2),113)IDSAV(313,I)
         WRITE(CTEMP3(3),113)IDSAV(314,I)
         IF(LOCFLD(CTEMP3,IARB,3,1,3).NE.0) THEN
C
C           AIRBORNE DOPPLER
C
            IABFLG(I)=1
            ICHECK=.FALSE.
            DO 5 L=1,2
               NFAZEL(L,I)=KRD(7+L)
               WRITE (CTEMP,150)NFAZEL(L,I)
               READ (CTEMP,106)NAMFLD
               M=LOCFLDID(NAMFLD,IDSAV(176,I),5,IDSAV(175,I),4)
               IF(M.EQ.0) THEN
                  ICHECK=.TRUE.
               ELSE
                  SCAZEL(L,I)=1./IDSAV(175+M*5,I)
               END IF
    5       CONTINUE
            IF(ICHECK) THEN
               CALL CEDERX(562,1)
               RETURN
            END IF
         END IF
C
         IF(I.GT.1) THEN
            IF(IFINDC(NAMRAD(I),NAMRAD,I-1,0).NE.0.AND.
     X               IABFLG(I).EQ.0) CALL CEDERX(531,0)
            IF(IFIND(LSYN(I),LSYN,I-1,0).NE.0) THEN
               CALL CEDERX(536,1)
               RETURN
            END IF
         END IF
         NFLDIN(1,I,2)=IBL
         NFLDIN(2,I,1)=IWEQ
         NFLDIN(2,I,2)=IENCD1
         IADTYP(I)=0
         NFLTIM(I)=NONAME
         NPLIN=MAX0(NPLIN,IDSAV(301,I))
         IF(IADV.NE.0) THEN
C
C           SET ADVECTION INFO IF ACTIVATED
C
            ITVOLM(I)=(IDSAV(119,I)*100+IDSAV(120,I))*100+IDSAV(121,I)
            IF(ITANAL.LE.0) ITANAL=ITVOLM(I)
            IF(IABFLG(I).EQ.0) THEN
               READ (KRD(8),102)ITEST
  102          FORMAT(A1)
               IADTYP(I)=IFINDC(ITEST,IADTAB,2,0)
               IF(IADTYP(I).NE.0) THEN
                  NFLTIM(I)=IDFTIM
C  TRANSFER TO INT ARRAY
                  IF(KRD(9).NE.CIBL) NFLTIM(I)=KRD(9)
                  WRITE (CTEMP,150)NFLTIM(I)
                  READ (CTEMP,106)NAMFLD
                  IF(NAMFLD(1).NE.ITEQ) THEN
                     M=LOCFLDID(NAMFLD,IDSAV(176,I),5,IDSAV(175,I),4)
                     IF(M.EQ.0) THEN
                        CALL CEDERX(538,1)
                        RETURN
                     END IF
                  END IF
               END IF
            END IF
         END IF
C
C           CHECK IF ADDITIONAL FIELDS ARE TO BE TRANSFERRED
C
         NTRAN=0
         IF(IRTRAN.NE.'Y') GO TO 15
         CALL KARDIN(KRD)
         READ (KRD,108)ITEST
C  108    FORMAT(8X,A1)
 108     FORMAT(/A1)
         IF(ITEST.NE.'I') THEN
            CALL CEDERX(528,1)
            RETURN
         END IF
         DO 153 ICNT=1,10
            WRITE (CIBUF(ICNT),150)IBUF(ICNT)
 153     CONTINUE
         CALL KARDIN(CIBUF)
         DO 163 ICNT=1,10
            READ (CIBUF(ICNT),150)IBUF(ICNT)
 163     CONTINUE
         WRITE (CTEMP,150)CIBUF(2)
         READ (CTEMP,160)ITEST
 160     FORMAT(A1)
         IF(ITEST.NE.'O') THEN
            CALL CEDERX(528,1)
            RETURN
         END IF

C        Process information on additional fields to be transferred
C        Attempt to process the maximum number of parameters (8) left
C        on the INPUT/OUTPUT commands.  Currently, MAXTRF is set to 8 
C        so the test for NTRAN.GT.MAXTRF does nothing.  Changed looping
C        from DO 10 L=1,MAXTRF to DO 10 L=1,8.
C
         DO 10 L=1,8
            IF(KRD(2+L).EQ.CIBL) GO TO 10
            NTRAN=NTRAN+1
            IF(NTRAN.GT.MAXTRF) THEN
               CALL CEDERX(527,1)
               RETURN
            END IF
            KFLD=KFLD+1
            IF(KFLD.GT.NFMAX) THEN
               CALL CEDERX(505,1)
               RETURN
            END IF
 150        FORMAT(A8)
            NFLDIN(L+NRQF,I,1)=KRD(2+L)
            NFLDIN(L+NRQF,I,2)=CIBUF(2+L)
            WRITE (CTEMP,150)NFLDIN(L+NRQF,I,1)
            READ (CTEMP,106)NAMFLD
            IF(LOCFLDID(NAMFLD,IDSAV(176,I),5,IDSAV(175,I),4).EQ.0)THEN
              CALL CEDERX(511,1)
              RETURN
           END IF
   10    CONTINUE
   15    CONTINUE
         IRADTB(I)=NTRAN
         MXKP=MAX0(MXKP,NTRAN)
   20 CONTINUE
      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      IF(KRD(1).NE.'END') THEN
         CALL CEDERX(557,1)
         RETURN
      END IF
      IF(NRADS+ITEQN.GT.3) THEN
         NMF=NMFMAX
         KEEPPE=.TRUE.
      ELSE
         NMF=NMFMAX-1
         KEEPPE=.FALSE.
      END IF
      CALL IDSYND(IDSAV,MAP,IVOLNM,IOFTAB,NMF,ISCL,IPR,NST)
      IF(NST.NE.0) THEN
         CALL CEDERX(533,1)
         RETURN
      END IF
      IF(ID(40)/ID(69).NE.90) WRITE(IPR,129)
  129 FORMAT(/5X,'+++  WARNING...  COORDINATE SYSTEM HAS BEEN',
     X           ' ROTATED  +++')
C
C        VERIFY THAT THE PROBLEM WILL FIT IN AVAILABLE STORAGE
C
C
C        NRADS*NRQF FIELDS TO BE LOADED INTO THE SYNTHESIS
C        NPLANE*NMF DATUMS TO BE WRITTEN TO OUTPUT
C        NEEDPL*2   SCRATCH DATUMS REQUIRED
C        NPLIN*MXKP DATUMS FOR HOLDING TRANSFERRED FIELDS
C
         NPRBSZ = MAX0( NPLANE*NRADS*NRQF , NPLANE*NMF )
C
C          IPT- POINTER TO TRANSFERRED FIELDS STORAGE
C          JPT- POINTER TO SCRATCH STORAGE
C
      NEEDPL=MAX0(NPLIN,NPLANE,NBKI)
         JPT=NPRBSZ+1
         NPRBSZ=NPRBSZ+NEEDPL*2
         IPT=NPRBSZ+1
         NPRBSZ=NPRBSZ+NPLIN*(MXKP+2)
C
         IF(NPRBSZ.GT.MAXBSZ) THEN
         WRITE (IPR,126) NPRBSZ,MAXBSZ
  126    FORMAT(/' +++  INSUFFICIENT STORAGE...  WORDS REQUIRED:',I7,
     X        4X,'AVAILABLE:',I7,'  +++'/)
         CALL CEDERX(518,1)
         RETURN
      END IF
C
C     DETERMINE COORDINATE SYSTEM IN WHICH SYNTHESIS IS TO BE DONE
C
C     IACTC=0 ==> 3-D CARTESIAN
C     IACTC=1 ==> COPLANE
C
      IF (ICORD.EQ.0) THEN
         WRITE(CTEMP1,44)IDSAV(16,1),IDSAV(17,1)
 44      FORMAT(2A2)
         IF (CTEMP1.EQ.'COPL') THEN
            IACTC=1
         ELSE 
            IACTC=0
         END IF
      ELSE IF (ICORD.EQ.1) THEN
         IACTC=0
      ELSE IF (ICORD.EQ.2) THEN
         IACTC=1
      ELSE
         WRITE(*,*)'***INVALID COORDINATE SYSTEM IN SYNDRV***'
      END IF
      IF(CTEMP1 .EQ. 'LLE') COORD(IACTC+1) = 'LONLATELE'
C
C        SEND A SUMMARY TO THE PRINT FILE IF REQUESTED
C
         CALL DSPSYN(IPR)
         
         WRITE(IPR,121) IVOLNM,DTEST,IACTD(ITEQN+1),IACTD(IADV+1),
     X                  COORD(IACTC+1)
  121 FORMAT(/3X,'MULTIPLE DOPPLER SYNTHESIS-  PARAMETERS ...'/
     X  8X,'1- SYNTHESIS VOLUME SCAN DESIGNATION:',1X,A8/
     X  8X,'2- ACCEPTANCE CRITERIA ...  DTEST(1):',F9.3/
     X  8X,'3-                          DTEST(2):',F9.3/
     X  8X,'4-                          DTEST(3):',F9.3/
     X  8X,'5-     TWO EQUATION SOLUTION ONLY IS:',1X,A2,'ACTIVATED'/
     X  8X,'6- ADVECTION CORRECTION PROCEDURE IS:',1X,A2,'ACTIVATED'/
     X  8X,'7- COORDINATE SYSTEM                :',1X,A9)

      IF(IADV.EQ.0) GO TO 88
C
C        PRINT ADVECTION INFORMATION
C
      DO 85 N=1,NRADS
         L=IADTYP(N)
         WRITE(IPR,123) NAMRAD(N),ITVOLM(N),NFLTIM(N),ITANAL,
     X                  WDADV,WSADV,ITCHAR(L+1)
  123    FORMAT(1X,A6,5X,'BEG. TIME:',I7,7X,'TIME FIELD: ',A8
     X    /11X,'ANAL. TIME:',I7,5X,'STORM MOTION:',F6.1,' (DEG) AT',
     X     F5.1,' (M/S)'/4X,A12,' ADVECTION IS TO BE PERFORMED.')
   85 CONTINUE
C
C        SAVE TIME INFO IN ID HEADER
C
      IBTIME(1)=ITANAL/10000
      IBTIME(2)=(ITANAL-(IBTIME(1)*10000))/100
      IBTIME(3)=ITANAL-(IBTIME(1)*100+IBTIME(2))*100
      DO 86 I=1,3
         ID( 23+I)=IBTIME(I)
         ID( 29+I)=IBTIME(I)
         ID(118+I)=IBTIME(I)
         ID(124+I)=IBTIME(I)
   86 CONTINUE
   88 CONTINUE
C
      KEEPW=.TRUE.
      IF(NRADS.EQ.2.OR.ITEQN.NE.0) THEN
C
C        ONLY U,V COMPUTED FOR ALL DATA POINTS
C
         KEEPW=.FALSE.
         CALL COPIX(ID(186),ID(191),15)
         CALL COPIX(ID(201),ITETB,10)
         CALL NEWHED(0)
      END IF
C
C
C        INVOKE THE SYNTHESIS
C
      CALL SYNNER(LASTLV,IBUF(JPT),NEEDPL,MXKP,IBUF(IPT),NPLIN,IBUF,
     X            NX,NY,NZ,IWEQ,ITEQ,IDSAV,MAP,DTEST,ITEQN,ITANAL,
     X            WDADV,WSADV,KEEPW,KEEPPE,NMF,NST,IACTC)
      IF (IACTC.EQ.1) THEN
C
C     FOR COPLANE SYNTHESIS, DELETE CT, EWU, EWV FIELDS
C
         DO 23 I=1,4
            NAMF(I,3)=' '
            NAMF(I,6)=' '
            NAMF(I,7)=' '
 23      CONTINUE
         SCLFLD(3)=0.0
         SCLFLD(6)=0.0
         SCLFLD(7)=0.0
         CALL DSINIT(0)
         CALL DSINIT(3)
         CALL DSINIT(6)
         CALL DSINIT(7)
         CALL UPDHED(0)
         CALL WINDFD(KRD,1)
      END IF

C
C        GENERATE A SUMMARY AND WRAP THINGS UP
C
      WRITE(IPR,122)
  122 FORMAT(//' SYNTHESIS OUTPUT VOLUME SUMMARY ...'//)
      CALL IMHSUM(IPR,ID)
      IF (IACTC.NE.1) CALL DSINIT(0)
C
C        POSITION INPUT TAPES AFTER THE VOLUMES
C
      DO 91 I=1,NRADS
         CALL SKPVOL(LSYN(I),1)
   91 CONTINUE
C
C        NORMAL TERMINATION
C

C
C     CLOSE NETCDF FILES, IF ANY
C
c*****Fix 970709 (LJM) - pass icdf to this routine
c*****CALL NETCLOS
      IF (ICDF.EQ.2) CALL NETCLOS()
      RETURN
      END
