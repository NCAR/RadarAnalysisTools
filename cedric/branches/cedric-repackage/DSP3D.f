      SUBROUTINE DSP3D(KRD,IBUF,RBUF,WBUF,TBUF,IPR)
C
C        DISPLAYS 3-D SURFACES
C
C           IBUF     TEMPORARY STORAGE (1-XY PLANE)
C           RBUF     TEMPORARY STORAGE     '
C           WBUF     TEMPORARY STORAGE (2- XY PLANES)
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MAXZ=30)
      PARAMETER (MAXPTS=MAXX*MAXY*MAXZ)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /DSPECS/ IWIND(2,3),PWIND(2,3),NCWORD(3),IFLDW(NFMAX),
     X                SCLFAC(NFMAX),NSYMCD(2,NFMAX),WIDCD(NFMAX),
     X                ZREFCD(NFMAX),THKLIN(NFMAX)
      COMMON /AXSPAC/ XD,YD,ZD
      DIMENSION IBUF(1),RBUF(1),WBUF(1),TBUF(1),AXSF(3),DEFEY(3),
     X          EPOS(3),EYE(3)
      CHARACTER*80 LABEL
      CHARACTER*(*) KRD(10)
      CHARACTER*8 NAMIN
      CHARACTER*3 ITEST
      CHARACTER*1 CIBL,ITMP
      CHARACTER LABX(3)*1,ICUSR(3)*1,NOUT(2)*5
      LOGICAL ICHECK
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      DATA LABX,NOUT / 'X','Y','Z','BELOW','ABOVE'/
      DATA DEFEY/ 5.0,-3.5, 2.0 /
      DATA CIBL/' '/
C
C        PLOTTING INITIALIZATION
C
      READ (KRD,100)IDEST,ICUSR,AXSF
C  100 FORMAT(8X,A1,7X,3A1,5X,3F8.0)
 100  FORMAT(/A1/3A1/F8.0/F8.0/F8.0)
      IFLAG=0
      DO 5 I=1,3
         ICAX=0
         DO 3 L=1,3
            IF(ICUSR(L).NE.LABX(I)) GO TO 3
            ICAX=1
    3    CONTINUE
         IF(AXSF(I).LE.0.0) AXSF(I)=1.0
         EPOS(I)=NCX(I)*DEFEY(I)*CSP(3,I)+CSP(1,I)
         IF(KRD(6+I).NE.CIBL) READ (KRD(6+I),101)EPOS(I)
  101    FORMAT(F8.0)
         IFLAG=ICEDOR(IFLAG,ICEDSHFT(ICAX,3-I))
C         IFLAG=OR(IFLAG,LSHIFT(ICAX,3-I))
    5 CONTINUE
         IFLAG=MAX0(IFLAG,1)
C
C        ESTABLISH THE FIELDS TO BE DISPLAYED
C
      CALL CONFLD(IFLDW,NFMAX,0)
   10 CONTINUE
C
C        NEXT FIELD SPECIFIER
C
         CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
         READ (KRD,102)ITEST,NAMIN,CL,ITMP
C  102    FORMAT(A3,5X,A8,F8.0,A1)
 102     FORMAT(A3/A8/F8.0/A1)
         IF(ITEST.EQ.'END') GO TO 20
         CB=1.0
         IF(ITMP.EQ.'B') CB=-1.0
         CALL FLDSET(NAMIN,NAMF,IFLDW,1,NST)
         IF(NST.NE.0) GO TO 10
         CALL FLDSETR(NAMIN,NAMF,ZREFCD,CL,NST)
         CALL FLDSETR(NAMIN,NAMF, WIDCD,CB,NST)
      GO TO 10
   20 CONTINUE
C
C        CHECK EYE POSITION
C
      ICHECK=.TRUE.
      DO 145 I=1,3
         IF(EPOS(I).GE.CSP(1,I).AND.EPOS(I).LE.CSP(2,I)) GO TO 145
         ICHECK=.FALSE.
 145  CONTINUE
      IF(ICHECK) GO TO 901
C
C        CALCULATE PARAMETERS FOR ISOSRF
C
      DO 210 I=1,3
         EYE(I)=(EPOS(I)-CSP(1,I))/CSP(3,I) + 1.0
 210  CONTINUE
      XD=CSP(3,1)*AXSF(1)
      YD=CSP(3,2)*AXSF(2)
      ZD=CSP(3,3)*AXSF(3)
      RXL=(NX)*XD
      RYL=(NY)*YD
      RZL=(NZ+1)*ZD
      MUVWP2=MAX0(NX,NY,NZ) + 2
      IZLIN= -SIGN(1.0,EYE(2))
C
C     SEE IF WE CAN FIT WHOLE 3-D ARRAY OF FIELD IN MEM AT ONCE
C
      IF ((NX*NY*NZ).LT.MAXPTS) THEN
         IMEM=0
         MX=NX
         MY=NY
         MZ=NZ
      ELSE
         CALL CEDERX(595,1)
         RETURN
      END IF
C
C        INITIATE THE PLOTTING
C
      NFDSP=KNTOCR(IFLDW,NFMAX,1)
      IF(NFDSP.EQ.0) GO TO 902
      JFLD=0
C
C     TURN ON FINER SCREEN RESOLUTION
C
      CALL ISSETR('SM',1.0)
      CALL GSCLIP(0)
      DO 75 LOOP=1,NFDSP
   71    CONTINUE
C
C           SEARCH FOR NEXT ACTIVE FIELD TO BE DISPLAYED
C
            JFLD=JFLD+1
            IV=MAPVID(JFLD,2)
            IF(IFLDW(IV).EQ.0) GO TO 71
            IND=2.0+WIDCD(IV)*0.5
            
C
C     FETCH 3-D ARRAY IF ENOUGH MEMORY
C
            IF (IMEM.EQ.0) THEN
               ZIP=-SIGN(1.E8,WIDCD(IV))
               L=0
               DO 130 K=1,NZ
                  CALL FETCHD(IEDFL,ID,K,JFLD,IBUF,RBUF,NIX,NIY,
     X                        3,ZIP,ZLEV,NST)
                  IF (NST.NE.0) THEN
                     WRITE(*,*)'***ERROR FETCHING FIELD IN DSP3D***'
                     CALL FLUSH_STDOUT
                  END IF
                  M=0
                  DO 120 J=1,NY
                     DO 115 I=1,NX
                        M=M+1
                        L=L+1
                        TBUF(L)=RBUF(M)
 115                 CONTINUE
 120              CONTINUE
 130           CONTINUE
            END IF
C
C        LABEL GRAPH
C
      WRITE (LABEL,211)(ID(I),I=116,121),(ID(I),I=125,127),
     X     (ID(I),I=13,15),(ID(I),I=101,104),(NAMF(I,IV),I=1,4),
     X     NOUT(IND),ZREFCD(IV)
 211  FORMAT (1X,I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),
     X        '-',I2.2,2(':',I2.2),4X,3A2,4X,4A2,3X,4A2,1X,A6,F8.2)
      CALL MY_PLCHMQ(10,1010,LABEL(1:78),12.,0.,-1.)
      WRITE (LABEL,212)EPOS(1),EPOS(2),EPOS(3)
 212  FORMAT('(X,Y,Z)    EYE POSITION (KM):    ','(',F6.1,',',F6.1,
     X       ',',F6.1,')')
      CALL PLCHMQ(10,985,LABEL(1:55),12.,0.,-1.)
c      CALL ISOSRF(ID,NID,JFLD,IBUF,RBUF,WBUF,NX,NY,NZ,EYE,MUVWP2,
c     X            ZREFCD(IV),WIDCD(IV),IFLAG,TBUF,MX,MY,MZ,IMEM)
      call isosrf(tbuf,mx,mx,my,my,mz,eye,muvwp2,wbuf,zrefcd(iv),iflag)
      CALL PWRZI(RXL, YD, ZD,'X',1,3,1,3,1)
      CALL PWRZI( XD,RYL, ZD,'Y',1,3,2,3,1)
      CALL PWRZI( XD, YD,RZL,'Z',1,3,IZLIN,3,0)
      CALL MYFRAME
   75 CONTINUE
 900  CONTINUE
      CALL GSCLIP(1)
      RETURN
 901  CALL CEDERX(561,0)
      RETURN
 902  CALL CEDERX(539,0)
      RETURN
      END
