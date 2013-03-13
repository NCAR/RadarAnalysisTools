      SUBROUTINE MS2DRV(KRD,IBUF,OBUF,RBUF,IPR,NST)
C
C        DRIVER TO SOLVE THE EQUATION OF MASS CONTINUITY USING
C        (U,V) AND ASSOCIATED FIELDS FROM A 2-RADAR SYNTHESIS
C        THE HORIZONTAL AND VERTICAL CONVERGENCE AT EACH LEVEL.
C
      INCLUDE 'CEDRIC.INC'
      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXPLN,10),IFLD(4),
     X          JUVW(3),NEWUVW(3),LIMZ(2),ZLWHR(2),XYDELI(2),
     X          VTCON(3),C2RD(2)
      CHARACTER*8 LIMUSR(2),INZUSR,MCRIT,IALPHA,CTEMP,INITYP(3)
      CHARACTER*8 METDEF,METUSR,INVUSR,INIVAL
      CHARACTER*2 INICOD(3),NAMINF(4,4),NAMOUF(4,3),NAMDBZ(4),INIFLD(4)
      CHARACTER*1 CIBL,INDCOD(2),IAST,IPLUS,JCHAR,IASTCK,IEXTRA
      DATA CIBL/' '/
      CHARACTER*(*) KRD(10)
      LOGICAL NEWUVW,LVTON
      CHARACTER IDWT*20,IUNITY*6,IYESNO(3)*8,INDIR(2)*8,IVTEST*56,
     X          ISETZ*12
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,MBUSR,ICHK
C
      EQUIVALENCE (NCX(1),NX), (NCX(2),NY), (NCX(3),NZ)
      EQUIVALENCE (LIMZ(1),K1), (LIMZ(2),K2)
C
      DATA NAMINF/'U ','  ',' ',' ',
     X            'V ','  ',' ',' ',
     X            'EW','U ',' ',' ',
     X            'EW','V ',' ',' ' /
      DATA INITYP/'CONSTANT','FRACTION','   FIELD'/
      DATA INICOD/'CO','FR','FI'/
      DATA INDIR/' UPWARD ','DOWNWARD'/
      DATA INDCOD/'U','D'/
      DATA ISETZ/'SET TO ZERO'/
      DATA IUNITY/'UNITY'/
      DATA IYESNO/'   YES','   NO',' NO DATA'/
      DATA IAST,IPLUS/'*','+'/
      DATA METDEF/'U* 0.1  '/
      DATA IUVSCL/64/
C
      RHOFUN(L,ALPHA) = EXP(-ALPHA*(CSP(1,3)+CSP(3,3)*(L-1)))
      HEIGHT(L) = CSP(1,3) + (CSP(3,3)*(L-1))

      IACTC=0

C
C                                      INITIALIZATION --DECODE COMMAND
C
      READ (KRD,101)IEXTRA,NAMOUF,METUSR,MBUSR,INVUSR,MCRIT,LIMUSR
C  101 FORMAT(7X,A1,12A2,A8,A2,6X,4A8)
 101  FORMAT(7X,A1/4A2/4A2/4A2/A8/A2/A8/A8/A8/A8)
      IF(METUSR.EQ.CIBL) METUSR=METDEF
      WRITE (CTEMP,123)METUSR
      READ (CTEMP,102)JCHAR,IASTCK,IALPHA
  102 FORMAT(2A1,A6)
      METINT=IFINDC(JCHAR,INDCOD,2,0)
      IF(METINT.LE.0) METINT=1
C                                      SET UP DENSITY WEIGHTING
      ALPHA=0.0
      IDWT=IUNITY
      IF(IASTCK.EQ.IAST) THEN
         READ (IALPHA,103)ALPHA
  103    FORMAT(F6.0)
         ALPHA=ABS(ALPHA)
         IF(ALPHA.GT.1.0) THEN
            CALL CEDERX(568,1)
            RETURN
         END IF
         IF(ALPHA.LE.0.0) ALPHA=0.1
         WRITE (IDWT,104)ALPHA
  104    FORMAT('RHO=EXP( -Z *',F5.2,' )')
      END IF
C                                      BOUNDARY CONDITION
      IUD=METINT
      IMET=IFINDC(MBUSR,INICOD,3,0)
      IF(IMET.EQ.0) THEN
         CALL CEDERX(524,1)
         RETURN
      END IF
      IF(IMET.EQ.3) THEN
         WRITE (CTEMP,123)INVUSR
         READ (CTEMP,106)(INIFLD(I),I=1,4)
  106    FORMAT(4A2)
         INIF=LOCFLDID(INIFLD,ID(176),5,NFL,4)
         IF(INIF.EQ.0) THEN
            CALL CEDERX(525,1)
            RETURN
         END IF
         INIVAL=INVUSR
         CONI=0.0
      ELSE
         INIF=0
         WRITE (CTEMP,123)INVUSR
 123     FORMAT(A8)
         READ (CTEMP,105)CONI
         WRITE (CTEMP,108)CONI
         READ (CTEMP,123)INIVAL
C         ENCODE(8,108,INIVAL) 
  108    FORMAT(F8.2)
      END IF
C                                      ITERATION CRITERIA
      IF(CSP(3,1).NE.CSP(3,2)) CALL CEDERX(523,0)
      READ (MCRIT,105)CRIT
  105 FORMAT(F8.0)
      CRIT1=IFIX(CRIT)
      CRIT2=ABS(CRIT)-ABS(CRIT1)
      ISGN=ISIGN(1,INT(CRIT1))
      CRIT1=BNDVAL(ABS(CRIT1),1.,500.,10.,0)
      CRIT2=BNDVAL(CRIT2,0.00009,1.0,0.01,0)
      CRIT1=CRIT1*ISGN
      ITRMAX=NINT(CRIT1)
      ERRMAX=CRIT2
      C2RD(1)=ERRMAX
      C2RD(2)=ITRMAX
C                                      DECODE LOWER/UPPER LIMITS
C                                      LIMZ(1) AND LIMZ(2) EQVD TO K1/K2
      LIMZ(1)=1
      LIMZ(2)=NZ
C                                      LIMITS OF INTEGRATION
         DO 21 I=1,2
            IF(LIMUSR(I).EQ.CIBL) GO TO 21
            READ (LIMUSR(I),107)ICHK,VAL
  107       FORMAT(A2,F6.0)
            IF(ICHK.EQ.'I=') THEN
C                                      INDEX SPACE
               LIMZ(I)=VAL
            ELSE IF(ICHK.EQ.'D=') THEN
C                                      DISTANCE SPACE
               LIMZ(I)=NINT((VAL-CSP(1,3))/CSP(3,3)+1.0)
            ELSE
               CALL CEDERX(567,1)
               RETURN
            END IF
   21    CONTINUE
C
      LIMZ(1)=MAX0(LIMZ(1),1)
      LIMZ(2)=MIN0(LIMZ(2),NZ)
      IF(LIMZ(1).GT.LIMZ(2)) THEN
C                                      SWITCH THEM IF BACKWARDS
         I=LIMZ(1)
         LIMZ(1)=LIMZ(2)
         LIMZ(2)=I
      END IF
      ZLWHR(1)=HEIGHT(K1)
      ZLWHR(2)=HEIGHT(K2)
C                                      INGEST EXTRA CARD WITH FINAL
C                                      INIT LEVEL
      IOPPD=MOD(IUD,2)+1
      KINTZ=LIMZ(IOPPD)
      LVTON=.FALSE.
      IVTEST=ISETZ
C
C+++++ ALWAYS READ AN EXTRA CARD IMAGE
C
C      IF(IEXTRA.EQ.IPLUS) THEN
         CALL KARDIN(KRD)
         READ (KRD,109)INZUSR,NAMDBZ,VTCON
C  109    FORMAT(8X,A8,4A2,3F8.0)
 109     FORMAT(/A8/4A2/F8.0/F8.0/F8.0)
         IF(INZUSR.NE.CIBL) THEN
            READ (INZUSR,107)ICHK,VAL
            IF(ICHK.EQ.'I=') THEN
               KINTZ=VAL
            ELSE IF(ICHK.EQ.'D=') THEN
               KINTZ=NINT((VAL-CSP(1,3))/CSP(3,3)+1.0)
            ELSE
               CALL CEDERX(567,1)
               RETURN
            END IF
            KINTZ=MAX0(LIMZ(1),KINTZ)
            KINTZ=MIN0(LIMZ(2),KINTZ)
         END IF
C                                            ... AND VT:Z RELATION
         IDBZ=LOCFLDID(NAMDBZ,ID(176),5,NFL,4)
         IF(IDBZ.GT.0) THEN
            LVTON=.TRUE.
            VTCON(1)=BNDVAL(VTCON(1), 0.01,10.,  1.5,0)
            VTCON(2)=BNDVAL(VTCON(2), 0.01,1.0,0.105,0)
            VTCON(3)=BNDVAL(VTCON(3), 0.01,1.0,  0.4,0)
            WRITE (IVTEST,147)VTCON(1),NAMDBZ,VTCON(2),VTCON(3)
  147       FORMAT('VT = -',F5.2,' *  (',4A2,'/10)**',F5.3,
     X             ' * (P(0)/P(Z))**',F5.2)
         END IF
C
C+++++ ALWAYS EXECUTE PRECEDING CODE
C
C      END IF
C
      ZINIT=HEIGHT(KINTZ)
C                                      DETERMINE FIELD PRESENCE
      DO 25 L=1,4
C                                         INPUT  FIELDS
         IFLD(L)=LOCFLDID(NAMINF(1,L),ID(176),5,NFL,4)
         IF(IFLD(L).EQ.0) THEN
            CALL CEDERX(501,1)
            RETURN
         END IF
         IF(L.LE.3) THEN
C                                         OUTPUT FIELDS
            JNEW=IADFLD(NAMOUF(1,L),IUVSCL,IPR)
            IF (JNEW.LT.0) RETURN
            NEWUVW(L)=(JNEW.NE.0)
         END IF
   25 CONTINUE
C                                         FIELD INDICES
      DO 26 L=1,4
         IFLD(L)=LOCFLDID(NAMINF(1,L),ID(176),5,NFL,4)
         IF(L.GT.3) GO TO 26
            JUVW(L)=LOCFLDID(NAMOUF(1,L),ID(176),5,NFL,4)
   26 CONTINUE
      IF(IMET.EQ.3) INIF=LOCFLDID(INIFLD,ID(176),5,NFL,4)
      IF (LVTON)    IDBZ=LOCFLDID(NAMDBZ,ID(176),5,NFL,4)
      IF(IFLD(1).EQ.JUVW(1).OR.IFLD(2).EQ.JUVW(2)) THEN
         CALL CEDERX(571,0)
      END IF
C
      WRITE(IPR,143) ((NAMOUF(I,J),I=1,4),J=1,3),IVTEST,INDIR(IUD),
     X                 INITYP(IMET),INIVAL,ZINIT,LABAXS(3,1),IDWT,
     X                 ERRMAX,ITRMAX,LIMZ,ZLWHR,LABAXS(3,1)
  143 FORMAT(/3X,'2-RADAR AIR MOTION RECOMPUTATION-'
     X/5X,'U (OUTPUT) = U (INPUT) + EWU * W (OUTPUT)'
     X/5X,'V (OUTPUT) = V (INPUT) + EWV * W (OUTPUT)'
     X/5X,'W (OUTPUT) = W SATISFYING THE MASS CONTINUITY EQN.'
     X   /3X,'PARAMETERS ...'
     X   /3X,' 1-    U COMPONENT OUTPUT FIELD NAME: ',4A2
     X   /3X,' 2-    V     "       "      "    "  : ',4A2
     X   /3X,' 3-    W     "       "      "    "  : ',4A2
     X   /3X,' 4-TERMINAL VELOCITY (VT) ESTIMATION: ',A
     X   /3X,' 5-        INITIALIZATION (' ,A8, '): ',A8,
     X          '  (',A8,')   THRU',F6.2,' ',A4,
     X   /3X,' 8-                DENSITY WEIGHTING: ',A20
     X   /3X,' 9-        TARGET CHANGE IN W (ITER): ',F6.4
     X   /3X,'10-        MAX. NUMBER OF ITER/LEVEL: ',I4
     X /3X,'RECOMPUTATION WILL BE PERFORMED USING LEVELS  (',
     X     I2,' -',I3,')   (',F7.2,' THRU ',F7.2,' ',A4,')')
C         WRITE(IPR,154)AXNAM(3),LABAXS(3,1)
C  154    FORMAT(/4X,A1,' LEVEL (',A4,')   ITER #   MEAN DIFF   ',
C     X          'CONVERGED')
C
      XYDELI(1)=1./CSP(3,1)
      XYDELI(2)=1./CSP(3,2)
C
C     RBUF STORAGE ALLOCATION:
C
C      (1) U-COMPONENT, (2) V-COMPONENT, (3) EWU-CURRENT, (4) EWV-CURRENT,
C      (5) DBZ, CONV-CURRENT, (6) W-CURRENT, (7) CONV-PREVIOUS,
C      (8) W-PREVIOUS, BOUNDARY VALUES, (9) EWU-PREVIOUS, (10) EWV-PREVIOUS
C
      IDIR=(1.5-IUD)*3.0
      DZH=(0.5*CSP(3,3))*IDIR
      L=0
      LEV=K1
      IF(IDIR.LT.0) THEN
         L=NZ+1
         LEV=K2
      END IF
      DO 470 LOOP=1,NZ
         L=L+IDIR
         IF(L.GE.K1.AND.L.LE.K2) GO TO 459
C
C        WRITE BAD DATA VALUES OUT TO UNUSED LEVELS OF THE A NEW FIELD
C
         IF(NEWUVW(1).OR.NEWUVW(2).OR.NEWUVW(3))
     X                                CALL CONFLD(OBUF,NPLANE,BAD)
         DO 458 I=1,3
            IF(.NOT.NEWUVW(I)) GO TO 458
            CALL PLACED(IEDFL,ID,L,JUVW(I),IBUF,OBUF,
     X                  NX,NY,3,BAD,NST)
            IF(NST.NE.0) GO TO 90
  458    CONTINUE
         GO TO 470
  459    CONTINUE
C
C        PROCEED WITH CALCULATION AT THIS LEVEL
C
         ZLEV=HEIGHT(L)
         RHOC=RHOFUN(   L  ,ALPHA)
         RHOP=RHOFUN(L-IDIR,ALPHA)
C
         DO 460 I=1,4
            CALL FETCHD(IEDFL,ID,L,IFLD(I),IBUF,RBUF(1,I),
     X                  NIX,NIY,3,BAD,RLEV,NST)
            IF(NST.NE.0) GO TO 90
  460    CONTINUE
C
         IF(LVTON) THEN
C                                          FETCH DBZ FIELD (FALLSPEED)
C
            CALL FETCHD(IEDFL,ID,L,IDBZ,IBUF,RBUF(1,5),
     X                  NIX,NIY,3,BAD,RLEV,NST)
            IF(NST.NE.0) GO TO 90
            AVDEN=VTCON(1)*EXP(VTCON(3)*ALPHA*ZLEV)
         END IF
         DO 461 I=1,NPLANE
C                                          IF EWU,EWV ARE BAD U,V ARE TOO!
C
            IF(RBUF(I,3).EQ.BAD) RBUF(I,1)=BAD
            IF(RBUF(I,4).EQ.BAD) RBUF(I,2)=BAD
C
            IF(LVTON) THEN
C                                          FALLSPEED INTIIALIZATION
C
               IF(RBUF(I,5).NE.BAD) THEN
                  VT = -AVDEN*10.0**(0.1*VTCON(2)*RBUF(I,5))
                  IF(RBUF(I,1).NE.BAD)
     X               RBUF(I,1)=RBUF(I,1)+VT*RBUF(I,3)
                  IF(RBUF(I,2).NE.BAD)
     X               RBUF(I,2)=RBUF(I,2)+VT*RBUF(I,4)
               ELSE
                  RBUF(I,1)=BAD
                  RBUF(I,2)=BAD
               END IF
            END IF
C
  461    CONTINUE
C                                           COMPUTE CONV, W-EST
C
         CALL PCONVG(RBUF(1,1),RBUF(1,2),RBUF(1,5),NX,NY,3,XYDELI,BAD,
     X               CSP,IACTC)
         CALL CONFLD(RBUF(1,6),NPLANE,BAD)
         IF(L.NE.LEV) THEN
C                                           PAST INITIAL LEVEL
C
            CALL DWITER(OBUF,RBUF(1,3),RBUF(1,4),
     X                  RBUF(1,5),RBUF(1,6),RBUF(1,7),RBUF(1,8),
     X                  RBUF(1,9),RBUF(1,10),NX,NY,XYDELI,DZH,
     X                  RHOC,RHOP,C2RD,BAD,ITER,DMN,KST,L,ZLEV)
C            WRITE(IPR,156) L,ZLEV,ITER,DMN,IYESNO(KST+1)
C  156       FORMAT(1X,I3,F12.2,3X,I6,3X,E9.2,4X,A8)
         END IF
C
         IF(IDIR*(KINTZ-L).GE.0) THEN
C                                           INITIALIZE BOUNDARY
            IF(IMET.EQ.1) THEN
               CON=CONI
               CALL CONFLD(RBUF(1,8),NPLANE,CON)
            ELSE IF(IMET.EQ.2) THEN
               CON=CONI*IDIR
               DO 462 I=1,NPLANE
                 IF(RBUF(I,5).NE.BAD) THEN
                    RBUF(I,8)=RBUF(I,5)*CON
                 ELSE
                    RBUF(I,8)=BAD
                 END IF
  462          CONTINUE
            ELSE
               CALL FETCHD(IEDFL,ID,L,INIF,IBUF,RBUF(1,8),
     X                     NIX,NIY,3,BAD,RLEV,NST)
               IF(NST.NE.0) GO TO 90
            END IF
            DO 463 I=1,NPLANE
               IF(RBUF(I,6).EQ.BAD.AND.RBUF(I,5).NE.BAD) THEN
                  RBUF(I,6)=RBUF(I,8)
               END IF
  463       CONTINUE
         END IF
C
C                                           PUT EVERYTHING IN ITS PLACE
C                                           FOR NEXT LEVEL
         DO 464 I=1,NPLANE
            RBUF(I, 7)=RBUF(I,5)
            RBUF(I, 8)=RBUF(I,6)
            RBUF(I, 9)=RBUF(I,3)
            RBUF(I,10)=RBUF(I,4)
            IF(RBUF(I,6).NE.BAD) THEN
               RBUF(I,1)=RBUF(I,1)+RBUF(I,6)*RBUF(I,3)
               RBUF(I,2)=RBUF(I,2)+RBUF(I,6)*RBUF(I,4)
            ELSE
               RBUF(I,1)=BAD
               RBUF(I,2)=BAD
            END IF
  464    CONTINUE
C
C        WRITE RECOMPUTED (U,V,W) COMPONENTS OUT TO DISK
C
         CALL PLACED(IEDFL,ID,L,JUVW(1),IBUF,RBUF(1,1),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
         CALL PLACED(IEDFL,ID,L,JUVW(2),IBUF,RBUF(1,2),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
         CALL PLACED(IEDFL,ID,L,JUVW(3),IBUF,RBUF(1,6),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
  470 CONTINUE
C
   90 CONTINUE
      CALL SHOEDF(IPR)
C
C     NORMAL TERMINATION
C
      RETURN
      END
