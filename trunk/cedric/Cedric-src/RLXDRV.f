      SUBROUTINE RLXDRV(KRD,IBUF,OBUF,RBUF,IPR,NST)
C
C        DRIVER TO RECOMPUTE (U,V) COMPONENTS USING A VARIATIONALLY
C        ADJUSTED W. ALGORITHM MINIMIZES THE DIFFERENCE BETWEEN
C        THE HORIZONTAL AND VERTICAL CONVERGENCE AT EACH LEVEL.
C

      INCLUDE 'CEDRIC.INC'
      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXPLN,6),IFLD(3),
     X          JUV(2),NEWUV(2),RHO(3),CUVPAR(3),ZLWHR(2),
     X          XYDELI(2),LIMZ(2)
      CHARACTER*8 LIMUSR(2),MCRIT,IALPHA,CTEMP
      CHARACTER*1 CIBL
      CHARACTER*(*) KRD(10)
      LOGICAL NEWUV
      CHARACTER IDWT*20,IUNITY*6,IYESNO(3)*8
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,NAMOUF(4,2),NAMINF(4,3),ICHK
      CHARACTER*1 IAST,IASTCK
      CHARACTER*8 METDEF,METWGT
      EQUIVALENCE (NCX(1),NX), (NCX(2),NY), (NCX(3),NZ)
      EQUIVALENCE (LIMZ(1),K1), (LIMZ(2),K2)
      DATA IUNITY/'UNITY'/
      DATA IYESNO/'   YES','   NO',' NO DATA'/
      DATA RELAX/1.0/
      DATA IAST/'*'/
      DATA METDEF/'Z* 0.1  '/
      DATA CIBL/' '/
C
      RHOFUN(L,ALPHA) = EXP(-ALPHA*(CSP(1,3)+CSP(3,3)*(L-1)))
C
C                                      INITIALIZATION --DECODE COMMAND
C
      READ (KRD,101)NAMOUF,NAMINF,METWGT,MCRIT,LIMUSR
C  101 FORMAT(8X,20A2,4A8)
 101  FORMAT(/4A2/4A2/4A2/4A2/4A2/A8/A8/A8/A8)
      IF(METWGT.EQ.CIBL) METWGT=METDEF
      WRITE (CTEMP,100)METWGT
 100  FORMAT(A8)
      READ (CTEMP,102)J,IASTCK,IALPHA
  102 FORMAT(2A1,A6)
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
C                                      ITERATION CRITERIA
      IF(CSP(3,1).NE.CSP(3,2)) CALL CEDERX(523,0)
      READ (MCRIT,105)CRIT
  105 FORMAT(F8.0)
      CRIT1=IFIX(CRIT)
      CRIT2=ABS(CRIT)-ABS(CRIT1)
      ISGN=ISIGN(1,INT(CRIT1))
      CRIT1=BNDVAL(ABS(CRIT1),1.,25.,10.,0)
      CRIT2=BNDVAL(CRIT2,0.005,0.99,0.1,0)
      CRIT1=CRIT1*ISGN
      ITRMAX=NINT(CRIT1)
      ERRMAX=CRIT2
      CUVPAR(1)=RELAX
      CUVPAR(2)=ERRMAX
      CUVPAR(3)=ITRMAX
C                                      DECODE LOWER/UPPER LIMITS
C                                      LIMZ(1) AND LIMZ(2) EQVD TO K1/K2
      LIMZ(1)=1
      LIMZ(2)=NZ
      IF(NZ.GT.1) THEN
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
      ELSE
         CALL CEDERX(570,1)
         RETURN
      END IF
      LIMZ(1)=MAX0(LIMZ(1),1)
      LIMZ(2)=MIN0(LIMZ(2),NZ)
      IF(LIMZ(1).GT.LIMZ(2)) THEN
C                                      SWITCH THEM IF BACKWARDS
         I=LIMZ(1)
         LIMZ(1)=LIMZ(2)
         LIMZ(2)=I
      END IF
      ZLWHR(1)=CSP(1,3)+(K1-1)*CSP(3,3)
      ZLWHR(2)=CSP(1,3)+(K2-1)*CSP(3,3)
C                                      DETERMINE FIELD PRESENCE
      DO 25 L=1,3
C                                         INPUT  FIELDS
         IFLD(L)=LOCFLDID(NAMINF(1,L),ID(176),5,NFL,4)
         IF(IFLD(L).EQ.0) THEN
            CALL CEDERX(501,1)
            RETURN
         END IF
         IF(L.LE.2) THEN
C                                         OUTPUT FIELDS
            I=IFLD(L)
            IV=MAPVID(I,2)
            JSCL=SCLFLD(IV)
            JNEW=IADFLD(NAMOUF(1,L),JSCL,IPR)
            IF (JNEW.LT.0) RETURN
            NEWUV(L)=(JNEW.NE.0)
         END IF
   25 CONTINUE
C                                      FIELD INDICES
      DO 26 L=1,3
         IFLD(L)=LOCFLDID(NAMINF(1,L),ID(176),5,NFL,4)
         IF(L.GT.2) GO TO 26
          JUV(L)=LOCFLDID(NAMOUF(1,L),ID(176),5,NFL,4)
   26 CONTINUE
      WRITE(IPR,151) ((NAMINF(I,J),I=1,4),J=1,3),
     X               ((NAMOUF(I,J),I=1,4),J=1,2),IDWT,ERRMAX,ITRMAX,
     X                 LIMZ,ZLWHR,LABAXS(3,1)
  151 FORMAT(/3X,' (U,V) RECOMPUTATION USING VARIATIONALLY ADJUSTED',
     X           ' (W) -  PARAMETERS ...'
     X /8X,'1-    U COMPONENT  INPUT FIELD NAME: ',4A2
     X /8X,'2-    V     "        "     "    "  : ',4A2
     X /8X,'3- VERTICAL MOTION   "     "    "  : ',4A2
     X /8X,'4-    U COMPONENT OUTPUT FIELD NAME: ',4A2
     X /8X,'5-    V COMPONENT    "     "    "  : ',4A2
     X /8X,'6-     DENSITY WEIGHTING TO BE USED: ',A20
     X /8X,'7-  MAX. ACCEPTABLE MEAN DIFFERENCE: ',F5.3
     X /8X,'8-  MAX. NUMBER OF ITERATIONS/LEVEL: ',I4
     X /3X,'RECOMPUTATION WILL BE PERFORMED USING LEVELS  (',
     X     I2,' -',I3,')   (',F7.2,' THRU ',F7.2,' ',A4,')')
C         WRITE(IPR,154)AXNAM(3),LABAXS(3,1)
C  154    FORMAT(/4X,A1,' LEVEL (',A4,')   ITER #     N     MEAN DIFF   ',
C     X          '      STDV     MIN DIFF     MAX DIFF      CONVERGED')
C
C        SET UP DATA STRUCTURES AND INITIATE CALCULATION
C         RBUF STORAGE IS MANAGED AS FOLLOWS...
C          1- W (LOWER),   2- W(CURRENT), 3- W(UPPER), 4- U COMPONENT
C          5- V COMPONENT, 6- CONV/DERR SCRATCH BUFFER
C
      XYDELI(1)=1./CSP(3,1)
      XYDELI(2)=1./CSP(3,2)
      KOLD=1
      KNOW=2
      KNEW=3
C
C        INITIALIZE CURRENT AND LOWER W FIELDS
C
      CALL FETCHD(IEDFL,ID,K1,IFLD(3),IBUF,RBUF(1,KNOW),
     X            NIX,NIY,3,BAD,RLEV,NST)
      IF(NST.NE.0) GO TO 90
      RHO(KNOW)=RHOFUN(K1,ALPHA)
      IF(K1.LE.1) THEN
         CALL COPRX(RBUF(1,KOLD),RBUF(1,KNOW),NPLANE)
         RHO(KOLD)=RHO(KNOW)
      ELSE
         CALL FETCHD(IEDFL,ID,K1-1,IFLD(3),IBUF,RBUF(1,KOLD),
     X               NIX,NIY,3,BAD,RLEV,NST)
         IF(NST.NE.0) GO TO 90
         RHO(KOLD)=RHOFUN(K1-1,ALPHA)
      END IF
C
C        LOOP FOR EACH LEVEL IN THE VOLUME
C
      DO 40 L=1,NZ
         IF(L.LT.K1.OR.L.GT.K2) THEN
C
C           DON'T PROCESS THIS LEVEL
C
            DO 30 I=1,2
               IF(NEWUV(I)) THEN
                  CALL PLACED(IEDFL,ID,L,JUV(I),IBUF,OBUF,
     X                        NX,NY,3,BAD,NST)
                   IF(NST.NE.0) GO TO 90
               END IF
   30 CONTINUE
            GO TO 39
         END IF
C
C           PROCESS THIS LEVEL-  FETCH U,V AND W (UPPER)
C
         CALL FETCHD(IEDFL,ID,L,IFLD(1),IBUF,RBUF(1,4),
     X               NIX,NIY,3,BAD,RLEV,NST)
         IF(NST.NE.0) GO TO 90
         CALL FETCHD(IEDFL,ID,L,IFLD(2),IBUF,RBUF(1,5),
     X               NIX,NIY,3,BAD,RLEV,NST)
         IF(NST.NE.0) GO TO 90
         IF(L.GE.NZ) THEN
            CALL COPRX(RBUF(1,KNEW),RBUF(1,KNOW),NPLANE)
            RHO(KNEW)=RHO(KNOW)
         ELSE
            CALL FETCHD(IEDFL,ID,L+1,IFLD(3),IBUF,RBUF(1,KNEW),
     X                  NIX,NIY,3,BAD,RLEV,NST)
            IF(NST.NE.0) GO TO 90
            RHO(KNEW)=RHOFUN(L+1,ALPHA)
         END IF
         PNORM=1./RHO(KNOW)
         ZLEV=CSP(1,3)+(L-1)*CSP(3,3)
         IF(L.EQ.1.OR.L.EQ.NZ) THEN
            DZHPN= (1.0/CSP(3,3)) * PNORM
         ELSE
            DZHPN= (0.5/CSP(3,3)) * PNORM
         END IF
C
C           COMPUTE DW/DZ AND PUT INTO W (LOWER) BUFFER
C
         DO 35 I=1,NPLANE
            IF(RBUF(I,KNEW).NE.BAD.AND.RBUF(I,KOLD).NE.BAD) THEN
               RBUF(I,KOLD)=( (RBUF(I,KNEW)*RHO(KNEW)) -
     X                        (RBUF(I,KOLD)*RHO(KOLD)) ) *DZHPN
            ELSE IF(RBUF(I,KNEW).EQ.BAD.AND.
     X              RBUF(I,KNOW).NE.BAD.AND.RBUF(I,KOLD).NE.BAD) THEN
               RBUF(I,KOLD)=( (RBUF(I,KNOW)*RHO(KNOW)) -
     X                        (RBUF(I,KOLD)*RHO(KOLD)) ) *(DZHPN * 2.0)
            ELSE IF(RBUF(I,KOLD).EQ.BAD.AND.
     X              RBUF(I,KNEW).NE.BAD.AND.RBUF(I,KNOW).NE.BAD) THEN
               RBUF(I,KOLD)=( (RBUF(I,KNEW)*RHO(KNEW)) -
     X                        (RBUF(I,KNOW)*RHO(KNOW)) ) *(DZHPN * 2.0)
            ELSE
               RBUF(I,KOLD)=BAD
            END IF
   35 CONTINUE
C
C           ITERATE UNTIL CONVERGENCE OR FAILURE
C
         CALL RELXUV(RBUF(1,4),RBUF(1,5),RBUF(1,6),RBUF(1,KOLD),
     X               XYDELI,NX,NY,CUVPAR,BAD,ITER,DMN,KST,CSP,L,ZLEV)
C
C         WRITE(IPR,156) L,ZLEV,ITER,DMN,IYESNO(KST+1)
C  156    FORMAT(1X,I3,F12.2,3X,I6,3X,E9.2,4X,A8)
C
C           WRITE OUT THE U,V OUTPUT FIELDS
C
         CALL PLACED(IEDFL,ID,L,JUV(1),IBUF,RBUF(1,4),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
         CALL PLACED(IEDFL,ID,L,JUV(2),IBUF,RBUF(1,5),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
C
C           CIRCULATE THE BUFFER POINTERS
C
         KTEMP=KOLD
         KOLD=KNOW
         KNOW=KNEW
         KNEW=KTEMP
C
   39    CONTINUE
C
   40 CONTINUE
C
   90 CONTINUE
      CALL SHOEDF(IPR)
C
C        NORMAL TERMINATION
C
      RETURN
      END
