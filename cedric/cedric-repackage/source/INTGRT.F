      SUBROUTINE INTGRT(KRD,IBUF,OBUF,RBUF,IPR,NST,ICORD)
C     
C     DRIVER FOR CALCULATION OF CONVERGENCE AND VERTICAL AIR MOTION
C     INTEGRATION.
C     
      INCLUDE 'CEDRIC.INC'
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXPLN,5),IFLD(2),
     X     IOPTAB(2),METB(2),INIF(2),CONI(2),
     X     NUV(2),XYDELI(2),LIMZ(2)
      CHARACTER*8 LIMUSR(2),INVUSR(2),INIVAL(2)
      CHARACTER*8 IALPHA,MBUSR(2),INICOD(3),INITYP(4),METUSR,METDEF
      CHARACTER*(*) KRD(10)
      CHARACTER INDIR(3)*12,IDWT*35,IUNITY*6
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      CHARACTER*2 NAMINF(4,2),NAMOUF(4),INIFLD(4,2),ICHK
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      EQUIVALENCE (LIMZ(1),K1),(LIMZ(2),K2)
      EQUIVALENCE (METB(1),IMU),(METB(2),IMD)
      CHARACTER*1 IOPTAB,JCHAR,NCMD,INDCOD(3),IASTCK,IAST
      CHARACTER*2 IBL
      DATA IUNITY/'UNITY'/
      DATA IOPTAB/'C','I'/
      DATA INICOD/'CO','FR','FI'/
      DATA INITYP/'CONSTANT','FRACTION','   FIELD','--------'/
      DATA INDIR/'UPWARD','DOWNWARD','VARIATIONAL'/
      DATA INDCOD/'U','D','V'/
      DATA IAST/'*'/
      DATA METDEF/'V* 0.1  '/
      DATA NUV/1HU,1HV/
      DATA ISCL/100/
      DATA IBL/'  '/
      CHARACTER*1 CIBL
      CHARACTER*2 CIBL2
      CHARACTER*4 CTEMP4
      CHARACTER*8 CTEMP
      CHARACTER*9 SPACE(2)
      DATA SPACE/'CARTESIAN', 'COPLANE'/
      DATA CIBL,CIBL2/' ','  '/
      DATA DTR/0.0174533/
      RHOFUN(L,ALPHA) = EXP(-ALPHA*(CSP(1,3)+CSP(3,3)*(L-1)))
      RHOFNCO(L,ALPHA,K)= EXP(-ALPHA*(CSP(1,1)+CSP(3,1)*(K-1))*
     X     SIN((CSP(1,3)+CSP(3,3)*(L-1))*DTR))
      
C     
C     DETERMINE COORDINATE SYSTEM FOR CALCULATIONS
C     
      IF (ICORD.EQ.0) THEN
         WRITE(CTEMP4,5)ID(16),ID(17)
 5       FORMAT(2A2)
         IF (CTEMP4.EQ.'COPL') THEN
            IACTC=1
         ELSE 
            IACTC=0
         END IF
      ELSE IF (ICORD.EQ.1) THEN
         IACTC=0
      ELSE IF (ICORD.EQ.2) THEN
         IACTC=1
      ELSE
         WRITE(*,*)'***INVALID COORD SYSTEM IN INTGRT***'
         CALL FLUSH_STDOUT
      END IF
      
C     
C     INITIALIZATION
C     
      READ (KRD,100)NCMD
      print *,'INTGRT: ',(KRD(I),I=1,10)
 100  FORMAT(A1)
      IOPT=IFINDC(NCMD,IOPTAB,2,0)
      GO TO (10,20), IOPT
 10   CONTINUE
C     
C     CALCULATE CONVERGENCE
C     
      READ (KRD,101)NAMOUF,NAMINF,RDER
C     101 FORMAT(8X,12A2,F8.0)
 101  FORMAT(/4A2/4A2/4A2/F8.0)
      IF (IACTC.EQ.1) WRITE(*,113)
 113  FORMAT(/,5X,'CONVERGENCE BEING CALCULATED IN COPLANE SPACE')
      NDER=RDER
      IF(NDER.NE.3.AND.NDER.NE.5) NDER=3
      I=IADFLD(NAMOUF,ISCL,IPR)
      IF (I.LT.0) RETURN
      JFLD=LOCFLDID(NAMOUF,ID(176),5,NFL,4)
      DO 11 I=1,2
         IFLD(I)=LOCFLDID(NAMINF(1,I),ID(176),5,NFL,4)
         IF(IFLD(I).EQ.0) THEN
            CALL CEDERX(501,1)
            RETURN
         END IF
 11   CONTINUE
C     IF(ID(40)/ID(69).NE.90) CALL CEDERX(560,0)
      WRITE(IPR,102) ((NAMINF(I,J),I=1,4),J=1,2),(NAMOUF(I),I=1,4),NDER,
     X               SPACE(IACTC+1)
 102  FORMAT(/3X,'CONVERGENCE COMPUTATION-  PARAMETERS ...'/
     X     8X,'1-  U COMPONENT INPUT FIELD NAME: ',4A2/
     X     8X,'2-  V COMPONENT INPUT FIELD NAME: ',4A2/
     X     8X,'3- CONVERGENCE OUTPUT FIELD NAME: ',4A2/
     X     8X,'4-    (3 OR 5) POINT DERIVATIVES: ',I4/
     X     8X,'5-      (CART. OR COPLANE) SPACE: ',A9)
      IF(CSP(3,1).NE.CSP(3,2)) CALL CEDERX(523,0)
      XYDELI(1)=1./CSP(3,1)
      XYDELI(2)=1./CSP(3,2)
C     
C     LOOP FOR EACH LEVEL
C     
      DO 19 LEV=1,NZ
         DO 17 I=1,2
            CALL FETCHD(IEDFL,ID,LEV,IFLD(I),IBUF,RBUF(1,I),
     X           NIX,NIY,3,BAD,RLEV,NST)
 17      CONTINUE
         CALL PCONVG(RBUF(1,1),RBUF(1,2),OBUF,NX,NY,NDER,XYDELI,BAD,CSP,
     X        IACTC)
         CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,OBUF,NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
 19   CONTINUE
      GO TO 90
 20   CONTINUE
C     
C     VERTICAL INTEGRATION SECTION
C     
      
      IF (IACTC.EQ.1) WRITE(*,117)
 117  FORMAT(/,5X,'INTEGRATION BEING PERFORMED IN COPLANE SPACE')
      READ (KRD,103)NAMOUF,(NAMINF(I,1),I=1,4),METUSR,MBUSR(1),
     X     INVUSR(1),MBUSR(2),INVUSR(2),LIMUSR
C     103 FORMAT(8X,8A2,A8,A2,6X,A8,A2,6X,3A8)
 103  FORMAT(/4A2/4A2/A8/A2/A8/A2/A8/A8/A8)
      IF(METUSR.EQ.IBL) METUSR=METDEF
      WRITE (CTEMP,123)METUSR
      READ (CTEMP,108)JCHAR,IASTCK,IALPHA
  108 FORMAT(2A1,A6)
      METINT=IFINDC(JCHAR,INDCOD,3,0)
      IF(METINT.LE.0) METINT=3
      print *,'INTGRT: ',metusr,mbusr(1),invusr(1),mbusr(2),invusr(2),
     +     limusr,metint
C
C     SET UP DENSITY WEIGHTING
C
      IYN=1
      ALPHA=0.0
      IDWT=IUNITY
      IF(IASTCK.EQ.IAST) THEN
         READ(IALPHA,109)ALPHA
  109    FORMAT(F6.0)
         ALPHA=ABS(ALPHA)
         IF(ALPHA.GT.1.0) THEN
            CALL CEDERX(568,1)
            RETURN
         END IF
         IYN=2
         IF(ALPHA.LE.0.0) ALPHA=0.1
         IF (IACTC.EQ.0) THEN
            WRITE (IDWT,114)ALPHA
 114        FORMAT('RHO=EXP( -Z *',F5.2,' )')
         ELSE IF (IACTC.EQ.1) THEN
            WRITE (IDWT,115)ALPHA
 115        FORMAT('RHO=EXP(-XSIN(C)*',F5.2,' )')
         END IF

      END IF
C
C     ESTABLISH PARAMETERS FOR UPWARD/DOWNWARD DIRECTIONS DEPENDING UPON
C     THE METHOD: 1-UP, 2-DOWN, 3-BOTH/VARIATIONAL
C
      IUD=MOD(METINT-1,2)+1
      M1=IUD
      M2=MIN0(METINT,2)
      J=0
      DO 18 I=M1,M2
         J=J+1
C
C        ESTABLISH BOUNDARY CONDITION FOR THIS DIRECTION:
C                  1-CONSTANT, 2-FRACTION, 3-FIELD
C
         IMET=IFINDC(MBUSR(J),INICOD,3,0)
         print *,'INTGRT: mbusr(j),inicod,imet=',j,mbusr(j),inicod,imet
         IF(IMET.EQ.0) THEN
            CALL CEDERX(524,1)
            RETURN
         END IF
         METB(I)=IMET
         IF(IMET.EQ.3) THEN
            WRITE (CTEMP,123)INVUSR(J)
            READ (CTEMP,104)(INIFLD(M,I),M=1,4)
  104       FORMAT(4A2)
            INIF(I)=LOCFLDID(INIFLD(1,I),ID(176),5,NFL,4)
            IF(INIF(I).EQ.0) THEN
               CALL CEDERX(525,1)
               RETURN
            END IF
            READ(INVUSR(J),127)INIVAL(I)
 127        FORMAT(A8)
c            INIVAL(I)=INVUSR(J)
            CONI(I)=0.0
         ELSE
            INIF(I)=0
            WRITE (CTEMP,123)INVUSR(J)
            READ (CTEMP,105)CONI(I)
  105       FORMAT(F8.0)
            WRITE (CTEMP,106)CONI(I)
            READ (CTEMP,123)INIVAL(I)
  106       FORMAT(F8.2)
         END IF
C
   18 CONTINUE
C


      IF(METINT.NE.3) THEN
         WRITE (CTEMP,123)MBUSR(2)
 123     FORMAT(A8)
         READ (CTEMP,104)(NAMINF(M,2),M=1,4)
         I=MOD(IUD,2)+1
         METB(I)=4
         INIVAL(I)=INITYP(4)
         INIF(I)=0
         CONI(I)=0.0
      ELSE
         CALL CONCHAR(NAMINF(1,2),4,CIBL2)
      END IF

C
C
C     DECODE THE LIMITS OF THE VERTICAL INTEGRATION
C     LIMZ(1) AND LIMZ(2) ARE EQUIVALENCED TO K1 AND K2
C
      LIMZ(1)=1
      LIMZ(2)=NZ
      IF(NZ.GT.1) THEN
         DO 21 I=1,2
            IF(LIMUSR(I).EQ.CIBL) GO TO 21
            READ (LIMUSR(I),107)ICHK,VAL
  107       FORMAT(A2,F6.0)
            IF(ICHK.EQ.'I=') THEN
C
C              INDEX SPACE
C
               LIMZ(I)=VAL
            ELSE IF(ICHK.EQ.'D=') THEN
C
C              DISTANCE SPACE
C
               LIMZ(I)=NINT((VAL-CSP(1,3))/CSP(3,3)+1.0)
            ELSE
               CALL CEDERX(567,1)
               RETURN
            END IF
   21    CONTINUE
      END IF
      LIMZ(1)=MAX0(LIMZ(1),1)
      LIMZ(2)=MIN0(LIMZ(2),NZ)
      IF(LIMZ(1).GT.LIMZ(2)) THEN
C
C        REVERSE K1 AND K2 -HELP THE POOR STUPID USER OUT
C
         I=LIMZ(1)
         LIMZ(1)=LIMZ(2)
         LIMZ(2)=I
      END IF
      ZBEG=CSP(1,3)+(K1-1)*CSP(3,3)
      ZEND=CSP(1,3)+(K2-1)*CSP(3,3)
      IFLD(1)=LOCFLDID(NAMINF(1,1),ID(176),5,NFL,4)
      IF(IFLD(1).EQ.0) THEN
         CALL CEDERX(501,1)
         RETURN
      END IF
      I=IFLD(1)
      IV=MAPVID(I,2)
      JSCL=SCLFLD(IV)
      JNEW=IADFLD(NAMOUF,JSCL,IPR)
      IF (JNEW.LT.0) RETURN
      IF(JNEW.NE.0) IFLD(1)=LOCFLDID(NAMINF(1,1),ID(176),5,NFL,4)
      IF(INIF(1).NE.0) INIF(1)=LOCFLDID(INIFLD(1,1),ID(176),5,NFL,4)
      IF(INIF(2).NE.0) INIF(2)=LOCFLDID(INIFLD(1,2),ID(176),5,NFL,4)
      JFLD=LOCFLDID(NAMOUF,ID(176),5,NFL,4)
      WRITE(IPR,110) (NAMINF(I,1),I=1,4),(NAMOUF(I),I=1,4),INITYP(IMU),
     X                INIVAL(1),INITYP(IMD),INIVAL(2),INDIR(METINT),
     X                IDWT,LIMZ,ZBEG,ZEND,LABAXS(3,1),SPACE(IACTC+1)
  110 FORMAT(/3X,'VERTICAL INTEGRATION-  PARAMETERS ...'/
     X 8X,'1-          INPUT FIELD NAME: ',4A2/
     X 8X,'2-         OUTPUT FIELD NAME: ',4A2/
     X 8X,'3- INITIALIZATION ( UPWARD ): ',A8,'  (',A8,')'/
     X 8X,'4- INITIALIZATION (DOWNWARD): ',A8,'  (',A8,')'/
     X 8X,'5-     METHOD OF INTEGRATION: ',A12/
     X 8X,'6-         DENSITY WEIGHTING: ',A35/
     X/3X,'INTEGRATION WILL BE PERFORMED USING LEVELS  (',
     X    I2,' -',I3,')   (',F7.2,' THRU ',F7.2,A4,')'/
     X 8X,'7-  (CART. OR COPLANE) SPACE: ',A9)
      IDIR=(1.5-IUD)*3.0
         LEV=K1
         IF(IDIR.LT.0) LEV=K2
         ZLEV=CSP(1,3)+(LEV-1)*CSP(3,3)
C
C        PROCEED WITH INTEGRATION
C
      ZD=CSP(3,3)
      KOLD=1
      KNEW=2
      DZH=(0.5*ZD)*IDIR
      IF (IACTC.EQ.1) DZH=DZH*DTR
      PFAC=RHOFUN(LEV,ALPHA)/RHOFUN(LEV+IDIR,ALPHA)
C
C        INITIALIZATION OF BOUNDARY
C
      CALL FETCHD(IEDFL,ID,LEV,IFLD(1),IBUF,RBUF(1,KOLD),
     X            NIX,NIY,3,BAD,RLEV,NST)
      IF(NST.NE.0) GO TO 90
      IMET=METB(IUD)
      IF(IMET.LE.2) THEN
C
C        CONSTANT/FRACTION TO BE SET
C
      CON=CONI(IUD)
      IF(IMET.EQ.2) CON=CON*IDIR
         DO 31 I=1,NPLANE
            RBUF(I,3)=BAD
            RBUF(I,4)=BAD
            IF(RBUF(I,KOLD).EQ.BAD) GO TO 31
              RBUF(I,4)=ZLEV
              RBUF(I,3)=CON
            IF(IMET.EQ.1) GO TO 31
              RBUF(I,3)=RBUF(I,KOLD)*CON
   31    CONTINUE
      ELSE
C
C        FIELD TO BE USED
C
         CON=BAD
         IF (INIF(IUD).EQ.0) THEN
            CALL CEDERX(594,1)
            RETURN
         END IF
         CALL FETCHD(IEDFL,ID,LEV,INIF(IUD),IBUF,OBUF,
     X               NIX,NIY,3,BAD,RLEV,NST)
         IF(NST.NE.0) GO TO 90
         DO 33 I=1,NPLANE
            RBUF(I,3)=BAD
            RBUF(I,4)=BAD
            IF(RBUF(I,KOLD).EQ.BAD.OR.OBUF(I).EQ.BAD) GO TO 33
              RBUF(I,3)=OBUF(I)
              RBUF(I,4)=ZLEV
   33    CONTINUE
      END IF
      CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF(1,3),NX,NY,3,BAD,NST)
      IF(NST.NE.0) GO TO 90
      IF(K1.EQ.K2) GO TO 90
      L=0
      IF(IDIR.LT.0) L=NZ+1
      WTFRAC= -1.0*CONI(2)
      IF(METINT.EQ.3) THEN
C
C        INITIALIZATION FOR VARIATIONAL METHOD  --SAVING FRACTIONAL
C                       CONDITION IN CASE IT IS NEEDED
C
         DO 34 I=1,NPLANE
            IF(RBUF(I,3).EQ.BAD) THEN
               RBUF(I,5)=BAD
            ELSE
               RBUF(I,5)=RBUF(I,KOLD)*WTFRAC
            END IF
   34    CONTINUE
C
      END IF
C
C
C        LOOP FOR EACH REMAINING LEVEL TO BE INTEGRATED
C
      DO 70 LOOP=1,NZ
         L=L+IDIR
         ZLEV=CSP(1,3)+(L-1)*CSP(3,3)
         IF(L.EQ.LEV) GO TO 70
         IF(L.GE.K1.AND.L.LE.K2) GO TO 35
         IF(JNEW.NE.0) THEN
C
C           NEW OUTPUT FIELD- ZAP OUT UNINTEGRATED LEVELS
C
            CALL CONFLD(RBUF(1,KNEW),NPLANE,BAD)
            CALL PLACED(IEDFL,ID,L,JFLD,IBUF,RBUF(1,KNEW),
     X                  NX,NY,3,BAD,NST)
            IF(NST.NE.0) GO TO 90
         END IF
         GO TO 70
   35    CONTINUE
         IF(IMET.EQ.3) THEN
            CALL FETCHD(IEDFL,ID,L,INIF(IUD),IBUF,OBUF,
     X                  NIX,NIY,3,BAD,RLEV,NST)
            IF(NST.NE.0) GO TO 90
         END IF
         CALL FETCHD(IEDFL,ID,L,IFLD(1),IBUF,RBUF(1,KNEW),
     X               NIX,NIY,3,BAD,RLEV,NST)
         IF(NST.NE.0) GO TO 90
C
C              CONTINUE THE INTEGRATION FOR EACH (X,Y) LOCATION
C
         DO 60 J=1,NY
            DO 60 K=1,NX
               I=(J-1)*NX + K
            W=BAD
            IF(RBUF(I,KNEW).EQ.BAD) GO TO 55
C
C           GOOD CONVERGENCE VALUE
C
            IF(RBUF(I,3).NE.BAD) GO TO 50
C
C           BAD W FROM PREVIOUS LEVEL
C
            IF(RBUF(I,4).NE.BAD) GO TO 55
C
C           UNITIALIZED PATH
C
            IF(IMET.EQ.1) THEN
               W=CON
            ELSE IF(IMET.EQ.2) THEN
               W=RBUF(I,KNEW)*CON
            ELSE
               IF(OBUF(I).EQ.BAD) GO TO 55
               W=OBUF(I)
            END IF
C
            RBUF(I,3)=W
            RBUF(I,4)=ZLEV
            RBUF(I,5)=RBUF(I,KNEW)*WTFRAC
C
         GO TO 60
C
   50       CONTINUE
            IF (IACTC.EQ.0) THEN
               W=RBUF(I,3)*PFAC+DZH*(RBUF(I,KOLD)*PFAC+RBUF(I,KNEW))
            ELSE IF (IACTC.EQ.1) THEN
               PFAC=RHOFNCO(LEV,ALPHA,K)/RHOFNCO(LEV+IDIR,ALPHA,K)
               X=CSP(1,1)+(K-1)*CSP(3,1)
C
C     CHECK FOR X=0 SINGULAR CONDITION
C
               IF (X.NE.0.0) THEN
                  W=RBUF(I,3)*PFAC+(DZH*(CSP(1,1)+(K-1)*CSP(3,1))*
     X                 (RBUF(I,KOLD)*PFAC+RBUF(I,KNEW)))
               ELSE
                  W=BAD
               END IF
            END IF
            RBUF(I,5)=RBUF(I,KNEW)*WTFRAC
   55       CONTINUE
            RBUF(I,3)=W
   60    CONTINUE
         KTEMP=KOLD
         KOLD=KNEW
         KNEW=KTEMP
         CALL PLACED(IEDFL,ID,L,JFLD,IBUF,RBUF(1,3),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
   70 CONTINUE
      IF(METINT.EQ.3) THEN
C
C        VARIATIONAL ADJUSTMENT-- PASS DOWNWARDS AND MODIFY INTEGRAL
C
         IUD=2
         IMET=METB(IUD)
         CALL CONFLD(RBUF(1,1),NPLANE,0.0)
         CALL CONFLD(RBUF(1,2),NPLANE,BAD)
         IF(IMET.EQ.1) THEN
            CALL CONFLD(RBUF(1,5),NPLANE,CONI(IUD))
         END IF
C
C        LOOP FOR EACH LEVEL REQUESTED
C
         L=K2+1
         DO 77 LOOP=K1,K2
            L=L-1
            ZLEV=CSP(1,3)+(L-1)*CSP(3,3)
            PLEV=RHOFUN(L,ALPHA)
            PNORM=1.0/PLEV
            IF(IMET.EQ.3) THEN
C
C              FETCH UPPER BOUNDARY FIELD
C
               CALL FETCHD(IEDFL,ID,L,INIF(IUD),IBUF,RBUF(1,5),
     X                     NIX,NIY,3,BAD,RLEV,NST)
               IF(NST.NE.0) GO TO 90
            END IF
C
C           FETCH THE ESTIMATE OF -W- GENERATED BY THE UPWARD INTEGRATION
C
            CALL FETCHD(IEDFL,ID,L,JFLD,IBUF,RBUF(1,3),
     X                  NIX,NIY,3,BAD,RLEV,NST)
            IF(NST.NE.0) GO TO 90
C
C           MODIFY THE (X,Y) LOCATIONS OF THIS PLANE
C
            DO 75 J=1,NY
               DO 75 K=1,NX
                  I=(J-1)*NX + K
               IF(RBUF(I,3).EQ.BAD) GO TO 75
C
C              ---GOOD VALUE OF W AT THIS POINT
C                 NO NEED TO CORRECT IF WE'RE AT THE LOWEST LEVEL---
C
               IF(RBUF(I,4).GT.ZLEV) GO TO 75
C
               IF(RBUF(I,1).EQ.0.0) THEN
C
C                 UNITIALIZED PATH DOWNWARD --CHECK FOR UPPER BOUNDARY
C
                  IF(RBUF(I,4).EQ.ZLEV.OR.RBUF(I,5).EQ.BAD) THEN
C
C                    FLAG SINGLETON CONVERGENCE VALUE
C                         OR
C                    NO UPPER BOUNDARY CONDITION AT THIS LEVEL
C                       (THIS CAN ONLY HAPPEN WITH FIELD INIT)
C
                     RBUF(I,3)=BAD
                     GO TO 75
                  END IF
C
C                 GENERATE DENOMINATOR OF THE SCALING TERM --RBUF(-,1)
C                          AND THE ( WT -WB -WK ) TERM     --RBUF(-,2)
C                 SET RESULT TO UPPER BOUNDARY AND BRANCH TO LOOP END
C
                  RBUF(I,1)=1.0
                  IF(IYN.EQ.2 .AND. IACTC.EQ.0) THEN
                     RBUF(I,1)=1./(EXP(-2.0*ALPHA*(ZLEV-RBUF(I,4)))-1.0)
                  ELSE IF(IYN.EQ.2 .AND. IACTC.EQ.1) THEN
                     X=CSP(1,1)+(K-1)*CSP(3,1)
C
C     CHECK FOR SINGULAR CONDITION AT X=0 (ON BASELINE)
C
                     IF (X.NE.0.0) THEN
                        RBUF(I,1)=1./(EXP(-2.0*ALPHA*X*
     X                       (SIN(ZLEV*DTR)-SIN(RBUF(I,4)*DTR)))-1.0)
                     ELSE
                        RBUF(I,1)=BAD
                     END IF
                  END IF
                  IF (IACTC.EQ.1) THEN
C
C     MODS FOR COPLANE COORD. SYSTEM
C
                     PLEV=RHOFNCO(L,ALPHA,K)
                     PNORM=1.0/PLEV
                  END IF
                  RBUF(I,2)=(RBUF(I,5)-RBUF(I,3))*PLEV
                  RBUF(I,3)=RBUF(I,5)
                  GO TO 75
               END IF
C
               IF(RBUF(I,4).EQ.ZLEV) GO TO 75
C
C              COMPUTE THE NUMERATOR OF THE SCALING TERM AND MODIFY
C                      THE VALUE OF THE INTEGRAL
C
               RNUM=1.0
               IF(IYN.EQ.2 .AND. IACTC.EQ.0) THEN
                  RNUM=EXP(-2.0*ALPHA*(ZLEV-RBUF(I,4)))-1.0
               ELSE IF(IYN.EQ.2 .AND. IACTC.EQ.1) THEN
                  X=CSP(1,1)+(K-1)*CSP(3,1)
                  RNUM=(EXP(-2.0*ALPHA*X*
     X                 (SIN(ZLEV*DTR)-SIN(RBUF(I,4)*DTR)))-1.0)
               END IF
               IF (IACTC.EQ.1) THEN
                  PLEV=RHOFNCO(L,ALPHA,K)
                  PNORM=1.0/PLEV
               END IF
               IF (RBUF(I,1).NE.BAD) THEN
                  RBUF(I,3)=RBUF(I,3)+(RBUF(I,1)*RBUF(I,2)*RNUM*PNORM)
               ELSE
                  RBUF(I,3)=BAD
               END IF
C
   75       CONTINUE
C
C           WRITE THE RESULTS BACK TO THE OUTPUT FIELD
C
            CALL PLACED(IEDFL,ID,L,JFLD,IBUF,RBUF(1,3),
     X                  NX,NY,3,BAD,NST)
            IF(NST.NE.0) GO TO 90
C
   77    CONTINUE
C
      GO TO 90
C
      END IF
C
C
C       SAVE TOPOGRAPHY IF A FIELD NAME HAS BEEN SPECIFIED
C
      IF(NAMINF(1,2).EQ.IBL) THEN
         WRITE(IPR,112)
  112    FORMAT(5X,'TOPOGRAPHY INFORMATION WILL NOT BE SAVED')
      ELSE
C
C       TOPOGRAPHY IS TO BE SAVED
C
      JFLD=LOCFLDID(NAMINF(1,2),ID(176),5,NFL,4)
      WRITE(IPR,111) (NAMINF(I,2),I=1,4)
  111 FORMAT(5X,'TOPOGRAPHY INFORMATION WILL BE SAVED IN ',4A2)
      IF(JFLD.EQ.0) THEN
         JNEW=IADFLD(NAMINF(1,2),ISCL,IPR)
         IF (JNEW.LT.0) RETURN
         JFLD=LOCFLDID(NAMINF(1,2),ID(176),5,NFL,4)
         CALL CONFLD(RBUF(1,3),NPLANE,BAD)
      END IF
      DO 80 LEV=1,NZ
         IF(LEV.LT.K1.OR.LEV.GT.K2) THEN
            IF(JNEW.NE.0) THEN
               CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF(1,3),
     X                     NX,NY,3,BAD,NST)
               IF(NST.NE.0) GO TO 90
            END IF
C
         ELSE
C
         CALL PLACED(IEDFL,ID,LEV,JFLD,IBUF,RBUF(1,4),
     X               NX,NY,3,BAD,NST)
         IF(NST.NE.0) GO TO 90
C
         END IF
C
   80 CONTINUE
      END IF
   90 CONTINUE
      CALL SHOEDF(IPR)
C
C        NORMAL TERMINATION
C
      RETURN
      END
