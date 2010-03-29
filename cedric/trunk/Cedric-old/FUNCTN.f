      SUBROUTINE FUNCTN(KRD,IBUF,OBUF,RBUF,NXYPLN,MAXPXY,IPR,NST,IFLAT,
     X                  LATLON)
C
C        DRIVER FOR ALGEBRAIC MANIPULATION OF CONSTANT Z-PLANES
C               UNDER WINDOWING CONTROL.
C
C          IBUF- SCRATCH BUFFER FOR READING DATA FROM DISK
C          OBUF- OUTPUT BUFFER FOR RESULTS OF CALCULATIONS
C          RBUF- STORAGE FOR HOLDING UP TO MAXPXY Z-PLANES AT A TIME
C        NXYPLN- NUMBER OF POINTS PER PLANE
C        MAXPXY- MAXIMUM NUMBER OF PLANES AVAILABLE
C           IPR- PRINT FILE
C           NST- STATUS FLAG: 0- O.K.
C          NFUN- Number of available FUNCTIONS (NFUN=73)
C        MXSTCK- Maximum number of functions allowed in current stack
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (NFUN=77,MXSTCK=60,MAXCON=4,MAXCPF=6,NP=20)
      DIMENSION IBUF(1),OBUF(1),RBUF(NXYPLN,MAXPXY),NAX(3)
      CHARACTER*(*) KRD(10)
      CHARACTER*8 IUSRSC,ILEV,INCIN(4),KFUN(NFUN)
      CHARACTER*8 CTEMP,IFNAM,ICONPF
      CHARACTER*1 CIBL,IWOP,IFLTYP,INFTYP,IASCHK,IASTSK
      CHARACTER*3 INBAD,IEND,INEND
      DATA CIBL/' '/
      DIMENSION INB(2),IAUXSTCK(3,MXSTCK),CONS(MAXCON,MXSTCK),
     X  IBNTYP(NP),MFUN(NFUN),JFUN(NFUN),CONEXT(MAXCON),
     X  ICONZ(MXSTCK),CIN(4),PFSCL(MXSTCK)
      COMMON /EDINFO/ IEDW(2,3),PEDW(2,3)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF,INIBAD(4),IRETN(6),ININFO(6,NP),IBNAM(4,NP),
     X            ISTACK(15,MXSTCK),NAMBUF(10,MAXCPF),NAMINF(4,2),
     X            NAMOUF(4),IDMODE,INUMOD,IBL
      CHARACTER*1 IFAX
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /GUI/ IFATAL,IGUISTAT
      CHARACTER LFUN(NFUN)*20
      CHARACTER*8 NAMESND(MXSTCK),SNDNAME
      LOGICAL LATLON
      EQUIVALENCE (NCX(1),NX), (NCX(2),NY), (NCX(3),NZ),
     X            (INB(1),IN1),(INB(2),IN2)
      EQUIVALENCE (NAX(1),I1),(NAX(2),I2),(NAX(3),I3)
      DATA LFUN/'DF1/DI','DF1/DJ','DF1/DI+DF2/DJ','DF1/DI-DF2/DJ',
     2 'DF1/DJ-DF2/DI','F1*DF2/DI','F1*DF2/DJ','DF1/DI*DF2/DJ',
     3 'C1*F1*F2','SQRT(F1)','C1*10**(C2*F1)','F1.ORELSE.F2',
     4 'F1*F1+F2*F2','C1*EXP(-Z*C2)*F1','C1*F1+C2*F2','C1*F1-C2*F2',
     5 'C1*F1/F2','F1/(C1*EXP(-Z*C2))','C1*F1+C2','ABS(F1)',
     6 'F1,IF,F2.NE.BAD','F1,IF,C1>F2','F1,IF,C1<F2','C1*LN(C2*F1)',
     7 'C1*EXP(F1)','C1*(F1**C2)','F1,AT,LEVEL(K+C1)','(X,Y)-(C1,C2)',
     8 'C1*Z+C2','SIN(C1*F1-C2)','F1,IF,F2.EQ.BAD','(X,Y,Z)-(C1,C2,C3)',
     9 'COS(C1*F1-C2)','C1<RANDOM<C2','MAX(F1,F2)','MIN(F1,F2)',
     X 'ATAN2(F2/F1)','AZ(XY)-(C1,C2)','EL(XYZ)-(C1,C2,C3)',
     1 'C1','BAD','C1*X+C2','C1*Y+C2','C1*MEAN(F1)','C1*LOG(C2*F1)',
     2 'F1.MODULO.C1','FLOOR(F1,C1)','CEILING(F1,C1)','PROJECT(F1,F2)',
     3 'BITCOUNT(F1)','NORMAL(C1,C2)','C1*MIN(F1)','C1*MAX(F1)',
     4 'F1:F2.NE.C1;OW:C2','F1:F2.IN.(C1,C2)','F1:F2.OUT.(C1,C2)',
     5 'C1*COUNT(F1)','MEANAX1','MEANAX2','F1,AT((I+C1),J)',
     6 'F1,AT(I,(J+C1))','F1,AT(I+C1),(J+C2)','LAT(X,Y)','LON(X,Y)',
     7 'SDEVAX1','SDEVAX2','XCART','YCART','USTOPO','ZINDEX','ZCART',
     8 'VADFLD','SOUND','THETA','THETA_E','MIXRAT','TDRY'/
CMFUN HOLDS HOW MANY CONSTANTS ARE NEEDED FOR EACH FUNCTION:c1,c2,c3
      DATA MFUN/ 0,0,0,0,0,0,0,0,
     2           1,0,2,0,0,2,2,2,
     3           1,2,2,0,0,1,1,2,
     4           1,2,1,2,2,2,0,3,
     5           2,2,0,0,0,2,3,1,
     6           0,2,2,1,2,1,1,1,
     7           1,0,2,1,1,2,2,2,
     8           1,1,1,1,1,2,2,2,
     9           1,1,2,2,2,2,3,3,
     1           3,0,0,0,0/    
CJFUN REFERS TO THE NUMBER OF INPUT FIELDS NECESSARY TO DO THE FUNCTION         
      DATA JFUN/ 1,1,2,2,2,2,2,2,
     2           2,1,1,2,2,1,2,2,
     3           2,1,1,1,2,2,2,1,
     4           1,1,1,0,0,1,2,0,
     5           1,0,2,2,2,0,0,0,
     6           0,0,0,1,1,1,1,1,
     7           2,1,0,1,1,2,2,2,
     8           1,1,1,1,1,1,0,0,
     9           0,0,0,0,0,0,0,0,
     1           0,2,2,2,2/
      DATA KFUN/'DF/DI   ','DF/DJ   ','DDI+DDJ ','DDI-DDJ ','DDJ-DDI ',
     2          '*D/DI   ','*D/DJ   ','DDI*DDJ ','*       ','SQRT    ',
     3          'TENLOG  ','ORELSE  ','SQ+SQ   ','RHOWGT  ','+       ',
     4          '-       ','/       ','RHONORM ','LINEAR  ','ABS     ',
     5          'ONLYIF  ','ONLYIFC>','ONLYIFC<','LN      ','EXP     ',
     6          'POWER   ','RELPLANE','XYDIST  ','FUNZ    ','SIN     ',
     7          'ONLYIFNO','RANGE   ','COS     ','RANDOM  ','MAX     ',
     9          'MIN     ','ATAN2   ','AZ      ','EL      ','CON     ',
     X          'BAD     ','FUNX    ','FUNY    ','PROFILE ','LOGTEN  ',
     1          'MODULO  ','FLOOR   ','CEILING ','PROJECT ','BITCOUNT',
     2          'NORMAL  ','PROFMIN ','PROFMAX ','IFNOC1C2','INSIDE  ',
     3          'OUTSIDE ','COUNT   ','MEANAX1 ','MEANAX2 ','RELI    ',
     4          'RELJ    ','RELIJ   ','LAT(X,Y)','LON(X,Y)','SDEVAX1 ',
     5          'SDEVAX2 ','XCART   ','YCART   ','USTOPO  ','ZINDEX  ',
     6          'ZCART   ','VADFLD  ','SOUND   ','THETA   ','THETA_E ',
     7          'MIXRAT  ','TDRY    '/
      DATA INUMOD,INEND,    ICONPF,INFTYP,INBAD/
     X        'NU','END','CONPLANE',   'P','BAD'/
      DATA INIBAD/'  ','BA','D ','  '/
      DATA IRETN/'  ','RE','TA','IN','ED','  '/
      DATA IASTSK/'*'/
      DATA IBL/'  '/
      DATA IDFSCL/64/
C
C        INITIALIZATION-  RESET CONSTANT PLANE COUNT
C
      CONEXT(1)=FLOAT(ID(40))/FLOAT(ID(69))
      SF=1./FLOAT(ID(68))
      CONEXT(2)=ID(41)*SF
      CONEXT(3)=ID(42)*SF
      NCPF=0
      MAXACF=MIN0(NP,MAXPXY)
C
C        DELETE ALL HELD FIELDS
C
      DO 2 I=1,MAXACF
         IBNTYP(I)=0
         DO 1 L=1,6
            ININFO(L,I)=IBL
            IF(L.GT.4) GO TO 1
               IBNAM(L,I)=IBL
    1    CONTINUE
    2 CONTINUE
C
C        RESET THE FUNCTION STACK
C           NSTACK - counter for number of functions
C           NSND   - counter for number of SOUND functions
C
      NSTACK=0
      NSND=0
      READ (KRD,100)IDMODE,DIR,IFAX,IWOP
C  100 FORMAT(8X,A2,6X,F8.0,40X,A1,7X,A1)
 100  FORMAT(/A2/F8.0//////A1/A1)
      IDIR=SIGN(1.0,DIR)
      CALL WINSET(IEDW,PEDW,IWOP)
      CALL SETAXS(NAX,IFAX,IPR)
      N1=NCX(I1)
      N2=NCX(I2)
      N3=NCX(I3)
      NPLIN=N1*N2
    5 CONTINUE
      IF(NCPF.NE.0) THEN
         J=0
         DO 8 I=1,MAXACF
            IF(IBNTYP(I).NE.2) GO TO 8
            J=J+1
            CALL COPCX(NAMBUF(1,J),IBNAM(1,I),4)
            CALL COPCX(NAMBUF(5,J),ININFO(1,I),6)
    8    CONTINUE
      END IF
C
C        BUILD UP THE FUNCTION STACK
C           NAMOUF - Name of output field
C           IFLTYP - Type of output field (P or T)
C           IASCHK - Blank or asterik (*) if user specifies scale factor
C           IUSRSC - Scaling factor specified by the user
C           IFNAM  - Name of the function
C           NAMINF - Array for input field names
C           INCIN  - Array for function constants
C
      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      READ (KRD,104)IEND,NAMOUF,IFLTYP,IASCHK,IUSRSC,
     X                   IFNAM,NAMINF,INCIN
C  104 FORMAT(A3,5X,4A2,2A1,A6,A8,8A2,4A8)
 104  FORMAT(A3/4A2/2A1,A6/A8/4A2/4A2/A8/A8/A8/A8)
      print *,'FUNCTN: namouf=',namouf,ifnam,naminf,ifltyp,incin,iend

C     END command encountered - end of current function stack
C
      IF(IEND.EQ.INEND) GO TO 60

      IF(IFNAM.EQ.'SOUND')THEN
         NSND=NSND+1
c         do i=1,4
c            do j=1,2
c               print *,'i,j,NAMINF(i,j)=',i,j,naminf(i,j)
c            end do
c         end do
         WRITE(NAMESND(NSND),106)(NAMINF(I,1),I=1,4)
         IF(NSND.GT.MXSTCK)THEN
            PRINT *,'MAX NUMBER OF SOUNDING FUNCTIONS EXCEEDED'
            STOP
         END IF
c         print *,' SOUND: namouf=',
c     +        namouf,ifnam,nsnd,'+',namesnd(nsnd),'+'
      END IF

      IF(IFNAM.EQ.ICONPF) WRITE (INCIN(1),106) (NAMINF(I,2),I=1,4)
  106 FORMAT(4A2)
      IF(INCIN(1).EQ.'X') THEN
         WRITE (INCIN(1),107)CONEXT(1)
  107    FORMAT(F8.2)
      ELSE IF(INCIN(1).EQ.'Y') THEN
         YAX=AMOD(CONEXT(1)+270.0,360.)
         WRITE (INCIN(1),107)YAX
      END IF
C
C        ESTABLISH CONSTANT INFORMATION
C
      DO 10 I=1,4
         CIN(I)=BAD
         IF(INCIN(I).EQ.INBAD) GO TO 10
         READ (INCIN(I),105)CIN(I)
 105     FORMAT(F8.0)
   10 CONTINUE
C
C        DETERMINE THE OPERATION
C
      IF(IFNAM.EQ.ICONPF) GO TO 20
      IF(IDMODE.NE.INUMOD) THEN
         IFN=IFINDC(IFNAM,KFUN,NFUN,0)
      ELSE
         WRITE (CTEMP,113)IFNAM
 113     FORMAT(A8)
         READ (CTEMP,105)FNUM
         IFN=FNUM
      END IF
      IF(IFN.LE.0.OR.IFN.GT.NFUN) THEN
         CALL CEDERX(515,1)
         GOTO 5
      END IF
      IF(LATLON .AND. (IFN.EQ.63 .OR. IFN.EQ.64)) THEN
         CALL CEDERX(515,1)
         GOTO 5
      END IF
      IF(.NOT.LATLON .AND. (IFN.EQ.67 .OR. IFN.EQ.68)) THEN
         CALL CEDERX(515,1)
         GOTO 5
      END IF
      IF(IFN.EQ.49.AND.INCIN(1).EQ.CIBL) CIN(1)=CONEXT(1)
      IF((IFN.EQ.14.OR.IFN.EQ.18).AND.(CIN(2).EQ.0.0)) CIN(2)=0.1
      GO TO 40
   20 CONTINUE
C
C        ESTABLISH A CONSTANT PLANE FIELD
C
      IF(NCPF.GE.MAXCPF) THEN
         CALL CEDERX(516,1)
         GOTO 5
      END IF
      CALL COPCX(NAMBUF(1,1),NAMOUF,4)
      I=LOCFLD(NAMBUF(1,1),NAMF,4,NFMAX,4)
      J=LOCFLD(NAMBUF(1,1),IBNAM,4,MAXACF,4)
      IF(I.NE.0.OR.J.NE.0) THEN
         CALL CEDERX(517,1)
         GOTO 5
      END IF
      K=IFIND(0,IBNTYP,MAXACF,0)
      IF(K.EQ.0) GO TO 91
      CALL COPCX(IBNAM(1,K),NAMBUF(1,1),4)
      IBNTYP(K)=2
      NCPF=NCPF+1

C     Use first input field name as the sounding variable to get
C        NAMOUF - output field name
C        IFNAM  - name of the function = 'SOUND'
C        NAMINF - name of the sounding variable
C
      IF(IFNAM.EQ.'SOUND')THEN
         print *,' FUNCT = SOUND: namouf=',namouf,ifnam,naminf
         GOTO 40
      END IF

      IF(NAMINF(1,1).EQ.IBL) THEN
         CONI=CIN(1)
         IF(CONI.EQ.BAD) THEN
            CALL COPCX(ININFO(1,K),INIBAD,4)
         ELSE
            CALL COPCX(ININFO(1,K),NAMINF(1,2),4)
         END IF
         CALL CONFLD(RBUF(1,K),NPLIN,CONI)
         WRITE(IPR,110) (IBNAM(I,K),I=1,4), CONI
  110    FORMAT(5X,4A2,' HAS BEEN INITIALIZED TO',F9.2)
      ELSE
         IFLD=LOCFLDID(NAMINF(1,1),ID(176),5,NFL,4)
         IF(IFLD.LE.0) THEN
            CALL CEDERX(501,1)
            NCPF=NCPF-1
            GOTO 5
         END IF
         IPLN=CIN(1)
         IF(IPLN.EQ.0) THEN
            I=2.0-(IDIR*0.5)
            IPLN=IEDW(I,I3)
         END IF
         IF(IPLN.LE.0.OR.IPLN.GT.NCX(I3)) THEN
            CALL CEDERX(508,1)
            NCPF=NCPF-1
            GOTO 5
         END IF
         CALL FETCHD(IEDFL,ID,IPLN,IFLD,IBUF,RBUF(1,K),N1,N2,I3,
     X               BAD,RLEV,NST)
         CALL COPCIX(ININFO(1,K),ID(171+IFLD*5),4)
         WRITE (ILEV,203)IPLN
  203    FORMAT('(',I2,')')
         READ (ILEV,112)ININFO(5,K),ININFO(6,K)
 112     FORMAT(2A2)
         WRITE(IPR,114) (IBNAM(I,K),I=1,4), (ININFO(I,K),I=1,4), IPLN
  114    FORMAT(5X,4A2,' HAS BEEN INITIALIZED TO ',4A2,' PLANE: ',I3)
      END IF
      GO TO 5
   40 CONTINUE
C
C        ADD A NEW FUNCTION TO THE CURRENT STACK
C
      K=NSTACK+1
      IF(K.GT.MXSTCK) THEN
         CALL CEDERX(519,1)
         GOTO 5
      END IF
      ITYP=0
      CALL COPCX(ISTACK(1,K),NAMOUF,4)
      IF(ISTACK(1,K).EQ.IBL) THEN
         CALL CEDERX(520,1)
         GOTO 5
      END IF
      IF(NSTACK.NE.0) THEN
         J=LOCFLD(ISTACK(1,K),ISTACK,15,NSTACK,4)
         IF(J.NE.0) ITYP=IAUXSTCK(1,J)
      END IF
      IF(ITYP.EQ.0) THEN
         J=LOCFLD(ISTACK(1,K),IBNAM,4,MAXACF,4)
         IF(J.NE.0) ITYP=2
      END IF
      IF(ITYP.EQ.0) THEN
         J=LOCFLD(ISTACK(1,K),NAMF,4,NFMAX,4)
         IF(J.NE.0) ITYP=1
      END IF
      IF(ITYP.EQ.0) THEN
         ITYP=3
         IF(IFLTYP.EQ.INFTYP) ITYP=1
      END IF
      IAUXSTCK(1,K)=ITYP
      IAUXSTCK(2,K)=IFN
C
C        SPECIFY INPUT FIELD NAMES
C
      DO 45 I=1,2
         M=4*I+2
         DO 42 L=1,4
            ISTACK(M+L,K)=IBL
   42    CONTINUE
         IF(I.GT.JFUN(IFN)) GO TO 45
         CALL COPCX(ISTACK(M+1,K),NAMINF(1,I),4)
         IF(IFN.EQ.27) THEN
            IF(LOCFLD(ISTACK(M+1,K),NAMF,4,NFMAX,4).NE.0) GO TO 45
            CALL CEDERX(501,1)
            GOTO 5
         END IF
         IF(ISTACK(M+1,K).EQ.IBL) THEN
            CALL CEDERX(501,1)
            GOTO 5
         END IF
C           CHECK LIST OF OUTPUT FIELDS
         IF(NSTACK.NE.0) THEN
            IF(LOCFLD(ISTACK(M+1,K),ISTACK,15,NSTACK,4).NE.0) GO TO 45
         END IF
C           CHECK LIST OF RETAINED FIELDS
         IF(LOCFLD(ISTACK(M+1,K),IBNAM,4,MAXACF,4).NE.0) GO TO 45
C           CHECK LIST OF PERMANENT FIELDS
         IF(LOCFLD(ISTACK(M+1,K),NAMF,4,NFMAX,4).NE.0) GO TO 45
         CALL CEDERX(501,1)
         GOTO 5
   45 CONTINUE
C
C        SPECIFY SCALING FACTOR FOR NEW FIELD
C
      PFSCL(K)=0.0
      IF(IASCHK.EQ.IASTSK) THEN
         READ (IUSRSC,108)USRSCL
  108    FORMAT(F6.0)
         PFSCL(K)=AMAX1(USRSCL,0.0)
      END IF
C
C        SPECIFY ANY REQUIRED CONSTANTS
C
      ICONZ(K)=0
         DO 48 I=1,MAXCON
            CONS(I,K)=0.0
            IF(I.GT.MFUN(IFN)) GO TO 48
            CONS(I,K)=CIN(I)
   48    CONTINUE
         IF(IFN.EQ.27) CONS(1,K)=IFIX(CIN(1))
      NSTACK=NSTACK+1
      GO TO 5

   60 CONTINUE

      print *,'FUNCTN: nstack=',nstack
      IF(NSTACK.LE.0) GO TO 90
C
C        EXECUTE THE FUNCTION STACK
C        SEND A SUMMARY TO THE PRINT FILE
C
         CALL SHOEDW(IPR)
         WRITE(IPR,101) NCPF
  101    FORMAT(1X,I2,' CONSTANT FIELD PLANES W/INITIALIZATION INFO')
         IF(NCPF.NE.0) WRITE(IPR,102) (J,(NAMBUF(I,J),I=1,10),J=1,NCPF)
  102    FORMAT(3(1X,I2,': ',4A2,'/',6A2))
      CALL DSFSTK(IPR,ISTACK,IAUXSTCK,CONS,0,MAXCON,NSTACK,LFUN,
     X            MFUN,NFUN,ICONZ)
         WRITE(IPR,126) AXNAM(I3),IDIR
  126 FORMAT(/' DIRECTION THROUGH THE ',A1,'-PLANES: ',I2)
C
C        ESTABLISH ALL NEW FIELDS
C
      DO 61 K=1,NSTACK
         IAUXSTCK(3,K)=0
         IF(IAUXSTCK(1,K).NE.1) GO TO 61
C
C           PERMANENT FIELD- SEE IF IT ALREADY EXISTS
C
            IF(LOCFLD(ISTACK(1,K),NAMF,4,NFMAX,4).NE.0) GO TO 61
C
C              NEW FIELD TO BE ESTABLISHED
C
               ISCL=PFSCL(K)
               IF(ISCL.GT.0) GO TO 58
               ISCL=IDFSCL
               IFN=IAUXSTCK(2,K)
C     RESET SCALE FACTOR IF LAT. OR LON FIELD
               IF (IFN.EQ.63 .OR. IFN.EQ.64) ISCL=180
               KINF=JFUN(IFN)
               IF(KINF.LE.0) GO TO 58
C
C                 USE SCALE FACTOR FROM FIRST PERMANENT INPUT FIELD
C
               DO 57 I=1,KINF
                  M=4*I+2+1
                  JI=LOCFLDID(ISTACK(M,K),ID(176),5,NFL,4)
                  IF(JI.EQ.0) GO TO 57
                  IV=MAPVID(JI,2)
                  ISCL=SCLFLD(IV)
                  GO TO 58
   57          CONTINUE
   58          CONTINUE
C
C                 ADD THE FIELD TO PERMANENT DISK
C
               J=IADFLD(ISTACK(1,K),ISCL,IPR)
               IF (J.LT.0) RETURN
               IAUXSTCK(3,K)=1
   61 CONTINUE

C     Begin outer (Do 80) LOOP-loop over all levels
C
      LEV=0
      IF(IDIR.LT.0) LEV=N3+1
      DO 80 LOOP=1,N3
         LEV=LEV+IDIR
         ZLEV=CSP(1,I3)+(LEV-1)*CSP(3,I3)
         ZLEV=ZLEV*SCLAXS(I3,IUNAXS)
         IF(LEV.GE.IEDW(1,I3).AND.LEV.LE.IEDW(2,I3)) GO TO 64
C
C           LEVEL IS OUTSIDE OF WINDOW
C
            CALL CONFLD(OBUF,NPLIN,BAD)
            DO 62 K=1,NSTACK
               IF(IAUXSTCK(3,K).EQ.0) GO TO 62
               JO=LOCFLDID(ISTACK(1,K),ID(176),5,NFL,4)
               CALL PLACED(IEDFL,ID,LEV,JO,IBUF,OBUF,
     X                     N1,N2,I3,BAD,NST)
   62       CONTINUE
            GO TO 80
   64    CONTINUE

C        Begin inner (Do 75) K-loop over function stack
C
         DO 75 K=1,NSTACK
            IFN=IAUXSTCK(2,K)
            IF(IFN.EQ.27) THEN
C
C           FETCH A PERMANENT FIELD AT LEVEL (LEV+C1) DIRECTLY INTO OBUF
C
               LGET=LEV+CONS(1,K)
               IF(LGET.LT.1.OR.LGET.GT.N3) THEN
                  CALL CONFLD(OBUF,NPLIN,BAD)
               ELSE
                  JO=LOCFLDID(ISTACK(7,K),ID(176),5,NFL,4)
                  CALL FETCHD(IEDFL,ID,LGET,JO,IBUF,OBUF,
     X                        NIX,NIY,I3,BAD,RLEV,NST)
               END IF
               GO TO 63
            END IF
C
C           ESTABLISH OUTPUT FIELD
C
            IOF=LOCFLD(ISTACK(1,K),IBNAM,4,MAXACF,4)
            IF(IOF.NE.0) THEN
C
C              OUTPUT FIELD IS STORED IN ACTIVE BUFFER
C
               CALL COPRX(OBUF,RBUF(1,IOF),NPLIN)
            ELSE IF(IAUXSTCK(3,K).NE.0.OR.IAUXSTCK(1,K).NE.1) THEN
C
C              INITIALIZE NEW OUTPUT FIELD TO BAD
C
               CALL CONFLD(OBUF,NPLIN,BAD)
            ELSE
C
C              OUTPUT FIELD IS ON DISK FILE
C
               JO=LOCFLDID(ISTACK(1,K),ID(176),5,NFL,4)
               CALL FETCHD(IEDFL,ID,LEV,JO,IBUF,OBUF,
     X                     NIX,NIY,I3,BAD,RLEV,NST)
            END IF
C
C           ESTABLISH INPUT FIELDS
C
            DO 65 I=1,2
               INB(I)=1
               IF(I.GT.JFUN(IFN)) GO TO 65
                  M=4*I+2+1
                  J=LOCFLD(ISTACK(M,K),IBNAM,4,MAXACF,4)
                  IF(J.EQ.0) THEN
                     JI=LOCFLDID(ISTACK(M,K),ID(176),5,NFL,4)
                     J=IFIND(0,IBNTYP,MAXACF,0)
                     IF(J.EQ.0) GO TO 91
                     CALL FETCHD(IEDFL,ID,LEV,JI,IBUF,RBUF(1,J),
     X                           NIX,NIY,I3,BAD,RLEV,NST)
                     IBNTYP(J)=1
                     CALL COPCX(IBNAM(1,J),ISTACK(M,K),4)
                  END IF
                  INB(I)=J
   65       CONTINUE

C           Current function name is 'SOUND' - number 73
C
c            print *,'FUNCTN: Before CALL FUNCTS: ifn=',ifn
            IF(IFN.EQ.73)THEN
               SNDNAME=NAMESND(K)
c               print *,' SOUND: k,sndname=',k,sndname
            END IF
            CALL FUNCTS(RBUF(1,IN1),RBUF(1,IN2),N1,N2,LEV,OBUF,
     X                  IFN,CONS(1,K),CONEXT,IEDW,NAX,BAD,IFLAT,
     X                  LATLON,IFAX,SNDNAME)
   63       CONTINUE
            KBEG=K+1
C
C           FUNCTION HAS BEEN EXECUTED- TIDY UP FOR THE NEXT ONE
C                    SAVE OUTPUT FIELD ON DISK IF PERMANENT.
C
            JOSAV=0
            IF(IAUXSTCK(1,K).NE.1) GO TO 67
            IF(IAUXSTCK(2,KBEG).EQ.27) GO TO 66
            IF(K.EQ.NSTACK) GO TO 66
            J=LOCFLD(ISTACK(1,K),ISTACK(1,KBEG),4,1,4)
            IJ=LOCFLD(ISTACK(1,K),ISTACK(7,KBEG),4,2,4)
            IF(J.EQ.0 .OR. IJ.EQ.0) GO TO 66
C
C           SET JOSAV NON-ZERO SO THAT OUTPUT FIELD WILL BE SAVED IN
C               THE BUFFER AREA TO BE USED AS POSSIBLE INPUT TO NEXT LEVEL.
C               THIS ONLY OCCURS SO LONG AS THE OUTPUT FIELD
C               IS CONSECUTIVELY SPECIFIED.
C
            JOSAV=2
            GO TO 67
   66       CONTINUE
            JO=LOCFLDID(ISTACK(1,K),ID(176),5,NFL,4)
            CALL PLACED(IEDFL,ID,LEV,JO,IBUF,OBUF,
     X                  N1,N2,I3,BAD,NST)
   67       CONTINUE
C
C              DETERMINE THE DISPOSITION OF ACTIVE FIELDS
C
               J=LOCFLD(ISTACK(1,K),IBNAM,4,MAXACF,4)
               IF(J.NE.0) THEN
                  IF(IBNTYP(J).EQ.2) CALL COPRX(RBUF(1,J),OBUF,NPLIN)
               END IF
            IF(K.LT.NSTACK) THEN
C
C              DETERMINE THE DISPOSITION OF ACTIVE FIELDS
C
               IF(J.NE.0) THEN
                  IF(IBNTYP(J).NE.2) CALL COPRX(RBUF(1,J),OBUF,NPLIN)
               ELSE IF(JOSAV.NE.0.OR.IAUXSTCK(1,K).EQ.3) THEN
                  J=IFIND(0,IBNTYP,MAXACF,0)
                  IF(J.EQ.0) GO TO 91
C              JOSAV CAN BE EQUAL TO 0 OR 2.
                  IBNTYP(J)=3-JOSAV
                  CALL COPCX(IBNAM(1,J),ISTACK(1,K),4)
                  CALL COPRX(RBUF(1,J),OBUF,NPLIN)
               END IF
               DO 70 I=1,MAXACF
                  ITYP=IBNTYP(I)
                  IF(ITYP.EQ.0.OR.ITYP.EQ.2) GO TO 70
                  KEND=KBEG
                  IF(ITYP.EQ.3) KEND=NSTACK
                  DO 68 L=KBEG,KEND
                     J=LOCFLD(IBNAM(1,I),ISTACK(7,L),4,2,4)
                     IF(J.NE.0) GO TO 70
   68             CONTINUE
C
C                 DISCARD THE FIELD FROM THE ACTIVE BUFFER
C
                  IBNTYP(I)=0
                  DO 69 L=1,4
                     IBNAM(L,I)=IBL
   69             CONTINUE
   70          CONTINUE
            END IF
   75    CONTINUE
C
C     End loop current function stack, this level

         DO 77 I=1,MAXACF
            IF(IBNTYP(I).EQ.2) GO TO 77
            IBNTYP(I)=0
            DO 76 L=1,4
              IBNAM(L,I)=IBL
   76       CONTINUE
   77    CONTINUE
   80 CONTINUE
C
C     End loop over all levels

      DO 85 I=1,MAXACF
        IF(IBNTYP(I).EQ.2)
     X     CALL COPCX(ININFO(1,I),IRETN,6)
   85 CONTINUE
   90 CONTINUE
C
C        NORMAL TERMINATION WITH NST FLAG SET
C
      NST=0
      CALL SHOEDF(IPR)
      RETURN
   91 CONTINUE
      CALL CEDERX(518,1)
      IGUISTAT=-2
      RETURN
      END
