      SUBROUTINE METHOD(IPR)
C
C        ESTABLISHES THE INTERPOLATION METHOD FOR THE VOLUME
C           ABOUT TO BE INTERPOLATED.
C
C     NIF   - Number of input fields
C     NOF   - Number of requested output fields
C     NFLI  - Number of requested output fields
C     CFAC  - Nyquist velocity
C
C     Scaled integers = (real values)/(scaling factor)
C             See INITAL for UNSCAZ, UNSCRG, UNSCEL, and UNSNYQ
C                            SCLAZ,  SCLRNG, SCLEL,  and SCLNYQ
C             where SCL* = 1/UNSC*
C
C     SCLIF  - Scaling factor for input  fields
C     SCLOF  -    "       "    "  output    "
C     SCLAZ  -    "       "    "  azimuth angles
C     SCLRNG -    "       "    "  distance
C     SCLEL  -    "       "    "  elevation angles
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (MAXFLD=16)
c-----DATA IBAD/-32768/

      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,
     X     CFAC(MAXFLD)

      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /NCARFB/ NF,IDXFV(MAXFLD)
      CHARACTER*8 CTEMP1,NAMTIM,NAMQAL
      CHARACTER*1 IDEC,IYES,ICC
      DATA SCOUT,SCOUTI/ 100, 0.01/
      DATA SCLTIM/8.0/
      DATA SCLQAL/100./
      DATA NAMTIM/'TIME'/
      DATA NAMQAL/'QUAL'/
      DATA IYES/ 'Y' /, ICC/ ' ' /
C
C        INITIALIZE TABLES FOR INPUT FIELDS AND METHOD SELECTION
C
      print *,'METHOD: maxfld,nif,nflds=',maxfld,nif,nflds
      print *,'METHOD: cinfld=',(cinfld(n),n=1,nif)
      print *,'METHOD: ifield=',(ifield(n),n=1,nflds)
      IBIG=IABS(IBAD)*2
      DO 15 I=1,MAXFLD
         IDXFV(I)=0
         INFLD(I,2)=-1
         INFLD(I,3)=0
         IOFLD(I,2)=0
         IOFLD(I,3)=IBAD
 15   CONTINUE
      NQSWIT=0
      NF=0
      NFLI=0
      NOF=0
      print *,'METHOD: initial nif,nof,nfli=',nif,nof,nfli
      METH=METSPC(1)
C
C        LOOP FOR EACH REQUESTED FIELD
C
      DO 60 I=1,NIF
         INFLD(I,2)=ITPFLDC(CINFLD(I))
         NFLI=NFLI+1
         print *,'METHOD: I,  cinfld,infld=',i,' ',cinfld(i),infld(i,2),
     +        nif,nfli,nof

         IWFLD(NFLI)=I
C
C        TOSS INTERPOLATED FIELD FROM OUTPUT TABLE IF SPECIFIED
C
         IF(INTINF(I,3).EQ.'D') GO TO 20
         NOF=NOF+1
         CIOFLD(NOF)=CINFLD(I)
         IOFLD(NOF,2)=NFLI
         SCLOF(NOF,1)=SCLIF(I,1)
         SCLOF(NOF,2)=SCLIF(I,2)
 20      CONTINUE
         IF(INFLD(I,2).EQ.3) THEN
C     VELOCITY FIELD LOCATED
            IF(CFAC(I).GE.0.0) GO TO 51
C     VELOCITY FIELD FLAGGED WITH NCAR BAD DATA BIT (1)
            WRITE (CTEMP1,500)INTINF(I,2)
 500        FORMAT(A4)
            READ (CTEMP1,125)IDEC
 125        FORMAT(A1)
            IF(IDEC.NE.'D') GO TO 51
            NF=NF+1
            IDXFV(NF)=I
 51         CONTINUE
            IF(METH.NE.1) GO TO 60
C     SEE IF UNFOLDING IS TO BE PERFORMED
            IF(INTINF(I,1).NE.'U'.AND.INTINF(I,1).NE.'Q') GO TO 60
            IF(NQSWIT.NE.0) THEN
               PRINT 126
 126           FORMAT(/5X,'+++ONLY ONE QUAL FIELD MAY BE GENERATED+++'/)
               STOP
            END IF
            NQSWIT=1
            NOF=NOF+1
            CIOFLD(NOF)=NAMQAL
            IOFLD(NOF,2)=0
            SCLOF(NOF,1)=SCLQAL
            SCLOF(NOF,2)=1./SCLQAL
            IF(CFAC(I).EQ.0.0) CALL TPQERX(330,1)
            IF(INTINF(I,1).EQ.'Q') THEN
               INFLD(I,3)=IBIG
            ELSE
               INFLD(I,3)=ABS(CFAC(I))*SCLIF(I,1)+0.5
            END IF
            GO TO 60
         END IF
         IF(METH.NE.1) GO TO 55
         IF(INFLD(I,2).NE.1.AND.INFLD(I,2).NE.2) GO TO 60
C     CONVERT 10(LOG) FIELD TO LINEAR UNITS FOR INTERPOLATION
         IF(INTINF(I,1).EQ.'L') INFLD(I,3)=1
 55      CONTINUE
C     CHECK FOR POWER FIELD, CONVERT TO DBZ IF DESIRED
         IF(INFLD(I,2).NE.1) GO TO 60
         IF(INTINF(I,2).EQ.' ') GO TO 60
         NOF=NOF+1
         READ(INTINF(I,2),33)CIOFLD(NOF)
 33      FORMAT(A8)
C         IOFLD(NOF,1)=INTINF(I,2)
         IOFLD(NOF,2)= -NFLI
         SCLOF(NOF,1)=SCLIF(I,1)
         SCLOF(NOF,2)=SCLIF(I,2)
         IF (CFAC(I).LE.0.0) THEN
C
C     MAKE SURE RADAR CONSTANT LOOKS OK
C
            WRITE(*,122)CFAC(I)
 122        FORMAT(/,5X,'+++RADAR CONSTANT FOR CONVERTING DBM TO DBZ',
     X           ' MUST BE GREATER THAN 0.'/,8X, 'CHECK CALIBRATION',
     X           ' FILE OR RESET CARD.+++')
            STOP
         END IF
 60   CONTINUE
      IF(IFIELD(NFLDS).NE.NAMTIM) GO TO 65
C
C        DERIVE A TIME FIELD DURING INTERPOLATION
C
      NFLI=NFLI+1
      IWFLD(NFLI)= -1
      NOF=NOF+1
      CIOFLD(NOF)=NAMTIM
      IOFLD(NOF,2)=NFLI
      SCLOF(NOF,1)=SCLTIM
      SCLOF(NOF,2)=1./SCLTIM
 65   CONTINUE
      IF(NOF.GT.MAXFLD) THEN
         PRINT 130, NOF
 130     FORMAT(5X,'*** NUMBER OF OUTPUT FIELDS:',I5) 
         PRINT 131, MAXFLD
 131     FORMAT(5X,' EXCEEDS MAXIMUM PERMITTED: ',I2)
         STOP
      END IF
C     
C           GET SMOOTHING INFORMATION AND
C            MAXIMUM RANGE OF INFLUENCE FOR ESTIMATES.
C
      DMIN=METSPC(6)*SCLRNG
      METSPC(4)=AMAX1(DMIN,DRG)*UNSCRG
      METSPC(5)=DRG*UNSCRG
C     
C     SEND SUMMARY TO PRINT
C
      print *,'METHOD: nif,nof,nfli=',nif,nof,nfli
      write(7,*)'METHOD: nif,nof,nfli=',nif,nof,nfli
      CALL MTHSUM(IPR,ICC,IYES)
      RETURN
      END
