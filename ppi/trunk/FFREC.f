c
c----------------------------------------------------------------------X
c
      SUBROUTINE FFREC(IUN,IPREC,IEOF,IEOT,IFORBLK,IYR,IMON,IDAY,IDATE,
     X     ITIME,IVOL,ISWP,ITP,FXANG,R0,DR,NGTS,IHSK,RNGCOR,DEC,DECWR,
     X     WORDSZ)
C
C  ROUTINE TO READ A SINGLE PHYSICAL RECORD FROM RP-7 OR -6 FORMAT RADAR
C  TAPES AND UNPACK HOUSEKEEPING FOR THE FIRST BEAM.  A PHYSICAL RECORD
C  CONTAINS (NLREC) BEAMS OR LOGICAL RECORDS.
C     DEC   - (1) Reading input on DEC,     (0) Reading input on non-DEC
C     DECWR - (1) Input was written on DEC, (0) Input was written on non-DEC
C
C     Dimension IBUF big enough for 1024 gates of 8 fields (8-bits per 
C     field) and 256*(16-bit housekeeping),with 10 beams per input record, 
C     i.e. 10*(1024*8*8+256*16)/32-bit = 21760 words for the workstation 
C     version.  NWDS from RDCOSREC is number of Cray (8-byte) words.

      CHARACTER*8 NAME
      DIMENSION IBUF(21760),IHSK(256)
      DATA NPARMX,NZERMX,N16H,NLREC/10,10,256,1/
      DATA CF/182.044444/

C
C  BUFFER IN THE FIRST PHYSICAL RECORD FROM TAPE.  UNIT(IUN) RETURNS THE
C  STATUS OF THE LAST READ AND LENGTH(IUN) RETURNS THE NUMBER OF WORDS READ.
C     RST = -1 (GOOD), 0 (EOF), 1 (PARITY ERROR)
C
      NPAR=0
      NZER=0
 10   CONTINUE
CANNE   10 BUFFER IN(IUN,1) (IBUF(1),IBUF(8192))
C
C  FORTRAN BLOCKING
      IF(IFORBLK.EQ.1)THEN
         CALL RDSUNREC(IA,NWDS,DEC,DECWR)
         NWDS=INT((NWDS-1)/8)+1
      ELSE
C
C  COS BLOCKING

         CALL RDCOSREC(IBUF,NWDS)
c         print *,'FFREC: nwds (32-bit)=',nwds*2
      END IF
      IPREC=IPREC+1
CANNE      RST=UNIT(IUN)

C  PARITY ERROR:  READ THE NEXT RECORD
C
C      IF(RST .EQ. 1.0)THEN
C         NPAR=NPAR+1
C         WRITE(6,15)IPREC
C   15    FORMAT(8X,' PARITY ERROR IN RECORD = ',I8,/)
C         IF(NPAR.GT.NPARMX)THEN
C            WRITE(6,17)
C   17       FORMAT(1X,'******  MORE THAN',I3,
C     +                ' PARITY ERRORS - EOT  ******')
C            IEOF=0
C            IEOT=1
C            RETURN
C         END IF
C         GO TO 10
C      END IF

C  END OF FILE:  CHECK IF ALSO LOGICAL EOT
C
      IF (NWDS .LE. 0.0)THEN
        WRITE(6,19)IUN,IPREC
   19   FORMAT(8X,' END OF FILE ON UNIT= ',I3,' RECORD=',I8)

C       DOUBLE EOF:  LOGICAL END-OF-TAPE
C
        IF(IEOF.EQ.1)THEN
           IEOT=1
           WRITE(6,21)
   21      FORMAT(1X,'******  LOGICAL END OF TAPE  ******')
           IEOF=0
           RETURN
        END IF
        IEOF=1
        NPAR=0
        GO TO 10
      END IF

C  GOOD READ:  PROCESS THIS RECORD
C     CALCULATE POINTER TO FIRST HOUSEKEEPING WORD OF NEXT
C     RAY (IWRD).  NTBITS IS THE NUMBER OF BITS PER RAY OF DATA
C
      IF(NWDS .GT.0)THEN
         NPAR=0
         IEOF=0
         IEOT=0
         N16=NINT(WORDSZ/16)
         N64IN=NWDS
CANNE         N64IN=LENGTH(IUN)
         N16IN=4*N64IN
      END IF

C     BYTE-SWAPPING CHECK
C
      IF((DEC .EQ. 1.0 .AND. DECWR .EQ. 0.0).OR.
     +   (DEC .EQ. 0.0 .AND. DECWR .EQ. 1.0))THEN
         CALL SWAP32(IBUF,NWDS*2)
      END IF

      CALL GBYTES(IBUF(1),IHSK,0,16,0,N16H)

C     OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
      IDATE = 10000*IHSK(4)+100*IHSK(5)+IHSK(6)
      ITIME = IHSK(7)*10000+IHSK(8)*100+IHSK(9)
      LREC  = IHSK(1)
      IYR   = IHSK(4)
      IMON  = IHSK(5)
      IDAY  = IHSK(6)
      AZ    = IHSK(10)/CF
      EL    = IHSK(11)/CF
      IF(EL.GT.180.0)EL=EL-360.0
      DR    = IHSK(14)/1000.
      R0    = IHSK(12)+IHSK(13)*.001+RNGCOR+DR
      NGTS  = IHSK(15)
      VNYQ  = FLOAT(IHSK(21))*FLOAT(IHSK(20))*.0000025
      ISWP  = IHSK(22)
      ITP   = IHSK(26)
      FXANG = IHSK(31)/CF
      IVOL  = IHSK(48)
      ITF   = IHSK(61)
      IDPROC= IHSK(62)
      IDRADR= IHSK(63)
      N16H  = IHSK(65)
      N16R  = IHSK(66)
      INT1   = ISHFT(IHSK(67),-8)
      NLR   = AND(INT1,255)
      LRECN = AND(IHSK(67),255)
      NFLD  = IHSK(68)
      IF(IDRADR.EQ.2.AND.IDPROC.EQ.6)THEN
         IZDR=AND(IHSK(247),1)
         IF(IZDR.EQ.1)VNYQ=0.5*VNYQ
      END IF
      IF(NLR.NE.0)NLREC=NLR
      N16RIN = N16R*NLREC
      IF(N16RIN.LE.0)THEN
         NZER=NZER+1
         WRITE(6,23)IPREC,N16R,NLREC
   23    FORMAT(8X,' APPARENT ZERO LENGTH PHYSICAL RECORD = ',3I8)
         WRITE(6,27)IDATE,ITIME,AZ,EL,FXANG,ITP,ITF,NFLD,NGTS,DR,
     +              VNYQ,IVOL,NLREC,LRECN,LREC,IPREC
         WRITE(6,231)
  231    FORMAT(/,1X,'HOUSEKEEPING:',/)
         CALL DMPINTGR(IHSK,256)
         IF(NZER.GT.NZERMX)THEN
            WRITE(6,24)NZERMX
   24       FORMAT(1X,'******  MORE THAN',I3,
     +                ' ZERO LENGTH RECORDS = EOT  ******')
            IEOF=0
            IEOT=1
            RETURN
         END IF
         GO TO 10
      END IF
      IF(ABS(N16RIN-N16IN).GT.4)NLREC=0.5+(N16IN/N16R)
      WRITE(6,25)IDATE,ITIME
   25 FORMAT(1X,'  Tape begins at D=',I6.6,' T=',I6.6)
      WRITE(6,27)IDATE,ITIME,AZ,EL,FXANG,ITP,ITF,NFLD,NGTS,DR,
     +           VNYQ,IVOL,NLREC,LRECN,LREC,IPREC
   27 FORMAT(2X,' D=',I6.6,' T=',I6.6,' A=',F6.2,' E=',F6.2,
     +          ' Fx=',F6.2,' M=',I1,' Fl=',I1,' Nfl=',I1,' Ng=',I4,
     +          ' Dr=',F5.3,' Nyq=',F5.2,' Vol=',I3,' Nlr=',I2,
     +          ' Lrn=',I2,' Lr=',I5,' Tr=',I5,/)
      WRITE(6,29)N64IN,NLREC,N16R,N16H,NGTS,NFLD
   29 FORMAT(
     +  5X,'Each physical record consists of ',I4,' 64-bit words.  ',
     +     'There are ',I2,' logical records (beams) of ',I4,' 16-bit ',
     +/,5X,'words in each physical record.  Each beam contains ',I3,1X,
     +     '16-bit housekeeping words and ',I4,' range gates of data, ',
     +/,5X,'consisting of ',I2,' fields at each range gate.  The ',
     +     'field indices, bits per field, scales and biases are:',/)

C     DESCRIPTORS FOR PARAMETERS 1- 6 START AT HK WORD  69; SCALE AT  79
C          "       "       "     7-15   "    "  "   "  161;   "    " 171
C
      IS1=69
      IS2=79
      IS3=161
      IS4=171
      NHBITS=16*N16H
      DO 100 N=1,NFLD
         IF(N.LE.6)THEN
            I1=IS1+N-1
            I2=IS2+2*(N-1)
         ELSE
            I1=IS3+N-7
            I2=IS4+2*(N-7)
         END IF
         INT1=ISHFT(IHSK(I1),-8)
         IFLD1=AND(INT1,255)
         NDBITS=AND(IHSK(I1),255)
         IF(IFLD1.NE.0)THEN
            ISCL=IHSK(I2)
            SCALE=ISCL*.01
            IBS=IHSK(I2+1)
            IF(IBS.GT.32767)IBS=IBS-65536
            BIAS=IBS*.01
            IF(IFLD1.GE.16.AND.IDPROC.LE.6)THEN
               SCALE=0.01
               BIAS=0.0
            END IF

C           SCALE FACTOR FOR CP2/RP6 ZDR
C
            IF(IFLD1.EQ.32.AND.IDPROC.EQ.6)THEN
               ISCL=AND(IHSK(247),12)
               ISCL=ISHFT(ISCL,-2)
               SCL=3.0
               IF(ISCL.EQ.1)SCL=6.0
               IF(ISCL.EQ.2)SCL=12.0
               IF(ISCL.EQ.3)SCL=24.0
               SCALE=SCL/127.0
            END IF
         END IF

C        FIELD-CALIBRATED ENGINEERING UNITS - LOWER 4 BITS
C
         NAME='UNKNOWN'
         IF(IFLD1.EQ.  1)NAME='DBZ'
         IF(IFLD1.EQ.  2)NAME='VF'
         IF(IFLD1.EQ.  3)NAME='SW'
         IF(IFLD1.EQ.  4)NAME='SNR'
         IF(IFLD1.EQ.  6)NAME='NCP'

C        UNSCALED COUNTS - UPPER 4 BITS
C
         IF(IFLD1.EQ. 16)NAME='DBM'
         IF(IFLD1.EQ. 32)NAME='ZDR'
         IF(IFLD1.EQ. 48)NAME='DBMSV'
         IF(IFLD1.EQ. 64)NAME='R(TAU)'
         IF(IFLD1.EQ. 80)NAME='R(0)'
         IF(IFLD1.EQ. 96)NAME='DBMXH'
         IF(IFLD1.EQ.112)NAME='DBMXV'
         IF(IFLD1.EQ.128)NAME='VEL'
         IF(IFLD1.EQ.144)NAME='VFLAG'
         IF(IFLD1.EQ.160)NAME='VELSV'
         IF(IFLD1.EQ.208)NAME='SPECW'

         IF(NAME.EQ.'UNKNOWN')THEN
            WRITE(6,95)NAME
 95         FORMAT(1X,'FIELD NAME=',A8,' *** SOMETHING IS WRONG *** ',
     +           'CHECK IF YOU HAVE SET BYTE SWAPPING CORRECTLY *** ')
            STOP
         ELSE
            WRITE(6,97)N,IFLD1,NAME,NDBITS,SCALE,BIAS
 97         FORMAT(15X,'  Field number=',I2,'  Index=',I3,'  Name= ',A8,
     +           ' No. bits=',I2,'  Scale =',F8.4,'  Bias =',F8.4)
         END IF
 100  CONTINUE

C     REWIND IUN
C
      IRW=1
      CALL INIT_COS(IUN,IRW)
      IPREC=IPREC-1
      RETURN
      END
