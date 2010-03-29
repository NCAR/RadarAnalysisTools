      SUBROUTINE WRITVL(KRD,IBUF,MAXFLD,RBUF)
C     
C     WRITE VOLUME FROM MEMORY TO TAPE
C     
      INCLUDE 'CEDRIC.INC'
      PARAMETER (LNNF=4)
      COMMON LCMB(MAXLCM)
      COMMON /IOTYPE/ ICDF
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL
      COMMON /CREAXS/ CSPN(3,3),NCXN(3),NPLNEW
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      DIMENSION IBUF(MAXFLD),NFIN(LNNF),RBUF(MAXPLN)
      DIMENSION KNTFL(99),FTFL(99)
      integer iprocess
      CHARACTER*(*) KRD(10)
      CHARACTER*8 LOUTNM,NAMTAP,INMTAP,NAMVOL
      CHARACTER*8 TIMVAL
      CHARACTER*3 IOTYP
      EQUIVALENCE (NCXORD(1),N1),(NCXORD(2),N2),(NCXORD(3),N3)
      DATA NFIN/'BEG','ADD','SKI','APP'/
      DATA KNTFL,FTFL/ 99*0, 99*0.0 /
      DATA NAMTAP/'UNKNOWN' /
      DATA MODE,NTYPE,NWDCNT/1,2,1/
      IBAD=ID(67)
      NX=ID(162)
      NY=ID(167)
      NZ=ID(172)


      CALL CDATE(TIMVAL)
      READ (KRD,101)ROUT,INMTAP,NAMVOL,NCOD,RSKP,RFEET,IOTYP,POSIT
C     101  FORMAT (8X,F8.0,2A8,A3,5X,2F8.0,A3)
 101  FORMAT(/F8.0/A8/A8/A3/F8.0/F8.0/A3/A3)
C     
C     CHECK THE I/O TYPE
C     
      LOUT=ROUT
      IF (IOTYP.EQ.'COS') THEN
C     
C     WRITE FILE OUT AS COS BLOCKED BINARY
C     
         ICDF=0
         WRITE(*,42)
 42      FORMAT(/,5X,'OUTPUTTING FILE IN COS BLOCKED BINARY ',
     X        'FORMAT')
         CALL UPDATE_COS(LOUT)

      ELSE IF (IOTYP.EQ.'MDV') THEN
         ICDF=4
         IFILE=1
         WRITE(*,31)
 31      FORMAT(/,5X,'OUTPUTTING FILE IN MDV  FORMAT')  
         CALL PREPMHD(LOUT,ID)
   
      ELSE IF (IOTYP.EQ.'CDF') THEN
C     
C     NETCDF
C     
         ICDF=2
         IFILE=1
         WRITE(*,33)
 33      FORMAT(/,5X,'OUTPUTTING FILE IN netCDF FORMAT')
      ELSE
C     
C     WRITE FILE OUT AS A PURE FILE
C     
         ICDF=1
         WRITE(*,22)
 22      FORMAT(/,5X,'OUTPUTTING FILE IN UNBLOCKED BINARY FORMAT')
      ENDIF
      IF(LOUT.LE.0) THEN 
         CALL CEDERX(521,1)
         RETURN
      END IF
C     
C     ENCODE THE FORTRAN UNIT NUMBER FOR USE IN CRAY JCL CALLS
C     
      IF(LOUT.LT.10) THEN
         WRITE (LOUTNM,102)LOUT
 102     FORMAT('fort.',I1,'  ')
      ELSE
         WRITE (LOUTNM,103)LOUT
 103     FORMAT('fort.', I2,' ')
      END IF
      IF (INMTAP.EQ.'        ' ) INMTAP=NAMTAP
C     
C     POSITION OUTPUT TAPE
C     
      IF (ICDF.EQ.0) THEN
         IGO=LOCATE(NCOD,NFIN,LNNF)
         IF (IGO.LE.0) THEN
            CALL CEDERX(534,1)
            RETURN
         END IF
         GOTO (80,100,120,130),IGO
C     
C     BEGINING
C     
 80      CONTINUE
         REWIND LOUT
         KNTFL(LOUT)=0
         FTFL(LOUT)=0.0
         GOTO 130
C     
C     ADD
C     
 100     CONTINUE
         REWIND LOUT
         KNTFL(LOUT)=0
         FTFL(LOUT)=RFEET
 110     CONTINUE
         CALL RDTAPE(LOUT,MODE,NTYPE,IDUM,NWDCNT)
         CALL IOWAIT(LOUT,NST,NWDS)
         IF (NST.EQ.1) THEN
            BACKSPACE LOUT
            GOTO 130
         ELSE IF(NST.EQ.3) THEN
            CALL CEDERX(572,1)
            RETURN
         ENDIF
         KNTFL(LOUT)=KNTFL(LOUT)+1
         CALL SKPVOL(LOUT,1)
         GOTO 110
C     
C     SKIP
C     
 120     CONTINUE
         REWIND LOUT
         KNTFL(LOUT)=RSKP
         FTFL(LOUT)=RFEET
         CALL SKPVOL(LOUT,KNTFL(LOUT))
 130     CONTINUE
C     
C     READY HEADER AND PACK
C     
         CALL REDHED(IBUF,ID,NID,INMTAP,NAMVOL,KNTFL(LOUT),FTFL(LOUT),
     X        NST)
         IF (NST.NE.0) RETURN
C     
C     LOOP THROUGH LEVELS THEN FIELDS AND WRITE OUT
C     (NOTE: NO PACKING IS NECESSARY SINCE BUFFER IS PACKED
C     
         NZ=NCX(N3)
         NWL=ID(451)
         DO 500 K=1,NZ
            CALL BLHED(IBUF,K,N3,VALLEV(K),VALNYQ(K),NST)
            IF (NST.NE.0) RETURN
            DO 400 IF=1,NFL
               LOCD=ID(400+IF)+(K-1)*NWL
               CALL OUTPCK(LCMB(LOCD),NWL,1)
 400        CONTINUE
 500     CONTINUE
C     
C     FILE MARKS
C     
         ENDFILE LOUT
         ENDFILE LOUT
         BACKSPACE LOUT
         
      ELSE IF (ICDF.EQ.1) THEN
C     
C     OUTPUTTING IN PURE BINARY FORMAT
C     
         READ(LOUTNM,525)L1
 525     FORMAT(5X,I1)
         READ(LOUTNM,526)L2
 526     FORMAT(6X,I1)
         IF (NCOD.EQ.NFIN(1)) THEN
C     
C     DELETE ALL THAT IS IN THIS FILE AND PUT THE CURRENT EDIT VOLUME
C     INTO IT
            IPOS=1
         ELSE  
C     
C     APPEND THE EDIT VOLUME TO WHATEVER IS IN THIS FILE ALREADY
C     
            IPOS=2
         END IF
C     
C     GO AND BUILD A 56 CHAR. DESCRIPTION OF THIS VOLUME
C     
         CALL BLDDES(ID,IBUF)
C     
C     CALL ROUTINE TO OPEN AND POSITION OUTPUT FILE
C     
         CALL COUT(L1,L2,IPOS,ISKP,IBUF,NST)
         IF (NST.NE.0) RETURN
C     
C     READY HEADER AND PACK
C     
         CALL REDHED(IBUF,ID,NID,INMTAP,NAMVOL,KNTFL(LOUT),FTFL(LOUT),
     X        NST)
         IF (NST.NE.0) RETURN
C     
C     LOOP THROUGH LEVELS THEN FIELDS AND WRITE OUT
C     (NOTE: NO PACKING IS NECESSARY SINCE BUFFER IS PACKED
C     
         NZ=NCX(N3)
         NWL=ID(451)
         IF (INMEM.EQ.1) THEN
C     
C     WRITE OUT DATA
C     
            DO 550 K=1,NZ
               CALL BLHED(IBUF,K,N3,VALLEV(K),VALNYQ(K),NST)
               IF (NST.NE.0) RETURN
               DO 450 IF=1,NFL
                  LOCD=ID(400+IF)+(K-1)*NWL
                  CALL CWRITE(LCMB(LOCD),NPLANE,NST)
                  IF (NST.NE.0) RETURN
 450           CONTINUE
 550        CONTINUE
         ELSE
C     
C     WRITE OUT DATA
C     
            DO 552 K=1,NZ
               CALL BLHED(IBUF,K,N3,VALLEV(K),VALNYQ(K),NST)
               IF (NST.NE.0) RETURN
               DO 452 IF=1,NFL
C     LOCD=ID(400+IF)+(K-1)*NWL
                  MULT=ID(175+IF*5)
                  CALL FETCHD(LOUT,ID,K,IF,IBUF,RBUF,NX,NY,3,BAD,
     X                 ZLEV,NST)
                  IF (NST.NE.0) THEN
                     WRITE(*,*)'+++ERROR FETCHING DATA IN WRITVL+++'
                     CALL FLUSH_STDOUT
                  END IF
C     
C     DATA IS ALWAYS STORED BY Z-PLANES, CONVERT TO I*2 AND STORE
C     ACCORDING TO THE FIXED AXIS OF THE INPUT ARRAY.
C     
                  NST=0
                  KBAD=0
                  DO 15 I=1,NPLANE
                     IBUF(I)=IBAD
                     IF(RBUF(I).EQ.BAD) GO TO 15
                     ICHECK=NINT(RBUF(I)*MULT)
                     IF(IABS(ICHECK).GE.32768) GO TO 14
                     IBUF(I)=ICHECK
                     GO TO 15
 14                  KBAD=KBAD+1
 15               CONTINUE
                  CALL SBYTES(IBUF,IBUF,0,16,0,NPLANE)
                  CALL CWRITE(IBUF,NPLANE,NST)
                  IF (NST.NE.0) RETURN
 452           CONTINUE
 552        CONTINUE
         END IF
C-----------------------ICDF = 2 NETCDF-----------------------
      ELSE IF (ICDF.EQ.2) THEN
C     
         WRITE (LPR,104)
 104  FORMAT (//' OUTPUT FILE SUMMARY ...'//)
         CALL WIDVARS(LOUTNM)

C      LOOP THROUGH LEVELS THEN FIELDS AND WRITE OUT
C     (NOTE: NO PACKING IS NECESSARY SINCE BUFFER IS PACKED
C     
         NZ=NCX(N3)
         NWL=ID(451)
C     
C     WRITE OUT DATA
C     
         DO 553 K=1,NZ
            DO 453 IF=1,NFL
C     LOCD=ID(400+IF)+(K-1)*NWL
               MULT=ID(175+IF*5)
               CALL FETCHD(LOUT,ID,K,IF,IBUF,RBUF,NX,NY,3,BAD,ZLEV,
     X                     NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'+++ERROR FETCHING DATA IN WRITVL+++'
                  CALL FLUSH_STDOUT
               END IF
C     
C     DATA IS ALWAYS STORED BY Z-PLANES, CONVERT TO I*2 AND STORE
C     ACCORDING TO THE FIXED AXIS OF THE INPUT ARRAY.
C     
               NST=0
               KBAD=0
               DO 65 I=1,NPLANE
                  IF(RBUF(I).EQ.BAD) THEN
                     RBUF(I)=IBAD
                     GO TO 65
                  END IF
                  ICHECK=NINT(RBUF(I)*MULT)
                  IF(IABS(ICHECK).GE.32768) GO TO 64
                  GO TO 65
 64               KBAD=KBAD+1
 65            CONTINUE
               CALL CDFPUT(RBUF,IF,K,NX,NY,NST)

               IF (NST.NE.0) RETURN
 453        CONTINUE
 553     CONTINUE
         CALL CLOSECDF(ICDFID(IFILE))
         ICDFID(IFILE) = 0
         IFILE = 0
C
C     MDV OUTPUT
      ELSE IF (ICDF.EQ.4) THEN
         DO 554 IF=1,NFL
            MULT=ID(175+IF*5)
            DO 454 K=1,NZ
               CALL FETCHD(LOUT,ID,K,IF,IBUF,RBUF,NX,NY,3,BAD,ZLEV,
     X                     NST)
               IF (NST.NE.0) THEN
                  WRITE(*,*)'+++ERROR FETCHING DATA IN WRITVL+++'
                  CALL FLUSH_STDOUT
               END IF
               NST=0
               KBAD=0
               DO 67 I=1,NPLANE
                  IF(RBUF(I).EQ.BAD) THEN
                     RBUF(I)=IBAD
                     GO TO 66
                  END IF
                  ICHECK=NINT(RBUF(I)*MULT)
                  IF(IABS(ICHECK).GE.32768) GO TO 66
                  GO TO 67
 66               KBAD=KBAD+1
 67               CONTINUE
                  iprocess = 0
                  if(k .eq. nz) iprocess = 1
                  call prepmdvdata(if,k,iprocess,rbuf)
 454        CONTINUE
 554     CONTINUE
C     
         WRITE (LPR,105)
 105     FORMAT (//' OUTPUT FILE SUMMARY ...'//)
         call wmdvfile(ID)
      END IF
      
      RETURN
      END
