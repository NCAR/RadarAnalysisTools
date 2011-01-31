      SUBROUTINE WRRYDK(KPKBUF,KUNBUF,NST,IUN,IFLG,NLEN)
C
C     WRITES A BLOCK OF (NLEN+1) 16-BIT WORDS INTO A BLOCK ON DISK
C                 IFLG<0  => DISCARD BLOCK (RECORD)
C                 IFLG=0  => NORMAL WRITE
C                 IFLG>0  => FORCE ONTO DISK
C
C     IUN      - SCRATCH DISK UNIT.  USUALLY fort.1, 
C                EXCEPT ANGLE-FILL ==> 2 AND FILTERING ==> 3.
C     NST      - STATUS OF THE WRITE: (0) GOOD, (1) ERROR OR EOF
C     NLEN     - RECORD LENGTH OF CURRENT RECORD
C     IBEGREC  - RECORD LENGTH OF FIRST RECORD IN VOLUME
C     ILSTREC  - RECORD LENGTH OF PREVIOUS RECORD
C     IWRYBEG  - FLAG SET WHEN A NEW VOLUME IS STARTED
C     IBEG     - FLAG SET WHEN A VOLUME IS FULLY WRITTEN
C     WRIT_CNT - Number of beams written
C
C     Note: The arrays KPKBUF and KUNBUF are the same as KPCK and KOUT.
C           Leave dimensioned as ILSTREC and NLEN
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)

      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),IBEGREC
      COMMON /INTIO/IBEG
      COMMON /WRRY/ILSTREC,IWRYBEG
      DIMENSION KPKBUF(ILSTREC),KUNBUF(NLEN)
      DATA K,MODE,NTYPE,IBITS,NBITS,NSKIP,KW,KBITS,icnt
     X      /1,1,2,0,16,0,1,0,1/
      DATA IFLGMX/10/

      INTEGER WRIT_CNT
      SAVE WRIT_CNT

c      LOGICAL DEBUGIT
c      DEBUGIT = .FALSE.

      IF (IFLG.LT.0) THEN
         IWRYBEG=1
         NST=0
c--------debug (ljm)
         if(debugit)then
            write(9,*)
     +      '   Wr-init: iflg,iwrybeg,ibegrec,curlen,prevlen=',iflg,
     +           iwrybeg,ibegrec,nlen,ilstrec
            WRIT_CNT = 0
         end if
c--------debug (ljm)
         RETURN

      ELSE IF (IFLG.EQ.0) THEN
         WRIT_CNT=WRIT_CNT+1
         IF (IWRYBEG.EQ.1) THEN
C
C     PUT INITIAL BEAM INTO BUFFER FOR WRITING AND INITIALIZE
C     RECORD LENGTHS.
C
            IWRYBEG=0
            IBEGREC=NLEN+1
            ILSTREC=IBEGREC
            DO I=1,NLEN
               KPKBUF(I+1)=KUNBUF(I)
            END DO
c--------debug (ljm)
            if(debugit)then
               write(9,1769)iflg,writ_cnt,ibegrec,nlen,ilstrec
 1769          format(3x,
     +         'WRRYDK-INIT: iflg,writ_cnt,ibegrec,curlen,prevlen=',
     +              i2,i14,3i6)
            end if
c--------debug (ljm)
         ELSE
C
C     REGULAR WRITE
C
            KPKBUF(1)=NLEN+1
c-----------debug (ljm)
            if(debugit)then
               az=kout(1)/float(jrh6)
               el=kout(2)/float(jrh7)
               write(9,1770)iflg,writ_cnt,ibegrec,nlen,ilstrec,az,el
 1770          format(3x,
     +         'Wr-beam: iflg,writ_cnt,ibegrec,curlen,prevlen=',
     +              i2,4i8,' az,el=',2f8.3)
            end if
c-----------debug (ljm)
            WRITE(IUN,ERR=100)KPKBUF
            DO I=1,NLEN
               KPKBUF(I+1)=KUNBUF(I)
            END DO
            ILSTREC=NLEN+1
         END IF
         NST=0
         RETURN

      ELSE IF (IFLG.GT.0 .AND. IFLG.LT.IFLGMX) THEN
C
C     CLOSE OFF VOLUME
C
c--------debug (ljm)
         if(debugit)then
            write(9,*)
     +      '   Wr-cl: iflg,writ_cnt,ibegrec,cur,prev,kpk(1)=',
     +           iflg,writ_cnt,ibegrec,nlen,ilstrec,kpkbuf(1)
         end if
c--------debug (ljm)
         WRITE(IUN,ERR=100)KPKBUF
         NST=0
         IBEG=1
         RETURN

      ELSE IF (IFLG.GT.IFLGMX) THEN
C
C     NEED TO BACKUP TO DISCARD A SWEEP
C
c--------debug (ljm)
         if(debugit)then
            write(9,*)
     +      '   Wr-bu: iflg,writ_cnt,ibegrec,curlen,prevlen=',
     +           iflg,writ_cnt,ibegrec,nlen,ilstrec
         end if
c--------debug (ljm)
         IFLG=IFLG-IFLGMX
         IF (NLEN.EQ.0) THEN
            NST=0
            RETURN
         END IF
         IF (IFLG.GT.1) THEN
C
C     IF SWEEP NUMBER GREATER THAN 1
            DO I=1,NLEN+1
               BACKSPACE(IUN,ERR=102)
            END DO
            READ(IUN,ERR=100,END=101)ILSTREC

c-----------(ljm) (02/11/1999) 
c           Next READ(IUN) in WRRYDK2 causes this error
c           eor/uio: [1010] off end of record
c           logical unit 2, named 'fort.2'
c           lately: reading sequential unformatted external IO
c           See RDRYDK for fix - restore RDRY common block
c-----------(ljm) (02/11/1999)

            CALL WRRYDK2(IUN,ILSTREC,KPKBUF)
            BACKSPACE(IUN,ERR=102)
            NST=0
            RETURN
         ELSE
C
C     IF FIRST SWEEP
            DO I=1,NLEN-1
               BACKSPACE(IUN,ERR=102)
            END DO
            IWRYBEG=1
            NST=0
            RETURN
         END IF
      END IF
      
 100  CONTINUE
      PRINT *,' WR READ-ERR: IUN,ILSTREC=',IUN,ILSTREC
      NST=1
      RETURN

 101  CONTINUE
      PRINT *,' WR READ-EOF: IUN,ILSTREC=',IUN,ILSTREC
      NST=1
      RETURN

 102  CONTINUE
      PRINT *,' WR BACKSPACE-ERR: IUN=',IUN
      NST=1
      RETURN
      END


      SUBROUTINE WRRYDK2(IUN,ILSTREC,KPKBUF)
C     NEEDED TO READ IN A RECORD OF LENGTH ILSTREC WORDS
      DIMENSION KPKBUF(ILSTREC)

      READ(IUN,ERR=100,END=101)KPKBUF

 100  CONTINUE
      PRINT *,' WR READ-ERR: IUN=',IUN
      NST=1
      RETURN

 101  CONTINUE
      PRINT *,' WR READ-EOF: IUN=',IUN
      NST=1
      RETURN

      END


      
