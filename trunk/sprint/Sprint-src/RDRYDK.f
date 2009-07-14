      SUBROUTINE RDRYDK(KPKBUF,KUNBUF,NST,IUN,IFLG,NLEN)
C
C     READS FROM SCRATCH DISK UNIT AND RETURNS A SINGLE BLOCK OF 
C     16-BIT INFORMATION IN KUNBUF.
C
C     Apparently not used: IFLG = 0 ==> NORMAL READ
C                              ELSE ==> FORCED READ FROM DISK
C
C     IUN      - SCRATCH DISK UNIT
C     NST      - STATUS OF THE READ: (0) GOOD, (1) ERROR OR EOF
C     IVAL     - LENGTH OF RECORD (FIRST WORD IN RECORD)
C     NLEN     - RECORD LENGTH OF CURRENT RECORD (RETURNED)
C     IBEGREC  - RECORD LENGTH OF FIRST RECORD IN VOLUME
C     ILSTREC  - RECORD LENGTH OF NEXT RECORD
C     IBEG     - FLAG SET WHEN A VOLUME IS FULLY WRITTEN; UNSET AFTER
C                FIRST RECORD IS READ IN
C     READ_CNT - Number of beams read
C
C     Note: KPKBUF and KUNBUF are the same arrays as KPCK and KOUT
C           Leave dimensioned as ILSTREC and ILSTREC-1
C     
      INCLUDE 'SPRINT.INC'
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)

      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),IBEGREC
      COMMON /INTIO/IBEG

c-----debug (ljm) (02/11/1999)
c     See WRRYDK: READ(IUN) in WRRYDK2 causes this error
c           eor/uio: [1010] off end of record
c           logical unit 2, named 'fort.2'
c           lately: reading sequential unformatted external IO
c     Fix that works is to restore RDRY common block here.
c     I had added WRRY when working on DORADE and NEXRAD, 
c     but that broke the UF path.  (02/11/1999)
c     Lots of debugs in UFNCAR and GENAZM around calls to WRRYDK to find 
c     problem with FXTABLE.  It makes no sense how restoring RDRY common 
c     block makes this work, but that's not surprising.  Other formats
c     may not work now.  Need to check it out.
c
c-----COMMON /WRRY/ILSTREC,IWRYBEG
c
c     Found out why /RDRY/ exists when I replaced with SAVE (ljm) (09/13/2000)
c     SAVE ILSTREC leads to a compiler error
c        "RDRYDK.f", line 47: Error: "ilstrec":  
c        Adjustable array's bounds must be a dummy argument or in a common block
c
      COMMON /RDRY/ILSTREC

      DIMENSION KPKBUF(ILSTREC),KUNBUF(ILSTREC-1)
      DATA K,MODE,NTYPE,NSKIP,NBITS
     X      /0,1,2,0,16/

      INTEGER READ_CNT
      SAVE READ_CNT

      LOGICAL DEBUGIT
      DEBUGIT = .FALSE.

      IF (IBEG.NE.1) THEN
C
C     NORMAL READ
C
         READ(IUN,ERR=100,END=101) IVAL, KUNBUF
         NLEN=ILSTREC-1
         ILSTREC=IVAL
         READ_CNT=READ_CNT+1
c--------debug (ljm)
         if(debugit)then
            az=kunbuf(1)/float(jrh6)
            el=kunbuf(2)/float(jrh7)
            write(9,1770)read_cnt,ibegrec,nlen,ilstrec,az,el
 1770       format(3x,' Rd-norm: read_cnt,ibegrec,nlen,ilstrec=',
     +           i14,3i6,' az,el=',2f8.3)
         endif
c--------debug (ljm)
      ELSE
C
C     READ FIRST RECORD
C
         CALL RDRYDK2(IUN,KPKBUF,IBEGREC,NST)
         READ_CNT=READ_CNT+1
c--------debug (ljm)
         if(debugit)then
            write(9,1771)read_cnt,ibegrec,nlen,ilstrec,nst
 1771       format(3x,' Rd-init: read_cnt,ibegrec,nlen,ilstrec,nst=',
     +           i14,3i6,i4)
         endif
c--------debug (ljm)
         IF (NST.NE.0) GOTO 100
         NLEN=IBEGREC-1
         DO I=1,IBEGREC-1
            KUNBUF(I)=KPKBUF(I+1)
         END DO
         ILSTREC=KPKBUF(1)
c--------debug (ljm)
         if(debugit)then
            write(9,*)' Rd-#1: read_cnt,ibegrec,nlen,ilstrec=',
     +           read_cnt,ibegrec,nlen,ilstrec
         end if
c--------debug (ljm)
         IBEG=0
      END IF

      NST=0
      RETURN

 100  CONTINUE
      PRINT *,' RD READ-ERR: IBEGREC,CURLEN,PREVLEN=',IBEGREC,NLEN,
     X     ILSTREC
      NST=1
      RETURN

 101  CONTINUE
      PRINT *,' RD READ-EOF: IBEGREC,CURLEN,PREVLEN=',IBEGREC,NLEN,
     X     ILSTREC
      NST=1
      if(nst.eq.1)stop

      RETURN

      END


      SUBROUTINE RDRYDK2(IUN,KPKBUF,IBEGREC,NST)
      DIMENSION KPKBUF(IBEGREC)

      
      READ(IUN,ERR=100,END=101) KPKBUF

      NST=0
      RETURN

 100  CONTINUE
      PRINT *,' RD READ-ERR: IUN,IBEGREC=',IUN,IBEGREC
      NST=1
      RETURN

 101  CONTINUE
      PRINT *,' RD READ-EOF: IUN,IBEGREC=',IUN,IBEGREC
      NST=1
      if(nst.eq.1)stop
      RETURN

      END
