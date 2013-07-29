      SUBROUTINE FETCHD(IN,IDD,LEV,IFLD,IBUF,RBUF,N1,N2,
     X     IFIXAX,BAD,ZLEV,NST)
C
Cf2py intent(out) n1,n2,zlev,nst
C     
C     FETCHES DATA FOR A SINGLE FIELD FROM A PLANE
C     -INVERSE FUNCTION OF PLACED
C     
C     IN- INPUT UNIT  (IGNORED)
C     ID- FILE ID HEADER
C     NID- DIMENSION OF ID
C     LEV- LEVEL (PLANE) NUMBER
C     IFLD- FIELD NUMBER
C     IBUF- SCRATCH BUFFER
C     RBUF- CONTENTS OF FIELD FOR THIS PLANE
C     N1- FIRST DIMENSION OF RBUF (RETURNED BY FETCHD)
C     N2- SECOND    '     '   '       '     '    '
C     IFIXAX- FIXED AXIS (1,2 OR 3)
C     BAD- BAD DATA FLAG IN RBUF
C     NST- STATUS OF OPERATION (0-OK, 1-ERROR)
C     
      INCLUDE 'CEDRIC.INC'
      COMMON LCMB(MAXLCM)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NNPLANE,BADD
      CHARACTER*2 NAMF
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      DIMENSION IBUF(MAXPLN),RBUF(MAXPLN),IDD(NID),ITEMP(MAXPLN)
      INTEGER CVMGP
      DATA NBITS / 16 /

      IBAD=ID(67)
      SCALE=1./ID(175+IFLD*5)
      NWL=ID(451)
      NX=ID(162)
      NY=ID(167)
      NZ=ID(172)
      LOCD=ID(400+IFLD)
      ZLEV=VALLEV(LEV)
c      write(*,1770)lev,zlev,ifld,locd,ifixax,(axnam(n),n=1,3)
c 1770 format('FETCHD: lev,zlev,ifld,locd=',i6,f8.3,i6,i10,
c     +     ' ifixax,axnam=',i6,2x,3a1)
c--------------------------------------------------------------
c      IF ((AXNAM(3).EQ.'E' .OR. AXNAM(3).EQ.'C') 
c     X     .AND. IFIXAX.NE.3) THEN
c         WRITE(*,5)
c 5       FORMAT(/,5X,'+++ERROR: CANNOT PROCESS CONSTANT ANGLE',
c     X          ' SURFACES WITH FIXED AXIS OTHER THAN Z+++')
c         CALL FLUSH_STDOUT
c      END IF
c--------------------------------------------------------------
      IF (INMEM.EQ.0) THEN
C     
C     GET FROM DISK
C     
         NPLANE=0
         NXX=NX
         FLNX=NX*16.0/WORDSZ
         INNX=INT(NX*16.0/WORDSZ)
         IDIFF = (FLNX - REAL(INNX))*(WORDSZ/16.0)
         IF (IDIFF.NE.0) THEN
            NXX = NX + (WORDSZ/16.0) - IDIFF
            NWL = (NXX*NY - 1)/(WORDSZ/16.0) + 1
            LOCD = 1 + NWL*NZ*(MAPVID(IFLD,2) - 1)
         END IF
         MEM=0
         IVOL=0
         
         GO TO (40,30,10), IFIXAX
 10      CONTINUE
C     
C     Z- PLANE IS REQUESTED
C     
         ZLEV=VALLEV(LEV)
         N1=NX
         N2=NY
         NPLANE=NXX*NY
         LOCD=LOCD+(LEV-1)*NWL
         IBIT=0
         NSKIP=0
         CALL GETD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,NPLANE,IFIXAX,
     X        ID,NX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
         IF (IDIFF.NE.0) THEN
C     
C     UNDO PADDING
C     
            K=1
            L=1
            DO 130 I=1,NY
               DO 120 J=1,NX
                  ITEMP(L)=IBUF(K)
                  K=K+1
                  L=L+1
 120           CONTINUE
               K=K+(NXX-NX)
 130        CONTINUE
            DO 140 I=1,(NY*NX)
               IBUF(I)=ITEMP(I)
 140        CONTINUE
         END IF
         GOTO 90
 30      CONTINUE
C     
C     Y- PLANE IS DESIRED
C     
         ZLEV=(LEV-1)*CSP(3,2) + CSP(1,2)
         N1=NX
         N2=NZ
         IBIT=NBITS*(LEV-1)*NXX
         NSKIP=0
         IRP=NXX
         CALL GETD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,
     X        ID,NX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
         IF (IDIFF.NE.0) THEN
            K=1
            L=1
            DO 200 I=1,NZ
               DO 220 J=1,NX
                  ITEMP(L)=IBUF(K)
                  K=K+1
                  L=L+1
 220           CONTINUE
               K=K+(NXX-NX)
 200        CONTINUE
            DO 250 I=1,NZ*NX
               IBUF(I)=ITEMP(I)
 250        CONTINUE
         END IF
         GO TO 50
 40      CONTINUE
C     
C     X- PLANE IS DESIRED
C     
         ZLEV=(LEV-1)*CSP(3,1)+CSP(1,1)
         N1=NY
         N2=NZ
         IBIT=NBITS*(LEV-1)
         NSKIP=NBITS*(NXX-1)
         IRP=NY
         CALL GETD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,
     X        ID,NXX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
         
         
 50      CONTINUE
C     
C     TRANSFER DATA FROM TEMPORARY BUFFER TO RBUF
C     
 90      CONTINUE
         NPLANE=N1*N2
         DO 100 I=1,NPLANE
            ITEST=IBUF(I)
            ITEST=CVMGP(ITEST-65536,ITEST,ITEST-32768)
            TEST = ITEST*SCALE
            RBUF(I)=CVMGT(TEST,BAD,ITEST.NE.IBAD)
 100     CONTINUE
         NST=0
         RETURN
      ELSE
C     
C     GET FROM MEMORY
C     
         GO TO (402,302,102), IFIXAX
 102     CONTINUE
C     
C     Z- PLANE IS REQUESTED
C     
         ZLEV=VALLEV(LEV)
         N1=NX
         N2=NY
         NPLANE=NX*NY
         LOCD=LOCD+(LEV-1)*NWL
         IBIT=0
         NSKIP=0
         CALL GBYTES(LCMB(LOCD),IBUF,IBIT,NBITS,NSKIP,NPLANE)
         GOTO 902
 302     CONTINUE
C     
C     Y- PLANE IS DESIRED
C     
         ZLEV=(LEV-1)*CSP(3,2)+CSP(1,2)
         N1=NX
         N2=NZ
         IBIT=NBITS*(LEV-1)*NX
         NSKIP=0
         IRP=NX
         GO TO 502
 402     CONTINUE
C     
C     X- PLANE IS DESIRED
C     
         ZLEV=(LEV-1)*CSP(3,1)+CSP(1,1)
         N1=NY
         N2=NZ
         IBIT=NBITS*(LEV-1)
         NSKIP=NBITS*(NX-1)
         IRP=NY
 502     CONTINUE
C     
C     PULL DATA FOR X AND Y PLANES
C     
         K=1
         DO 602 I=1,NZ
            CALL GBYTES(LCMB(LOCD),IBUF(K),IBIT,NBITS,NSKIP,IRP)
            K=K+IRP
            LOCD=LOCD+NWL
 602     CONTINUE
C     
C     TRANSFER DATA FROM TEMPORARY BUFFER TO RBUF
C     
 902     CONTINUE
         NPLANE=N1*N2
         DO 1002 I=1,NPLANE
            ITEST=IBUF(I)
            ITEST=CVMGP(ITEST-65536,ITEST,ITEST-32768)
            RBUF(I)=CVMGT(ITEST*SCALE,BAD,ITEST.NE.IBAD)
 1002    CONTINUE
         NST=0
         RETURN
         
      END IF
      
      END
