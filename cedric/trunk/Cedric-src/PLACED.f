      SUBROUTINE PLACED(IOUT,IDD,LEV,IFLD,IBUF,RBUF,N1,N2,
     X     IFIXAX,BAD,NST)
C     
C     WRITES A SINGLE FIELD (IN A PLANE) TO DISK
C     -INVERSE FUNCTION OF FETCHD
C     
C     IOUT- OUTPUT UNIT (IGNORED)
C     ID- FILE ID HEADER
C     LEV- LEVEL (PLANE) NUMBER
C     IFLD- FIELD NUMBER
C     IBUF- SCRATCH BUFFER
C     RBUF- CONTENTS OF FIELD FOR THIS PLANE
C     N1- FIRST DIMENSION OF RBUF
C     N2- SECOND    '     '   '
C     IFIXAX- FIXED AXIS (1,2 OR 3)
C     BAD- BAD DATA FLAG IN RBUF
C     NST- STATUS OF OPERATION (0-OK, 1-ERRORC

      INCLUDE 'CEDRIC.INC'
      COMMON LCMB(MAXLCM),MEMUSE
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NNPLANE,BADD
      COMMON /FILTYPE/ FILE_TYPE
      INTEGER FILE_TYPE
      CHARACTER*2 NAMF
      DIMENSION IBUF(1),RBUF(1),IDD(NID),ITEMP(MAXPLN)
      DATA IBIT,NBITS,NSKIP/0,16,0/
      IBAD=ID(67)
      MULT=ID(175+IFLD*5)
      NWL=ID(451)
      LOCD=ID(400+IFLD)
      NX=ID(162)
      NY=ID(167)
      NZ=ID(172)
      NPLANE=N1*N2
      IF (INMEM.EQ.0) THEN
C     
C     GET FROM DISK
C     
         NXX=NX
         FLNX=NX*16.0/WORDSZ
         INNX=INT(NX*16.0/WORDSZ)
         IDIFF = (FLNX - REAL(INNX))*(WORDSZ/16.0)
         IF (IDIFF.NE.0) THEN
            NXX = NX + (WORDSZ/16.0) - IDIFF
            NWL = (NXX*NY - 1)/(WORDSZ/16.0) + 1
            LOCD = 1 + NWL*NZ*(MAPVID(IFLD,2) - 1)
         END IF
C     
C     DETERMINE IF EDIT VOLUME IS IN MEMORY OR ON DISK
C     
         IF (MEMUSE.GE.1) THEN
            MEM=1
         ELSE
            MEM=0
         END IF
         IVOL=0
C     
C     DATA IS ALWAYS STORED BY Z-PLANES, CONVERT TO I*2 AND STORE
C     ACCORDING TO THE FIXED AXIS OF THE INPUT ARRAY.
C     
         NST=0
         KBAD=0
         DO 15 I=1,NPLANE
            IBUF(I)=IBAD
            IF(RBUF(I).EQ.BAD .OR. RBUF(I) .EQ. -32768.) GO TO 15
            IF (ABS(RBUF(I)*MULT).GE.32768) GO TO 14
            ICHECK=NINT(RBUF(I)*MULT)
            IF(IABS(ICHECK).GE.32768)GO TO 14  
            IBUF(I)=ICHECK
            GO TO 15
 14      IF(FILE_TYPE .EQ. 0) KBAD=KBAD+1
 15      CONTINUE
C     
C     TYPE OUT MESSAGE IF OUT OF BOUNDS VALUES HAVE BEEN DETECTED AND
C     RESET TO IBAD.
C     
         IF(KBAD.NE.0) THEN
            I1=171+IFLD*5
            I2=I1+3
            PRINT 101, KBAD,(ID(I),I=I1,I2),LEV
 101        FORMAT(' +++  ',I5,' VALUES OUT OF RANGE IN FIELD: ',4A2,
     X           ' AT PLANE=',I3,'  +++')
         ENDIF
         GO TO (40,30,20), IFIXAX
 20      CONTINUE
C     
C     CONSTANT Z-ARRAY
C     
         IBIT=0
         NSKIP=0
         NPLANE=NXX*NY
         LOCD=LOCD + (LEV-1)*NWL
         IF (IDIFF.NE.0) THEN
C     
C     PUT IN PADDING
C     
            K=1
            L=1
            DO 100 I=1,NY
               DO 120 J=1,NX
                  ITEMP(L)=IBUF(K)
                  K=K+1
                  L=L+1
 120           CONTINUE
               L=L+NXX-NX
 100        CONTINUE
            DO 150 I=1,NPLANE
               IBUF(I)=ITEMP(I)
 150        CONTINUE
         END IF
         CALL PUTD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,ID,
     X        NX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
C     CALL SBYTES(LCMB(LOCD),IBUF,IBIT,NBITS,NSKIP,NPLANE)
         RETURN
 30      CONTINUE
C     
C     CONSTANT Y-ARRAY
C     
         IBIT=NBITS*(LEV-1)*NXX
         NSKIP=0
         IRP=NXX
         IF (IDIFF.NE.0) THEN
C     
C     PUT IN PADDING
C     
            K=1
            L=1
            DO 200 I=1,NZ
               DO 220 J=1,NX
                  ITEMP(L)=IBUF(K)
                  K=K+1
                  L=L+1
 220           CONTINUE
               L=L+NXX-NX
 200        CONTINUE
            DO 250 I=1,NZ*NXX
               IBUF(I)=ITEMP(I)
 250        CONTINUE
         END IF
         CALL PUTD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,ID,
     X        NX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
         RETURN
 40      CONTINUE
C     
C     CONSTANT X-ARRAY
C     
         IBIT=NBITS*(LEV-1)
         NSKIP=NBITS*(NXX-1)
         IRP=NY
         CALL PUTD(IBUF,NPLANE,LOCD,IBIT,NBITS,NSKIP,IRP,IFIXAX,ID,
     X        NXX,NY,NZ,LEV,ITEMP,NWL,LCMB,MEM,IVOL)
         RETURN
      ELSE
C     
C     GET FROM MEMORY
C     
C     
C     DATA IS ALWAYS STORED BY Z-PLANES, CONVERT TO I*2 AND STORE
C     ACCORDING TO THE FIXED AXIS OF THE INPUT ARRAY.
C     
         NST=0
         KBAD=0
         DO 152 I=1,NPLANE
            IBUF(I)=IBAD
            IF(RBUF(I).EQ.BAD) GO TO 152
            ICHECK=NINT(RBUF(I)*MULT)
            IF(IABS(ICHECK).GE.32768) GO TO 142
            IBUF(I)=ICHECK
            GO TO 152
 142        KBAD=KBAD+1
 152     CONTINUE
C     
C     TYPE OUT MESSAGE IF OUT OF BOUNDS VALUES HAVE BEEN DETECTED AND
C     RESET TO IBAD.
C     
         IF(KBAD.NE.0) THEN
            I1=171+IFLD*5
            I2=I1+3
            PRINT 102, KBAD,(ID(I),I=I1,I2),LEV
 102        FORMAT(' +++  ',I5,' VALUES OUT OF RANGE IN FIELD: ',4A2,
     X           ' AT PLANE=',I3,'  +++')
         ENDIF
         GO TO (42,32,22), IFIXAX
 22      CONTINUE
C     
C     CONSTANT Z-ARRAY
C     
         IBIT=0
         NSKIP=0
         LOCD=LOCD + (LEV-1)*NWL
         CALL SBYTES(LCMB(LOCD),IBUF,IBIT,NBITS,NSKIP,NPLANE)
         RETURN
 32      CONTINUE
C     
C     CONSTANT Y-ARRAY
C     
         IBIT=NBITS*(LEV-1)*NX
         NSKIP=0
         IRP=NX
         GO TO 52
 42      CONTINUE
C     
C     CONSTANT X-ARRAY
C     
         IBIT=NBITS*(LEV-1)
         NSKIP=NBITS*(NX-1)
         IRP=NY
 52      CONTINUE
         K=1
         DO 62 I=1,NZ
            CALL SBYTES(LCMB(LOCD),IBUF(K),IBIT,NBITS,NSKIP,IRP)
            K=K+IRP
            LOCD=LOCD+NWL
 62      CONTINUE
         RETURN
         
      END IF
      
      END
