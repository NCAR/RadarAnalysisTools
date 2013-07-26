      SUBROUTINE NEWHED(IOPT)
C
C        FILLS THE TABLE INFORMATION STRUCTURE FROM A NEW VOLUME HEADER
C              AND TIDIES UP THE ID HEADER JUST A BIT.
C                  IOPT=0, UPDATE ALL INFORMATION
C                      =1, UPDATE COORDINATE SPECS ONLY
C
C     VOLUME HEADER WORDS REFERENCED HERE:
C        ID(068) - General scaling factor, SF = 100
C        ID(175) - Number of fields (NFMAX = 25 set in CEDRIC.INC
C        ID(301) - Number of grid points per plane (For example,
C                  NX*NY in constant Z-planes)
C        ID(106) - Number of planes in a volume
C     VOLUME HEADER WORDS CALCULATED HERE:
C        ID(451) - Number of whole computer words (32- or 64-bit) 
C                  per plane rounded up to nearest computer word
C                  Note: WORDSZ = 32 or 64 so that
C        ID(452) - Number of whole computer words per field

      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      CHARACTER*2 CTEMP
      DATA IBL/1H /
      SAVE SFI

      IF(IOPT.EQ.1) GO TO 12
      J=175
      SFI=1./ID(68)
      NFL=ID(175)
      NPLANE=ID(301)
      ID(451)=(NPLANE-1)/(WORDSZ/16)+1
      ID(452)=ID(106)*ID(451)
      ID(453)=MIN0(NFMAX,MAXBUF/ID(452))
      PRINT *,'NEWHED: ID(068),ID(175),ID(301)=',ID(068),ID(175),ID(301)
      PRINT *,'NEWHED: ID(106),ID(451),ID(452)=',ID(106),ID(451),ID(452)
      PRINT *,'NEWHED:    NFMAX,MAXBUF/ID(452)=',NFMAX,MAXBUF/ID(452)
      PRINT *,' '

C     Note: The maximum number of fields in a run is limited to 25
C           as specified in CEDRIC.INC and cannot be increased due
C           to the structure within the 510 word header.  See
C           ID(176-300).  ID(301) and beyond used for other
C           purposes and cannot be shifted to accomodate more
C           fields. (LJM 9/12/2012)
C
      PRINT *,'++++ NEWHED: Maximum fields allowed =',ID(453),' ++++'
      ID(400)=ID(452)
      DO 10 I=1,NFMAX
      DO 5 L=1,4
         J=J+1
         IF(I.GT.NFL) ID(J)=IBL
         WRITE(CTEMP,17)ID(J)
 17      FORMAT(A2)
         NAMF(L,I)=CTEMP
    5 CONTINUE
      J=J+1
      IF(I.GT.NFL) ID(J)=0
      SCLFLD(I)=ID(J)
      IRCP(I)=1+ID(400)*(I-1)
      ID(400+I)=IRCP(I)
      IF(I.LE.NFL) THEN
         MAPVID(I,1)=I
         MAPVID(I,2)=I
      ELSE
         MAPVID(I,1)=0
         MAPVID(I,2)=0
      END IF
   10 CONTINUE
   12 CONTINUE
      J=160
      CM=1.0
      DO 15 I=1,3
C
C           MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
C
         IF(I.EQ.3) CM=FLOAT(ID(68))/1000.
         CSP(1,I)=ID(J)*SFI*CM
         CSP(2,I)=ID(J+1)*SFI*CM
         CSP(3,I)=ID(J+3)*0.001
C
C         SURFACE AND SINGLE PLANE DATA
C
          IF(ID(J+2).EQ.1.AND.ID(J+3).GT.0) CSP(3,I)=0.0
          IF(ID(J+3).LT.0) CSP(3,I) = -0.001
C
         NCX(I)=ID(J+2)
         NCXORD(I)=ID(J+4)
         J=J+5
   15 CONTINUE

C     OUTPUT FIELD NAMES TO A FILE FOR USE WITH GUI
      CALL FLDNAMOUT(ID)

      RETURN
      END
