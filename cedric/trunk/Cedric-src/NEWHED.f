      SUBROUTINE NEWHED(IOPT)
C
C        FILLS THE TABLE INFORMATION STRUCTURE FROM A NEW VOLUME HEADER
C              AND TIDIES UP THE ID HEADER JUST A BIT.
C                  IOPT=0, UPDATE ALL INFORMATION
C                      =1, UPDATE COORDINATE SPECS ONLY
C
      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      CHARACTER*2 CTEMP
      DATA IBL/' '/
      SAVE SFI

      IF(IOPT.EQ.1) GO TO 12
      J=175
      SFI=1./ID(68)
      NFL=ID(175)
      NPLANE=ID(301)
      ID(451)=(NPLANE-1)/(WORDSZ/16)+1
      ID(452)=ID(106)*ID(451)
      ID(453)=MIN0(NFMAX,MAXBUF/ID(452))
      PRINT *,'NEWHED: ID(106),ID(451),ID(452)=',ID(106),ID(451),ID(452)
      PRINT *,'NEWHED:    NFMAX,MAXBUF/ID(452)=',NFMAX,MAXBUF/ID(452)
      PRINT *,' '
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
