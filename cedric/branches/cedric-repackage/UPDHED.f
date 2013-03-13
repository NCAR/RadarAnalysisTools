      SUBROUTINE UPDHED(IOPT)
C
C        UPDATES THE EDIT HEADER BASED UPON THE CURRENT INFORMATION
C           IN THE TABLE STRUCTURE, FIELD MAPPINGS AND COUNT.
C
      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      DATA IBL/1H /
      SF=ID(68)
      ID(301)=NPLANE
      I=NCXORD(3)
      ID(106)=NCX(I)
      ID(451)=(NPLANE-1)/(WORDSZ/16)+1
      ID(452)=ID(106)*ID(451)
      ID(453)=MIN0(NFMAX,MAXBUF/ID(452))
c-----debug
c      PRINT *,'UPDHED: ID(106),ID(451),ID(452)=',ID(106),ID(451),ID(452)
c      PRINT *,'UPDHED:    NFMAX,MAXBUF/ID(452)=',NFMAX,MAXBUF/ID(452)
c      PRINT *,'++++ UPDHED: Maximum fields allowed =',ID(453),' ++++'
c-----debug
      ID(400)=ID(452)
      J=160
      CM=1.0
      DO 10 I=1,3
C        MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
      IF(I.EQ.3) CM=1000./FLOAT(ID(68))
      ID(J)=NINT(CSP(1,I)*SF*CM)
      ID(J+1)=NINT(CSP(2,I)*SF*CM)
      ID(J+3)=NINT(CSP(3,I)*1000.)
C
C     SURFACE AND SINGLE PLANE DATA
C
      IF(NCX(I).EQ.1.AND.ID(J+3).GT.0) ID(J+3)=0
      IF(ID(J+3).LT.0) ID(J+3) = -1
C
      ID(J+2)=NCX(I)
      ID(J+4)=NCXORD(I)
      J=J+5
   10 CONTINUE
      K=175
      J=175
      N=0
      DO 25 I=1,NFMAX
      MAPVID(I,1)=0
      MAPVID(I,2)=0
      DO 15 L=1,4
         ID(J+L)=IBL
   15 CONTINUE
      ID(J+5)=0
      IRCP(I)=1+ID(400)*(I-1)
      ID(400+I)=0
      IF(SCLFLD(I).EQ.0.0) GO TO 24
      N=N+1
      MAPVID(I,1)=N
      MAPVID(N,2)=I
      DO 20 L=1,4
         READ(NAMF(L,I),35)ID(K+L)
 35      FORMAT(A2)
C         ID(K+L)=NAMF(L,I)
   20 CONTINUE
      ID(K+5)=SCLFLD(I)
      ID(400+N)=IRCP(I)
      K=K+5
   24 CONTINUE
      J=J+5
   25 CONTINUE
      NFL=N
      ID(175)=N
      ID(96)=(ID(301)-1)/ID(65)+1
      ID(97)=ID(96)*ID(175)
      ID(98)=ID(97)*ID(106)
      ID(99)=ID(98)+ID(106)+1
      ID(100)=ID(98)+1

C     OUTPUT FIELD NAMES TO A FILE FOR USE WITH GUI
      CALL FLDNAMOUT(ID)

      RETURN
      END
