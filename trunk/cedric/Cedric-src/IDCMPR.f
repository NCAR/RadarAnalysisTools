      SUBROUTINE IDCMPR(ID1,ID2,NID,IST)
C
C        COMPARES TWO HEADERS TO SEE THAT THE CARTESIAN STRUCTURES
C           MATCH. IF NST=0, OK
C                        =1, INCOMPATIBLE
C
      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL
      DIMENSION ID1(NID),ID2(NID)
      IST=0
      IDST=0
C
C        COMPARE ORIGIN HEIGHTS AND AXIS ORIENTATION
C
      DO 10 I=39,42
      IF(ID1(I).EQ.ID2(I)) GO TO 10
      IDST=2
      WRITE(LPR,101) I,ID1(I),ID2(I)
  101 FORMAT(5X,'WORD ',I3,' DIFFERS BETWEEN HEADERS:  1:',
     X       I6,4X,'2:',I6)
   10 CONTINUE
C
C        COMPARE MISSING DATA FLAG AND HEADER SCALING FACTORS
C
      DO 11 I=67,69
      IF(ID1(I).EQ.ID2(I)) GO TO 11
      IF(IDST.NE.2.AND.I.EQ.68) THEN
         IDST=1
      ELSE
         IDST=2
      END IF
      WRITE(LPR,101) I,ID1(I),ID2(I)
   11 CONTINUE
C
C        COMPARE COORDINATE ORIGINS
C
      DO 15 I=309,311
      IF(ID1(I).EQ.ID2(I)) GO TO 15
      IDST=2
      WRITE(LPR,101) I,ID1(I),ID2(I)
   15 CONTINUE
C
C        CHECK AXES ALIGNMENTS
C
      METR1=1000/ID1(68)
      METR2=1000/ID2(68)
      DO 30 I=160,170,5
         IF(I.GE.170) THEN
            METR1=1
            METR2=1
         END IF
         print *,'IDCMPR: i,id1, id2=',i+3,id1(i+3),id2(i+3)
         IF(ID1(I+3).LT.0.OR.ID2(I+3).LT.0) GO TO 30
C
C           NON-SURFACE (SINGLE-LEVEL) DATA SITUATION - 
C
            NZ1=ID1(I+2)
            NZ2=ID2(I+2)
            K1=1
            IZ1=ID1(I)*METR1
            DO 25 K2=1,NZ2
               IZ2=(ID2(I)*METR2)+((K2-1)*ID2(I+3))
   21          CONTINUE
                  IF(IZ1.EQ.IZ2) GO TO 30
                  IF(IZ1.GT.IZ2) GO TO 25
C                                       POSITION IN ID1 IS .LT. ID2

                     K1=K1+1
                     IF(K1.GT.NZ1) GO TO 26
                     IZ1=IZ1+ID1(I+3)
                     GO TO 21
   25       CONTINUE
C                                       NO MATCH ALONG THIS AXIS -SET FLAG
   26       IDST=2
C
   30 CONTINUE
C
      IF(IDST.EQ.2) GO TO 45
C
C        NON-FATAL AGREEMENT SO FAR, CHECK FOR MINOR DISAGREEMENT
C
      DO 40 I=160,174
      IF(ID1(I).EQ.ID2(I)) GO TO 40
      IDST=1
      GO TO 45
   40 CONTINUE
   45 CONTINUE
      IF(IDST.EQ.2) THEN
         WRITE(LPR,102)
  102    FORMAT(/5X,'+++  CARTESIAN STRUCTURES OF INPUT AND EDIT FILES',
     X          ' ARE INCOMPATIBLE  +++')
         IST=1
         RETURN
      ELSE IF(IDST.EQ.1) THEN
         WRITE(LPR,103)
  103    FORMAT(/' WARNING...  CARTESIAN STRUCTURES OF INPUT AND EDIT',
     X           ' FILES ARE NOT IDENTICAL')
      END IF
      RETURN
      END
