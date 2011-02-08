c
c----------------------------------------------------------------------X
c
      FUNCTION IFNDCOL(JCOL)

      INCLUDE 'colors.inc'
      CHARACTER*8 COLOR(20),JCOL
      DATA COLOR/'BLACK   ','WHITE   ','GRAY    ',17*'        '/
      DATA NCOLS/3/

      IC=0
      DO 10 I=1,NCOLS
      IF(JCOL.EQ.COLOR(I))THEN
         IC=I
         GO TO 20
      END IF
10    CONTINUE
20    IF(IC.EQ.0)THEN
         PRINT 30,JCOL
30       FORMAT(1X,'UNKNOWN COLOR ',A8,1X,'WILL ASSUME BLACK FOR
     +   VECTOR COLOR')
         IFNDCOL=IBLACK
      END IF
      IFNDCOL=IC+61
      RETURN
      END
