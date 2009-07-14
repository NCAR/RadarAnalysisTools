c
c----------------------------------------------------------------------X
c
      SUBROUTINE DISP(INDAT,NFRAME,BGFLAG,GRAYTYP,GSTR,GRAYEST)
C
C  DISPOSE METAFILES TO FILM, MASS STORE OR ANOTHER MACHINE.
C
      PARAMETER (NDSP=10)
      CHARACTER*1 BGFLAG
      CHARACTER*8 INDAT(10),JNDAT(10)
      CHARACTER*8 NAMDSP(NDSP)
      CHARACTER*64 QUALIF
      CHARACTER*120 INSTR
      CHARACTER*4 GRAYTYP
      DATA NAMDSP/'MSS     ','RCP     ','CP      ','BW      ',
     +            'FICHE   ','COL_HI  ','COL_MED ','COL_SCR ',
     +            'COL_LOW ','COL_BET '/

      WRITE(6, 3)NFRAME
    3 FORMAT(/,1X,'TOTAL FRAMES DISPOSED= ',I4,/)

C     Close GKS
C
      CALL CLSGKS
      INSTR='ls -l gmeta'
      IERR=ishell(INSTR)

C     READ COMMANDS AND QUALIFIERS UNTIL END-OF-STACK.
C     Open GKS and continue execution.
C
    4 READ(5, 5)(JNDAT(I),I=1,10)
    5 FORMAT(10A8)
      IF(JNDAT(1).EQ.'END     ')THEN
         CALL OPNGKS
         CALL GSCLIP(0)
         NFRAME=0
         CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)
         CALL PCSETC('FC', '&')
         RETURN
      END IF
      IF(JNDAT(1).NE.'        ')THEN
         WRITE(6, 7)
    7    FORMAT(1X,'*** DISPOSE: NO END LINE ENCOUNTERED ***')
         STOP
      END IF

      QUALIF=JNDAT(3)//JNDAT(4)//JNDAT(5)//JNDAT(6)//JNDAT(7)//
     +JNDAT(8)//JNDAT(9)//JNDAT(10)
      WRITE(6, 9)JNDAT(2),QUALIF
    9 FORMAT(9X,A8,A64)
      IDST=IFIND(JNDAT(2),NAMDSP,NDSP)
      IF(IDST.EQ.0)IDST=1

      GO TO (10,20,30,40,50,60,70,80,90,100)IDST

C     DISPOSE TO MASS STORE
C
   10 INSTR='lwrite local=gmeta format=tr remote='//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     REMOTE COPY TO A UNIX MACHINE
C
   20 INSTR='rcp gmeta '//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     COPY TO ANOTHER SHAVANO FILE
C
   30 INSTR='cp gmeta '//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO B/W FILM
C
   40 INSTR='sendtg gmeta -r macr=VIEWERBW titl='//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO B/W FICHE
C
   50 INSTR='sendtg gmeta -r macr=FICHE titl='//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO COLOR USING 4096 RASTERIZATION
C
   60 INSTR='sendtg gmeta -r macr=VIEWERCL qual=BEST res=HIGH titl='
     +//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO COLOR USING 2048 RASTERIZATION
C
   70 INSTR='sendtg gmeta -r macr=VIEWERCL qual=BEST res=MEDIUM titl='
     +//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO COLOR USING 1024 RASTERIZATION
C
   80 INSTR='sendtg gmeta -r macr=VIEWERCL qual=BEST res=SCREEN titl='
     +//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO COLOR USING  512 RASTERIZATION
C
   90 INSTR='sendtg gmeta -r macr=VIEWERCL qual=BEST res=LOW titl='
     +//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

C     DISPOSE TO COLOR USING CTRANS
C
  100 INSTR='sendtg gmeta -r macr=VIEWERCL qual=BETTER titl='
     +//QUALIF
      IERR=ishell(INSTR)
      GO TO 4

      END
