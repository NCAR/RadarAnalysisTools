c
c----------------------------------------------------------------------X
c
      SUBROUTINE SAVPLT(INDAT,IFMT,SINDAT,NAMFLD,IFLD,NFLDS,NP,MXPLT)
C
C  STORE PLOT CHARARACTERISTICS FOR LATER EXECUTION
C     INDAT  - INPUT ARRAY CONTAINING CHARACTERISTICS FOR ONE PLOT
C     SINDAT - OUTPUT ARRAY CONTAINING ALL (MXPLT) PLOT CHARACTERISTICS
C     NP     - NUMBER OF PLOTS TO BE DONE

      INCLUDE 'dim.inc'

      CHARACTER*8 INDAT(10),SINDAT(10,MXPLT)
      CHARACTER*8 NAM,NAMFLD(MXF),IFMT
      CHARACTER*4 NAMOUT
      CHARACTER*1 BGFLAG
      SAVE BGFLAG
      DATA NAMOUT/'    '/
      DIMENSION IFLD(MXF)

      NP=NP+1
      IF(NP.GT.MXPLT)THEN
         WRITE(6,11)MXPLT
 11      FORMAT(1X,'*** MAXIMUM NUMBER OF PLOTS (',I3,') EXCEEDED ***')
         STOP
      END IF

c      WRITE(6,55)(INDAT(I),I=2,10),NP
 55   FORMAT(1X,' CONT: ',9A8,' NP=',I8)
      READ(INDAT,60)NAM
 60   FORMAT(/A8)
      DO 70 I=1,10
         SINDAT(I,NP)=INDAT(I)
 70   CONTINUE

c      print *,'SAVPLT: indat(1)=',indat(1)
c      print *,'SAVPLT: indat(2)=',indat(2)
c      print *,'SAVPLT: indat(4)=',indat(4)
      IF(INDAT(1).EQ.'LABELS  ')RETURN
      IF(INDAT(1).EQ.'BCKGRND ')THEN
         BGFLAG=INDAT(2)(1:1)
c         print *,'SAVPLT before CALL SETBCKGRND, bgflag=',bgflag
         CALL SETBCKGRND(BGFLAG)
         RETURN
      ENDIF
      IF(INDAT(1).EQ.'SETWIN  ')RETURN
      IF(INDAT(4).EQ.'SAMPLOC ')RETURN
      IF(INDAT(4)(1:6).EQ.'DIGTIZ')RETURN

      CALL FIELD(NAM,NAMFLD,NFLDS,NAMOUT)

      IF(INDAT(7).EQ.'NON')THEN
 80      READ(5,81)(INDAT(I),I=1,10)
 81      FORMAT(10A8)
c         WRITE(6,82)(INDAT(I),I=1,10)
c 82      FORMAT('Kardin=',10A8)
         NP=NP+1
         DO I=1,10
            SINDAT(I,NP)=INDAT(I)
         END DO
         IF(INDAT(1).EQ.'        ')THEN
            WRITE(6,83)(INDAT(I),I=2,10),NP
 83         FORMAT(8X,9A8,' NP=',I8)
            GO TO 80
         ELSE IF(INDAT(1).EQ.'END')THEN
            WRITE(6,85)NP
 85         FORMAT(1X,' END',75X,' NP=',I8)
            RETURN
         END IF
      END IF

      RETURN
      END
