      SUBROUTINE OUTFIL(KRD)
C     
C     FILLS COMMON /UNITS/
C     FORMAT OF OUT CARD
C     
C     
C     VARIABLE  DESCRIPTION                  FIELD  NOTES
C     --------  -----------                  -----  -----
C     
C     KOMM      'OUT'                         P1    COMMAND
C     LUN       UNIT NUMBER                   P2
C     LTAP      TAPE NAME                     P3
C     NCOD      SPECIFIES WHERE TO START      P4
C     WRITING:
C     'BEG'                         WRITE BEGINNING OF TAPE
C     'ADD'                         WRITE AFTER ALL INFO
C     'SKI'                         SKIP # FILES SPECIFIED
C     LSKP      FILES TO SKIP                 P5
C     FEETS     NUMBER OF FEET TO SKIP        P6
C     ISCI      SCIENTIST                     P7
C     IPROJ     PROJECT                       P8
C     IOTYPE    TYPE OF DISK I/O              P9
C     NCOD2     SPECIFIED WHERE TO START IF   P10
C     P9='PURE'
C     'BEG' = BEGINNING OF FILE
C     'APP' = APPEND TO EXISTING FILE
C     
C     
      PARAMETER (LNNF=3)
      CHARACTER*3 NFIN(LNNF),NCOD
      CHARACTER*8 KRD(10)
      CHARACTER*28 IBUF(2)
      CHARACTER*8 CTEMP1

      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI

      COMMON /DISKIO/ IOTYPE,IPOS
      DATA IBUF/'COS BLOCKED','PURE BINARY'/
      DATA NFIN/'BEG','ADD','SKI'/
      DATA MODE,NTYPE,NWDCNT/1,2,1/
      CHARACTER*3 IO,ISTART
      
      READ (KRD,101)UNTMP,LTAP,NCOD,SKPTMP,FEETS
 101  FORMAT(/F8.0/A8/A3,5X/F8.0/F8.0)
C     
      IF (LTAP.EQ.' ') THEN
         LTAP = 'UNKNOWN'
      END IF
C     
      LUN = UNTMP
      LSKP = AMAX1(SKPTMP,0.0)
C     
C     
      READ (KRD,112)ISCI,IPROJ,IO,ISTART
 112  FORMAT(//////A8/A4,4X/A3/A3)
C     
      IF (ISCI.EQ.' ') THEN
         ISCI = 'NONE'
      END IF
C     
      IF (IPROJ.EQ.' ') THEN
         IPROJ = 'COPE'
      END IF
      
      IF (LUN.LE.0 .OR. LUN.GT.99) THEN
         CALL TPQERX(324,1)
      ELSE
 2       IF (LUN.LT.10) THEN
            WRITE (CTEMP1,302)LUN
 500        FORMAT(A8)
 302        FORMAT('FT00',I1,'   ')
         ELSE
            WRITE (CTEMP1,303)LUN
 303        FORMAT('FT0',I2,'   ')
         END IF
      END IF
      
C     
C     USING C I/O ROUTINES TO WRITE DATA TO DISK INSTEAD OF COS BLOCKED I/O
C     
      IOTYPE=1
      IF (NCOD.EQ.'BEG') THEN
C     
C     WRITE AT BEGINNING OF FILE; ANY INFO ALREADY IN FILE IS LOST
C     
         IPOS=1
      ELSE
C     
C     APPEND TO WHATEVER IS CURRENTLY IN FILE
C     
         IPOS=2
      END IF
      
C     
C     
C     SUMMARIZE OUTPUT COMMAND
C     
      PRINT 885
 885  FORMAT(//5X,'SUMMARY OF OUTPUT COMMAND ')
      PRINT 887
 887  FORMAT(5X,'------- -- ------ ------- ')
      PRINT 888, LUN,IBUF(IOTYPE+1),LTAP,NCOD,LSKP,ISCI,IPROJ
 888  FORMAT(
     X     /19X,'          UNIT NUMBER: ',I5
     X     /19X,'        OUTPUT FORMAT: ',A6
     X     /19X,'            TAPE NAME: ',A6
     X     /19X,'WRITING POSITION CODE: ',A3
     X     /19X,' NUMBER FILES TO SKIP: ',I5
     X     /19X,'            SCIENTIST: ',A8
     X     /19X,'              PROJECT: ',A4)
C     
      
      RETURN
      END
