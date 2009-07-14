c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTNET(INDAT,PLT_NMRK,MXK,KMRK)
C
C  READ IN SPECIFIC LANDMARK NAMES TO BE PLOTTED
C     PLT_NMRK - Specific landmark names to be plotted
C
      CHARACTER*7 PLT_NMRK(MXK)
      CHARACTER*8 CUR_NMRK(9),INDAT(10),BLANK
      DATA BLANK/'        '/

C     See PLTNET: PLTRING - Range ring is plotted if .TRUE.
C
      LOGICAL PLTRING
      COMMON/PLTRNG/PLTRING
      DATA PLTRING/.FALSE./

      KMRK=0
 10   CONTINUE
      WRITE(6,11)(INDAT(I),I=1,10)
 11   FORMAT(10A8)
      READ(INDAT,13)(CUR_NMRK(I),I=1,9)
 13   FORMAT(/A8/A8/A8/A8/A8/A8/A8/A8/A8)

      DO I=1,9
         IF(CUR_NMRK(I).NE.BLANK)THEN
            KMRK=KMRK+1
            PLT_NMRK(KMRK)=CUR_NMRK(I)(1:7)
            print *,'Pltnet: ',kmrk,' ',plt_nmrk(kmrk)
         END IF
      END DO

      READ(5,11)(INDAT(I),I=1,10)
c      WRITE(6,22)(INDAT(I),I=1,10)
c   22 FORMAT('Kardin=',10A8)
      IF(INDAT(1)(1:3).EQ.'END')THEN
         PLTRING=.TRUE.
         RETURN
      END IF

      GO TO 10
      END

