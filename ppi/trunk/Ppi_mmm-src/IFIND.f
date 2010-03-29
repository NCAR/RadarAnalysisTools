c
c----------------------------------------------------------------------X
c
      FUNCTION IFIND(IWRD,LIST,NLIST)
C
C     FIND THE INDEX(IFIND) OF THE STRING IWRD WITHIN LIST
C
      CHARACTER*8 IWRD,LIST(NLIST)

      IFIND=0
      DO 10 I=1,NLIST
      IF(IWRD.EQ.LIST(I))THEN
         IFIND=I
         RETURN
      END IF
   10 CONTINUE
c*ljm* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c      WRITE(6,11)
c   11 FORMAT(1X,'*** WARNING - NO MATCH WITHIN THE CURRENT LIST ***')
c      WRITE(6,13)IWRD
c   13 FORMAT(/,1X,'IWRD = ',A8,/,1X,'MATCHES NOTHING IN THE LIST:',/)
c      CALL DMPCHAR(LIST,NLIST)
c*ljm* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      RETURN
      END
