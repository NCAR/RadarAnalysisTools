      SUBROUTINE SCNSET(KZBUF,IBKNT,IBLV,KZBM,IDS,J,
     X                  IND,LPMN,LPMX,KZOUT,IS360,IZAD,ICROSS,NST,
     X                  IBELOW)
C
C     INITIALIZATION FOR OUTPUT GRID LOCATIONS BELOW THIS RADAR SCAN
C     INPUTS:
C             IDS    - Scan direction flag: (1) CW, (-1) CCW
C             IDSV   - Scan direction flag: (1) CW, (-1) CCW
C     OUTPUTS:
C             NST    - (0)   , (1)
C             IBELOW - (0) No grid points below scan that match the 
C                          previous upward interpolation
C                      (1) 
C             LPMN   -
C             LPMX   - 
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (IDIM=64/WORDSZ,IDIM2=4)
      LOGICAL IS360
c      DIMENSION KZBUF(2),IBKNT(4),IBLV(IDIM,IDIM2,NRCBF)
      DIMENSION KZBUF(4),IBKNT(8),IBLV(IDIM,IDIM2,NRCBF)

      LOGICAL DEBUGIT
      DEBUGIT=.FALSE.

      LPMN=IBKNT(1)
      LPMX=IBKNT(3)

      if(debugit)then
         write(8,1770)(kzbuf(i),i=1,4)
 1770    format(1X,'SCNSET: kzbuf=',4i8)
         write(8,1771)(ibknt(i),i=1,8)
 1771    format(1X,'SCNSET: ibknt=',8i8)
         write(8,1772)kzbm,ids,j,ind,lpmn,lpmx,kzout,izad,icross,
     +        nst,ibelow
 1772    format(1X,'SCNSET:  rest=',11i8)
      end if
      DEBUGIT=.FALSE.
C
C     THE FOLLOWING TEST IS FOR AIRBORNE SITUATION WHERE, FOR SOME REASON,
C     THERE WERE NO GRID POINTS AHEAD OF PREV. SCAN WHERE DATA COULD BE MAPPED
C     TO. THUS, THERE WILL BE NO GRID POINTS BEHIND CURRENT SCAN WHERE
C     DATA COULD BE MAPPED TO. IBELOW WILL BE SET TO 0 TO REFLECT THIS.
C
      if(debugit)write(8,*)'SCNSET goto 701: is360,nrcbf=',
     +     is360,nrcbf+1
      IF (LPMX.EQ.0 .OR. LPMN.EQ.(NRCBF+1)) GOTO 701
      IBELOW=1
      if(debugit)then
         write(8,1772)kzbm,ids,j,ind,lpmn,lpmx,kzout,izad,icross,
     +        nst,ibelow
      end if
C
C        LOCATE MIN,MAX AZIMUTH AND ASSOCIATED POINTERS
C
      IF(IS360) THEN
         MINAZ=IZAD*2
         MAXAZ= -MINAZ
         DO I=LPMN,LPMX
            CALL IGETCP(IBLV(1,1,I),KZOUT,IX,IY)
            IF(KZOUT.LT.MINAZ) THEN
               MINAZ=KZOUT
               INDMN=I
            END IF
            IF(KZOUT.GT.MAXAZ) THEN
               MAXAZ=KZOUT
               INDMX=I
            END IF
         END DO
         IND=INDMN
         IF(IDS.LT.0) IND=INDMX
      ELSE
         ITST=(KZBUF(J)-KZBM)*IDS
         if(debugit)then
            write(8,*)'SCNSET goto 701: itst=',itst,kzbuf(j),kzbm,ids
         end if
         IF(ITST.LT.0) GO TO 701
         IND=LPMN
         IF(IDS.LT.0) IND=LPMX
      END IF
      ISTRT=IND

 30   CONTINUE
      if(debugit)then
         write(8,*)'SCNSET after 30'
      end if
      CALL IGETCP(IBLV(1,1,IND),KZOUT,IX,IY)
      ITST=(KZOUT-KZBM)*IDS
      IF(ITST.GE.0) GO TO 40
      IND=IND+IDS
      IF(IND.LT.LPMN) IND=LPMX
      IF(IND.GT.LPMX) IND=LPMN
      if(debugit)write(8,*)'SCNSET: is360,ind,istrt=',is360,ind,istrt
      IF(IND.EQ.ISTRT) THEN
         IF(IS360) THEN
            CALL IGETCP(IBLV(1,1,IND),KZOUT,IX,IY)
            GO TO 40
         ELSE
            GO TO 701
         END IF
      END IF
      GO TO 30

 40   CONTINUE
      if(debugit)then
         write(8,*)'SCNSET after 40'
      end if
      NST=0
      if(debugit)then
         write(8,1772)kzbm,ids,j,ind,lpmn,lpmx,kzout,izad,icross,
     +        nst,ibelow
      end if
      RETURN

 701  CONTINUE
      if(debugit)then
         write(8,*)'SCNSET after 701'
         write(8,*)'SCNSET: calling TPQERX(307)'
      end if
      IBELOW=0
      CALL TPQERX(307,0)
      NST=1
      RETURN
      END
