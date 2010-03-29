c
c----------------------------------------------------------------------X
c
      SUBROUTINE FIELD(NAM,NAMFLD,NFLDS,NAMOUT)
C
C  CHECKS FIRST IF NAM IS ALREADY IN NAMFLD ARRAY, IF NOT NAM IS ADDED
C
C     NAM    - CURRENT REQUESTED FIELD NAME
C     NAMFLD - ACCUMULATED ARRAY OF FIELD NAMES
C     NFLDS  - TOTAL NUMBER OF FIELDS REQUESTED (MAXIMUM = MXF)
C
C     NOTE:  If NAM = 'STATS   ', five field names are added to the list.
C               These fields are named NAMOUT(1:4) plus one each of the 
C               following suffixes: 'max','min','mean','sdev', and 'npts'.
C            If NAM = 'ANGLE   ', two field names are added to the list.
C               These fields are named NAMOUT(1:4) plus one each of the 
C               following suffixes: 'amax' and 'angl'.
C            If NAM = 'HEIGHT  ', two field names are added to the list.
C               These fields are named NAMOUT(1:4) plus one each of the 
C               following suffixes: 'zmax' and 'zalt'.
C            If NAM = 'GAM_DSD ', six field names are added to the list.
C            These fields are named NAMOUT(1:3) plus one each of the 
C            following suffixes: 'lgnt','lam','mu','lgn0','kdp', and 'rain'
C
      INCLUDE 'dim.inc'

      CHARACTER*8 NAM,NAMFLD(MXF)
      CHARACTER*4 NAMOUT

      IF(NAM.EQ.'        ')RETURN
      IF(NFLDS.EQ.0        .AND. 
     x   NAM.NE.'STATS   ' .AND.
     x   NAM.NE.'ANGLE   ' .AND.
     x   NAM.NE.'HEIGHT  ' .AND.
     x   NAM.NE.'GAM_DSD ')THEN
         NFLDS=1
         NAMFLD(NFLDS)=NAM
      ELSE
         DO 10 I=1,NFLDS
            IF(NAM.EQ.NAMFLD(I))RETURN
 10      CONTINUE
         NFLDS=NFLDS+1
         IF(NAM.EQ.'STATS   ')NFLDS=NFLDS+4
         IF(NAM.EQ.'ANGLE   ')NFLDS=NFLDS+1
         IF(NAM.EQ.'HEIGHT  ')NFLDS=NFLDS+1
         IF(NAM.EQ.'GAM_DSD ')NFLDS=NFLDS+5
         IF(NFLDS.GT.MXF)THEN
            PRINT 13,MXF
 13         FORMAT(1X,'NUMBER OF FIELDS REQUESTED EXCEEDS ',I2)
            STOP
         END IF
         IF(NAM.EQ.'STATS   ')THEN
            WRITE(NAMFLD(NFLDS-4),15)NAMOUT
            WRITE(NAMFLD(NFLDS-3),16)NAMOUT
            WRITE(NAMFLD(NFLDS-2),17)NAMOUT
            WRITE(NAMFLD(NFLDS-1),18)NAMOUT
            WRITE(NAMFLD(NFLDS  ),19)NAMOUT
 15         FORMAT(A4,'max')
 16         FORMAT(A4,'min')
 17         FORMAT(A4,'mean')
 18         FORMAT(A4,'sdev')
 19         FORMAT(A4,'npts')
         ELSE IF(NAM.EQ.'ANGLE   ')THEN
            WRITE(NAMFLD(NFLDS-1),20)NAMOUT
            WRITE(NAMFLD(NFLDS  ),21)NAMOUT
 20         FORMAT(A4,'amax')
 21         FORMAT(A4,'angl')
         ELSE IF(NAM.EQ.'HEIGHT  ')THEN
            WRITE(NAMFLD(NFLDS-1),22)NAMOUT
            WRITE(NAMFLD(NFLDS  ),23)NAMOUT
 22         FORMAT(A4,'zmax')
 23         FORMAT(A4,'zalt')
         ELSE IF(NAM.EQ.'GAM_DSD ')THEN
            WRITE(NAMFLD(NFLDS-5),25)NAMOUT(1:3)
            WRITE(NAMFLD(NFLDS-4),26)NAMOUT(1:3)
            WRITE(NAMFLD(NFLDS-3),27)NAMOUT(1:3)
            WRITE(NAMFLD(NFLDS-2),28)NAMOUT(1:3)
            WRITE(NAMFLD(NFLDS-1),29)NAMOUT(1:3)
            WRITE(NAMFLD(NFLDS  ),30)NAMOUT(1:3)
 25         FORMAT(A3,'rain')
 26         FORMAT(A3,'kdp')
 27         FORMAT(A3,'lgn0')
 28         FORMAT(A3,'mu')
 29         FORMAT(A3,'lam')
 30         FORMAT(A3,'lgnt')
         ELSE
            NAMFLD(NFLDS)=NAM
         END IF
      END IF
      IF(NAM.EQ.'STATS   ')THEN
         DO 54 J=1,5
            MFLDS=NFLDS-5+J
            PRINT 57,MFLDS,NAMFLD(MFLDS)
 54      CONTINUE
      ELSE IF(NAM.EQ.'ANGLE   ')THEN
         DO 55 J=1,2
            MFLDS=NFLDS-2+J
            PRINT 57,MFLDS,NAMFLD(MFLDS)
 55      CONTINUE
      ELSE IF(NAM.EQ.'HEIGHT  ')THEN
         DO 56 J=1,2
            MFLDS=NFLDS-2+J
            PRINT 57,MFLDS,NAMFLD(MFLDS)
 56      CONTINUE
      ELSE
         PRINT 57,NFLDS,NAMFLD(NFLDS)
 57      FORMAT(8X,'NFLDS,NAMFLD=',I6,2X,A8)
      END IF
      RETURN
      END

