      SUBROUTINE GENAZM(JPCK,IELCHK,MNBEM,ELTOL,NST,ICOPLANE2,BASANG)
C     
C     DRIVER ROUTINE FOR REGENERATION OF MISSING AZIMUTHAL BEAMS.
C     READS THE TEMPORARY DISK FILE CONTAINING A FULL VOLUME
C     SCAN AND FILLS IN SCAN ANGLE GAPS GREATER THAN USGAP.
C     LTMP CONTAINS THE INPUT FILE AND IS REASSIGNED THE LOGICAL
C     UNIT NUMBER OF THE OUTPUT FILE.
C     JPCK- SCRATCH BUFFER
C     IELCHK- T: REORGANIZE DATA TO CORRECT ELEVATION PROBLEMS
C     IAZCHK- T: REORGANIZE DATA TO CORRECT AZIMUTH   PROBLEMS (BELOW)
C     MNBEM- MINIMUM NUMBER OF BEAMS/SWEEP TO ACCEPT
C     ELTOL- TOLERANCE IN ELEVATION BETWEEN FIXED AND ACTUAL ANGLE
C     NST- IF NON-ZERO VOLUME COULD NOT BE REBUILT
C     
C     FORTRAN LOGICAL UNIT NUMBER 2 MUST BE ASSIGNED TO A
C     SCRATCH FILE.
C     
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c      DATA IBAD/-32768/

      DIMENSION JPCK(1)
      COMMON /IO/ IPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),JBUF(MAXIN),
     X     IZ8(17),ILSTREC
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /IDBLK/ ID(NID)
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
      COMMON /SCRDSK/ LTMP
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      DATA DTR /0.0174533/
      LOGICAL IFLADJ,K360,IFIRST
      LOGICAL IELCHK,IAZCHK,ITOSEL
      DATA IFLGMX/10/
C     
C     THE FOLLOWING FUNCTION CALCULATES COPLANE ANGLE FROM ELEV. AND HOR AZIM. 
C     
      CALCOP(E,A)=ATAN((TAN(E*DTR)/ABS(SIN(A*DTR))))/DTR
C     
C     THE FOLLOWING FUNCTION CALCULATES ELEV ANGLE FROM FIXED AND HOR AZIM.
C
      CALEL(F,A)=ATAN((TAN(F*DTR)*ABS(SIN(A*DTR))))/DTR
C     
C     Pointer was originally hardwired to 129
C
C      IPTR=129
      IPTR=IPTR_INT
      NEL=ID(35)

      NST=0
      IAZCHK=(USGAP.GT.0.0)

c-----debug (ljm)
      print *,'GENAZM: iptr, nel, id(35)=',iptr,nel,id(35)
      print *,'Genazm: might write fort.2'
      print *,'Genazm: iazchk,ielchk,usgap=',iazchk,ielchk,usgap
c-----debug (ljm)

      IF((.NOT.IAZCHK).AND.(.NOT.IELCHK))THEN
         print *,'Return from Genazm - no need to execute'
         RETURN
      END IF
      PRINT 200
 200  FORMAT(//5X,'RECOMPUTED INPUT VOLUME SUMMARY ...'/)
      IF (ICOPLANE.EQ.0 .OR. ICOPLANE .EQ. 5) THEN
         PRINT 236
      ELSE IF (ICOPLANE.GE.1 .AND. ICOPLANE.LT.4) THEN
         PRINT 237
      ELSE IF (ICOPLANE.EQ.4) THEN
         PRINT 238
      END IF
 236  FORMAT (//6X,'SCAN',18X,'ELEVATION',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD')
 237  FORMAT (//6X,'SCAN',18X,'COPLANE',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD')
 238  FORMAT (//6X,'SCAN',18X,'AZIMUTH',29X,'ELEVATION',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD')
C     
C     FILL IN DATA GAPS FOR THIS VOLUME (Outputs to fort.2)
C     
      IREC=0
      JREC=0
      L=0
      MTMP=2
      WRITE(MTMP) MTMP
      REWIND MTMP
C     
C     PRODUCE A DUMMY BEAM OF INFO
C     
      NRHD=ID(37)
      DO 5 I=1,MAXIN
         JBUF(I)=IBAD
    5 CONTINUE
C     
C     SEE IF 360 OR SECTOR SCAN
C     
      K360=.FALSE.
      IF((USAZ2-USAZ1).GE.360.0) K360=.TRUE.
      AT1=USAZ1-USINC
      IF(USAZ1.LT.USAZ2) THEN
         IFLADJ=.FALSE.
         AT2=USAZ2+USINC
      ELSE
         IFLADJ=.TRUE.
         AT2=USAZ2+USINC+360.0
      END IF
      LASTEL= -1000
      LOUT=0
C     
C     RESET OUTPUT BUFFERING
C     
      CALL WRRYDK(JPCK,JBUF,JST,MTMP,-9,NLEN)
c-----debug (ljm) - (02/11/1999)
c     eor/uio: "off end of record" in WRRYDK at READ(IUN)
c     Fix that works is to restore RDRY common block in RDRYDK.
c      iflg=-9      
c      mlen=nlen
c      print *,'Genaz: unit,jst,iflg,len,plen=',
c     +     mtmp,jst,iflg,mlen,ilstrec
c-----debug (ljm)
 10   CONTINUE
C     
C     ELEVATION SCAN LOOP
C     
      L=L+1
c-----print *,'Genazm - scan loop:',nel,l
      IF(L.GT.NEL) GO TO 50
C     
C     INITIALIZE STATISTICAL SUMMARY ACCUMULATORS
C     
      ELMAX=0.0
      ELMIN=1000.0
      ELSUM=0.0
      AZMAX=0.0
      AZMIN=1000.0
      AZSUM=0.0
      NBAD=0
      IBEAM=0

c      IEL=   ID(126+L*3)
c      DIR=   ID(127+L*3)
c      KNDREC=ID(128+L*3)
      IEL=   ID(IPTR-3 + L*3)
      DIR=   ID(IPTR-2 + L*3)
      KNDREC=ID(IPTR-1 + L*3)

c-----print *,'Genazm: old pointers'
c-----print *,'Genazm:    iel =',126 + L*3
c-----print *,'Genazm:    dir =',127 + L*3
c-----print *,'Genazm: kndrec =',128 + L*3

c-----print *,'Genazm: new pointers'
c-----print *,'Genazm:    iel =',IPTR-3 + L*3
c-----print *,'Genazm:    dir =',IPTR-2 + L*3
c-----print *,'Genazm: kndrec =',IPTR-1 + L*3

      IF(ICOPLANE .EQ. 5) DIR = 1.0
      IF(IFORDR.NE.0) DIR=IFORDR
      ITOSEL=(IEL.LE.LASTEL)
c-----print *,'Genazm: iel,lastel,itosel=',iel,lastel,lastel
      IF(ITOSEL) PRINT 102,L
 102  FORMAT(5X,'*** ORIGINAL SCAN',I2,' DISCARDED - IT IS LESS ',
     X     'THAN OR EQUAL TO THE PRECEDING ONE')
      ELFIX=FLOAT(IEL)/FLOAT(ID(44))
      JRCLST=JREC
      IF(DIR.GE.0.0) THEN
         AZLAST=AT1
         AZEND =AT2
      ELSE
         AZLAST=AT2
         AZEND =AT1
      END IF
      IRDOPT=1
      IFIRST=.TRUE.
      JBUF(2)=IEL
C     
C     RESET OUTPUT BUFFERING
C     
c      CALL WRRYDK(JPCK,JBUF,JST,MTMP,-9,NLEN)
 15   CONTINUE
C     
C     BEAM WITHIN AN ELEVATION SCAN LOOP
C     
c-----debug (ljm)
c      print *,'Genazm: irec,kndrec=',irec,kndrec
c-----debug (ljm)
      IF(IREC.GE.KNDREC) GO TO 20
      IREC=IREC+1
c-----debug (ljm)
c      print *,'Genazm: rdrydk: ',ltmp,irdopt,nlen
c-----debug (ljm)
      CALL RDRYDK(IPCK,IBUF,INSTA,LTMP,IRDOPT,NLEN)
      IF(INSTA.NE.0) GO TO 91
      IRDOPT=0
      IF(ITOSEL) GO TO 15
C     
C     THIS AWFUL LOOKING FRACTION IS 1/64
C     
      AZBM=FLOAT(IBUF(1))*0.015625
      ELBM=FLOAT(IBUF(2))*0.01
      IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         IF (ELBM.GE.90.0) THEN
            WRITE(*,*)'***BAD ANGLE IN GENAZM***'
            STOP
         END IF
         IF (AZBM.EQ.90.0) THEN
            AZBM=90.0
         ELSE
            AZBM=ATAN(COS(ELFIX*DTR)*TAN(AZBM*DTR))/DTR
         END IF
         IF (AZBM.LT.0.0) AZBM=AZBM+180.0
         IF (AZBM.GT.180.0) THEN
            WRITE(*,*)'***BAD ANGLE IN GENAZM***'
            STOP
         END IF
         ELANG=CALEL(ELFIX,AZBM)
      ELSE IF (ICOPLANE.EQ.3) THEN
         AZBM = AZBM - BASANG
         ELANG=CALEL(ELFIX,AZBM)
      ELSE IF (ICOPLANE.EQ.4) THEN
         ELANG=ELFIX
         TMP  =ELBM
         ELBM =AZBM
         IF (ELANG.GT.360.0 .AND. ELBM.LT.180.0) ELBM=ELBM+360.0
         IF (ELBM.LT.0.0)    ELBM=ELBM+360.0
         AZBM =TMP
      ELSE
         ELANG=ELFIX
      END IF


      IF(IELCHK.AND.ABS(ELANG-ELBM).GT.ELTOL) THEN
C     
C     BAD ELEVATION ANGLE
C     
         NBAD=NBAD+1
         GO TO 15
      END IF
      IF (ICOPLANE.GE.1 .AND. ICOPLANE.LT.4) THEN
         ELBM=CALCOP(ELBM,AZBM)
         AZBM=AZBM + BASANG
         IF (AZBM.LT.0.0) AZBM=AZBM+360.0
      END IF
C
C     AT THIS POINT, AZBM IS HOR. AZ. REL TO TRUE NORTH
C
      IF(IAZCHK) THEN
C     
C     FILL ALONG AZIMUTH
C     
         IF(IFLADJ.AND.AZBM.LT.USAZ2) AZBM=AZBM+360.0
         IF(AZBM.LT.AT1.OR.AZBM.GT.AT2) GO TO 15
         IF(K360.AND.IFIRST) THEN
            AZLAST=AZBM
            AZEND =AZBM
         END IF
         DAZ=AZBM-AZLAST
         IF (SIGN(1.0,DAZ).NE.DIR .AND. ABS(DAZ).GE.10.0) THEN
            DAZ=DAZ+DIR*360.0
         END IF
         IF(K360) THEN
            IF(DAZ.NE.0.0.AND.SIGN(1.0,DAZ).NE.DIR) DAZ=DAZ+DIR*360.0
            IF(DAZ*DIR.GT.350.) GO TO 15
         END IF
         TEST=DAZ*DIR
         IF(TEST.LT.0.0) GO TO 15
         IF(TEST.GT.USGAP .AND. .NOT.IFIRST) THEN
            CALL FILLAZ(MTMP,AZLAST,DAZ,USINC,NRHD,NLEN,IBUF,JBUF,
     X                  JPCK,JREC,ICOPLANE,ELBM,ANGXAX)
            IFIRST=.FALSE.
         END IF
         IFIRST=.FALSE.
C     
      END IF
C     
C     ACCUMULATE STATISTICS FOR THIS SCAN
C     
      IBEAM=IBEAM+1
      ENDAZ=AZBM
      ELMIN=AMIN1(ELMIN,ELBM)
      ELMAX=AMAX1(ELMAX,ELBM)
      ELSUM=ELSUM+ELBM
      IF(IBEAM.EQ.1) THEN
         BEGAZ=AZBM
      ELSE
         DIF=AZBM-AZLAST
         IF(ABS(DIF).GT.180.0) DIF=DIF-SIGN(360.0,DIF)
         ABDIF=ABS(DIF)
         IF(ABDIF.GT.0.0) AZMIN=AMIN1(AZMIN,ABDIF)
         AZMAX=AMAX1(AZMAX,ABDIF)
         AZSUM=AZSUM+ABDIF
      END IF
C     
C     WRITE OUT THE ORIGINAL BEAM
C     
      JREC=JREC+1
      CALL WRRYDK(JPCK,IBUF,JST,MTMP,0,NLEN)
c-----debug (ljm)
c      iflg=0      
c      mlen=nlen
c      print *,'Genaz: unit,jst,iflg,len,plen=',
c     +     mtmp,jst,iflg,mlen,ilstrec
c-----debug (ljm)
      AZLAST=AZBM
      GO TO 15
 20   CONTINUE
c-----print *,'Genazm-after 20: iazchk,itosel=',iazchk,itosel
      IF(ITOSEL) GO TO 10
C     
C     END OF THE CURRENT ELEVATION SCAN
C     
      IF(IAZCHK) THEN
C     
C         DAZ=AZEND-AZLAST
C         IF(K360) THEN
C            IF(DAZ.NE.0.0.AND.SIGN(1.0,DAZ).NE.DIR) DAZ=DAZ+DIR*360.0
C            IF(DAZ*DIR.GT.350.) GO TO 30
C         END IF
C         IF(DAZ*DIR.GT.USGAP) THEN
C            CALL FILLAZ(MTMP,AZLAST,DAZ,USINC,NRHD,NLEN,IBUF,JBUF,
C     X                  JPCK,JREC,ICOPLANE,ELBM,ANGXAX)
C         END IF
 30      CONTINUE
C     
C     ADJUST STATS FOR FILLED SCAN
C     
         AZMIN=AMIN1(AZMIN,USINC)
         AZMAX=AMIN1(AZMAX,USGAP)
C         IF(K360) THEN
C            ENDAZ=BEGAZ
C         ELSE
C            BEGAZ=USAZ1
C            ENDAZ=USAZ2
C         END IF
         I=(JREC-JRCLST)-IBEAM
         ELSUM=ELSUM+(ELFIX*I)
         IBEAM=IBEAM+I
         AZSUM=USINC*(IBEAM-1)
C     
      END IF
C     
C     
C     UPDATE THE VOLUME HEADER AND FORCE OUT LAST RECORD OF SCAN
C     
c-----print *,"JREC ",JREC," JRCLST ",JRCLST," MNBEN ",MNBEM
      IF(JREC-JRCLST.LT.MNBEM) THEN
C     
C     TOO FEW BEAMS IN THIS SCAN
C     
         PRINT 103, L,MNBEM
 103     FORMAT(5X,'*** ORIGINAL SCAN ',I2,' DISCARDED - IT CONTAINS ',
     X        'FEWER THAN ',I2,' BEAMS.')
c--------debug (ljm)
c         iflg=l+iflgmx
c         mlen=jrec-jrclst
c         print *,'Genaz: unit,jst,iflg,len,plen=',
c     +        mtmp,jst,iflg,mlen,ilstrec
c--------debug (ljm)
         CALL WRRYDK(JPCK,IBUF,JST,MTMP,(L+IFLGMX),(JREC-JRCLST))
c--------debug (ljm)
c         iflg=l+iflgmx
c         mlen=jrec-jrclst
c         print *,'Genaz: unit,jst,iflg,len,plen=',
c     +        mtmp,jst,iflg,mlen,ilstrec
c--------debug (ljm)
         JREC=JRCLST
      ELSE
C     
C     ELEVATION SCAN IS TO BE SAVED
C     
         LOUT=LOUT+1
         LASTEL=IEL

c         ID(126+LOUT*3)=IEL
c         ID(127+LOUT*3)=DIR
c         ID(128+LOUT*3)=JREC
         ID(IPTR-3 + LOUT*3)=IEL
         ID(IPTR-2 + LOUT*3)=DIR
         ID(IPTR-1 + LOUT*3)=JREC

c         CALL WRRYDK(JPCK,IBUF,JST,MTMP,9,NLEN)
C     
C     GENERATE STATISTICAL SUMMARY OF THIS SCAN
C     
         DELAZ=AZSUM/(IBEAM-1)
         DELEL=ELSUM/IBEAM
c         PRINT 322, LOUT,ID(127+LOUT*3),ELFIX,ELMIN,ELMAX,DELEL,
         PRINT 322, LOUT,ID(IPTR-2+LOUT*3),ELFIX,ELMIN,ELMAX,DELEL,
     X        BEGAZ,ENDAZ,AZMIN,AZMAX,DELAZ,IBEAM,NBAD
 322     FORMAT(2(4X,I2),8X,4(F6.2,3X),4X,2(F6.1,4X),
     X        3(F6.2,3X),I7,I6)
      END IF
      GO TO 10
 50   CONTINUE
C     
C     END OF VOLUME SCAN
C     

      CALL WRRYDK(JPCK,IBUF,JST,MTMP,9,NLEN)
c-----debug (ljm)
c      iflg=9      
c      mlen=nlen
c      print *,'Genaz: unit,jst,iflg,len,plen=',
c     +     mtmp,jst,iflg,mlen,ilstrec
c-----debug (ljm)
      ID(35)=LOUT
      print *,'GENAZM: iptr, nel, id(35)=',iptr,nel,id(35)
      ID(36)=JREC
      LTMP=MTMP
      REWIND LTMP
      IF(LOUT.LT.2 .AND. IPPI.EQ.0) GO TO 92
      NST=0
      RETURN
 91   CONTINUE
      PRINT 101, LTMP,INSTA
 101  FORMAT(5X,'UNIT=',I2,5X,'STATUS=',I3)
      NST=1
      RETURN
 92   CONTINUE
      NST=1
      RETURN
      END
