      SUBROUTINE BEAMIN
C
C  IDIR             SWEEP DIRECTION.  CW=1, CCW=-1
C  IHR              TIME IN HOURS.
C  IRAY (NWORD)     INTEGER ARRAY OF (1-NG) FIELD VALUES AND HOUSEKEEPING
C  ISEC             TIME IN SECONDS (=60*MIN+SEC)
C  KAZ(KAZC)        INTEGER ARRAY OF AZIMUTHS
C  KAZC             CURRENT RAY INDEX IN ARRAYS IRAY AND KAZ
C  KAZP             PREVIOUS RAY INDEX IN ARRAYS IRAY AND KAZ
C  NRDOPT           REQUEST FROM CALLER FOR ACTION.
C                   =0, SEND NEXT RAY.
C                   =1, GO TO NEXT ELEVATION SWEEP WITHIN ELB(IEL1) AND
C                           ELB(IEL2).  RESET NRDOPT=0.
C                   =2, READ FIRST BEAM IN SCAN, RESET RECORD COUNTER TO
C                           END OF SWEEP.
C  NSTBM            BEAM STATUS FROM BEAMIN.
C                   =0, NEW BEAM IN OUTPUT WORDS.
C                   =1, END OF SWEEP INDICATED.
C                   =2, SOMETHING IS NOT CORRECT.
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c      PARAMETER (MAXPLN=65536)
c      PARAMETER (MAXFLD=16)
c      DATA LFTIM,JRH6,JRH7,IBAD /0,64,100,-32768/

      PARAMETER (NRHD=10)
      LOGICAL IS360
      LOGICAL DEBUGIT
C
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,
     X     CFAC(MAXFLD)
      COMMON /IO/KPCK(NIOB),NWORD(MAXIN,3),KAZ(3),KAZC,KAZP,IDIR,
     X     ITMDIF,IHR,IMIN,ISEC,IREC,IBEGT(4),NSTBM,NRDOPT,ILSTREC
      COMMON /NCARFB/ NF,IDXFV(MAXFLD)
      COMMON /COPE/ IADJAZ
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /RHIS/ IRHICRS,LOWAZ2,MAXAZ2,MINAZ2,ICART2(MAXPLN),
     X     ICTAB2(4),KZV(3),IDSV

c      common /junk/ azc1,azp1

      DATA SCLTIM/8.0/

C     COPLANE relationships:
C          a - horizontal azimuth angle
C          b - baseline azimuth
C          c - coplane tilt angle
C          d - coplane azimuth angle
C          e - elevation angle
C       tan(a-b) = tan(d)*cos(c) ==> a-b = InvTan[tan(d)*cos(c)]
C         tan(c) = tan(e)/sin(a-b) ==> c = InvTan[tan(e)/sin(a-b)]
C
C     RHI scans mapped into COPLANE: 
C         Azimuth angle --> coplane tilt angle
C         Elevation angle --> coplane azimuth angle
C
C     CALHAZ - function to calculate horizontal azimuth from
C              (A,C) = azimuth in the coplane and coplane tilt angle
C              (A,C) = (el,az) ==> (AZC,RHIAZ)
C     CALVAZ - function to calculate vertical azimuth (coplane tilt)
C              from (A,C) = azimuth in the coplane and coplane tilt
C              angle (azimuth) (RHIs only).  
C              (A,C) = (el,az) ==> (AZC,RHIAZ)
C
C     DTR - DEGREES TO RADIANS CONVERSION ==> Radians = DEG*DTR 
C
      DATA DTR /0.0174533/

      CALHAZ(A,C)=ATAN(TAN(A*DTR)*COS(C*DTR))/DTR
      CALVAZ(A,C)=ATAN2(TAN(A*DTR),SIN(C*DTR))/DTR 

      DEBUGIT = .FALSE.

      IF(NRDOPT.EQ.1) THEN
C
C        BEGINNING OF SCAN
C
         KPEL=KEL
         IDIR=KDIR(KPEL)
         IDSV=IDIR
         IF (IRHICRS.EQ.2) THEN 
            IDIR=-IDIR
         END IF
      ELSE IF(NRDOPT.EQ.2) THEN
C
C        END OF SCAN (360 ONLY)
C
         DO I=1,MAXIN
            NWORD(I,KAZC)=NWORD(I,3)
         END DO
         NG=NWORD(8,KAZC)
         GO TO 110
      END IF
      if(nrdopt.ne.0)then
         write(8,*)'BEAMIN: nrdopt,kpel,idir,idsv,irhicrs,idir=',
     +        nrdopt,kpel,idir,idsv,irhicrs,idir
      end if

 100  CONTINUE
c      write(8,*)'BEAMIN: after 100'

c-----debug (ljm)
      if(debugit)then
         write(8,770)nrdopt,irec,kpel,kndrec(kpel),nf
 770     format(1X,'Beamin#1: nrdopt,irec,kpel,kndrec(kpel),nf=',i1,4i5)
      end if
c-----debug (ljm)
      IF(IREC.GE.KNDREC(KPEL)) THEN
c         write(8,*)'Beamin: irec.ge.kndrec(kpel) - goto 800'
         GO TO 800
      END IF
      IREC=IREC+1
C
C        READ IN NEXT BEAM
C
      CALL RDRYDK(KPCK,NWORD(1,KAZC),INSTA,LTMP,NRDOPT,NLEN)
c-----debug (ljm)
      if(debugit)then
         ascl=float(nword(6,kazc))
         escl=float(nword(7,kazc))
         az =nword(1,kazc)/ascl
         el =nword(2,kazc)/escl
         ihr=nword(3,kazc)
         imn=nword(4,kazc)
         isc=nword(5,kazc)
         nrg=nword(8,kazc)
         itime = 10000*ihr + 100*imn + isc
         write(8,771)nrdopt,irec,kpel,kndrec(kpel),nf,itime,az,el,nrg,
     +        nrdopt,nlen,insta
 771     format(1X,'Beamin#2: nrdopt,irec,kpel,kndrec(kpel),nf=',i1,4i5,
     +        ' T=',I6.6,' A=',F8.3,' E=',F8.3,' Ng=',I4,' Length=',I1,
     +        ' Status=',I4)
      end if
c-----debug (ljm)
C
C     Note: ljm 4/16/99 - not sure why reading NCDC Level II data sometimes
C           wants to read beyond the SPRINT scratch disk (fort.1).  It seems
C           to be related to the grid spacing, with small delta ==> read-eof.
C           Adding additional test to make sure this is not a short sweep is
C           a bandaid around the actual problem.
C
c-----IF(INSTA.NE.0) GO TO 900
      IF(INSTA.NE.0 .AND. KNDREC(KPEL).LT.2) GO TO 900
      NG=NWORD(8,KAZC)
C
C        COPY FIRST BEAM IN CASE OF 360 SCAN
C
      IF(NRDOPT.EQ.0) GO TO 110
      DO I=1,MAXIN
         NWORD(I,3)=NWORD(I,KAZC)
      END DO
      NRDOPT=0

 110  CONTINUE
c      write(8,*)'BEAMIN: after 110'

C     AZIMUTH
C
      KAZ(KAZC)=(FLOAT(NWORD(1,KAZC))/FLOAT(JRH6))*UNSCAZ+0.5

C**********************************************************
C     Correct azimuths for convergence of longitude lines 
C     (true north) across the network of radars.  This is
C     necessary since +Y direction (true north through the
C     origin) will have slightly different azimuth angles
C     for each of the radars not located at the origin.
C
      KAZ(KAZC)=KAZ(KAZC)+IADJAZ
      IF(KAZ(KAZC).LT.0) KAZ(KAZC)=KAZ(KAZC)+IZAD
C
C     End azimuth corrections
C**********************************************************
      IF (ICOPLANE.EQ.4) THEN

C     RHI CALCULATION: CONVERT AZ AND ELEV FROM BEAM HEADER 
C                      TO COPLANE ANGLES
C
         az = float(nword(1,kazc))/float(jrh6)
         el = float(nword(2,kazc))/float(jrh7)
c         write(8,*)'BEAMIN: Rhi header az,el=',az,el

         RHIAZ=FLOAT(NWORD(1,KAZC))/FLOAT(JRH6)
c         RHIAZ=RHIAZ-(ANGXAX-90.0)
         IF (RHIAZ.GT.360.) RHIAZ=RHIAZ-360.
         IF (RHIAZ.LT.0.)   RHIAZ=RHIAZ+360.
         AZC=90.0 - FLOAT(NWORD(2,KAZC))/FLOAT(JRH7)
         AZTMP=CALHAZ(AZC,RHIAZ)
         IF (AZTMP.LT.0.0) AZTMP=AZTMP+360.0
         KAZ(KAZC)=AZTMP*UNSCAZ + 0.5

         RHIAZ=FLOAT(NWORD(1,KAZC))/FLOAT(JRH6)
         AZC=FLOAT(NWORD(2,KAZC))/FLOAT(JRH7)
         KZV(KAZC)=CALVAZ(AZC,RHIAZ)*UNSCAZ + 0.5

c         write(8,*)'BEAMIN: kaz(kazc),kzv(kazc)=',kaz(kazc),kzv(kazc)
c         write(8,*)'BEAMIN: kaz(kazc),kzv(kazc)=',
c     +        kaz(kazc)/unscaz,kzv(kazc)/unscaz
      END IF

C  360 SCANS ONLY
C
      IF(IS360) GO TO 150
c      write(8,*)'BEAMIN: is360=',is360
      IF(KAZ(KAZC)  .LT.  LOWAZ) KAZ(KAZC) = KAZ(KAZC)+IZAD
      IF(KZV(KAZC)  .LT.  LOWAZ2) KZV(KAZC) = KZV(KAZC)+IZAD
c      write(8,*)'BEAMIN: kaz(kazc),kzv(kazc)=',kaz(kazc),kzv(kazc)
c      write(8,*)'BEAMIN: kaz(kazc),kzv(kazc)=',
c     +     kaz(kazc)/unscaz,kzv(kazc)/unscaz
      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IF(IDIR.LT.0) GO TO 125
         IF(KAZ(KAZC).GT.MAXAZ) THEN
            GO TO 100
         END IF
      ELSE
         IF(IDSV.LT.0) GO TO 125
         IF(KZV(KAZC).GT.MAXAZ2) THEN
            GO TO 100
         END IF
      END IF
      GO TO 150

 125  CONTINUE
c      write(8,*)'BEAMIN: after 125'

      IF (IRHICRS.EQ.0 .OR. IRHICRS.EQ.3) THEN
         IF(KAZ(KAZC).LT.MINAZ) THEN
            GO TO 100
         END IF
      ELSE
         IF(KZV(KAZC).LT.MINAZ2) THEN
            GO TO 100
         END IF
      END IF

 150  CONTINUE
c      write(8,*)'BEAMIN: after 150'

C  TIME.
      IHR =NWORD(3,KAZC)
      IMIN=NWORD(4,KAZC)
      ISEC=NWORD(5,KAZC)
      ITMDIF=( ((  IHR   *60+ IMIN   )*60+ ISEC   )
     X      -  ((IBEGT(1)*60+IBEGT(2))*60+IBEGT(3)) ) * SCLTIM
      IF (ITMDIF.LT.0) THEN
         WRITE(*,*)'***ITMDIF,IHR,IMIN,ISEC=',ITMDIF,IHR,IMIN,ISEC
      END IF
      NRDOPT=0
      NSTBM=0

      IF(NF.EQ.0) RETURN
C
C  DECIMATE NCAR FLAGGED VELOCITY FIELD
C
      DO 200 J=1,NF
         I=IDXFV(J)
         K=NRHD+(I-1)*NG
         DO 175 L=1,NG
            K=K+1
C      ITEST=AND(NWORD(K,KAZC),1)
            ITEST=ICEDAND(NWORD(K,KAZC),1)
            IF(ITEST.NE.0) GO TO 175
C  HERE IF VELOCITY VALUE IS FLAGGED WITH 0-BIT
            NWORD(K,KAZC)=IBAD
            
 175     CONTINUE

 200  CONTINUE
c      write(8,*)'BEAMIN: after 200'
      RETURN

 800  CONTINUE
c      write(8,*)'BEAMIN: after 800'
      NRDOPT=0
      NSTBM=1
      RETURN
      
 900  CONTINUE
c      write(8,*)'BEAMIN: after 900'
      NRDOPT=0
      NSTBM=2
      STOP 400
      END

