      SUBROUTINE SETVOL

C
C     Save information about the radar scan for later use.
C
C     NEL - Number of input radar scans within the volume
C
C     Coplane and GRIDPPI flags set in CRTSET or other routines.
C
C     ICOPLANE = 0  ==>  PPI SCANS, INTERPOLATING TO CART GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 5  ==>  AIRBORNE SWEEPS, INTERPOLATING TO CART GRID
C                        Set in RADAR (DORADE format)
C     IPPI     = 0  ==>  Normal interpolations to cartesian grid
C     IPPI     = 1  ==>  XY interpolations to original elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to original elevation surfaces
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (MAXRNG=1024,MAXFLD=16)

      LOGICAL IS360
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /IDBLK/ ID(NID)
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /IO/ KPCK(NIOB),IRAY(MAXIN,3),KAZ(3),KAZC,KAZP,IDSNEW,
     X     ITMDIF,ITIME(4),IBEGT(4),NSTBM,NRDOPT,ILSTREC
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,
     X     CFAC(MAXFLD)
      COMMON /COPE/ IADJAZ
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /COPLAN/ ICOPLN,BASAD
      DATA EPS/10.0/
      CHARACTER*8 CTEMP1,NAMF
      LOGICAL ICOPLN
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG,THE_TILT
      EQUIVALENCE (ID(41),KNQ), (ID(42),KRC)
C
C        READ IN VOLUME HEADER
C
      IREC=0
C     READ IN THE BEGINNING TIME
      IBEGT(1)=ID(4)
      IBEGT(2)=ID(5)
      IBEGT(3)=ID(6)
      IBEGT(4)=IREC
      ITIME(4)=IREC
      NEL=ID(35)
      SCLA=1./ID(44)
c      K=129
      K=IPTR_INT
c-----debug (ljm)
      write(7,*)'SETVOL: irec,nel,icoplane=',irec,nel,icoplane
      if(nel.gt.maxel)then
         write(7,*)'SETVOL - error nel > maxel=',maxel
      end if
c      write(7,*)'SETVOL: ID as integer array, nid=',nid
c      CALL DMPINTGR(ID,NID)
c      CALL DMPCHAR(ID,NID)
c-----debug (ljm)

      IF (ICOPLANE.NE.5) THEN
c--------debug (ljm)
         ibeg=iptr_int
         iend=ibeg+nel
         write(7,*)'SETVOL: scla,nel=',scla,nel
         write(7,*)'     z1,z2,zd,nz=',z1,z2,zd,nz
         write(7,*)'     id=',(id(i),i=ibeg,iend)
c--------debug (ljm)
         DO 40 I=1,NEL
            IF (ICOPLANE.NE.4) THEN
               ELB(I)=ID(K)*SCLA 
            ELSE
C     ADJUST FIXED ANGLE FOR RHIS; DONE DIFFERENTLY THAN OTHER SCANS
C     DUE TO CONTORTIONS USED TO INTERPOLATE RHIS
               ELB(I)=ID(K)*SCLA 
c               ELB(I)=ID(K)*SCLA - (ANGXAX-90.)
            END IF
            IF (I.GT.1) THEN
               IF (ELB(I).LT.ELB(I-1)) ELB(I)=ELB(I)+360.0
            END IF 
            KDIR(I)=ID(K+1)
            KNDREC(I)=ID(K+2)+IREC
c-----------debug (ljm)
            if(i.eq.1)then
               nrays=kndrec(i)
            else
               nrays=kndrec(i)-kndrec(i-1)
            end if
            write(7,*)'SETVOL: lvl,nrays,el,dir,kndrec=',
     +           i,nrays,elb(i),kdir(i),kndrec(i)
c-----------debug (ljm)
            K=K+3
 40      CONTINUE
      ELSE
C
C     AIRBORNE/DORADE
C
         DO 43 I=1,NEL
            KDIR(I)=1
            IF (I.GT.1) THEN
               KNDREC(I)=ID(K+2)+IREC+KNDREC(I-1)
            ELSE
               KNDREC(I)=ID(K+2)+IREC
            END IF
c-----------debug (ljm)
            write(7,*)'SETVOL dorade: i,kndrec,kdir=',
     +           i,kndrec(i),kdir(i)
c-----------debug (ljm)
            K=K+3
 43      CONTINUE
      END IF
      IEL1=1
      IEL2=NEL
      ISD=SIGN(1.0,ELB(2)-ELB(1))
      J=ISD+0.5
      IF (ICOPLANE.NE.5) THEN
         DO 50 I=2,NEL
            DEI(I-J)=ABS(UNSCEL/(ELB(I)-ELB(I-1)))
 50      CONTINUE
      END IF
      IF (IRP.EQ.0 .OR. IRP.EQ.2) THEN
C
C     VARIABLE NUMBERS OF RANGE GATES ALLOWED FOR UF AND DORADE;
C     SET NG=MAXRNG FOR READING RAYS BACK IN IN BEAMIN
C
         NG=MAXRNG
      ELSE
C
C     CONSTANT NUMBERS OF RANGE GATES ASSUMED FOR REST
C
         NG=ID(34)
      END IF
      RNOT=ID(31)+ID(32)*0.001
      DRG=ID(33)*0.001

      write(7,*)'SETVOL: irp,# gates,r0,drg=',irp,ng,rnot,drg
      write(7,*)'SETVOL:       nif,nof,nfli=',id(75),nof,nfli

      ZRAD=ID(46)*0.001
      XORG=ID(47)*0.01
      YORG=ID(48)*0.01
      IADJAZ=ID(49)*0.001*UNSCAZ
      ICOPLN=.FALSE.
      NIF=ID(75)
      MAXRD=ID(37)+ID(10)
      KNQ=0
      KRC=0
c      J=76
      J=IDPTR_INT
      DO 60 I=1,NIF
         WRITE (CTEMP1,101)ID(J),ID(J+1)
 101     FORMAT(2A4)
         READ (CTEMP1,500)NAMF
 500     FORMAT(A8)
         CFAC(I)=ID(J+2) * 0.01
C
C        SAVE NYQUIST VELOCITY AND RADAR CONSTANT IF PRESENT
C
         IF(ITPFLDC(NAMF).EQ.3.AND.KNQ.EQ.0) KNQ=ABS(CFAC(I))*100.0+0.5
         IF(ITPFLDC(NAMF).EQ.1.AND.KRC.EQ.0) KRC=CFAC(I)*100.0+0.5
         CINFLD(I)=NAMF
         SCLIF(I,1)=ID(J+4)
         SCLIF(I,2)=1./SCLIF(I,1)
c--------debug (ljm)
         write(7,*)'SETVOL: i,j,ctemp1,namf,fldinp=',i,j,' ',ctemp1,
     +        namf,cinfld(i),' ',(infld(i,ii),ii=1,3)
         write(7,*)'       vnyq,rcon=',i,cfac(i),metspc(i),iwfld(i)
         write(7,*)'       sclfactor=',i,(sclif(i,ii),ii=1,2)
c--------debug (ljm)
         J=J+5
 60   CONTINUE
      RETURN
      END





