c
c----------------------------------------------------------------------X
c
      SUBROUTINE RDHRD(IUN,DEC,DECWR,WORDSZ,IFD,NRST,
     X     ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,BGFLAG)

C READ A SCAN OF RADAR DATA WRITTEN IN THE HURRICANE RESEARCH DIVIDISION'S (HRD)
C (NOAA, ENVIRONMENTAL RESEARCH LABORATORIES) FORMAT.  EACH SCAN IS PRECEEDED
C BY A 100-WORD HEADER AND EACH RAY BY AN 8 BYTE HEADER CONTAINING AZIMUTH,
C ELEVATION AND TIME INFORMATION.  THE DATA ARE COMPRESSED WITH STRINGS OF
C DATA AT OR BELOW THE NOISE LEVEL REPLACED BY TWO BYTES: A 0 BYTE FOLLOWED
C BY THE NUMBER OF GATES BELOW NOISE.  THE HRD DATA ONLY CONTAIN THE POWER
C FIELD IN DBM.

      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI

      CHARACTER*8 NAMDZ
      CHARACTER*8 NAMDM

      CHARACTER*3 LABLS
      CHARACTER*1 BGFLAG
      LOGICAL COLRFIL,FRSTREC,PLTSW,VECTS

      DATA NAMDM/'DZ      '/
      DATA MNANG/10/
CC      DATA DROLD/-99./
      DATA R0OLD/-99./
      DATA IPREC/0/

C  Scanning modes:
C
C     (1) PPI (sector) - scan an azimuth sector at constant elevation
C               project (R,A) location onto constant z-plane at (x,y)
C     (3) RHI - scan an elevation sector at constant azimuth angle
C               project (R,E) location onto constant vertical plane at (h,z)
C     (4) Vertical - may scan an azimuth sector while pointing vertically
C               project (R,A) location onto constant z-plane at (x,y),
C               set E=0 to do so.
C     (8) Surveillance (360 dg) - scan 360 dg in azimuth at fixed elevation
C               project (R,A) location onto constant z-plane at (x,y)
      VNYQ=0.0
      NFLD=1
      IFL=IFIND(NAMDM,NAMFLD,NFLDS)

 5    NAZ=1
      NTANG=1
      ELOLD=-99.
C
C     READ A RAY OF HRD DATA STORE AND STORE THE DBM DATA, AZIMUTH AND
C     ELEVATION INFORMATION.  KEEP READING UNTIL EITHER THE END OF SCAN
C     FLAG (IEOS=1) OR END OF TAPE FLAG HAS BEEN SET, OR UNITL THE NUMBER
C     OF BEAMS READ (NAZ) EXCEEDS THE MAXIMUM NUMBER ALLOWED (MXA- SET IN
C     PARAMETER STATEMENT).  THE END OF SCAN FLAG IS SET IF A NEW SCAN
C     HEADER HAS BEEN DETECTED.
C
 10   CALL GETHRD(IUN,AZ,EL,ITIME,IDATE,IYR,IMON,IDAY,R0,DR,
     +   NGTS,RADCON,IEOS,IEOT,BDVAL,IBTIME,DAT,IFL,NAZ,RCOR,
     +   MXR,MXA,MXF,DEC,DECWR,WORDSZ)

      IF(IFD.EQ.1.AND.NAZ.EQ.1)PRINT *,' Begin Sweep'
      FXANG=EL
      IPREC=IPREC+1

C     CHECK FOR END OF SCAN, END OF TAPE OR THE MAXIMUM NUMBER OF
C     BEAMS BEING EXCEEDED.

      IF(IEOS.EQ.1.OR.IEOT.EQ.1.OR.NAZ.GT.MXA)THEN
         IEOS=0
         IF(IFD.EQ.1)PRINT *,' End Sweep'

C        IF EITHER THE CURRENT TIME IS LESS THAN THE REQUESTED BEGINNING
C        TIME OR THE NUMBER OF BEAMS IN THIS SCAN IS LESS THAN THE
C        MINIMUM REQUIRED, GO BACK AND READ THE NEXT SCAN

         IF(ITIME.LT.IBTIME)GO TO 5
         IF(NAZ.LE.MNANG)GO TO 5

         ISWP=ISWP+1
         NAZ=NAZ-1
         NTANG=NTANG-1
         IFTIME=ITIME
         NANG(1)=NAZ
         IHF=NAZ/2
         AZDIF=ABS(AZA(IHF,1)-AZA(1,1))
c--------write(*,*)'azdif= ',azdif
         IF(AZDIF.GT.3.0)THEN
            FXANG=EL
            ITP=8
         ELSE
            FXANG=AZ
            ITP=3
            DO 310 J=1,NAZ
               TMP=AZA(J,1)
               AZA(J,1)=ELA(J,1)
               ELA(J,1)=TMP
 310        CONTINUE
            NAZ=241
            NANG(1)=NAZ
            DO 312 J=1,NAZ
               AZA(J,1)=AZA(J+68,1)
               ELA(J,1)=ELA(J+68,1)
               DO 311 I=1,NGTS
                  DAT(I,J,IFL)=DAT(I,J+68,IFL)
 311           CONTINUE
 312        CONTINUE
         END IF
         ITPOLD=ITP
         NGTSOLD=NGTS
         ISWPOLD=ISWP
         IF(DR.NE.DROLD.OR.R0.NE.R0OLD)THEN
            DROLD=DR
            R0OLD=R0
            CALL RNGST
            CALL MNMX(DROLD)
         END IF
         FXOLD=ELA(10,1)
         AZSUM=0.
         DO 301 J=2,NAZ
            DIF=ABS(AZA(J,1)-AZA(J-1,1))
            IF(DIF.GT.300.)DIF=360.-DIF
            AZSUM=AZSUM+DIF
 301     CONTINUE
         AVGI=AZSUM/FLOAT(NAZ-1)
         PLTSW=.FALSE.
         VECTS=.FALSE.
         CALL LABELPR(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     +        BGFLAG)
         RETURN
      END IF
      
C     PUT DBM VALUES INTO OUTPUT ARRAY AND STORE AZIMUTH AND ELEVATION
C     INFORMATION.  GO BACK TO READ NEXT BEAM.
      
c      IF(EL.LT.ELOLD)GO TO 10
c      ELOLD=EL
c      TMP=EL
c      EL=AZ
c      AZ=TMP

      IF(IFD.EQ.1 .AND. MOD(NAZ,NRST).EQ.0)THEN
         WRITE(6,771)IDATE,ITIME,AZ,EL,FXANG,ITP,NFLD,NGTS,
     +        R0,DR,NAZ,IPREC
 771     FORMAT(1X,' D=',I6.6,' T=',I6.6,' A=',F6.2,' E=',F6.2,
     +        ' Fx=',F6.2,' Md=',I1,' Nfl=',I1,' Ng=',I4,
     +        ' R0=',F5.2,' Dr=',F5.3,' Naz=',I4,' Tr=',I5)
      END IF
      
      IF(AZ.GT.180..AND..NOT.IAZC)AZ=AZ-360.
      AZA(NAZ,1)=AZ
      ELA(NAZ,1)=EL
      ITM(NAZ,1)=ITIME
      NAZ=NAZ+1
      NTANG=NTANG+1
            
      GO TO 10
      END
