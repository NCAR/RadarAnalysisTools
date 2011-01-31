      SUBROUTINE RADAR(KRD,CTDBM,CTDBMXH,CTDBMXV)
C     
C     THIS SUBROUTINE PROCESSES THE 'RADAR' CARD, PARSING FOR
C     SUCH INFO AS THE PROCESSOR ID (DATA FORMAT) AND THE RADAR
C     COORDINATES.
C     
C     IRP - Input radar format (Used in UFCART for branching to read routines.)
C           (0) Universal format          --> UFNCAR
C           (1) ATD/RSF processors RP3-7  --> RPNCAR
C           (2) DORADE                    --> DORVOL --> DORSWP
C           (3) NEXRAD Level II from NCDC --> NEXVOL --> NEXSWP
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C                        Set in RADAR (DOR-GND format)
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
C      
      PARAMETER (MXCNT=500)
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /FORMAT/ IRP,IBLOCK
      DIMENSION CTDBM(MXCNT), CTDBMXH(MXCNT), CTDBMXV(MXCNT)
      CHARACTER*8 KRD(10)
      CHARACTER*8 INT1,BLOCK
      CHARACTER*30 LINE
      
      ICOPLANE = -1
C     
C     INITIALIZE POWER TABLES WITH BAD VALUES
C     
      DO 10 I=1,MXCNT
         CTDBM(I)  =-32768
         CTDBMXH(I)=-32768
         CTDBMXV(I)=-32768
 10   CONTINUE
      
      READ(KRD,100)INT1,FUNIT,FNUM,BLOCK
 100  FORMAT(/A8/F8.0/F8.0/A8)
      IF(INT1.EQ.'UF') THEN
         IRP=0
         INT1='UNIV'
      ELSE IF(INT1(1:2).EQ.'RP' .OR. INT1(1:2).EQ.'FF') THEN
         IRP=1
         CFAC1=0
         CFAC2=0
         CFAC3=0
         INT1='FFORMAT'
      ELSE IF(INT1.EQ.'DORADE') THEN
         IRP=2
         CFAC1=0
         CFAC2=0
         CFAC3=0
      ELSE IF(INT1 .EQ. 'NEXRAD') THEN
         IRP=3
         CFAC1=0
         CFAC2=0
         CFAC3=0  
      ELSE IF(INT1 .EQ. 'WSR88D') THEN
         IRP=3
         CFAC1=0
         CFAC2=0
         CFAC3=0         
      ELSE IF(INT1 .EQ. 'NOWRAD') THEN
         IRP=3
         CFAC1=0
         CFAC2=0
         CFAC3=0         
      ELSE 
         IRP=0
         INT1='UNIV'
      ENDIF
      IUNIT=INT(FUNIT)
      INUM =INT(FNUM)
      IF (IUNIT.NE.0 .AND. IUNIT.LT.10) THEN
         WRITE(*,*)'***INVALID CALIBRATION FILE UNIT NUMBER: ',IUNIT
         STOP
      END IF
      IF (BLOCK(1:6).EQ.'SUNFOR') THEN
         IBLOCK=1
      ELSE
         IBLOCK=0
      END IF
      WRITE(*,120)
 120  FORMAT(//,5X,'SUMMARY OF RADAR COMMAND')
      WRITE(*,140)
 140  FORMAT(5X,'------- -- ----- -------')
      WRITE(*,160)INT1
 160  FORMAT(/,5X,'                     DATA FORMAT: ',A8)
C     
C     READ IN CALIBRATION INFO INCLUDING RADAR CONSTANT
C     
      IF (IUNIT.GE.10) THEN
         OPEN(UNIT=IUNIT,ACCESS='SEQUENTIAL',STATUS='OLD')
         READ(IUNIT,20)LINE
 20      FORMAT(A30)
         
         IF (LINE(1:1).EQ.'C') THEN
C     
C     PPI FORMAT OF CALIBRATION FILES
C     
 40         IF (LINE(1:7).EQ.'CT-DBM ') THEN
               I=1
               READ(LINE,25) CFAC1
 25            FORMAT(10X,F10.1)
 35            READ(IUNIT,20,END=200) LINE
               IF (LINE(1:1).EQ.' ') THEN
                  READ(LINE,30) CTDBM(I)
 30               FORMAT(10X,F10.1)
                  I=I+1
                  GOTO 35
               ELSE
                  WRITE(*,170)CFAC1
 170              FORMAT(14X,'RADAR CONSTANT FOR DMNE: ',F8.2)
                  GOTO 40
               END IF
            ELSE IF (LINE(1:8).EQ.'CT-DBMXH') THEN
               I=1
               READ(LINE,25) CFAC2
 45            READ(IUNIT,20,END=200) LINE
               IF (LINE(1:1).EQ.' ') THEN
                  READ(LINE,30) CTDBMXH(I)
                  I=I+1
                  GOTO 45
               ELSE
                  WRITE(*,183)CFAC2
 183              FORMAT(13X,'RADAR CONSTANT FOR DBMXH: ',F8.2)
                  GOTO 40
               END IF
            ELSE IF (LINE(1:8).EQ.'CT-DBMXV') THEN
               I=1
               READ(LINE,25) CFAC3
 55            READ(IUNIT,20,END=200) LINE
               IF (LINE(1:1).EQ.' ') THEN
                  READ(LINE,30) CTDBMXV(I)
                  I=I+1
                  GOTO 55
               ELSE
                  WRITE(*,187)CFAC3
 187              FORMAT(13X,'RADAR CONSTANT FOR DBMXV: ',F8.2)
                  GOTO 40
               END IF
            END IF
         ELSE
C     
C     OLD SPRINT FORMAT
C     
            READ(LINE,500)CFAC1
 500        FORMAT(F8.2)
            WRITE(*,170)CFAC1
            DO 75 I=1,MXCNT
               READ(IUNIT,60,END=80)IX,VALUE
 60            FORMAT(I6,F8.2)
               IF (IX.GT.0 .AND. IX.LE.MXCNT) THEN
                  CTDBM(IX)=VALUE
               ELSE IF (IX.LT.0) THEN
                  WRITE(*,*)'***ZERO OR NEGATIVE COUNT IN CAL CURVE***'
                  STOP
               ENDIF
 75         CONTINUE
 80         CONTINUE
         END IF
      ELSE IF (INUM.GT.0) THEN
C
C     INPUT CARDS PROVIDED BY USER TO SUPPLY CAL. INFO
C
         DO 85 I=1,INUM
            CALL KARDIN(KRD)
            READ(KRD,90)IVAL,VALUE
 90         FORMAT(/I8/F8.2)
            IF (IVAL.LT.0) THEN
               WRITE(*,*)'***NEGATIVE COUNT IN CAL CURVE***'
               STOP
            ELSE
               CTDBM(IVAL)=VALUE
            END IF
 85      CONTINUE
      END IF
C     
C     SUMMARIZE CARD
C     
 200  IF (IUNIT.GE.10) THEN
         WRITE(*,175)IUNIT
 175     FORMAT(5X,'CALIBRATION DATA TAKEN FROM UNIT: ',I4)
      ELSE IF (INUM.GT.0) THEN
         WRITE(*,190)
 190     FORMAT(5X,'CALIBRATION DATA TAKEN FROM INPUT CARDS',/)
      END IF
      IF (IRP.EQ.3) THEN
         WRITE(*,184)
 184     FORMAT(15X,
     X        'BLOCKING OF NEXRAD INPUT FILE DETERMINED BY SPRINT'/)
      ELSE
         IF (IBLOCK.EQ.0) THEN
            WRITE(*,185)
 185        FORMAT(15X,'BLOCKING ON INPUT FILE: COS'/)
         ELSE
            WRITE(*,193)
 193        FORMAT(15X,'BLOCKING ON INPUT FILE: SUN FORTRAN'/)
         END IF
      END IF
      print *,'RADAR: irp,icoplane=',irp,icoplane
      
      END
      
