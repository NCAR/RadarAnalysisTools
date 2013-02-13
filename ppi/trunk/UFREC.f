c
c----------------------------------------------------------------------X
c
      SUBROUTINE UFREC(IUN,IPREC,IEOF,IEOT,NAMUF,MXUF,NFLD,IFORBLK,
     X     DEC,DECWR,WORDSZ,NDUMP,IRATYP,ICORD)
C
C  READ A UNIVERSAL FORMAT BEAM TO INITIALIZE FIELD NAMES
C     DEC   - (1) Reading input on DEC,     (0) Reading input on non-DEC
C     DECWR - (1) Input was written on DEC, (0) Input was written on non-DEC
C
C     IOHB  - FIRST WORD ADDRESS OF THE OPTIONAL HEADER BLOCK
C     ILUH  - FIRST WORD ADDRESS OF THE LOCAL USE HEADER BLOCK
C     IDHB  - FIRST WORD ADDRESS OF THE DATA HEADER BLOCK
C           IF (ILUH - IOHB) = 0 ==> NO OPTIONAL HEADER BLOCK
C           IF (IDHB - ILUH) = 0 ==> NO LOCAL USE HEADER BLOCX
C     NFLD  - NUMBER OF FIELDS IN THIS RECORD
C     IBAD  - BAD DATA FLAG
C     NAME  - INTEGER NAME OF THE FIELDS
C     IFWA  - FIRST WORD ADDRESS OF THE DATA FIELD
C     ISFAC - SCALE FACTOR, MET VALUE =TAPE VALUE/SCALE FACTOR
C     NFV   - NUMBER OF VALUES IN THE DATA FIELD
C
C  MANDATORY HEADER BLOCK (45 16-BIT WORDS):
C      (2 ASCII CHARACTERS/WORD - 8 CHARACTER NAMES IN 4 WORDS)
C-----------------------------------------------------------------------
C   1. UF (ASCII)       2. RECORD LENGTH     3. FWA NON-MANDATORY
C   4. FWA LU HEADER    5. FWA FIELD HEADER  6. RECORD NUMBER IN FILE
C   7. VOLUME NUMBER    8. RAY NUMBER        9. RECORD NUMBER IN RAY
C  10. SWEEP NUMBER    11. RADAR NAME (2)   12. RADAR NAME (2)
C  13. RADAR NAME (2)  14. RADAR NAME (2)   15. SITE NAME (2)
C  16. SITE NAME (2)   17. SITE NAME (2)    18. SITE NAME (2)
C  19. DEG LAT         20. MIN LAT          21. SEC*64 LAT
C  22. DEG LONG (+ E)  23. MIN LONG (+ E)   24. SEC*64 LONG (+ E)
C  25. RADAR HGT (M)   26. YEAR (2 DIGITS)  27. MONTH
C  28. DAY             29. HOUR             30. MINUTES
C  31. SECONDS         32. TIME ZONE (UT..) 33. AZIMUTH (DEG*64)
C  34. ELEV (DEG*64)   35. SWEEP MODE       36. FIXED ANGL (DEG*64)
C  37. DEG*64 / SEC    38. DATE WRITTEN     39. MONTH WRITTEN
C  40. DAY WRITTEN     41. GENERATOR NAME   42. GENERATOR NAME
C  43. GENERATOR NAME  44. GENERATOR NAME   45. MISSING DATA FLAG
C-----------------------------------------------------------------------
C  FIELD HEADER BLOCK (20+ 16-BIT WORDS, DEPENDING ON FIELD):
C      (2 ASCII CHARACTERS/WORD - 8 CHARACTER NAMES IN 4 WORDS)
C-----------------------------------------------------------------------
C   1. FWA DATA         2. SCALE FACTOR      3. RANGE TO FIRST GATE (KM)
C   4. ADJ 1stGate (KM) 5. GATE SPACING (M)  6. NUMBER OF RANGE GATES
C   7. GATE LENGTH (M)  8. H-WIDTH (DegX64)  9. V-WIDTH (DegX64)
C  10. RECEIVER BW     11. POLARIZATION     12. WAVELENGTH (CmX64)
C  13. NMB TS SAMPLES  14. THRESH FIELD     15. THRESHOLD VALUE
C  16. THRESHOLD SCALE 17. EDIT CODE        18. PRT (MICROSEC)
C  19. BITS/SAMPLE VOL
C      Field-dependent words, starting with #20 
C  **     Velocity     20. NYQUIST          21. NCAR BAD DATA FLAG
C  *8     Power (DM)   20. RADAR CONSTANT   21. NOISE POWER
C  22. RECEIVER GAIN   23. PEAK POWER       24. ANTENNA GAIN
C  25. PULSE DURATION
C-----------------------------------------------------------------------
C  Scanning modes:
C     (0) Calibration
C     (1) PPI (sector) - scan an azimuth sector at constant elevation
C               project (R,A) location onto constant z-plane at (x,y)
C     (2) Coplane - scan an azimuth sector at constant coplane angle
C               project (R,A) location onto constant coplane at (x,y)
C     (3) RHI - scan an elevation sector at constant azimuth angle
C               project (R,E) location onto constant vertical plane at (h,z)
C     (4) Vertical - may scan an azimuth sector while pointing vertically
C               project (R,A) location onto constant z-plane at (x,y),
C               set E=0 to do so.
C     (5) Target - hold antenna at fixed azimuth and elevation angles
C               project (R,A) location onto constant z-plane at (x,y),
C               set E=0 to do so.
C     (6) Manual - move antenna to any azimuth and elevation angle
C               project (R,A) location onto constant z-plane at (x,y),
C               set E=0 to do so.
C     (7) Idle - antenna is not moving
C     (8) Surveillance (360 dg) - scan 360 dg in azimuth at fixed elevation
C               project (R,A) location onto constant z-plane at (x,y)
C-----------------------------------------------------------------------
C
      INTEGER FWA_OHB,FWA_LUHB,FWA_DHB,FWA_FFHB

      CHARACTER*8 IRATYP,ICORD

      DIMENSION IA(40000),IB(80000)
      DIMENSION IC(4096)
      CHARACTER*8 ICHAR(512)
      CHARACTER*8 INPFORM,NAMUF(MXUF),KCODE(3)
      DATA NPARMX,NZERMX,NSMLMX/10,10,10/
      DATA MDFLG/-32768/

      NPAR=0
      IEOF=0
      IEOT=0
      NZER=0
      NSML=0
    1 CONTINUE
CANNE      BUFFER IN(IUN,1) (IA(1),IA(1092))
C     FORTRAN BLOCKING
C
      IF(IFORBLK.EQ.1)THEN
         CALL RDSUNREC(IA,NWDS,DEC,DECWR)
         print *,'UFREC nwds=',nwds
         NWDS=INT((NWDS-1)/8)+1
      ELSE

C     COS BLOCKING
C
         CALL RDCOSREC(IA,NWDS)
      END IF
      IPREC=IPREC+1
CANNE      RST=UNIT(IUN)
CANNE      N64=LENGTH(IUN)
      N64=NWDS
      N16IN=4*N64
      if(iprec.eq.1)print *,'UFREC: n64,n16,ia(1)=',n64,n16in,ia(1)

      IF(N64.LE.0)THEN
         NZER=NZER+1
         WRITE(6,3)IPREC
    3    FORMAT(8X,' ZERO LENGTH RECORD: PHYSICAL RECORD = ',I8)
         IF(NZER.LT.NZERMX)GO TO 1
         WRITE(6,5)NZERMX
    5    FORMAT(1X,'******  MORE THAN',I3,
     +        ' ZERO LENGTH RECORDS = EOT  ******')
         IEOF=0
         IEOT=1
         RETURN
      END IF

C     GOOD READ OF A RECORD - RESET INTERNAL FLAGS
C
      IF(NWDS.GT.0.)THEN
         NPAR=0
         IEOF=0
         IEOT=0
         NZER=0
         NSML=0

C        END OF FILE
C
      ELSE IF (NWDS.LE.0.0)THEN
         PRINT 7,IUN
 7       FORMAT(8X,' END OF FILE ON UNIT= ',I3)

C       IF DOUBLE EOF = LOGICAL END OF TAPE
C
        IF(IEOF.EQ.1)THEN
           IEOF=0
           IEOT=1
           PRINT 9
    9      FORMAT(1X,'******  LOGICAL END OF TAPE  ******')
           RETURN
        END IF
        IEOF=1

C     PARITY ERROR - READ THE NEXT RECORD
C
C      ELSE IF(RST.EQ.1.0)THEN
C        NPAR=NPAR+1
C        N64=0
C        WRITE(6,11)IPREC
C   11   FORMAT(8X,' PARITY ERROR IN RECORD = ',I8)
C        IF(NPAR.LT.NPARMX)GO TO 1
C        WRITE(6,13)NPARMX
C   13   FORMAT(1X,'******  MORE THAN',I3,
C     +            ' PARITY ERRORS - EOT  ******')
C        IEOT=1
C        RETURN
      END IF

C     GOOD READ - START UNPACKING BY FINDING LOGICAL RECORD LENGTH
C     CHECK IF BYTE SWAPPING IS REQUIRED.
C
      idec=int(dec)
      idecwr=int(decwr)
      if(iprec.eq.1)print *,'UFREC:     dec,decwr=',idec,idecwr
      IF((DEC .EQ. 1.0 .AND. DECWR .EQ. 0.0).OR.
     +   (DEC .EQ. 0.0 .AND. DECWR .EQ. 1.0))THEN
         CALL SWAP32(IA,NWDS*2)
         if(iprec.eq.1)print *,'UFREC: swapped ia(1)=',ia(1)
      END IF

C     Get length of record in 16-bit words (2nd 16 bits of IA)
C
      CALL GBYTES(IA(1),ITER,16,16,0,1)

      IF( (N16IN-ITER) .GT. 4)THEN
         NSML=NSML+1
         WRITE(6,131)N64
 131     FORMAT(8X,' SHORT RECORD OF = ',I5,' 64-BIT WORDS')
         IF(NSML.LT.NSMLMX)GO TO 1
         WRITE(6,133)NSMLMX
 133     FORMAT(1X,'****** MORE THAN',I3,
     +        ' SHORT RECORDS = EOT  ******')
         IEOF=0
         IEOT=1
         RETURN
      END IF

C     RECORD IS APPARENTLY GOOD, UNPACK ITER 16-BIT WORDS.
C     
      CALL GBYTES(IA(1),IB(1),0,16,0,ITER)
      DO 14 I=1,ITER
         IF(IB(I).GT.32767)IB(I)=IB(I)-65536
 14   CONTINUE
      if(iprec.eq.1)then
         INP_INT=IB(1)
         CALL ASDPMD(INP_INT,INPFORM,DEC,DECWR,WORDSZ)
         FWA_OHB  = IB(3)
         FWA_LUHB = IB(4)
         FWA_DHB  = IB(5)
         FWA_FFHB = IB(FWA_DHB+4)
         print *,'UFREC:       length =',iter
         print *,'            fwa_ohb =',fwa_ohb
         print *,'           fwa_luhb =',fwa_luhb
         print *,'            fwa_dhb =',fwa_dhb
         print *,'           fwa_ffhb =',fwa_ffhb
         print 339,ib(2),inpform,ib(1),ib(1),inp_int
 339     format(1x,'UF record length (16-bit words)=',i8,
     +        ' First word in UF record=',a8,2x,a8,2x,i8,2x,i8)
      end if

      IF(INPFORM.NE.'UF')THEN
         WRITE(6,141)
  141    FORMAT(1X,'**** Dataset may not be Universal Format **** ',
     +             '****     or Byte-swapping is required    **** ')
         IEOT=1
         RETURN
      END IF

      IF(NDUMP.EQ.1)THEN
         PRINT *,' '
         PRINT *,' Integer dump of full record of length = ',ITER
         CALL DMPINTGR(IB,ITER)
         PRINT *,' '
      END IF

C     OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
      IDATH = IB(5)
      NRF   = IB(6)
      IVOL  = IB(7)
      IRY   = IB(8)
      IRC   = IB(9)
      ISWP  = IB(10)
      IF(IB(26).GT.99)IB(26)=IB(26)-100
      IDT   = IB(26)*10000+IB(27)*100+IB(28)
      IF(IDT.GT.992359)THEN
         JDT=1000000*(IDT/1000000)
         IDT=IDT-JDT
         print *,'JDT,IDT=',JDT,IDT
      END IF
      ITM   = IB(29)*10000+IB(30)*100+IB(31)
      AZ    = IB(33)/64.0
      EL    = IB(34)/64.0
      IF(EL.GT.180.0)EL=EL-360.0
      ITP   = IB(35)
      FXANG = IB(36)/64.0

c     LJM - added 02/20/98 for debugging UF from S-Pol Piraq-->UF
c     
      NFH   = IB(IDATH+4)
      RMIN  = FLOAT(IB(NFH+2))+0.001*FLOAT(IB(NFH+3))
      IGSP  = IB(NFH+4)
      NRNG  = IB(NFH+5)
      IRN   = RMIN
      IRX   = RMIN+0.001*(NRNG-1)*IGSP+0.5

C     Angles and such for airborne radars - stored in LOCAL USE HEADER BLOCK
C
      IF(ICORD.EQ.'AIRBRNE ')THEN
         LUHB  = IB(4)
         PITCH = IB(LUHB+ 2)/64.
         ROLL  = IB(LUHB+ 3)/64.
         PHDG  = IB(LUHB+ 4)/64.
         VNS   = IB(LUHB+ 5)/10.
         VEW   = IB(LUHB+ 6)/10.
         WP3   = IB(LUHB+ 7)/10.
         HGME  = IB(LUHB+ 8)/10.
         UAIR  = IB(LUHB+ 9)/10.
         VAIR  = IB(LUHB+10)/10.
         WAIR  = IB(LUHB+11)/10.
         DRIFT = IB(LUHB+12)/64.
         ROT1  = IB(LUHB+13)/64.
         TILT1 = IB(LUHB+14)/64.
         ROT2  = IB(LUHB+17)/64.
         TILT2 = IB(LUHB+18)/64.
         AZ    = ROT1+ROLL
         EL    = TILT1
         AZR   = AZ
         ELR   = EL
         ITP   = 8
         write(*,*)'Airborne rotation angles 1 and 2=',rot1,rot2
         write(*,*)'Airborne tilt     angles 1 and 2=',tilt1,tilt2
      END IF

      IF(IPREC.EQ.1)THEN
         print *,'IDT,ITM=',idt,itm
         WRITE(6,770)IDT,ITM,AZ,EL,FXANG,IVOL,NRF,ISWP,ITP
 770     FORMAT(1X,'  File begins at D=',I6.6,' T=',I6.6,4X,
     +        ' First UF record: Az=',F6.1,' El=',F6.1,
     +        ' Fx=',F6.1,' Vol=',I3,' Vr=',I5,
     +        ' Swp=',I2,' Md=',I1)
         
C     FIND THE FIELD NAMES, FIRST WORD ADDRESSES,AND NUMBER OF VALUES
C
         IOHB=IB(3)
         ILUH=IB(4)
         IDHB=IB(5)
         IFNL=IDHB+3
         IFPL=IDHB+4
         NFTOT=IB(IDHB)
         NREC=IB(IDHB+1)
         NFLD=IB(IDHB+2)
         IBAD=IB(45)

C     Mandatory header block: LENGTH = 45
C
         LENGTH = 45
         IF(LENGTH .GT.0)THEN
            DO J=1,LENGTH
               IC(J)=IB(J)
            END DO
            PRINT *,' Mandatory header block: ',
     X           'Length=',LENGTH,' FWA=',1
            CALL DMPINTGR(IC,LENGTH)
         END IF

C     Optional header block: LENGTH = ILUH - IOHB
C
         LENGTH = ILUH - IOHB
         IF(LENGTH .GT.0)THEN
            DO J=1,LENGTH
               IC(J)=IB(IOHB+J-1)
            END DO
            PRINT *,' Optional header block: ',
     X           'Length=',LENGTH,' FWA=',ILUH
            CALL DMPINTGR(IC,LENGTH)
         END IF

C     Local use header block: LENGTH = IDHB - ILUH 
C
         LENGTH = IDHB - ILUH
         IF(LENGTH .GT.0)THEN
            DO J=1,LENGTH
               IC(J)=IB(ILUH+J-1)
            END DO
            PRINT *,' Local user header block: ',
     X           'Length=',LENGTH,' FWA=',ILUH
            CALL DMPINTGR(IC,LENGTH)
         END IF

C     Data header block: LENGTH = IB(IFPL) - IDHB
C
         LENGTH = IB(IFPL) - IDHB
         IF(LENGTH .GT.0)THEN
            DO J=1,LENGTH
               IC(J)=IB(IDHB+J-1)
            END DO
            PRINT *,' Data header block: ',
     X           'Length=',LENGTH,' FWA=',IDHB
            CALL DMPINTGR(IC,LENGTH)
            J=0
            DO I=4,NFLD*2+2,2
               J=J+1
               INP_INT=IC(I)
               CALL ASDPMD(INP_INT,ICHAR(J),DEC,DECWR,WORDSZ)
            END DO
            PRINT *,' Data header block: character dump'
            CALL DMPCHAR(ICHAR,NFLD)
            jter = 450
            print *,' Integer dump of first ',jter,' words'
            call dmpintgr(ib,jter)
          END IF

         WRITE(6,19)NFTOT,IBAD
 19      FORMAT(/,9X,' Number of UF fields =',I4,' BAD data value =',I8)
         IF(NFTOT.GT.MXUF)THEN
            WRITE(6,21)MXUF
 21         FORMAT(8X,' MORE THAN',I3,' UNIVERSAL FORMAT FIELDS ')
            STOP
         END IF
         WRITE(6,27)NFLD,NFREC
 27      FORMAT(9X,' Number of records in a ray =',I4,
     x             ' Number of fields per record =',I4)
         IF(NDUMP.EQ.1)THEN
            DO 40 I=1,NFLD
               LOCN=IFNL+2*(I-1)
               INP_INT=IB(LOCN)
               NAMUF(I)='xxxxxxxx'
               CALL ASDPMD(INP_INT,NAMUF(I),DEC,DECWR,WORDSZ)
               LOC=IFPL+2*(I-1)
               LOCI=IB(LOC)
               IFWA=IB(LOCI)
               ISFAC=IB(LOCI+1)
               ISPAC=IB(LOCI+4)
               NGATE=IB(LOCI+5)
               print *,'NAMEUF=',namuf(i)
               if(namuf(i).eq.'\0\0'.or.namuf(i).eq.'  ')then
                  write(namuf(i),35)i
 35               format('B',i1)
               end if
               write(6,37)i,namuf(i),loci,ifwa,isfac,ispac,ngate
 37            format (/,
     x              ' Field #',I2.2,' Name=',A4,' field header fwa=',I5,
     x              ' data fwa=',I5,' sclfac=',I5,' gspac=',I5,
     x              ' # gates=',i5)

C     Field header block: LENGTH = IFWA - LOCI
C
               LENGTH = IFWA - LOCI
               IF(LENGTH .GT.0)THEN
                  DO J=1,LENGTH
                     IC(J)=IB(LOCI+J-1)
                  END DO
                  PRINT *,' Field header block: ',
     X                 'Length=',LENGTH,' FWA=',IFWA
                  CALL DMPINTGR(IC,LENGTH)
               END IF

 40         CONTINUE
         END IF

         DO 50 I=1,NFLD
            LOCN=IFNL+2*(I-1)
            INP_INT=IB(LOCN)
            print *,'NAMUF(I)=',namuf(i),'x'
            CALL ASDPMD(INP_INT,NAMUF(I),DEC,DECWR,WORDSZ)
            print *,'NAMUF(I)(1:2)=',namuf(i)(1:2)
            print *,'NAMUF(I)(3:4)=',namuf(i)(3:4)
            print *,'NAMUF(I)(5:8)=',namuf(i)(5:8)
            print *,'NAMUF(I)=',namuf(i),'x'
            IF(NAMUF(I)(1:2).EQ.'RE')NAMUF(I)='RE      '
            IF(NAMUF(I)(1:2).EQ.'VE')NAMUF(I)='VE      '
            print *,'NAMUF(I)=',namuf(i),'x'
            LOC=IFPL+2*(I-1)
            LOCI=IB(LOC)
            IFWA=IB(LOCI)
            ISFAC=IB(LOCI+1)
            RMN=FLOAT(IB(LOCI+2))+0.001*FLOAT(IB(LOCI+3))
            DEL=0.001*FLOAT(IB(LOCI+4))
            NFV=IB(LOCI+5)
c-----------debugging UF records
c            print *,'uf: ',locn,loc,loci,ifwa,isfac
c            do j=1,285
c               ic(j)=ib(loci+j-1)
c            end do
c            call dmpintgr(ic,285)
c            print 43,rmn,del,nfv
c 43         format('Rmin=',f8.3,' Gspacing=',f8.3,' Number gates=',i8)
c     Debug print - july l0
            print *,'UFREC ib(16),ib(13),ib(20),ib(14),ib(19)=',
     +      ib(loci+16),ib(loci+13),ib(loci+20),ib(loci+14),ib(loci+16)
            IF(IB(LOCI+16).EQ.IBAD .OR. IB(LOCI+16).EQ.0)THEN
               KCODE(1)='..'
            ELSE
               INP_INT=IB(LOCI+16)
               CALL ASDPMD(INP_INT,KCODE(1),DEC,DECWR,WORDSZ)
               IF(KCODE(1).EQ.'  ')KCODE(1)='xx'
            END IF
            IF(IB(LOCI+13).EQ.IBAD .OR. IB(LOCI+13).EQ.0)THEN
               KCODE(2)='..'
            ELSE
               INP_INT=IB(LOCI+13)
               CALL ASDPMD(INP_INT,KCODE(2),DEC,DECWR,WORDSZ)
               IF(KCODE(1).EQ.'  ')KCODE(1)='xx'
            END IF
            IF(IB(LOCI+20).EQ.IBAD .OR. IB(LOCI+20).EQ.0)THEN
               KCODE(3)='..'
            ELSE
               INP_INT=IB(LOCI+20)
               CALL ASDPMD(INP_INT,KCODE(3),DEC,DECWR,WORDSZ)
               IF(KCODE(3).NE.'FL')KCODE(3)='..'
            END IF
            IF(IB(LOCI+14).EQ.IBAD .OR. IB(LOCI+14).EQ.0)THEN
               THR=-999.9
            ELSE
               THR=FLOAT(IB(LOCI+14))/FLOAT(IB(LOCI+15))
            END IF
            IF(IB(LOCI+19).EQ.IBAD .OR. IB(LOCI+19).EQ.0)THEN
               WRD20=-999.9
            ELSE
               WRD20=FLOAT(IB(LOCI+19))/FLOAT(ISFAC)
            END IF
            ILWA=IFWA+NFV-1
            if(namuf(i).eq.'\0\0'.or.namuf(i).eq.'  ')then
               write(namuf(i),35)i
            end if
            WRITE(6,47)I,NAMUF(I),(KCODE(J),J=1,3),IFWA,ILWA,NFV,ISFAC,
     +           THR,WRD20,LOCI,RMN,DEL
 47         FORMAT(10X,I3,' NAME=',A2,'.',A2,' THR FLD=',A2,' FL=',A2,
     +           ' WRDS=',I5,'-',I5,' N=',I4,' SC=',I4,' TH=',F7.1,
     +           ' W20=',F7.1,' LOCI=',I5,' RMN=',F7.3,' GSP=',F5.3)
 50      CONTINUE
      END IF
      
c     LJM - added 02/20/98 for debugging UF from S-Pol Piraq-->UF
c
      WRITE(6,773)IDT,ITM,AZ,EL,FXANG,IRN,IRX,IGSP,
     +     VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
 773  FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +     ' F=',F5.1,' R=',I3,'-',I4,' Gs=',I4,' Ny=',F5.1,
     +     ' Vl=',I3,' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,
     +     ' Md=',I1,' Na=',I4,' Tr=',I5)
      call sflush
C     REWIND IUN
      IRW=1
c-----      CALL INIT_COS(IUN,IRW)
      IPREC=IPREC-1
      RETURN
      END
