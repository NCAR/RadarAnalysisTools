c
c----------------------------------------------------------------------X
c
      SUBROUTINE UFWRITE(DAT,ITPOLD,NAMOUT,NMOUTUF,NUMUF,IUFUN,NUMUN,
     X     IYR,IMON,IDAY,ITM,FXANG,AZA,ELA,NANG,NAMFLD,NFLDS,R0,
     X     DROLD,NGTSINP,BDVAL,VNYQ,IWRUF,IWFOR,MXR,MXA,MXF,DEC,DECWR,
     X     WORDSZ,NETWORK,IRATYP,GRDTYP)

C     ROUTINE TO (FORTRAN) WRITE A SCAN OF DATA OUT IN UNIVERSAL FORMAT.
C        DEC - (1) Writing UF on DEC, (0) Writing UF on non-DEC
C
C     Universal format words are 16 bits long.  A UF record consists of 
C     a mandatory header block (45 words), optional and local use header
C     blocks (not used here), a data header block (3 + 2*NUMUF words),
C     and a LEN_FHB word field header block plus NGTS of data for each 
C     of NUMUF fields.
C
C     IWRUF  - UF writing flag: (0) not writing, (1) writing UF output.
C     IWFOR  - Output blocking flag: (0) COS-blocked, (1) Fortran-blocked.
C     IUFUN  - Array of UF output units (maximum of 20).
C     NUMUN  - The number of output UF units that have been requested on the
C              UFOUT input command.  If the number of units being used exceeds
C              NUMUN, then output of UF data stops.
C
C     IDATA  - Buffer to hold the unpacked UF physical record.
C     ID     - Buffer to hold the packed UF record that gets written.
C     NAMOUT - Names of fields in current file to be written
C     NMOUTUF- Names that fields written to UF file are to have.
C     MAX16B - Maximum number of 16-bit words that can be contained in a 
C              UF output physical record.  If a logical record (beam of 
C              data) exceeds MAX16B, multiple physical records are written.
C     MAXCW  - Maximum number of WORDSZ-bit words required to hold a UF output
C              physical record.
C     MAX16F - The maximum number of 16-bit words that any output file is
C              allowed to have.  Once MAX16F has been exceeded a double
C              eof is written to the current output unit and a new unit
C              is opened.  A mass store volume cannot exceed 400 Mbytes so the
C              default value for MAX16F is set to 320 Mbytes (8 x 10^7 32-bit
C              words).
C
C     MDFLG    - Missing data flag (usually -32768)
C     NGTS     - Number of gates within a ray of data.
C     IDATA(1) - 'UF', the UF record identifier
C     IDATA(2) - Number of 16-bit words in UF output record
C     IDATA(6) - Physical record number from beginning of file (IREC)
C     IDATA(7) - Volume scan number from beginning of file (NVOL)
C     IDATA(8) - Ray number within a volume scan (NRAY)
C     IDATA(9) - Physical record number within a ray (IR)
C     IDATA(10)- Sweep number within a volume scan (NSCAN)
C     IDATA(46)- Total number of fields in a ray (NUMUF).
C     IDATA(47)- Total number of records in a ray (NREC).
C     IDATA(48)- Total number of fields in a record.
C     NBYTES   - Number of bytes in UF output record
C     WORDSZ   - Number of bits in a computer word
C     NREC     - Number of records in a ray of data
C     NCW      - Number of computer words of size WORDSZ
C     N16B     - Number of 16-bit words in a computer word (WORDSZ/16)
C     N8B      - Number of  8-bit words in a computer word (WORDSZ/8)
C
      PARAMETER(MAX16B=12000,MAXCW=6000)
      PARAMETER(MXUF=30,MXUN=30)

      CHARACTER*8 NAMFLD(MXF),NAMOUT(MXUF),NMOUTUF(MXUF)
      CHARACTER*8 CTEMP,DTEMP,NETWORK,IRATYP,CFTGEN
      CHARACTER*8 NETCH(4),RADCH(4),GENCH(4),EDITCH
      CHARACTER*8 GRDTYP
      CHARACTER*15 TODAY
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2),ITM(MXA,2)
      DIMENSION NANG(2)
      DIMENSION IDATA(MAX16B),ID(MAXCW)
      DIMENSION IUFUN(MXUN)
      DIMENSION NFLD1(2),I1(2),I2(2)

      DATA IDATA(2),LEN_FHB/0,20/
      DATA MAX16F/8E7/
      DATA NVOL,NSCAN,IREC,NRAY,FXOLD,NUMWRD/1,1,1,1,-99.,0/
      DATA SCALE,MDFLG/100.,-32768/
      DATA IBW,IWL,IPRT/3*-32768/
      DATA IWR,IEOR,IEOF,IEOD/1,8,14,15/
      DATA IOPEN/0/
      DATA NUN/1/
      DATA ZTOP/20.0/

C     Calculate the number of 16- and 8-bit words in a computer word
C     of size WORDSZ. Also, their floating-point inverses.
C
      N16B=NINT(WORDSZ/16)
      N8B =NINT(WORDSZ/8)
      FI_N16B=1.0/N16B
      FI_N8B=1.0/N8B
      
C     Open initial output unit if writing COS-blocked records (IWFOR=0) 
C     INIT_COSW is in cinitcos.c module.
C     
      IF(IWFOR.EQ.0.AND.IOPEN.EQ.0)THEN
         CALL INIT_COSW(IUFUN(NUN))
         print *,'Open initial output unit = ',iufun(nun),
     +        ' for writing COS-blocked records'
         IOPEN=1
         NVOL=1
         IREC=1
         NSCAN=1
         NRAY=1
      END IF
      
      NGTS=NGTSINP
      R0K=FLOAT(INT(R0))
      R0M=(R0-R0K)*1000.
      
C     If end of volume (fixed angle less than the previous fixed
C     angle) write an EOF. additionally, if the number of words written
C     out so far exceeds the maximum allowed per file, write a double EOF
C     (EOD, end-of-data), reset some counters and increment the output unit.
      
c      write(*,*)'fxang,fxold,vny,ngts=',fxang,fxold,vnyq,ngts
      IF(FXANG.LT.FXOLD)THEN

         IF(NUMWRD.GT.MAX16F)THEN
            print *,'Maximum file size reached: numwrd = ',numwrd
            IF(IWFOR.EQ.0.AND.IOPEN.EQ.1)THEN
               CALL WRCOSREC(IEOD,ID,NBYTES)
               print *,'Write COS-blocked EOD and close unit ',
     +              iufun(nun)
               IOPEN=0
            ELSE
               ENDFILE(IUFUN(NUN))
               ENDFILE(IUFUN(NUN))
               print *,'Write Fortran-blocked EOD on unit ',
     +              iufun(nun)
            END IF
            NVOL=1
            IREC=1
            NSCAN=1
            NRAY=1
            NUMWRD=0
            NUN=NUN+1
            IF(NUN.GT.NUMUN)THEN
               IWRUF=0
               WRITE(6,5)
 5             FORMAT(/,
     +              '******THE NUMBER OF UNITS EXCEEDS THE NUMBER',/,
     +              'REQUESTED- OUTPUT OF UF DATA WILL STOP AT THIS',/,
     +              'POINT.  OTHER PROGRAM PROCESSES WILL CONTINUE',/,
     +              'NORMALLY.**********',/)
               RETURN
            END IF
            IF(IWFOR.EQ.0.AND.IOPEN.EQ.0)THEN
               CALL INIT_COSW(IUFUN(NUN))
               print *,'Open new unit = ',iufun(nun),
     +              ' for writing COS-blocked records'
               IOPEN=1
            END IF
         END IF
         
C     Write COS-blocked or Fortran-blocked end-of-file at the 
C     end-of-radar-volume-scan.  Allows for multiple volume scans 
C     within single disk file.  If IOPEN=1, that unit will be closed
C     and a new unit will be opened.  See above call init_cosw.
C     
         print *,'End of volume scan    =',nvol,irec,nscan,nray
         NSCAN=1
         NRAY=1
         IF(IWFOR.EQ.0)THEN
            IF(NUMUN.GT.1)THEN
               CALL WRCOSREC(IEOD,ID,NBYTES)
               print *,'Wrote COS-blocked EOD and closed unit ',
     +              iufun(nun)
               IOPEN=0
               NUN=NUN+1
               CALL INIT_COSW(IUFUN(NUN))
               print *,'Open new unit ',iufun(nun),
     +              ' for writing COS-blocked records'
               IOPEN=1
               NVOL=1
               IREC=1
            ELSE
               CALL WRCOSREC(IEOF,ID,NBYTES)
               print *,'Wrote COS-blocked EOF on unit ',
     +              iufun(nun)
               NVOL=NVOL+1
               IREC=IREC+1
               IOPEN=1
            END IF
         ELSE
            IF(NUMUN.GT.1)THEN
               ENDFILE(IUFUN(NUN))
               ENDFILE(IUFUN(NUN))
               print *,'Wrote Fortran EOD and closed unit ',
     +              iufun(nun)
               IOPEN=0
               NUN=NUN+1
               print *,'Open new unit ',iufun(nun),
     +              ' for writing Fortran-blocked records'
               IOPEN=1
               NVOL=1
               IREC=1
            ELSE
               ENDFILE(IUFUN(NUN))
               print *,'Wrote Fortran EOF on unit ',iufun(nun)
               NVOL=NVOL+1
               IREC=IREC+1
               IOPEN=1
            END IF
         END IF
         print *,'Begin new volume scan =',nvol,irec,nscan,nray
      END IF

C     Calculate the number of physical records (NREC) required to hold
C     a logical record (a beam of data).
C     
      CALL REC(MAX16B,NUMUF,NGTS,NREC,NFLD1,I1,I2)
      
C     Fill words in the mandatory (1-45) and data header (46 onward)
C     blocks that remain the same for all beams.
C        DEC - (1) Writing UF on DEC, (0) Writing UF on non-DEC
C      IWFOR - (0) COS-blocked, (1) Fortran-blocked
C     TODAY  - Generation date for UF output.  Calls C routine cdate.
C              Convert yymmdd of TODAY = 19yymmddhhmmss to integers.
C     Radar, network, and UF generator name
C        IDATA(11-14)='RADAR   '
C        IDATA(15-18)='NETWORK '
C        IDATA(41-44)='PPI_MMM '
C
      CALL CDATE(TODAY)
      READ(TODAY(3:4),9)IDATA(38)
      READ(TODAY(5:6),9)IDATA(39)
      READ(TODAY(7:8),9)IDATA(40)
 9    FORMAT(I2)
      
      CFTGEN='PPI_MMM '
      DO J=1,4
         RADCH(J)='        '
         NETCH(J)='        '
         GENCH(J)='        '
      END DO

      IF(DEC.EQ.1.AND.IWFOR.EQ.0)THEN

C        Writing COS-blocked on DEC - Swap bytes 
C        Swapped again in cb_write before writing the UF record
C
         CTEMP='      FU'
         DTEMP='      TL'
         DO J=1,4
            J1=2*J-1
            J2=2*J
            RADCH(J)(8:8)=IRATYP(J1:J1)
            RADCH(J)(7:7)=IRATYP(J2:J2)
            NETCH(J)(8:8)=NETWORK(J1:J1)
            NETCH(J)(7:7)=NETWORK(J2:J2)
            GENCH(J)(8:8)=CFTGEN(J1:J1)
            GENCH(J)(7:7)=CFTGEN(J2:J2)
         END DO

      ELSE

         CTEMP='      UF'
         DTEMP='      LT'
         DO J=1,4
            J1=2*J-1
            RADCH(J)(7:8)=IRATYP(J1:J1+1)
            NETCH(J)(7:8)=NETWORK(J1:J1+1)
            GENCH(J)(7:8)=CFTGEN(J1:J1+1)
         END DO

      END IF
      IF(WORDSZ.EQ.64)THEN
         READ(CTEMP,10)IDATA(1)
         READ(DTEMP,10)IDATA(32)
         DO J=1,4
            READ(RADCH(J),10)IDATA(J+10)
            READ(NETCH(J),10)IDATA(J+14)
            READ(GENCH(J),10)IDATA(J+40)
         END DO
      ELSE
         IF(DEC.NE.1)THEN
            READ(CTEMP,15)IDATA(1)
            READ(DTEMP,15)IDATA(32)
            DO J=1,4
               READ(RADCH(J),15)IDATA(J+10)
               READ(NETCH(J),15)IDATA(J+14)
               READ(GENCH(J),15)IDATA(J+40)
            END DO
         ELSE
            READ(CTEMP,16)IDATA(1)
            READ(DTEMP,16)IDATA(32)
            DO J=1,4
               READ(RADCH(J),16)IDATA(J+10)
               READ(NETCH(J),16)IDATA(J+14)
               READ(GENCH(J),16)IDATA(J+40)
            END DO
         END IF
      END IF
 10   FORMAT(A8)
 15   FORMAT(4X,A4)
 16   FORMAT(6X,A2)

C     Starting addresses for optional, local-use, and field header
C     blocks.  Optional and local-use header blocks not used.
      
      IDATA(3)=46
      IDATA(4)=46
      IDATA(5)=46

      IDATA(7)=NVOL
      IDATA(10)=NSCAN

      IDATA(26)=IYR
      IDATA(27)=IMON
      IDATA(28)=IDAY
      IDATA(35)=ITPOLD
      IDATA(36)=FXANG*64.
      IDATA(37)=MDFLG
      IDATA(45)=MDFLG

      IDATA(46)=NUMUF
      IDATA(47)=NREC
      
C     Loop over the number of beams in this scan filling up the
C     data buffer with some housekeeping and data information.
C     Pack the data into WORDSZ-bit words and write out to unit IUFUN.

      IF(GRDTYP.EQ.'SWATH')THEN
         ISW=2
      ELSE
         ISW=1
      END IF

      DO 100 IAZ=1,NANG(ISW)
         
         IDATA(6)=IREC
         IDATA(8)=NRAY
         IF(AZA(IAZ,ISW).LT.0.0)THEN
            IDATA(33)=64*(AZA(IAZ,ISW)+360.0)
         ELSE
            IDATA(33)=64*AZA(IAZ,ISW)
         END IF
         IDATA(34)=64*ELA(IAZ,ISW)
         
C     If this is an RHI scan, restore AZIMUTH and ELEVATION information.
C     These angles were reversed during input of the current radar file. 
         
         IF(ITPOLD.EQ.3)THEN
            ITMP=IDATA(33)
            IDATA(33)=IDATA(34)
            IDATA(34)=ITMP
            IF(AZA(IAZ,ISW).GT.0.5)THEN
               RNGMAX=ZTOP/TAN(AZA(IAZ,ISW)*0.0174533)
               NGTS=(RNGMAX-R0)/DROLD+1.001
               NGTS=MIN0(NGTS,NGTSINP)
            END IF
         END IF
         IDATA(29)=ITM(IAZ,ISW)/10000
         IDATA(30)=(ITM(IAZ,ISW)-IDATA(29)*10000)/100
         IDATA(31)=ITM(IAZ,ISW)-IDATA(29)*10000-IDATA(30)*100
         ITIMSEC=IDATA(31)+60*IDATA(30)+3600*IDATA(29)

C        Make sure time is monotonically increasing for Sprint
C        Apparently Sprint doesn't care (ljm - 5/19/97).  ITMDIF
C        output is only for information; it isn't fatal error.
C
c         IF(IREC.EQ.1)THEN
c            ITIMOLD=ITIMSEC
c            write(*,*)'Start: itimsec,itimold=',itimsec,itimold
c         END IF
c         IF(ITIMSEC.LT.ITIMOLD)THEN
c            IHOUR=ITIMOLD/3600.0
c            IMIN =(ITIMOLD-3600.0*IHOUR)/60.0
c            ISEC =ITIMOLD-3600.0*IHOUR-60.0*IMIN
c            IDATA(29)=IHOUR
c            IDATA(30)=IMIN
c            IDATA(31)=ISEC
c         ELSE
c            ITIMOLD=ITIMSEC
c         END IF
c         write(*,*)'       itimsec,itimold=',itimsec,itimold
         
C     Loop over the number of physical records to write this beam.
C        IPT        - For each field, starting address of data
C                     header block [3 + 2*NFLD(IR) words]
C        IFHD       - For each field, starting address of field
C                     header block consisting of LEN_FHB words
C        IDHD       - For each field, starting address of data
         
         DO 80 IR=1,NREC
            IDATA(9)=IR
            IPT=49
            
C     Determine the number of 16-bit words (rounded up to next whole 
C     number of computer words) in each record, number of bytes, and 
C     number of computer words.  Keep track of number of 16-bit words
C     written so far.  
C           IDATA(2)=LEN_MHB + LEN_DHB + (LEN_FHB+NGTS)*NFLDS
C            
            IDATA(2)=45 + 3+2*NFLD1(IR) + (LEN_FHB+NGTS)*NFLD1(IR)
            IDATA(2)=N16B*INT(FI_N16B*IDATA(2)+0.99)
            NBYTES=IDATA(2)*2
            NCW=(FI_N16B*IDATA(2)-0.001)+1
c            write(*,*)'iaz,ir,cw,16,by=',iaz,ir,ncw,idata(2),nbytes
            
            NUMWRD=NUMWRD+IDATA(2)
            IDATA(48)=NFLD1(IR)
            IFHD=49+2*NFLD1(IR)
            IDHD=IFHD+LEN_FHB
            
C     LOOP OVER THE NUMBER OF FIELDS IN THE PHYSICAL RECORD
            
            DO 60 I=I1(IR),I2(IR)
               I3=IFIND(NAMOUT(I),NAMFLD,NFLDS)
               IF(I3.LE.0)GO TO 60
               
C     LOAD IN THE FIELD HEADER BLOCK INFORMATION
               
               IF(WORDSZ.EQ.64)THEN
                  READ(NMOUTUF(I),10)IDATA(IPT)
               ELSE
                  IF(DEC.NE.1)THEN
                     READ(NMOUTUF(I),15)IDATA(IPT)
                  ELSE
                     READ(NMOUTUF(I),16)IDATA(IPT)
                  END IF
               END IF
               
               IDATA(IPT+1)=IFHD
               IDATA(IFHD)=IDHD
               IDATA(IFHD+1)=INT(SCALE)
               IDATA(IFHD+2)=R0K
               IDATA(IFHD+3)=R0M
               IDATA(IFHD+4)=(DROLD+0.00001)*1000.
               IDATA(IFHD+5)=NGTS
               IDATA(IFHD+6)=(DROLD+0.00001)*1000.
               IDATA(IFHD+7)=IBW
               IDATA(IFHD+8)=IBW
               IDATA(IFHD+9)=MDFLG
               IDATA(IFHD+10)=0
               IDATA(IFHD+11)=IWL
               IDATA(IFHD+12)=MDFLG
               IDATA(IFHD+13)=MDFLG
               IDATA(IFHD+14)=MDFLG
               IDATA(IFHD+15)=INT(SCALE)
               IF(DEC.EQ.1.AND.IWFOR.EQ.0)THEN
                  EDITCH='EN      '
               ELSE
                  EDITCH='NE      '
               END IF
               READ(EDITCH,10)IDATA(IFHD+16)
               IDATA(IFHD+17)=IPRT
               IDATA(IFHD+18)=16
               
C     IF A RADIAL VELOCITY FIELD IS BEING WRITTEN, ADD NYQUIST.
               
               IF(NMOUTUF(I).EQ.'VE      '.OR.
     +            NMOUTUF(I).EQ.'VR      '.OR.
     +            NMOUTUF(I).EQ.'VF      '.OR.
     +            NMOUTUF(I).EQ.'      VE'.OR.
     +            NMOUTUF(I).EQ.'      VR'.OR.
     +            NMOUTUF(I).EQ.'      VF'.OR.
     +            NMOUTUF(I).EQ.'      EV'.OR.
     +            NMOUTUF(I).EQ.'      RV'.OR.
     +            NMOUTUF(I).EQ.'      FV')THEN
                  IDATA(IFHD+19)=NINT(VNYQ*SCALE)
               ELSE
                  IDATA(IFHD+19)=MDFLG
               END IF
               IF(LEN_FHB.GT.20)THEN
                  DO N=20,LEN_FHB-1
                     IDATA(IFHD+N)=MDFLG
                  END DO
               END IF
c               write(*,*)'i,pt,fh,dh,w20=',i,ipt,ifhd,idhd,
c     +              idata(ifhd+19)
               
C     LOOP OVER THE NUMBER OF RANGE GATES FILLING UP THE
C     DATA BUFFER WITH THE SCALED DATA.
               
               DO 40 K=1,NGTS
                  IDATA(IDHD+K-1)=NINT(DAT(K,IAZ,I3)*SCALE)
                  IF(DAT(K,IAZ,I3).EQ.BDVAL)IDATA(IDHD+K-1)=MDFLG
 40            CONTINUE

               IPT=IPT+2
               IFHD=IFHD+LEN_FHB+NGTS
               IDHD=IDHD+LEN_FHB+NGTS
 60         CONTINUE
            
C     Pack the data in 16-bit chunks into WORDSZ-bit computer words.
C     Then write the current record according to IWFOR:
C     (1) Fortran-blocked record of NCW computer words (size WORDSZ)
C     (0) COS-blocked record of NBYTES bytes.
C     
            CALL SBYTES(ID(1),IDATA(1),0,16,0,IDATA(2))
            IF(IWFOR.EQ.1)THEN
               WRITE(IUFUN(NUN))(ID(I),I=1,NCW)
            ELSE
               CALL WRCOSREC(IWR,ID,NBYTES)         
            END IF
            IREC=IREC+1
            NRAY=NRAY+1
            
CANNE BUFFER OUT(IUFUN(NUN),1) (ID(1),ID(NCW))
 80      CONTINUE
 100  CONTINUE
      NSCAN=NSCAN+1
      FXOLD=FXANG

      IF(IWFOR.EQ.1)THEN
         write(6,111)nang(isw),nrec,ncw*n8b
 111     format(1x,'Wrote ',I4,' beams of Fortran-blocked records,',
     +             ' each beam contained',i3,' record(s),',
     +             ' and each record was',I6,' bytes long.')
      ELSE
         write(6,113)nang(isw),nrec,nbytes
 113     format(1x,'Wrote ',I4,' beams of COS-blocked records,',
     +             ' each beam contained',i3,' record(s),',
     +             ' and each record was',I6,' bytes long.')
      END IF

      RETURN
      END
