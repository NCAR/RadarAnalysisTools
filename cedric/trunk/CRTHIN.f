
      SUBROUTINE CRTHIN(IUN,IBUF,NMAX,NST,ITEM,IREW)
C     
C     READS IN 16 BIT PACKED CARTESIAN HEADER
C     -UNPACKS       AND
C     -CHECKS FOR NEGATIVE VALUES
C     

      INCLUDE 'CEDRIC.INC'      
      COMMON /IOTYPE/ ICDF
      CHARACTER*7 CTEMP
      DIMENSION IBUF(NID),ITEM(NID)
      DATA MODE,NTYPE,IBIT,NBITS,NSKIP/1,2,0,16,0/
      INTEGER MBYTE,FBYTE
      ISIXT(NUM)=(NUM-1)/4 + 1.01
      LEN=ISIXT(NMAX)
      
      NST=0
      MBYTE=0
      FBYTE=0
      INUNIT=IUN


      IF (ICDF.EQ.0) THEN
C
C     READ IN ID HEADER FROM CRAY COS BLOCKED FILE
C
         CALL RDTAPE(IUN,MODE,NTYPE,IBUF,LEN)
         CALL IOWAIT(IUN,NST,NWDS)
         IF (NST.NE.0) RETURN
         
C     UNPACK AND CONVERT HEADER
         
         CALL GBYTES(IBUF,ITEM,IBIT,NBITS,NSKIP,NMAX)


      ELSE IF (ICDF.EQ.1) THEN
C     
C     READ IN ID HEADER FROM PURE BINARY FILE
C     
         IF(INUNIT .LT. 100) THEN
           WRITE(CTEMP,27)INUNIT
 27        FORMAT(I2)
         ELSE
           WRITE(CTEMP,28)INUNIT
 28        FORMAT(I3)  
         END IF   
        
         NUM3 = -1
         READ(CTEMP,35)NUM1
 35      FORMAT(I1)
         READ(CTEMP,37)NUM2
 37      FORMAT(1X,I1)
         IF(INUNIT .GT. 99) THEN
            READ(CTEMP,38)NUM3
 38         FORMAT(2X,I1)  
         END IF  
C     
C     GET ID HEADER FOR FILE
C     
         print *,'CRTHIN: read from unit = ',inunit,num1,num2,num3
         print *,'CRTHIN: reading and unpacking 1540-byte file header'
         print *,'        followed by 510 16-bit volume header'
         CALL CINHEAD(INUNIT,NUM1,NUM2,NUM3,IBUF,IREW,MBYTE,FBYTE,NST)
         print *,'        read from unit = ',inunit,num1,num2,num3
         IF (NST.NE.0) RETURN
         CALL GBYTES(IBUF,ITEM,IBIT,NBITS,NSKIP,NMAX)
      
      END IF

      IF (MBYTE.EQ.0 .AND. FBYTE.EQ.0) THEN
C
C     BIG ENDIAN -> BIG ENDIAN (E.G., SUN->SUN)
C
         CALL ASDPMD(ITEM(1),IBUF(1),20)
         CALL ALTER(ITEM(21),IBUF(21),22)
         CALL ASDPMD(ITEM(43),IBUF(43),16)
         CALL ALTER(ITEM(60),IBUF(60),2)
         CALL ASDPMD(ITEM(62),IBUF(62),1)
         CALL ALTER(ITEM(63),IBUF(63),3)
         CALL ASDPMD(ITEM(66),IBUF(66),1)
         CALL ALTER(ITEM(67),IBUF(67),3)
         CALL ASDPMD(ITEM(71),IBUF(71),24)
         CALL ALTER(ITEM(95),IBUF(95),6)
         CALL ASDPMD(ITEM(101),IBUF(101),4)
         CALL ALTER(ITEM(106),IBUF(106),45)
         CALL ASDPMD(ITEM(151),IBUF(151),1)
         CALL ALTER(ITEM(152),IBUF(152),11)
         CALL ALTER(ITEM(165),IBUF(165),2)
         CALL ALTER(ITEM(170),IBUF(170),2)
         IBUF(163)=ITEM(163)
         IBUF(164)=ITEM(164)
         IBUF(167)=ITEM(167)
         IBUF(168)=ITEM(168)
         IBUF(169)=ITEM(169)
         IBUF(172)=ITEM(172)
         CALL ALTER(ITEM(173),IBUF(173),1)
C         IBUF(173)=ITEM(173)
         IBUF(174)=ITEM(174)
         IBUF(175)=ITEM(175)
         DO 200 I=1,25
            IN=176+(I-1)*5
            CALL ASDPMD(ITEM(IN),IBUF(IN),4)
            CALL ALTER(ITEM(IN+4),IBUF(IN+4),1)
 200     CONTINUE
         IF(ITEM(301).LE.0)
     X        ITEM(301)=ITEM(301)+MAXPLN
         IBUF(301)=ITEM(301)
         CALL ALTER(ITEM(302),IBUF(302),4)
         DO 40 I=1,15
            IN=306+(I-1)*6
            CALL ASDPMD(ITEM(IN),IBUF(IN),3)
            CALL ALTER(ITEM(IN+3),IBUF(IN+3),3)
 40      CONTINUE
         CALL ALTER(ITEM(400),IBUF(400),26)
         IBUF(451)=0
         IBUF(452)=0
         IBUF(453)=0
      ELSE IF (MBYTE.EQ.1 .AND. FBYTE.EQ.0) THEN
C
C     BIG ENDIAN->LITTLE ENDIAN (E.G., SUN->DEC)
C
         CALL SWAPCHAR(ITEM(1),IBUF(1),20)
         CALL ALTER(ITEM(21),IBUF(21),22)
         CALL SWAPCHAR(ITEM(43),IBUF(43),16)
         CALL ALTER(ITEM(60),IBUF(60),2)
         CALL SWAPCHAR(ITEM(62),IBUF(62),1)
         CALL ALTER(ITEM(63),IBUF(63),3)
         CALL SWAPCHAR(ITEM(66),IBUF(66),1)
         CALL ALTER(ITEM(67),IBUF(67),3)
         CALL SWAPCHAR(ITEM(71),IBUF(71),24)
         CALL ALTER(ITEM(95),IBUF(95),6)
         CALL SWAPCHAR(ITEM(101),IBUF(101),4)
         CALL ALTER(ITEM(106),IBUF(106),45)
         CALL SWAPCHAR(ITEM(151),IBUF(151),1)
         CALL ALTER(ITEM(152),IBUF(152),11)
         CALL ALTER(ITEM(165),IBUF(165),2)
         CALL ALTER(ITEM(170),IBUF(170),2)
         IBUF(163)=ITEM(163)
         IBUF(164)=ITEM(164)
         IBUF(167)=ITEM(167)
         IBUF(168)=ITEM(168)
         IBUF(169)=ITEM(169)
         IBUF(172)=ITEM(172)
         CALL ALTER(ITEM(173),IBUF(173),1)
C         IBUF(173)=ITEM(173)
         IBUF(174)=ITEM(174)
         IBUF(175)=ITEM(175)
         DO 20 I=1,25
            IN=176+(I-1)*5
            CALL SWAPCHAR(ITEM(IN),IBUF(IN),4)
            CALL ALTER(ITEM(IN+4),IBUF(IN+4),1)
 20      CONTINUE
         IF(ITEM(301).LE.0)
     X        ITEM(301)=ITEM(301)+MAXPLN
         IBUF(301)=ITEM(301)
         CALL ALTER(ITEM(302),IBUF(302),4)
         DO 400 I=1,15
            IN=306+(I-1)*6
            CALL SWAPCHAR(ITEM(IN),IBUF(IN),3)
            CALL ALTER(ITEM(IN+3),IBUF(IN+3),3)
 400     CONTINUE
         CALL ALTER(ITEM(400),IBUF(400),26)
         IBUF(451)=0
         IBUF(452)=0
         IBUF(453)=0
      ELSE IF (MBYTE.EQ.1 .AND. FBYTE.EQ.1) THEN
C
C     LITTLE ENDIAN -> LITTLE ENDIAN (E.G., DEC->DEC)
         DO 500 I=1,510
            IBUF(I)=ITEM(I)
 500     CONTINUE
         CALL ALTER(ITEM(21),IBUF(21),22)
         CALL ALTER(ITEM(60),IBUF(60),2)
         CALL ALTER(ITEM(63),IBUF(63),3)
         CALL ALTER(ITEM(67),IBUF(67),3)
         CALL ALTER(ITEM(95),IBUF(95),6)
         CALL ALTER(ITEM(106),IBUF(106),45)
         CALL ALTER(ITEM(152),IBUF(152),11)
         CALL ALTER(ITEM(165),IBUF(165),2)
         CALL ALTER(ITEM(170),IBUF(170),2)
         IBUF(163)=ITEM(163)
         IBUF(164)=ITEM(164)
         IBUF(167)=ITEM(167)
         IBUF(168)=ITEM(168)
         IBUF(169)=ITEM(169)
         IBUF(172)=ITEM(172)
         CALL ALTER(ITEM(173),IBUF(173),1)
C         IBUF(173)=ITEM(173)
         IBUF(174)=ITEM(174)
         IBUF(175)=ITEM(175)
         DO 220 I=1,25
            IN=176+(I-1)*5
            CALL ALTER(ITEM(IN+4),IBUF(IN+4),1)
 220     CONTINUE
         IF(ITEM(301).LE.0)
     X        ITEM(301)=ITEM(301)+MAXPLN
         IBUF(301)=ITEM(301)
         CALL ALTER(ITEM(302),IBUF(302),4)
         DO 44 I=1,15
            IN=306+(I-1)*6
            CALL ALTER(ITEM(IN+3),IBUF(IN+3),3)
 44      CONTINUE
         CALL ALTER(ITEM(400),IBUF(400),26)
         IBUF(451)=0
         IBUF(452)=0
         IBUF(453)=0

      ELSE IF (MBYTE.EQ.0 .AND. FBYTE.EQ.1) THEN
C
C     LITTLE ENDIAN -> BIG ENDIAN (DEC->SUN)
C
         CALL SWAPCHAR(ITEM(1),ITEM(1),20)
         CALL ASDPMD(ITEM(1),IBUF(1),20)
         CALL ALTER(ITEM(21),IBUF(21),22)
         CALL SWAPCHAR(ITEM(43),ITEM(43),16)
         CALL ASDPMD(ITEM(43),IBUF(43),16)
         CALL ALTER(ITEM(60),IBUF(60),2)
         CALL SWAPCHAR(ITEM(62),ITEM(62),1)
         CALL ASDPMD(ITEM(62),IBUF(62),1)
         CALL ALTER(ITEM(63),IBUF(63),3)
         CALL SWAPCHAR(ITEM(66),ITEM(66),1)
         CALL ASDPMD(ITEM(66),IBUF(66),1)
         CALL ALTER(ITEM(67),IBUF(67),3)
         CALL SWAPCHAR(ITEM(71),ITEM(71),24)
         CALL ASDPMD(ITEM(71),IBUF(71),24)
         CALL ALTER(ITEM(95),IBUF(95),6)
         CALL SWAPCHAR(ITEM(101),ITEM(101),4)
         CALL ASDPMD(ITEM(101),IBUF(101),4)
         CALL ALTER(ITEM(106),IBUF(106),45)
         CALL SWAPCHAR(ITEM(151),ITEM(151),1)
         CALL ASDPMD(ITEM(151),IBUF(151),1)
         CALL ALTER(ITEM(152),IBUF(152),11)
         CALL ALTER(ITEM(165),IBUF(165),2)
         CALL ALTER(ITEM(170),IBUF(170),2)
         IBUF(163)=ITEM(163)
         IBUF(164)=ITEM(164)
         IBUF(167)=ITEM(167)
         IBUF(168)=ITEM(168)
         IBUF(169)=ITEM(169)
         IBUF(172)=ITEM(172)
         CALL ALTER(ITEM(173),IBUF(173),1)
C         IBUF(173)=ITEM(173)
         IBUF(174)=ITEM(174)
         IBUF(175)=ITEM(175)
         DO 57 I=1,25
            IN=176+(I-1)*5
            CALL SWAPCHAR(ITEM(IN),ITEM(IN),4)
            CALL ASDPMD(ITEM(IN),IBUF(IN),4)
            CALL ALTER(ITEM(IN+4),IBUF(IN+4),1)
 57      CONTINUE
         IF(ITEM(301).LE.0)
     X        ITEM(301)=ITEM(301)+MAXPLN
         IBUF(301)=ITEM(301)
         CALL ALTER(ITEM(302),IBUF(302),4)
         DO 47 I=1,15
            IN=306+(I-1)*6
            CALL SWAPCHAR(ITEM(IN),ITEM(IN),3)
            CALL ASDPMD(ITEM(IN),IBUF(IN),3)
            CALL ALTER(ITEM(IN+3),IBUF(IN+3),3)
 47      CONTINUE
         CALL ALTER(ITEM(400),IBUF(400),26)
         IBUF(451)=0
         IBUF(452)=0
         IBUF(453)=0
      END IF

      print *,"CRTHIN: Vol.header length ( 61) = ",ibuf(61)
      print *,"          # of x grid pts (162) = ",ibuf(162)
      print *,"          # of y grid pts (167) = ",ibuf(167)
      print *,"          # of x by y pts (301) = ",ibuf(301)
      print *,"          # of fields     (175) = ",ibuf(175)
      print *,"          # of z levels   (172) = ",ibuf(172)
      print *,"        Missing data flag ( 67) = ",ibuf(67)
      print *,"        GeneralScaling *  ( 68) = ",ibuf(68)
      print *,"          AngleScaling *  ( 69) = ",ibuf(69)
      print *," "
      IF(IBUF(61).NE.510)THEN
         PRINT *,'+++   WARNING - WARNING - WARNING   +++'
         PRINT *,'+++ APPARENT ERROR IN CEDRIC HEADER +++'
         PRINT *,'+++     IBUF(61) should be 510      +++'
         PRINT *,'+++  DISKFILE REWIND MAY BE NEEDED  +++'
         PRINT *,'+++ OR VOLUME NOT FOUND (PAST EOF)  +++'
         PRINT *,'  '
c         STOP
      END IF
      RETURN
      END



