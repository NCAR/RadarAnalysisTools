      SUBROUTINE CARTAP(IPR,IREORD,ICOB,NDX,NDY,NDZ,ITOB,IDB,NST,
     X                  DASANG,VNYQ,VNYQUIST,MAXEL)
C
C        PRODUCES THE CARTESIAN OUTPUT FILES ON TAPE, FIXED AXIS OF
C                 EACH OUTPUT VOLUME IS DETERMINED BY IREORD ARRAY.
C
      PARAMETER (NMD=510,NML=10)
c      DIMENSION ICOB(1),ITOB(1),IDB(1)
      DIMENSION ICOB(1),ITOB(1),IDB(510)
      DIMENSION VNYQUIST(MAXEL)
      CHARACTER*1 IREORD(3), IYES
      COMMON /MUDBLK/ IDMUD(NMD),IDMLV(NML)

      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI

      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /TRANS/ CSP(3,3),NXYZ(3),XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      EQUIVALENCE (IDMUD(164),I1),(IDMUD(169),I2),(IDMUD(174),I3),
     X   (IDMUD(301),NTOB),(IDMUD(106),NLEVS),(IDMUD(175),NFLOUT),
     X   (IDMUD( 65),IBLK)
      COMMON /BYTORD/ MBYTE,SWAPPING
      CHARACTER*8 CTEMP1
      CHARACTER*1 LAX(3)
      DATA LAX/'X','Y','Z'/
      DATA IYES/'Y'/
      DATA FTMAX/2200./


      CALL HEDSET(LTAP,ISCI,SF)
      K=4
      IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         CTEMP1='CO'
         READ(CTEMP1,23)IDMUD(151)
 23      FORMAT(A2)
      ELSE 
         CTEMP1='PP'
         READ(CTEMP1,23)IDMUD(151)
      END IF
C     BASELINE ANGLE
      IDMUD(158)=DASANG
         

   10 CONTINUE
      K=K-1
      IF(K.LE.0) GO TO 90
C
C        CHECK IF REORDERING IS TO BE DONE ALONG THIS AXIS
C
      IF(IREORD(K).NE.IYES) GO TO 10
      LN=2**(K-1)
      LAD=LN+K-1
      DO 15 I=164,174,5
         IDMUD(I)=MOD(LAD,3)+1
         LAD=LAD+LN
   15 CONTINUE
      IDMUD(106)=NXYZ(I3)
      IDMUD(301)=NXYZ(I1)*NXYZ(I2)
      LSKP=LSKP+1
      IDMUD(111)=LSKP
      IDMUD(96)=(IDMUD(301)-1)/IBLK+1
      IDMUD(97)=IDMUD(96)*IDMUD(175)
      IDMUD(98)=IDMUD(97)*IDMUD(106)
      IDMUD(99)=IDMUD(98)+IDMUD(106)+1
C
C     Assemble Volume Name: 
C        Fixed axis + Starting date (MMDD) + Starting time (HHMM)
C        Drop Fixed axis to accomodate MM > 9.
C
      IVOL_NAM=100*(100*(100*IDMUD(117)+IDMUD(118))+IDMUD(119))
     X     +IDMUD(120)

c      WRITE (CTEMP1,101)LAX(I3),IVOL_NAM
c  101 FORMAT(A1,I7)
      WRITE (CTEMP1,101)IVOL_NAM
  101 FORMAT(I8.8)
      READ (CTEMP1,102)(IDMUD(I),I=101,104)
  102 FORMAT(4A2)

      FEETS = FEETCL(NFLOUT,NLEVS,NTOB,IBLK) + FEETS
c-----No longer applicable since output is to disk
c      IF(FEETS.GT.FTMAX) THEN
c         PRINT 103, FTMAX
c  103    FORMAT(/' +++  WARNING--  MAXIMUM OUTPUT TAPE FOOTAGE=',
c     X           F6.0,' FEET REACHED AT 6250 BPI  +++')
c      END IF
c-----No longer applicable since output is to disk
C
C        MOVE ALL ALPHA INFO TO RIGHT-MOST 16 BITS OF HEADER (BIG END ONLY)
C
      
      IF (MBYTE.EQ.0) THEN
         CALL HEDSHF(IDMUD,IDB,NMD)
      ELSE
         DO I=1,NMD
            IDB(I)=IDMUD(I)
         END DO
      END IF
C
C        TRANSFER INTERPOLATED VOLUME TO OUTPUT TAPE
C

c-----debug (ljm)
      write(6,1669)
 1669 format(/,3x,'Cartap: Scaled header values for min-max-del-nmb')
      write(6,1770)idb(68),idb(69)
 1770 format(3x,'          Sclfac:   id(68-69)=',2I10)
      write(6,1771)idb(160),idb(161),idb(163),idb(162)
 1771 format(3x,'               X: id(160-163)=',4i10)
      write(6,1772)idb(165),idb(166),idb(168),idb(167)
 1772 format(3x,'               Y: id(165-168)=',4i10)
      write(6,1773)idb(170),idb(171),idb(173),idb(172)
 1773 format(3x,'               Z: id(170-173)=',4i10)
c-----debug (ljm)

      CALL OUTPCK(ICOB,NDX,NDY,NDZ,ITOB,NTOB,NFLOUT,IDB,CSP,I3,SF,
     X     VNYQ,VNYQUIST)
C
C        SEND SUMMARIES TO THE PRINTER
C
      CALL IMHSUM(IPR,' ',IYES)
c-----No longer applicable since output is to disk
c      PRINT 104, LTAP,LUN,LSKP,LSKP,FEETS
c  104 FORMAT(/10X,'CURRENT STATUS OF OUTPUT TAPE ',A6,
c     X   ' ON UNIT ',I3/20X,'VOLUME COUNT: ',I6/20X,
c     X   'PHYSICAL FILES: ',I4/20X,'FOOTAGE: ',F11.2/)
c-----No longer applicable since output is to disk
      PRINT 104, LTAP,LUN,LSKP,LSKP
  104 FORMAT(/10X,'CURRENT STATUS OF OUTPUT TAPE ',A6,
     X   ' ON UNIT ',I3/20X,'VOLUME COUNT: ',I6/20X,
     X   'PHYSICAL FILES: ',I4/)
      GO TO 10
   90 CONTINUE
C
C        FINISHED GENERATING ALL OUTPUT VOLUMES
C
      NST=0
      RETURN
      END
