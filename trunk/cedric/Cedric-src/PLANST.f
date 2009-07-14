      SUBROUTINE PLANST(IUNIT,NLEV,ILHD,NST,IOTYPE)
C
C     READS AND UNPACKS THE LEVEL HEADER
C     Note:  For constant elevation surfaces and coplanes, the level
C            header contains information about the current level coordinate
C            and Nyquist velocity.  NEXRADs have different Nyquist velocities 
C            at different elevations.
C
C           ILHD(1) - "LE
C           ILHD(2) - "VE"
C           ILHD(3) - "L "
C           ILHD(4) - Coordinate of current level * 1000 (meters or degrees)
C           ILHD(5) - Index of current level
C           ILHD(6) - Number of fields
C           ILHD(7) - Number of grid points per plane (NX*NY)
C           ILHD(8) - Number of records per field
C           ILHD(9) - Number of records per plane
C           ILHD(10)- Nyquist velocity * 100
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (LBF=10)
      DIMENSION ILHD(LBF),ITEM(LBF)
      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV),VNYQ_VOL
      DATA MODE,NTYPE,IBIT,NBITS,NSKIP/1,2,0,16,0/

C     LJM - May need to change ZSCALE to 100.0 for some SPRINT files
C           that used GRIDPPI and elevation angle > 32.768 deg.
C
      DATA ZSCALE/1000.0/

      ISIXT(NUM)=(NUM-1)/4 + 1.01
      LEN=ISIXT(LBF)
      IF (IOTYPE.EQ.0) THEN
         CALL RDTAPE(IUNIT,MODE,NTYPE,ILHD,LEN)
         CALL IOWAIT(IUNIT,NST,NWDS)
         IF (NST.NE.0) GOTO 40
      ELSE
         IVAL = LBF
         CALL CREAD(IUNIT,ILHD,IVAL)
      END IF
      CALL GBYTES(ILHD,ITEM,IBIT,NBITS,NSKIP,LBF)

      CALL ASDPMD(ITEM(1),ILHD(1),3)
      CALL ALTER(ITEM(4),ILHD(4),7)

C     If the level header Nyquist velocity [ILHD(10)] is zero, 
C     fill it with Nyquist velocity from the volume header.
C
      VNYQ_LEV=ILHD(10)/100.0
      IF(VNYQ_LEV .EQ. 0.0 .AND. VNYQ_VOL .NE. 0.0)THEN
         ILHD(10)=100.0*VNYQ_VOL
      ENDIF

      ZLEV=ILHD(4)/ZSCALE
      VALLEV(NLEV)=ILHD(4)/ZSCALE

      VALNYQ(NLEV)=ILHD(10)/100.0
      write(*,1770)nlev,zlev,vnyq_vol,valnyq(nlev)
 1770 format('    PLANST: nlev,zlev,vnyq_vol,vnyq_lev=',i5,3f10.3)

      IF (NLEV.NE.ILHD(5)) GOTO 50
      RETURN
 40   CONTINUE
      CALL TAPMES(IUNIT,NST)
      RETURN
 50   CONTINUE
      NST=4
      GOTO 40
      END
