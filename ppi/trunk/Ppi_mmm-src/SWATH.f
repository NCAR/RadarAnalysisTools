c
c----------------------------------------------------------------------X
c
      SUBROUTINE SWATH(INDAT,IFMT,NAMFLD,NFLDS,IFLD,NOUT)
C
C  SET UP FOR ANY SWATH'D FIELDS:
C     These functions are done at each radar sample point, for several
C     sweeps.  Since data may not be located at the same (range, angle)
C     positions, a new (range, angle) grid is setup (see STORE) and data
C     are mapped to the nearest location within this grid.  The maximum
C     number of angles allowed [NANG(2)=720] is set in data statement in 
C     PPI_MMM and the step between angles (DELA) is set in STORE.  
C     The SETGRID command must be included when doing any swathing.
C
C     NFUN - 'SWATH   ' (IFLD = -1), pick the maximum value
C            'GRID    ' (IFLD = -1), grid the original value
C            'INTEGR  ' (IFLD = -2 AND -3), do time averaging
C            'FXSWATH ' (IFLD = -4), OR, do pseudo-rhi's from azm scans
C            'AVRAGE  ' (IFLD = -5 AND -6). do averaging of sweeps
C
C     NOUT - Name of swath output field 
C            (or special mnemonics 'STATS' or 'ANGLE' or 'HEIGHT')
C     NIN1 - Name of input field to be SWATH'd
C     NIN2 - Name of swath output time (or count) field for INTEGR (AVRAGE)
C
C     NSWATH will be name of field (in swth.inc) being SWATH'd.
C
C     NOTE:  If NOUT = 'STATS   ' in SWATH function, the following fields
C            are available for plotting: NIN2(1:4) + ('max','min','mean',
C            'sdev', or 'npts').  These field names are set in module FIELD 
C            (called from SAVFUN), and the number of fields is increased by 5.
C     NOTE:  If NOUT = 'ANGLE   ' in SWATH function, the following fields
C            are available for plotting: NIN2(1:4) + ('amax' or 'angl').  
C            These field names are set in module FIELD (called from SAVFUN), 
C            and the number of fields is increased by 2.
C     NOTE:  If NOUT = 'HEIGHT  ' in SWATH function, the following fields
C            are available for plotting: NIN2(1:4) + ('zmax' or 'zalt').  
C            These field names are set in module FIELD (called from SAVFUN), 
C            and the number of fields is increased by 2.
C
C     Note: MAX and MIN statistics assume that BDVAL=-999.0 (See STORE).
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'swth.inc'
      DIMENSION IFLD(MXF)
      CHARACTER*8 INDAT(10)
      CHARACTER*8 NAMFLD(MXF),NFUN,NOUT,NIN1,NIN2,IFMT
      CHARACTER*8 NAMEOUT

      READ(INDAT,10)NFUN,NOUT,NIN1,NIN2
 10   FORMAT(/A8/A8/A8/A8)
      
      IF(NOUT.EQ.'STATS   ')THEN
         WRITE(NAMEOUT,11)NIN2(1:4)
 11      FORMAT(A4,'max')
         IFLSW=IFIND(NAMEOUT,NAMFLD,MXF)
         IFLD(IFLSW)  =-1
         IFLD(IFLSW+1)=-1
         IFLD(IFLSW+2)=-1
         IFLD(IFLSW+3)=-1
         IFLD(IFLSW+4)=-1
         NSWATH=NIN1
      ELSE IF(NOUT.EQ.'ANGLE   ')THEN
         WRITE(NAMEOUT,12)NIN2(1:4)
 12      FORMAT(A4,'amax')
         IFLSW=IFIND(NAMEOUT,NAMFLD,MXF)
         IFLD(IFLSW)  =-1
         IFLD(IFLSW+1)=-1
         NSWATH=NIN1
      ELSE IF(NOUT.EQ.'HEIGHT  ')THEN
         WRITE(NAMEOUT,13)NIN2(1:4)
 13      FORMAT(A4,'zmax')
         IFLSW=IFIND(NAMEOUT,NAMFLD,MXF)
         IFLD(IFLSW)  =-1
         IFLD(IFLSW+1)=-1
         NSWATH=NIN1
      ELSE
         IFLSW=IFIND(NOUT,NAMFLD,MXF)
         IFLD(IFLSW)=-1
         NSWATH='NOTSTATS'
      END IF

C  CLEAR STORAGE ARRAYS
C
      DO 110 J=1,NANG(2)
c         ELA(J,2)=0.0
         JBSWT(J)=0.0
         FXSWT(J)=0.0
         IF(NOUT.EQ.'STATS   ')THEN
            DO 100 I=1,MXR
               DAT(I,J,IFLSW)  =BDVAL
               DAT(I,J,IFLSW+1)=-1.0*BDVAL
               DAT(I,J,IFLSW+2)=BDVAL
               DAT(I,J,IFLSW+3)=BDVAL
               DAT(I,J,IFLSW+4)=BDVAL
 100        CONTINUE
         ELSE IF(NOUT.EQ.'ANGLE   ')THEN
            DO 102 I=1,MXR
               DAT(I,J,IFLSW)  =BDVAL
               DAT(I,J,IFLSW+1)=BDVAL
 102        CONTINUE
         ELSE IF(NOUT.EQ.'HEIGHT  ')THEN
            DO 104 I=1,MXR
               DAT(I,J,IFLSW)  =BDVAL
               DAT(I,J,IFLSW+1)=BDVAL
 104        CONTINUE
         ELSE
            DO 105 I=1,MXR
               DAT(I,J,IFLSW)=BDVAL
 105        CONTINUE
         END IF
 110  CONTINUE

      IF(NFUN.EQ.'INTEGR  ')THEN
         NAMFLD(IFLSW+1)=NIN2
         IFLD(IFLSW)=-2
         IFLD(IFLSW+1)=-3
         DO 130 J=1,NANG(2)
            DO 120 I=1,MXR
               DAT(I,J,IFLSW)=BDVAL
               DAT(I,J,IFLSW+1)=BDVAL
 120        CONTINUE
 130     CONTINUE
      END IF

      IF(NFUN.EQ.'FXSWATH ')THEN
         IFLD(IFLSW)=-4
         FRSTBM=.TRUE.
         JBEAM=0
      END IF

      IF(NFUN.EQ.'AVRAGE  ')THEN
         NAMFLD(IFLSW+1)=NIN2
         IFLD(IFLSW)=-5
         IFLD(IFLSW+1)=-6
         DO 150 J=1,NANG(2)
            DO 140 I=1,MXR
               DAT(I,J,IFLSW)=0.0
               DAT(I,J,IFLSW+1)=0.0
 140        CONTINUE
 150     CONTINUE
      END IF

      RETURN
      END





