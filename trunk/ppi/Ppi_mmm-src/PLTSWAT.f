c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTSWAT(INDAT,ZSTR,PLTSW,VECTS,NFRAME,IGRPLT,IGRYCON,
     X     JMAP,JACT,JMRK,JNLD,BGFLAG,SINDAT,MP,MXPLT,DELAMX,PROCESS,
     X     NFXVOL,COLRFIL,NOLAB,ICOLTYP)
C
      CHARACTER*8 IGRYCON,SINDAT(10,MXPLT)

      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON /PLTWIN/XRT(26),YTP(26),SIDEX(26),SIDEY(26),DXL,DXR,DYB,
     + DYT,NROW,NCOL,NWIN,IWIN,LABX(26),LABY(26),ILFLG,XBTP(26),
     + YBTP(26),SIZBX,XSHIFT,YSHIFT
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL

      LOGICAL COLRFIL,PLTSW,VECTS,PROCESS
      CHARACTER*8 INDAT(10)
      CHARACTER*8 AVNAM
      CHARACTER*3 LABLS
      CHARACTER*1 BGFLAG
      CHARACTER*8 ICOLTYP
      CHARACTER*8 NAMEOUT
      DATA CNTMN/2.0/

C     NOTE: When NOUT='STATS   ' in the SWATH function, four fields are 
C           created: NIN2(1:4) + ('max','min','mean','sdev', and 'npts')
C           which are stored in DAT field locations IFL, IFL+1, IFL+2, 
C           IFL+3, and IFL+4.  When plotting MEAN, the summation is in 
C           IFL and the count is in IFL+2.  When plotting SDEV, the 
C           summation is in IFL-1, the sum of squared values in IFL, 
C           and the count is in IFL+1.  Also see FIELD and SAVFUN.
C     NOTE: When NOUT='ANGLE   ' in the SWATH function, two fields are 
C           created: NIN2(1:4) + ('amax' and 'angl')
C           which are stored in DAT field locations IFL and IFL+1.
C           The field 'angl' has fixed angle associated with maximum value.
C     NOTE: When NOUT='HEIGHT  ' in the SWATH function, two fields are 
C           created: NIN2(1:4) + ('zmax' and 'zalt')
C           which are stored in DAT field locations IFL and IFL+1.
C           The field 'zalt' has height (msl) associated with maximum value.
C
      IF(INDAT(4)(1:7) .NE. 'SAMPLOC' .OR.
     X   INDAT(4)(1:4) .NE. 'DIGT'    .OR.
     X   INDAT(4)(1:5) .NE. 'RESET'   )THEN
         READ(INDAT(2),10)AVNAM
 10      FORMAT(A8)
         IFL=IFIND(AVNAM,NAMFLD,MXF)
         print *,' pltswat: ',indat(4),' ',avnam,' ',ifl
         IF(IFL.EQ.0)THEN
            WRITE(6,11)
 11         FORMAT(1X,'+++ PLTSWAT: FIELD DOES NOT EXIST +++ ')
            RETURN
         END IF
      END IF

      PLTSW=.TRUE.
      VECTS=.FALSE.
      ITPOLD=ITPSWA
      FXOLD=FXSWA
      DROLD=DRSW

C     ROUTINE CALLED ONLY TO RESET SWATHs, AND NOT DO ANY CONTOURING.
C
      IF(INDAT(4).EQ.'RESET   ')THEN
         READ(INDAT(6),13)RSWPAVG
 13      FORMAT(F8.0)
         NSWPAVG=NINT(RSWPAVG)
         IF(NSWPAVG.LE.0 .OR. NSWPAVG.GE.9999)NSWPAVG=MSCAN
         GO TO 80
      END IF

      CALL CONLEV(INDAT,NAMFLD,IFLD,IFL,ISW,COLRFIL,ICOLTYP,
     X            ITERGT,CL,NL,CMIN,CMAX,CINC,ISHADE,IOV,
     X            DRSW,DROLD,IBSCAN,PLTSW,IGRPLT,IGRYCON,
     X            JMAP,JACT,JMRK,JNLD,NOLAB,THIK,MXF,SINDAT,
     X            MP,MXPLT,PROCESS,ITERBM,DIGMIN,DIGMAX,DIGOFF,
     X            DIGCOLR,DIGSIZE)
c-----print *,' pltswat: ',pltsw,drsw,cmin,cmax,cinc
      IF(ICOLTYP.EQ.'SAMPLOC '.OR.
     X   ICOLTYP(1:4).EQ.'DIGT')GO TO 30

C     GENERATE OR CHANGE SOME PARAMETERS FOR PSEUDO-RHI'S EXTRACTED
C     FROM SURVEILLANCE OR SECTOR SCANS OVER A LONG TIME PERIOD
C
      IF(IFLD(IFL).EQ.-4)THEN

C        REQUESTED FIELD IS TO BE EXTRACTED AS PSEUDO-RHI

         NANG(ISW)=JBEAM

         IHR=JBSWT(1)/3600
         IMN=INT(FLOAT(JBSWT(1)-IHR*3600)/60.0)
         ISC=JBSWT(1)-IHR*3600-IMN*60
         ITIME1=IHR*10000+IMN*100+ISC

         IHR=JBSWT(JBEAM)/3600
         IMN=INT(FLOAT(JBSWT(JBEAM)-IHR*3600)/60.0)
         ISC=JBSWT(JBEAM)-IHR*3600-IMN*60
         ITIME=IHR*10000+IMN*100+ISC

         DO J=1,JBEAM
            AZA(J,ISW)=FXSWT(J)
         END DO
         DO I=1,MXR
            RNG(I,2)=-R0-DRSW*(I-1)
         END DO
      END IF

C      write(6,1770)namfld(ifl),ifl,bdval,nwin,nrow*ncol,iwin
C 1770 format(1x,' pltswat: ',a8,' ifl=',i8,' bdval=',f8.0,
C     +     ' nwin,nrow*ncol,iwin=',3i8)

      IF(AVNAM(5:8).EQ.'min')THEN
         DO J=1,NANG(2)
            DO I=MNGATE,MXGATE
               IF(DAT(I,J,IFL) .EQ. -1.0*BDVAL)THEN
                  DAT(I,J,IFL)=BDVAL
               END IF
            END DO
         END DO
      END IF
      
 30   CONTINUE

C     When doing INTEGR, AVRAGE, OR STATS, compute outputs through 
C     current sweep from the accumulator arrays,
C        Older code: IF(IFLD(IFL).EQ.-2.OR.IFLD(IFL).EQ.-3)CALL INTEGR
C
      IF(IFLD(IFL).EQ.-5 .OR. 
     X   IFLD(IFL).EQ.-6 .OR.
     X   AVNAM(5:8).EQ.'mean' .OR. 
     X   AVNAM(5:8).EQ.'sdev')THEN
         CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X        NAMFLD,IFLD,IFL,AVNAM)
      END IF

      IF(ICOLTYP.EQ.'LQCOL1  '.OR.ICOLTYP .EQ.'LQCOL2  ')THEN
C  solid           Set fill area interior style.
C  produce color fill
         COLRFIL=.TRUE.
         CALL GSFAIS(1)
         CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X              SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X              DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
         CALL CONFILL1(IGRPLT,IGRYCON,PLTSW,DELAMX)
      ELSE IF (ICOLTYP .EQ.'HQCOL1  '.OR.ICOLTYP .EQ.'HQCOL2  ')THEN
         COLRFIL=.TRUE.
         CALL GSFAIS(1)
         CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X              SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X              DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
         CALL CONFILL2(IGRPLT,IGRYCON,PLTSW,DELAMX)
      ELSE IF (ICOLTYP .EQ.'SAMPLOC ')THEN
         COLRFIL=.FALSE.
         CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X              SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X              DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
         IFL=0
         VECTS=.TRUE.
c--------print *,' samploc: avnam,ifl,vects=',avnam,ifl,vects
         CALL SAMPLOC(ICOLTYP,DIGCOLR,DIGSIZE)
      ELSE IF (ICOLTYP(1:4) .EQ.'DIGT')THEN
         COLRFIL=.FALSE.
         CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X              SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X              DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
         VECTS=.TRUE.
c--------print *,' digitize: avnam,ifl,vects=',avnam,ifl,vects
         CALL SAMPLOC(ICOLTYP,DIGCOLR,DIGSIZE)
      ELSE
         COLRFIL=.FALSE.
         CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X              SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X              DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
         JLW=1000*THIK
         CALL CONTUR(IGRPLT,JLW,DELAMX)
      END IF

      IF(ICOLTYP.EQ.'SAMPLOC ')RETURN

C     When doing INTEGR, AVRAGE, OR STATS, restore accumulators.
C        Older code: IF(IFLD(IFL).EQ.-2.OR.IFLD(IFL).EQ.-3)CALL UNINTEGR
C
      IF(IFLD(IFL).EQ.-5 .OR. 
     X   IFLD(IFL).EQ.-6 .OR.
     X   AVNAM(5:8).EQ.'mean' .OR. 
     X   AVNAM(5:8).EQ.'sdev')THEN
         CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X        NAMFLD,IFLD,IFL,AVNAM)
      END IF

      IF(AVNAM(5:8).EQ.'min')THEN
         DO J=1,NANG(2)
            DO I=MNGATE,MXGATE
               IF(DAT(I,J,IFL) .EQ. BDVAL)THEN
                  DAT(I,J,IFL)=-1.0*BDVAL
               END IF
            END DO
         END DO
      END IF

      RETURN

C     RESET ALL STORAGE ARRAYS IF IN 'SWATH' MODE
C     UNLESS NUMBER OF SCANS LESS THAN NSWPAVG.
C
 80   CONTINUE
      IF(MOD(MSCAN,NSWPAVG).NE.0)THEN
         WRITE(6,81)NAMFLD(IFL),IFL,MSCAN,NSWPAVG
 81      FORMAT(1X,' PLTSWAT: ',A8,I6,'  Scan #',2I4)
         RETURN
      ELSE
         WRITE(6,83)NAMFLD(IFL),IFL,MSCAN,NSWPAVG
 83      FORMAT(1X,' PLTSWAT: ',A8,I6,'  Scan #',2I4,'  Reset flds')
         NFXVOL=0
      END IF

      IF(AVNAM(5:8).EQ.'max'.OR.
     +   AVNAM(5:8).EQ.'min'.OR.
     +   AVNAM(5:8).EQ.'mean'.OR.
     +   AVNAM(5:8).EQ.'sdev'.OR.
     +   AVNAM(5:8).EQ.'npts')THEN

C        SWATH (STATS) MODE:

         WRITE(NAMEOUT,85)AVNAM(1:4)
 85      FORMAT(A4,'max')
         IFLSW=IFIND(NAMEOUT,NAMFLD,MXF)
         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFLSW)=BDVAL
               DAT(I,J,IFLSW+1)=-1.0*BDVAL
               DAT(I,J,IFLSW+2)=BDVAL
               DAT(I,J,IFLSW+3)=BDVAL
               DAT(I,J,IFLSW+4)=BDVAL
            END DO
         END DO

      ELSE IF(AVNAM(5:8).EQ.'amax'.OR.
     X        AVNAM(5:8).EQ.'angl')THEN

C        SWATH (MAX and ANGLE) MODE:

         WRITE(NAMEOUT,91)AVNAM(1:4)
 91      FORMAT(A4,'amax')
         IFLSW=IFIND(NAMEOUT,NAMFLD,MXF)
         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFLSW)=BDVAL
               DAT(I,J,IFLSW+1)=BDVAL
            END DO
         END DO

      ELSE IF(AVNAM(5:8).EQ.'zmax'.OR.
     X        AVNAM(5:8).EQ.'zalt')THEN

C        SWATH (MAX and HEIGHT) MODE:

         WRITE(NAMEOUT,95)AVNAM(1:4)
 95      FORMAT(A4,'zmax')
         IFLSW=IFIND(NAMEOUT,NAMFLD,MXF)
         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFLSW)=BDVAL
               DAT(I,J,IFLSW+1)=BDVAL
            END DO
         END DO

      ELSE

C        SWATH (NORMAL, MAX ONLY) MODE:
 
         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFL)=BDVAL
            END DO
         END DO
      END IF

      IF(IFLD(IFL).EQ.-2)THEN

C        INTEGR MODE: REQUESTED FIELD IS RESULT OF INTEGRATION

         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFL)=BDVAL
               DAT(I,J,IFL+1)=BDVAL
            END DO
         END DO
      END IF
      IF(IFLD(IFL).EQ.-3)THEN

C        INTEGR MODE: REQUESTED FIELD IS THE INTEGRATION TIME

         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFL-1)=BDVAL
               DAT(I,J,IFL)=BDVAL
            END DO
         END DO
      END IF

      IF(IFLD(IFL).EQ.-5)THEN

C        AVRAGE MODE: REQUESTED FIELD IS THE SWEEP-TO-SWEEP AVERAGE

         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFL)=0.0
               DAT(I,J,IFL+1)=0.0
            END DO
         END DO
      END IF
      IF(IFLD(IFL).EQ.-6)THEN

C        AVRAGE MODE: REQUESTED FIELD IS THE NUMBER OF SWEEPS AVERAGED
         DO J=1,NANG(2)
            DO I=1,MXR
               DAT(I,J,IFL-1)=0.0
               DAT(I,J,IFL)=0.0
            END DO
         END DO
      END IF

      RETURN
      END





