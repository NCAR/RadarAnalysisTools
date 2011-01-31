      SUBROUTINE LOADBMS(IOP,ISWP,DATA,MAXVAL,MAXBM)
C
C     THIS SUBROUTINE IS CALLED FROM DOFILT TO LOAD BEAMS INTO THE ARRAY
C     DATA.   Initially LOADBMS is called (IOP=0) and MAXBMs beams are 
C     read and loaded into memory (DATA).  LOADBMS is then called (IOP=1) 
C     the DOFILT do loop (#500) to shift beams within DATA and then read 
C     a single new beam to be appended at the end of the DATA array.
C
C     IOP = 0  ==>  READ IN MIN(NRAYS, MAXBM) RAYS AND UNPACK INTO DATA
C     IOP = 1  ==>  SHIFT BEAMS IN DATA ARRAY, READ A SINGLE BEAM, AND 
C                   PUT THIS NEW BEAM AT THE END OF DATA.
C
C     ISWP   - SWEEP NUMBER
C     DATA   - ARRAY THAT HOLDS DATA FOR BEAMS
C
C-------------------------------------------------------------------
C     KOUT contains a beams worth of information which is written to
C     Sprint's internal disk (fort.1) in WRRYDK where KOUT (KUNBUF) is 
C     first put into array KPKBUF, whose 1st value is the record length.
C        KOUT(  1)  - scaled integer azimuth (AZ*JRH6 = AZ*64)
C        KOUT(  2)  - scaled integer fixed angle (FXANG*JRH7 = FXANG*100)
C        KOUT(3-5)  - integer time (hour-min-sec) of beam
C        KOUT(6-7)  - scaling factors (JRH6, JRH7) = (64, 100)
C        KOUT(  8)  - number of range gates (NRG)
C        KOUT(9-10) - 1000*latitude/longitude of the radar 
C        KOUT(>10)  - contains NOF*(NRG field values)
C-------------------------------------------------------------------
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c      PARAMETER (MAXRNG=1024,MAXFLD=16)
c      PARAMETER (BDVAL=-32768.)

c      PARAMETER (MAXVAL=(MAXRNG+10)*MAXFLD,MAXBM=51)
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /IDBLK/  ID(NID)
      COMMON /SCRDSK/ LTMP
      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      COMMON /FILT/ ISTFLG,NRAYS,IDIR,C,NLEN,NUMFILT,INPFLD(MAXFLD),
     X              SCLFLD(MAXFLD),MTMP

      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      CHARACTER*8 NAMTIM
      DATA NAMTIM/'TIME'/
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM

      DIMENSION DATA(MAXVAL,MAXBM)

      IPTR=IPTR_INT
      NFLINP=NFLDS
      IF (IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1

      IF (IOP.EQ.0) THEN
c--------debug(ljm)
         print *,'LOADBM: iptr,nflinp,iop,id(37)=',
     +        iptr,nflinp,iop,id(37)
c--------debug(ljm)

C     READ IN MIN(NRAYS, MAXBM) RAYS AND UNPACK INTO DATA
C     The variables IDIR (scanning direction) and C (fixed scan angle,
C     usually the elevation angle of the sweep) are set here and used
C     inside DOFILT.
C 
c         IDIR=ID(130+3*(ISWP-1))
c         C=FLOAT(ID(129+3*(ISWP-1)))/FLOAT(ID(44))
         IDIR=ID(IPTR+1+3*(ISWP-1))
         C=FLOAT(ID(IPTR+3*(ISWP-1)))/FLOAT(ID(44))
         
         j=1
         CALL RDRYDK(KPCK,KOUT,NST,LTMP,1,NLEN)
c--------debug(ljm)
         print *,'LOADBM: #,j,len,nf,nrg,mxrg,e=',
     +        ltmp,j,nlen,nflinp,kout(8),maxrng,c
c--------debug(ljm)
         DO J=1,MAXBM
            DO I=1,MAXVAL
               DATA(MAXVAL,MAXBM)=BDVAL
            END DO
         END DO
         
C        Move 1st ID(37)=10 values from KOUT to DATA
C
         DO 137 K=1,ID(37)
            DATA(K,1)=FLOAT(KOUT(K))
c--------debug(ljm)
            print *,'LOADBM: k,data(k,1)=',k,data(k,1)
c--------debug(ljm)
 137     CONTINUE
         DO I=1,NFLINP
            ISTRT=(I-1)*KOUT(8) + ID(37)
            JSTRT=(I-1)*MAXRNG  + ID(37)
            DO K=1,MAXRNG
               IF (K.LE.KOUT(8)) THEN 
                  DATA(JSTRT+K,1)=FLOAT(KOUT(ISTRT+K))
               ELSE
                  DATA(JSTRT+K,1)=BDVAL
               END IF
            END DO
         END DO
                  
C     Read in MIN(NRAYS, MAXBM) rays and unpack into DATA
C
         JEND=MIN(NRAYS,MAXBM)
         DO 125 J=2,JEND
            CALL RDRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
c-----------debug(ljm)
            if(j.eq.jend)then
               print *,'LOADBM: #,j,len,nf,nrng,mxrng=',
     +              ltmp,j,nlen,nflinp,kout(8),maxrng
            end if
            jj=j
c-----------debug(ljm)
            DO K=1,ID(37)
               DATA(K,J)=FLOAT(KOUT(K))
            END DO
            DO I=1,NFLINP
               ISTRT=(I-1)*KOUT(8) + ID(37)
               JSTRT=(I-1)*MAXRNG  + ID(37)
               DO K=1,MAXRNG
                  IF (K.LE.KOUT(8)) THEN 
                     DATA(JSTRT+K,J)=FLOAT(KOUT(ISTRT+K))
                  ELSE
                     DATA(JSTRT+K,J)=BDVAL
                  END IF
               END DO
            END DO
 125     CONTINUE
C     
C     UNSCALE ALL FIELDS THAT WILL BE FILTERED
C     
         DO 400 J=1,NUMFILT
            ISTART=INPFLD(J)
            DO 425 L=1, MIN(NRAYS,MAXBM)
               NRG=DATA(8,L)
               DO 450 K=1,NRG
                  IF (DATA(ISTART+K-1,L).NE.BDVAL)
     X                 DATA(ISTART+K-1,L)=DATA(ISTART+K-1,L)/
     X                 SCLFLD(J)
 450           CONTINUE
 425        CONTINUE
 400     CONTINUE
            
C     
C     UNSCALE AZIMUTHS
C     
         DO 475 L=1,MIN(NRAYS,MAXBM)
            DATA(1,L)=DATA(1,L)/64.
 475     CONTINUE
            

      ELSE IF (IOP.EQ.1) THEN

C     First shift the data in angular direction, then
C     read in a single beam and append it to the end.
C     This part of code will never be executed if the
C     MAXBM variable is large enough to hold an entire
C     sweep (2004.0527 - ljm).
C
         DO I=2,MAXBM
            DO J=1,MAXVAL
               DATA(J,I-1) = DATA(J,I)
            END DO
         END DO
         
C
C     Read in a beam and unpack and unscale it
C
         jj=jj+1
         CALL RDRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
c--------debug(ljm)
         print *,'LOADBM: #,j,len,nf,nrng,mxrng=',
     +        ltmp,jj,nlen,nflinp,kout(8),maxrng
c--------debug(ljm)
         DO K=1,ID(37)
            DATA(K,MAXBM)=FLOAT(KOUT(K))
         END DO
         DO I=1,NFLINP
            ISTRT=(I-1)*KOUT(8) + ID(37)
            JSTRT=(I-1)*MAXRNG  + ID(37)
            DO K=1,MAXRNG
               IF (K.LE.KOUT(8)) THEN
                  DATA(JSTRT+K,MAXBM)=FLOAT(KOUT(ISTRT+K))
               ELSE
                  DATA(JSTRT+K,MAXBM)=BDVAL
               END IF
            END DO
         END DO

         NRG=KOUT(8)
         DO J=1,NUMFILT
            ISTART=INPFLD(J)
            DO K=1,NRG
               IF (DATA(ISTART+K-1,MAXBM).NE.BDVAL)
     X              DATA(ISTART+K-1,MAXBM)=DATA(ISTART+K-1,MAXBM)/
     X              SCLFLD(J)
            END DO
         END DO
         DATA(1,MAXBM)=DATA(1,MAXBM)/64.

      END IF

      RETURN

      END
