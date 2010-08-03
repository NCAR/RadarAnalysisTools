      SUBROUTINE FFSCL(IBUF,NAMFLD,IUNPAD,THVAL,KOUT,INUM,J1,J2,ID,
     X     IDPTR,IFC,MTFIEL,IOVER,ITIFLG,N,I,VNY,KST,NGFLD,CTDBM,
     X     CTDBMXH,CTDBMXV,NDBMXH)
C     
C     THIS SUBROUTINE CONVERTS A FIELD FROM A RAY OF FIELD FORMAT DATA
C     TO EITHER METEOROLOGICAL UNITS (FOR THRESHOLDING FIELDS) OR 
C     TO SPRINT INTERNALLY SCALED UNITS (FOR FIELDS TO BE INTERPOLATED).
C     PROGRAM SHOULD WORK FOR RP-3 THROUGH RP-7.
C     
C     IBUF     - RAY OF DATA TO BE CONVERTED
C     NAMFLD   - NAME OF FIELD
C     IUNPAD   - SOME SORT OF STARTING GATE
C     THVAL    - OUTPUT ARRAY WHEN FIELD IS TO BE USED FOR THRESHOLDING
C     KOUT     - OUTPUT ARRAY WHEN FIELD IS TO BE INTERPOLATED
C     INUM     - THRESHOLD FIELD #
C     J1,J2    - GATE LIMITS
C     ID       - INFO ARRAY THAT STORES FIELD NAMES, INTERNAL SCALE FACTORS,ETC
C     IDPTR    - POINTER INTO ID ARRAY
C     IFC      - SOME FIELD # COUNTER
C     MTFIEL   - SOME ARRAY OF FIELD NUMBERS
C     POWER    - ARRAY TO CONVERT RETURNED POWER (COUNTS) TO DBM
C     IBAD     - INTEGER BAD VALUE
C     IOVER    - ARRAY TO KEEP TRACK OF # OF VALUES THAT HAVE OVERFLOWED
C     ITIFLG   - FLAG THAT INDICATES IF DATA IS TO BE RETURNED IN THVAL
C     (IN MET. UNITS) OR KOUT (IN SPRINT SCALED UNITS)
C     N        - FIELD NUMBER
C     I        - ANOTHER FIELD NUMBER
C     VNY      - NYQUIST VELOCITY
C     KST      - INDEX INTO OUTPUT ARRAY FOR BEAM
C     NDBMXH   - FIELD # OF DBMXH FOR USE IN UNFOLDING DBMXV
C     
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (MAXRNG=1024)
c-----PARAMETER (MAXFLD=16)

c      PARAMETER (MXCNT=500)
      DIMENSION IBUF(MAXIN),THVAL(2,MAXRNG),KOUT(MAXIN),ID(NID)
      DIMENSION MTFIEL(MAXFLD),IOVER(6),CTDBM(MXCNT),CTDBMXH(MXCNT)
      DIMENSION CTDBMXV(MXCNT)
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      CHARACTER*8 NAMFLD
      
      
      IF (ITIFLG.EQ.1) THEN
C     
C     FIELD TO BE USED FOR THRESHOLDING
C     
         IF (NAMFLD.EQ.'DZNE' .OR. NAMFLD.EQ.'VRNE' .OR.
     &        NAMFLD.EQ.'SWNE' .OR. NAMFLD.EQ.'SNR'  .OR.
     &        NAMFLD.EQ.'MHR'  .OR. NAMFLD.EQ.'NCP') THEN
C     
C     FIELD CALIBRATED ENGINEERING VALUES (RP-7)
C     
            SCL=IBUF(79+2*(N-1))
            IBIAS=IBUF(80+2*(N-1))
            IF(IBIAS.GT.32767) IBIAS=IBIAS-65536
            BIAS=IBIAS*.01
            
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN FFSCL +++'
               STOP
            END IF
            DO 222 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 222
               IF(J.GT.NGFLD) GO TO 222
               X=(FLOAT(IBUF(IPF+K))*SCL*.01 + BIAS)
               THVAL(INUM,J)=X
               K=K+1
 222        CONTINUE
         ELSE IF (NAMFLD.EQ.'DMNE') THEN
C     
C     RETURNED POWER, PRIMARY WAVELENGTH
C     
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN RPNCAR +++'
               STOP
            END IF
            IF (CTDBM(1).EQ.-32768) THEN
               WRITE(*,700)
            END IF
            DO 232 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 232
               IF(J.GT.NGFLD) GO TO 232
               XY=(FLOAT(IBUF(IPF+K)))
               IF (XY.EQ.0.0) XY=1.0
               IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                  X=32768
               ELSE
                  X=CTDBM(NINT(XY))
               END IF
               THVAL(INUM,J)=X
               K=K+1
 232        CONTINUE
            
         ELSE IF (NAMFLD.EQ.'DBMXH') THEN
C     
C     RETURNED POWER, SECONDARY WAVELENTH, HOR. POLARIZATION
C     
            
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN RPNCAR +++'
               STOP
            END IF
            IF (CTDBMXH(1).EQ.-32768) THEN
               WRITE(*,710)
               STOP
            END IF
            DO 236 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 236
               IF(J.GT.NGFLD) GO TO 236
               XY=(FLOAT(IBUF(IPF+K)))
               IF (XY.EQ.0.0) XY=1.0
               IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                  X=32768
               ELSE
                  X=CTDBMXH(NINT(XY))
               END IF
               THVAL(INUM,J)=X
               K=K+1
 236        CONTINUE
         ELSE IF (NAMFLD.EQ.'DBMXV') THEN
C     
C     RETURNED POWER, SECONDARY WAVELENTH, VER. POLARIZATION
C     
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            IPFHOR=(IBUF(65)+1)+(NDBMXH-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN RPNCAR +++'
               STOP
            END IF
            IF (CTDBMXH(1).EQ.-32768) THEN
               WRITE(*,710)
               STOP
            END IF
            IF (CTDBMXV(1).EQ.-32768) THEN
               WRITE(*,720)
               STOP
            END IF
            DO 238 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 238
               IF(J.GT.NGFLD) GO TO 238
               XY=(FLOAT(IBUF(IPF+K)))
               IDBMXH=(FLOAT(IBUF(IPFHOR+K)))
               IF (IDBMXH.EQ.0.0) IDBMXH=1.0
               IF (IDBMXH.LT.1.0 .OR. IDBMXH.GT.FLOAT(MXCNT)) THEN
                  DBMXH=-32768
               ELSE
                  DBMXH=CTDBMXH(INT(IDBMXH))
               END IF
               IF (XY.EQ.0.0) XY=1.0
               IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                  X=32768
               ELSE
                  X=CTDBMXV(NINT(XY))
               END IF
C
C     NOW CHECK FOR FOLDING
C
               IF (X/ID(IDPTR+4).LT.-100. .AND. DBMXH.GT.-65.) THEN
                  XY=XY+256
                  IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                     X=32768
                  ELSE
                     X=CTDBMXV(NINT(XY))*ID(IDPTR+4)
                  END IF
               END IF
               THVAL(INUM,J)=X
               K=K+1
 238        CONTINUE
         ELSE IF (NAMFLD.EQ.'VEL') THEN
C     
C     RADIAL VELOCITY: CONVERT FREQUENCY COUNTS TO M/S
C     PRIMARY WAVELENGTH, HORIZONTAL POLARIZATION
C     COUNTS FROM -127 TO 127 COVER 2*VNYQ RANGE
C     
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN RPNCAR +++'
               STOP
            END IF
            DO 242 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 242
               IF(J.GT.NGFLD) GO TO 242
               IF (IBUF(IPF+K).GE.128) IBUF(IPF+K)=IBUF(IPF+K)-255
               X=VNY*IBUF(IPF+K)/127.0
               THVAL(INUM,J)=X
               K=K+1
 242        CONTINUE
         ELSE IF (NAMFLD.EQ.'VELV') THEN
C     
C     RADIAL VELOCITY: CONVERT FREQUENCY COUNTS TO M/S
C     PRIMARY WAVELENGTH, VERTICAL POLARIZATION
C     COUNTS FROM -127 TO 127 COVER 2*VNYQ RANGE
C     
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN RPNCAR +++'
               STOP
            END IF
            DO 252 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 252
               IF(J.GT.NGFLD) GO TO 252
               IF (IBUF(IPF+K).GE.128) IBUF(IPF+K)=IBUF(IPF+K)-255
               X=VNY*IBUF(IPF+K)/127.0
               THVAL(INUM,J)=X
               K=K+1
 252        CONTINUE
            
         ELSE IF (NAMFLD.EQ.'ZDR') THEN
C
C     ZDR: S-BAND REFL. RATIO IN DB
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
C
C     DETERMINE SCALING FACTOR FOR CP2/RP6
C
            SCL=1.0
            IF (IBUF(62).EQ.6) THEN
               ISCL=ICEDAND(IBUF(247),12)
               ISCL=ICEDSHFT(ISCL,-2)
               SCL=3.0
               IF (ISCL.EQ.1) SCL=6.0
               IF (ISCL.EQ.2) SCL=12.0
               IF (ISCL.EQ.3) SCL=24.0
               SCL=SCL/127.0
            END IF
            DO 257 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 257
               IF(J.GT.NGFLD) GO TO 257
               IF (IBUF(IPF+K).GE.128) IBUF(IPF+K)=IBUF(IPF+K)-255
               X=IBUF(IPF+K)*SCL
               THVAL(INUM,J)=X
               K=K+1
 257        CONTINUE
         ELSE IF (NAMFLD.EQ.'SPECW') THEN
C     
C     SPECTRAL WIDTH: CONVERT FREQUENCY COUNTS TO M/S
C     PRIMARY WAVELENGTH, HORIZONTAL POLARIZATION
C     COUNTS FROM 0 TO 255 COVER VNYQ/SQRT(3) RANGE
C     
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            IF (NRG.GT.MAXRNG) THEN
               WRITE(*,*)'+++ TOO MANY RANGE GATES IN RPNCAR +++'
               STOP
            END IF
            DO 262 J=1,NRG
               THVAL(INUM,J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 262
               IF(J.GT.NGFLD) GO TO 262
               X=0.57735*VNY*IBUF(IPF+K)/255.0
               THVAL(INUM,J)=X
               K=K+1
 262        CONTINUE
         END IF
      ELSE IF (ITIFLG.EQ.0) THEN
C     
C     FIELD WILL BE INTERPOLATED
C     
         IF (NAMFLD.EQ.'DZNE' .OR. NAMFLD.EQ.'VRNE' .OR.
     &        NAMFLD.EQ.'SWNE' .OR. NAMFLD.EQ.'SNR'  .OR.
     &        NAMFLD.EQ.'MHR'  .OR. NAMFLD.EQ.'NCP') THEN
C     
C     FIELD CALIBRATED ENGINEERING VALUES
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
 401        FORMAT (2A4)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
            SCL=IBUF(79+2*(N-1))
            IBIAS=IBUF(80+2*(N-1))
            IF(IBIAS.GT.32767)IBIAS=IBIAS-65536
            BIAS=IBIAS*.01
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.1) THEN
               IF (CFAC1.EQ.0.0) THEN
                  ID(IDPTR+2)=-1
               ELSE
                  ID(IDPTR+2)=CFAC1*100.
               ENDIF
            ELSE IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 420 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 420
               IF(J.GT.NGFLD) GO TO 420
               X=(FLOAT(IBUF(IPF+K))*SCL*.01 + BIAS)*
     +              ID(IDPTR+4)
               Y=X*FACT
               KOUT(KST+J)=NINT(Y)
 410           CONTINUE
               K=K+1
 420        CONTINUE
         ELSE IF (NAMFLD.EQ.'DMNE') THEN
C     
C     RETURNED POWER, PRIMARY WAVELENGTH
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.1) THEN
               IF (CFAC1.EQ.0.0) THEN
                  ID(IDPTR+2)=-1
               ELSE
                  ID(IDPTR+2)=CFAC1*100.
               ENDIF
               IF (CTDBM(1).EQ.-32768) THEN
                  WRITE(*,700)
 700              FORMAT(/,5X,'+++NEED CALIBRATION DATA FOR DMNE FIELD',
     X                 '+++')
                  STOP
               END IF
            ELSE IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 430 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 430
               IF(J.GT.NGFLD) GO TO 430
               XY=(FLOAT(IBUF(IPF+K)))
               IF (XY.EQ.0.0) XY=1.0
               IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                  X=32768
               ELSE
                  X=CTDBM(NINT(XY))*ID(IDPTR+4)
               END IF
               Y=X*FACT
               IF (Y.GT.32767 .OR. Y.LT.-32767) THEN 
                  Y=IBAD
                  IF (ITYP.GE.1 .AND. ITYP.LE.5) THEN
                     IOVER(ITYP)=IOVER(ITYP)+1
                  ELSE
                     IOVER(6)=IOVER(6)+1
                  END IF
               END IF
               KOUT(KST+J)=NINT(Y)
               K=K+1
 430        CONTINUE
         ELSE IF (NAMFLD.EQ.'DBMXH') THEN
C     
C     RETURNED POWER, SECONDARY WAVELENGTH, HOR. POLARIZATION
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.1) THEN
               IF (CFAC2.EQ.0.0) THEN
                  ID(IDPTR+2)=-1
               ELSE
                  ID(IDPTR+2)=CFAC2*100.
               ENDIF
               IF (CTDBMXH(1).EQ.-32768) THEN
                  WRITE(*,710)
 710              FORMAT(/,5X,'+++NEED CALIBRATION DATA FOR DBMXH ',
     X                 'FIELD+++')
                  STOP
               END IF
            ELSE IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 433 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 433
               IF(J.GT.NGFLD) GO TO 433
               XY=(FLOAT(IBUF(IPF+K)))
               IF (XY.EQ.0.0) XY=1.0
               IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                  X=32768
               ELSE
                  X=CTDBMXH(NINT(XY))*ID(IDPTR+4)
               END IF
               Y=X*FACT
               IF (Y.GT.32767 .OR. Y.LT.-32767) THEN 
                  Y=IBAD
                  IF (ITYP.GE.1 .AND. ITYP.LE.5) THEN
                     IOVER(ITYP)=IOVER(ITYP)+1
                  ELSE
                     IOVER(6)=IOVER(6)+1
                  END IF
               END IF
               KOUT(KST+J)=NINT(Y)
               K=K+1
 433        CONTINUE
         ELSE IF (NAMFLD.EQ.'DBMXV') THEN
C     
C     RETURNED POWER, SECONDARY WAVELENGTH, VERT. POLARIZATION
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.1) THEN
               IF (CFAC3.EQ.0.0) THEN
                  ID(IDPTR+2)=-1
               ELSE
                  ID(IDPTR+2)=CFAC3*100.
               ENDIF
               IF (CTDBMXH(1).EQ.-32768) THEN
                  WRITE(*,710)
                  STOP
               END IF
               IF (CTDBMXV(1).EQ.-32768) THEN
                  WRITE(*,720)
 720              FORMAT(/,5X,'+++NEED CALIBRATION DATA FOR DBMXH ',
     X                 'FIELD+++')
                  STOP
               END IF
            ELSE IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            IPFHOR=(IBUF(65)+1)+(NDBMXH-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 437 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 437
               IF(J.GT.NGFLD) GO TO 437
               XY=(FLOAT(IBUF(IPF+K)))
               IDBMXH=(FLOAT(IBUF(IPFHOR+K)))
               IF (IDBMXH.EQ.0.0) IDBMXH=1.0
               IF (IDBMXH.LT.1.0 .OR. IDBMXH.GT.FLOAT(MXCNT)) THEN
                  DBMXH=-32768
               ELSE
                  DBMXH=CTDBMXH(INT(IDBMXH))
               END IF
               IF (XY.EQ.0.0) XY=1.0
               IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                  X=32768
               ELSE
                  X=CTDBMXV(NINT(XY))*ID(IDPTR+4)
               END IF
C
C     NOW CHECK FOR FOLDING
C
               IF (X/ID(IDPTR+4).LT.-100. .AND. DBMXH.GT.-65.) THEN
                  XY=XY+256
                  IF (XY.LT.1.0 .OR. XY.GT.FLOAT(MXCNT)) THEN
                     X=32768
                  ELSE
                     X=CTDBMXV(NINT(XY))*ID(IDPTR+4)
                  END IF
               END IF
               Y=X*FACT
               IF (Y.GT.32767 .OR. Y.LT.-32767) THEN 
                  Y=IBAD
                  IF (ITYP.GE.1 .AND. ITYP.LE.5) THEN
                     IOVER(ITYP)=IOVER(ITYP)+1
                  ELSE
                     IOVER(6)=IOVER(6)+1
                  END IF
               END IF
               KOUT(KST+J)=NINT(Y)
               K=K+1
 437        CONTINUE
         ELSE IF (NAMFLD.EQ.'VEL') THEN
C     
C     RADIAL VELOCITY: CONVERT FREQUENCY COUNTS TO M/S
C     PRIMARY WAVELENGTH, HORIZONTAL POLARIZATION
C     COUNTS FROM -127 TO 127 COVER 2*VNYQ RANGE
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 440 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 440
               IF(J.GT.NGFLD) GO TO 440
               IF (IBUF(IPF+K).GE.128) IBUF(IPF+K)=IBUF(IPF+K)-255
               X=(VNY*IBUF(IPF+K)/127.0)*ID(IDPTR+4)
               Y=X*FACT
               KOUT(KST+J)=NINT(Y)
               K=K+1
 440        CONTINUE
         ELSE IF (NAMFLD.EQ.'VELV') THEN
C     
C     RADIAL VELOCITY: CONVERT FREQUENCY COUNTS TO M/S
C     PRIMARY WAVELENGTH, VERTICAL POLARIZATION
C     COUNTS FROM -127 TO 127 COVER 2*VNYQ RANGE
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 450 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 450
               IF(J.GT.NGFLD) GO TO 450
               IF (IBUF(IPF+K).GE.128) IBUF(IPF+K)=IBUF(IPF+K)-255
               X=(VNY*IBUF(IPF+K)/127.0)*ID(IDPTR+4)
               Y=X*FACT
               KOUT(KST+J)=NINT(Y)
               K=K+1
 450        CONTINUE
         ELSE IF (NAMFLD.EQ.'ZDR') THEN
C
C     ZDR: S-BAND REFL. RATIO IN DB
C
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
C
C     DETERMINE SCALING FACTOR FOR CP2/RP6
C
            SCL=1.0
            IF (IBUF(62).EQ.6) THEN
               ISCL=ICEDAND(IBUF(247),12)
               ISCL=ICEDSHFT(ISCL,-2)
               SCL=3.0
               IF (ISCL.EQ.1) SCL=6.0
               IF (ISCL.EQ.2) SCL=12.0
               IF (ISCL.EQ.3) SCL=24.0
               SCL=SCL/127.0
            END IF
            DO 455 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 455
               IF(J.GT.NGFLD) GO TO 455
               IF (IBUF(IPF+K).GE.128) IBUF(IPF+K)=IBUF(IPF+K)-255
               X=IBUF(IPF+K)*ID(IDPTR+4)*SCL
               Y=X*FACT
               KOUT(KST+J)=NINT(Y)
               K=K+1
 455        CONTINUE
         ELSE IF (NAMFLD.EQ.'SPECW') THEN
C     
C     SPECTRAL WIDTH: CONVERT FREQUENCY COUNTS TO M/S
C     PRIMARY WAVELENGTH, HORIZONTAL POLARIZATION
C     COUNTS FROM 0 TO 255 COVER VNYQ/SQRT(3) RANGE
C     
            READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(NAMFLD)
C     
C     
            FACT=1.0
            ID(IDPTR+2)=0
            IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               ENDIF
            ENDIF
            IPF=(IBUF(65)+1)+(N-1)*IBUF(15)
            NGFLD=IBUF(15)
            K=IUNPAD
            ID(IDPTR+4)=100
            DO 460 J=1,NRG
               KOUT(KST+J)=IBAD
               IF (J.LT.J1 .OR. J.GT.J2) GOTO 460
               IF(J.GT.NGFLD) GO TO 460
               X=(0.57735*VNY*IBUF(IPF+K)/255.0)*ID(IDPTR+4)
               Y=X*FACT
               KOUT(KST+J)=NINT(Y)
               K=K+1
 460        CONTINUE
         END IF
      END IF
      
      
      RETURN
      
      END
      
      
