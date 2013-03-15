c
c----------------------------------------------------------------------X
c
      SUBROUTINE SAVVAD(KRD,VADTYPE,NAMPLVD,NAMFLD,JVD,NVD,ZMNVD,ZMXVD,
     X     ISKPVD,AZMVD,WFILT,XMNVD,XMXVD,XSCLVD,XREFVD,TYPVD,NVDMX,
     X     MXVD)
C
C  READ PLTVAD STACK FOR VAD OUTPUT SCATTERGRAM CHARACTERISTICS
C
C     Parameters that apply to the whole PLTVAD command.
C
C     VADTYPE     - TYPE OF VAD ANALYSIS (VAD or COV) TO BE PLOTTED
C     NAMPLVD     - NAME OF VAD ANALYSIS               "  "    "
C     ZMNVD,ZMXVD - MINIMUM AND MAXIMUM HEIGHT (KM)    "  "    "
C     ISKPVD      - HEIGHT SKIPPING FACTOR
C     U_VD        - AMOUNT TO SUBTRACT FROM U COMPONENT
C     V_VD        -    "    "     "      "  V     "
C     AZMVD       - AZIMUTH ANGLE OF +U-COMPONENT OF WINDS
C     WFILT       - WIDTH OF VERTICAL FILTER (KM)
C     JVD         - NUMBER OF VAD ANALYSES             "  "    "
C                   =1 for Cedric.
C
C     Individual plots within the PLTVAD command.
C
C     NVD         - Number of individual plots within a PLTVAD frame,
C     NVDMX       - Maximum number of scatter-plots within a single
C                   PLTVAD command (6).
C     NAMFLD      - Names of VAD field to be plotted.   Usually 
C                   ordered as U0, VO, SPD, DIR, CON, and ERR.
C     XMNVD,XMXVD - MINIMUM AND MAXIMUM VALUE OF THE ABSCISSA FIELD
C     XREFVD      - REFERENCE VALUE
C     XSCLVD      - SCALING FACTOR (PLOT VALUE*XSCL)
C     TYPVD       - TYPE OF PLOT: 'SCAT', 'LINE', OR 'BOTH'
C
      CHARACTER*8 KRD(10)
      CHARACTER*8 NAMFLD(NVDMX),NAMPLVD(MXVD)
      CHARACTER*4 TYPVD(NVDMX,MXVD),VADTYPE(MXVD)
      DIMENSION XMNVD(NVDMX,MXVD),XMXVD(NVDMX,MXVD)
      DIMENSION XSCLVD(NVDMX,MXVD),XREFVD(NVDMX,MXVD)
      DIMENSION ZMNVD(MXVD),ZMXVD(MXVD),ISKPVD(MXVD)
      DIMENSION U_VD(MXVD),V_VD(MXVD),AZMVD(MXVD),WFILT(MXVD)

      JVD=JVD+1
      WRITE(6,11)(KRD(I),I=2,10)
 11   FORMAT(10X,'PVAD : ',9A8)
      READ(KRD,13)VADTYPE(JVD),NAMPLVD(JVD),ZMNVD(JVD),ZMXVD(JVD),
     +     ZSKIP,U_VD(JVD),V_VD(JVD),AZMVD(JVD),WFILT(JVD)
 13   FORMAT(/A4,4X/A8/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(ZSKIP.LE.0.0)THEN
         ISKPVD(JVD)=1
      ELSE
         ISKPVD(JVD)=ZSKIP
      END IF
      
C     READ FIELD NAMES, MIN VALUES, MAX VALUES UNTIL END-OF-STACK
C     FOR A SINGLE PLOT OF VAD RESULTS.  NVD will be the number
C     of individual plots within a single PLTVAD command.
C     
      NVD=0
 20   CALL KARDIN(KRD)
      IF(KRD(1)(1:1).EQ.'*')GO TO 20

      IF(KRD(1).EQ.'END     ')THEN
         DO N=1,NVD
            WRITE(6,23)N,NAMFLD(N),XMNVD(N,JVD),XMXVD(N,JVD),
     +           XREFVD(N,JVD),XSCLVD(N,JVD),TYPVD(N,JVD)
 23         FORMAT(8X,'#',I2,' X AXIS: NAM,FMN-MX,REF,SCL= ',
     +      A8,4F8.2,4X,A4)
         END DO
         RETURN
      END IF

      IF(KRD(1).NE.'        ')THEN
         WRITE(6,25)
 25      FORMAT(1X,'*** SAVVAD: NO END LINE ENCOUNTERED ***')
         STOP
      END IF
      NVD=NVD+1
      IF(NVD.GT.NVDMX)THEN
         WRITE(6,27)NVDMX
 27      FORMAT(1X,'*** SAVVAD: NO. PLOTS EXCEEDS',I3,' - STOP ***')
         STOP
      END IF
      READ(KRD,29)NAMFLD(NVD),XMNVD(NVD,JVD),XMXVD(NVD,JVD),
     +     XREFVD(NVD,JVD),XSCLVD(NVD,JVD),TYPVD(NVD,JVD)
 29   FORMAT(/A8/F8.0/F8.0/F8.0/F8.0/A4,4X)
      IF(XSCLVD(NVD,JVD).EQ.0.0)XSCLVD(NVD,JVD)=1.0
      IF(TYPVD(NVD,JVD).NE.'SCAT' .AND. TYPVD(NVD,JVD).NE.'LINE' .AND.
     +     TYPVD(NVD,JVD).NE.'BOTH')TYPVD(NVD,JVD)='BOTH'

      GO TO 20
      END
