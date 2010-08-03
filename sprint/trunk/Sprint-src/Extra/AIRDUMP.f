      SUBROUTINE AIRDUMP(IUN,IVOL,IYR,IMON,IDAY,IHR,IMIN,ISEC,MSEC,
     X     NUMRADS,ITP,NFLDDAT,NUMFREQ,NUMIPP,NRNG,ISWP,JULDAY,
     X     IRYSTAT,RADAR_TYPE,REQTIME,RADCON,SCANRATE,ALON,ALAT,
     X     VNYDAT,RNGMX,RMIN,GATSPAC,AZ,EL,ALTMSL,PRESALT,ALTGND,
     X     GNDSPDEW,GNDSPDNS,VERVEL,HEADING,ROLL,PITCH,DRIFT,ROTANG,
     X     TILT,UAIR,VAIR,WAIR,HEDCHGRT,PITCHGRT,BAD,FXANG,RADNAM,
     X     PROJ_NAME,FLTNUM)

C     DUMPS DETAILED HOUSEKEEPING FROM THE CURRENT BEAM
C
C     IUN - Current Fortran unit number (sweep file)
C     IVOL,ISWP - Volume and Sweep numbers
C     JULDAY - Julian Day
C     IYR,IMON,IDAY - Julian Day, Year, month, and day
C     IHR,IMIN,ISEC,MSEC - Hour, minute, second, millisec
C     NUMRADS,RADNAM,PROJ_NAME,FLTNUM - 
C     ITP - scan mode returned from RDBEAM
C
C        ITP < 0: Invalid scan mode
C        ITP = 0: Calibration
C        ITP = 1: PPI (constant elevation)
C        ITP = 2: Coplane
C        ITP = 3: RHI (constant azimuth)
C        ITP = 4: Vertical pointing
C        ITP = 5: Target (stationary)
C        ITP = 6: Manual
C        ITP = 7: Idle (out of control)
C        ITP = 8: Full sweep (surveilance?)
C        ITP = 9: Partial sweep (airborne?)
C        ITP > 9: Unknown scan mode
C
C     NFLDDAT - Number of fields in the present elevation scan.
C     NUMFREQ,NUMIPP,RADCON,VNYDAT-
C     NRNG,RNGMX,RMIN,GATSPAC - slant range information
C     GNDSPDEW,GNDSPDNS,VERVEL - Platform ground and vertical speeds
C     HEADING,ROLL,PITCH,DRIFT,ROTANG,TILT - aircraft angles 
C     ALTMSL,PRESALT,ALTGND - Various heights 
C     AZ,EL,FXANG - Normal radar angles
C     UAIR,VAIR,WAIR - Air motion at the aircraft
C     HEDCHGRT,PITCHGRT,BAD -
C     IRYSTAT,REQTIME,SCANRATE,ALON,ALAT,
C
C     RADAR_TYPE - radar type returned from RDBEAM
C
C        RADAR_TYPE < 0: Unknown radar type
C        RADAR_TYPE = 0: Ground based
C        RADAR_TYPE = 1: Airborne for
C        RADAR_TYPE = 2: Airborne aft
C        RADAR_TYPE = 3: Airborne tail
C        RADAR_TYPE = 4: Airborne lower fuselage
C        RADAR_TYPE = 5: Shipborne
C        RADAR_TYPE > 5: Unknown radar type
C     NFLDDAT - Number of fields in the present elevation scan.

      INTEGER RADAR_TYPE
      CHARACTER*8 RADNAM,FLTNUM
      CHARACTER*4 PROJ_NAME

      RETURN
      END
