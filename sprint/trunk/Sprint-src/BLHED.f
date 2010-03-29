      SUBROUTINE BLHED(CVAL,K,NFLDS,NXY,LHED,LHD8,LHD9,SF,VNYQUIST)
C
C     Builds 10 word level header for Cartesian (XYZ), Coplane (XYC),
C     Elevation (XYE) or Lon-Lat (LLE or LLZ) coordinate systems.  
C     Packed to 16-bit words in OUTPCK.
C
C     Note: For Coplane ('COPL') and Elevation ('ELEV') CEDRIC 
C           uses this value as third coordinate for unequally spaced data
C           rather than generating it from C or E min/max and spacing.
C           Level coordinate is scaled by 1000 (not SF) to be consistent
C           with Z-coordinates in HEDSET.  Currently, output is always
C           for "z-planes" (see IREORD in BLKDAT and ordering in CARTAP)
C           CARTAP calls OUTPCK with I3=3 --> ICAX
C
C           NEXRADs have different Nyquist velocities at different elevations.
C
C     Since elevation angles can exceed 16-bit representation
C     when doing GRIDPPI, the scaling here needs to be changed.
C     This will also be true for CEDRIC's FETCHZ and PLANST.
C
C        LHED(4) - Coordinate of current level * 1000
C        LHED(5) - Index of current level
C        LHED(6) - Number of fields
C        LHED(7) - Number of grid points per plane (NX*NY)
C        LHED(8) - Number of records per field
C        LHED(9) - Number of records per plane
C        LHED(10)- Nyquist velocity
C
      DIMENSION LHED(10)
 
      CHARACTER*2 CTEMP
      DATA IBIT,NBITS,NSKIP/48,16,0/
      DATA ZSCALE/1000./
 
      WRITE(CTEMP,10)'LE'
      READ(CTEMP,10)LHED(1)
 10   FORMAT(A2)
      WRITE(CTEMP,10)'VE'
      READ(CTEMP,10)LHED(2)
      WRITE(CTEMP,10)'L '
      READ(CTEMP,10)LHED(3)

C     Since elevation angles can exceed 16-bit representation
C     when doing GRIDPPI, the scaling here needs to be changed.
C     LJM - temporarily changed in sprint and cedric on Alpha.
C
      LHED(4)=CVAL*ZSCALE

      LHED(5)=K
      LHED(6)=NFLDS
      LHED(7)=NXY
      LHED(8)=LHD8
      LHED(9)=LHD9
      LHED(10)=VNYQUIST*100.0
c-----debug (ljm)
c      write(6,1700)k,nflds,nxy,cval,vnyquist
c 1700 format('Blhed: k,nflds,nxy,zlvl,nyq=',3I8,2F8.3)
c-----debug (ljm)
      RETURN
      END



