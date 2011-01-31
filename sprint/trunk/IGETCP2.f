      SUBROUTINE IGETCP2(IWRD,IAZ,IX,IY)
C
C     Unpacks azimuth of (X,Y) grid along with JI-indices after two 
C     beams have been read (BEAMIN).  All (X,Y) grid locations between 
C     these two beams are interplated, then a new beam is read.
C     Integer scaled azimuth and indices were packed in IPUTCP.
C     
C     CALL GBYTES (PACKED,UNPACKED,NOFF,NBITS,NSKIP,ITER)
C
C     IWRD (Bit 1)      = Sign bit (never used - see IPUTCP)
C      so skip it when unpacking.  AZ unpack has an offset of
C      one bit, followed by unpacking of 13 (15) bits.
C
C      DATA MASK08,MASK16/ O'377', O'177777'/
C      IX =ICEDAND(IWRD,MASK08)
C      IY =ICEDAND(ICEDSHFT(IWRD,-8),MASK08)
C      IAZ=ICEDAND(ICEDSHFT(IWRD,-16),MASK16)

C------------------------------------------------------------------
C     Note: Azimuth scaling controlled by UNSCAZ=32 set in INITAL.f
C           Integer scaled azimuth = unscaz*float(azimuth)
C     IWRD (Bits 02-16) = Scaled azimuth (15 bits => max Iaz = 32767)
C     IWRD (Bits 17-24) = Y index (8 bits => max Ny = 255)
C     IWRD (Bits 25-32) = X index (8 bits => max Nx = 255)
C     
      CALL GBYTES(IWRD,IAZ,1,15,0,1)
      CALL GBYTES(IWRD,IY,16,8,0,1)
      CALL GBYTES(IWRD,IX,24,8,0,1)

C------------------------------------------------------------------
C     Note: Azimuth scaling controlled by UNSCAZ=20 set in INITAL.f
C           Integer scaled azimuth = unscaz*float(azimuth)
C     IWRD (Bits 02-14) = Scaled azimuth (13 bits => max Iaz = 8191)
C     IWRD (Bits 15-23) = Y index (9 bits => max Ny = 511)
C     IWRD (Bits 24-32) = X index (9 bits => max Nx = 511)
C     
c      CALL GBYTES(IWRD,IAZ,1,13,0,1)
c      CALL GBYTES(IWRD,IY,14,9,0,1)
c      CALL GBYTES(IWRD,IX,23,9,0,1)

      RETURN
      END








