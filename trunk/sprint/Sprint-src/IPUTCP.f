      SUBROUTINE IPUTCP(IWRD,IAZ,IX,IY)
C
C     Packs azimuth of (X,Y) grid along with JI-indices for sorting 
C     (SINSRT) the grid locations into monotonically increasing order.
C
C     CALL SBYTES (PACKED,UNPACKED,NOFF,NBITS,NSKIP,ITER)
C
C     IWRD (Bit 1)      = Sign bit (don't use, pack it with 0)
      CALL SBYTES(IWRD,0,0,1,0,1)

C------------------------------------------------------------------
C     Note: Azimuth scaling controlled by UNSCAZ=32 set in INITAL.f
C           Integer scaled azimuth = unscaz*float(azimuth)
C     IWRD (Bits 02-16) = Scaled azimuth (15 bits => max Iaz = 32767)
C     IWRD (Bits 17-24) = Y index (8 bits => max Ny = 255)
C     IWRD (Bits 25-32) = X index (8 bits => max Nx = 255)
C     
      CALL SBYTES(IWRD,IAZ,1,15,0,1)
      CALL SBYTES(IWRD,IY,16,8,0,1)
      CALL SBYTES(IWRD,IX,24,8,0,1)

C------------------------------------------------------------------
C     Note: Azimuth scaling controlled by UNSCAZ=20 set in INITAL.f
C           Integer scaled azimuth = unscaz*float(azimuth)
C     IWRD (Bits 02-14) = Scaled azimuth (13 bits => max Iaz = 8191)
C     IWRD (Bits 15-23) = Y index (9 bits => max Ny = 511)
C     IWRD (Bits 24-32) = X index (9 bits => max Nx = 511)
C     
c      CALL SBYTES(IWRD,IAZ,1,13,0,1)
c      CALL SBYTES(IWRD,IY,14,9,0,1)
c      CALL SBYTES(IWRD,IX,23,9,0,1)

      RETURN
      END

