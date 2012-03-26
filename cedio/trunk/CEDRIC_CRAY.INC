C
C     INCLUDE FILE FOR CEDRIC ON CRAY
C     NOTE: ON 32 BIT MACHINES, MAXBUF MUST BE 4000000
C
C     INMEM   -INDICATES WHETHER THE PROGRAM WILL BE RUN WITH THE EDIT
C              VOLUME IN MEMORY (=1) OR ON DISK(=0)
C     MAXLCM  -FOR IN MEMORY, MAXLCM SHOULD BE SET TO 2E06 * (64/WORDSZ).
C              FOR ON DISK, THIS SHOULD BE SET TO 1. THIS CONTROLS THE
C              SIZE OF AN INTERNAL ARRAY THAT STORES THE DATA IN A
C              PACKED FORMAT.
C     WORDSZ  -WORDSIZE OF MACHINE. ONLY 32 AND 64 BIT ARE CURRENTLY SUPPORTED.
C     MAXBUF  -SHOULD BE SET TO (2E06 * (64/WORDSZ)). CONTROLS SOME 
C              INTERNAL ADDRESSING.
C
C     MAXX    -Maximum number of grid points in X-direction
C     MAXY    -Maximum number of grid points in Y-direction
C     MAXZ    -Maximum number of grid points in Z-direction
C     MAXFLD  -Maximum number of fields
C     MAXPLN  -Maximum number of grid points in an XY-plane (MAXX*MAXY)
C     NID     -Number of words in the CEDRIC volume header
C     MAXF    -Maximum number of fields for putting into FDAT 
C              from an input CEDRIC file (see driver_read and driver_rdwrt)
C
      PARAMETER (INMEM=1, MAXLCM=2000000, WORDSZ=64, MAXBUF=2000000)
      PARAMETER (MAXX=256,MAXY=256,MAXZ=20,MAXFLD=25)
      PARAMETER (MAXPLN=MAXX*MAXY,NID=510)
      PARAMETER (MAXF=6)
