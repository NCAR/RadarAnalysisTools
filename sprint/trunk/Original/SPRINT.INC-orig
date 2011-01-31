C
C     INCLUDE FILE FOR SPRINT COMPILATION
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     WORDSZ - Computer word size (either 64 or 32 set in SPRINT.INC)
C     INTSZ  - Word size used for interpolated field values (16)
C     IDIM   - First dimension of ICOB used in memory allocation for 
C              packing/unpacking INTSZ words into computer words
C              IDIM=64/WORDSZ
C     MAXWRD - Maximum number of INTSZ words contained in a computer word
C              MAXWRD=IDIM*WORDSZ/INTSZ=(64/WORDSZ)*WORDSZ/INTSZ=64/INTSZ
C     IDIM2  - Second dimension of ICOB used in memory allocation for 
C              packing/unpacking INTSZ words into computer words (5).
C              A free parameter which directly controls the number of
C              fields that can be processed.
C     MAXFLD - Maximum number of input or output fields.
C              MAXFLD=IDIM2*MAXWRD=IDIM2*IDIM*WORDSZ/INTSZ=IDIM2*64/INTSZ
C
C     MXFLD  - Maximum number of input nexrad fields set in nexrad.h
C              Maximum number of input dorade fields set in dorade.h
C     MXFNEX - Maximum number of input nexrad fields set in NEXVOL and NEXSWP
C     MXFDOR - Maximum number of input dorade fields set in DORVOL and DORSWP
C
C     MXGAT  - Maximum number of input nexrad gates set in nexrad.h
C              Maximum number of input dorade gates set in dorade.h
C     MXGNEX - Maximum number of input nexrad gates set in NEXVOL and NEXSWP
C     MXGDOR - Maximum number of input dorade gates set in DORVOL and DORSWP
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     MXCRT  - Maximum number of grid points along any one axis (255)
C              Packing of the scaled integer azimuth along with the (y,x)
C              indices into KOUT in routine IPUTCP now requires that the
C              azimuth occupy the upper 2-14 bits, and the (j,i) indices
C              occupy the next two 9-bit chunks.  This places an absolute
C              upper bound of 255 x 255 grid points with an azimuth scaling
C              of UNSCAZ=20 as set in INITAL.f (ljm).
C              IPUTCP-SINSRT-IGETCP2 comprise the three pack-sort-unpack 
C              routines that must be consistent for the Sprint sorting
C              of the (x,y) grid into increasing azimuths to work.
C     MAXPLN - Maximum number of grid points in a plane (MXCRT*MXCRT)
C     MAXZ   - Maximum number of interpolated levels (35)
C     MAXYZ  - Maximum number of grid points in a volume
C              MAXYZ=MAXPLN*MAXZ
C     MAXRNG - Maximum number of input range gates
C     MAXEL  - Maximum number of input sweeps (scans)
C     NID    - Maximum storage needed in ID for sweep information
C     MAXSKP - Maximum number of entries allowed for fixed table (FXTABLE)
C     MXCNT  - Maximum number of entries allowed in calibration file
C     IDPTR  - Starting address in ID for storing field name (IDPTR,IDPTR+1),
C              Radar constant or Nyquist (IDPTR+2), Scale factor (IDPRT+4).
C              Starts at 76 and increments by 5 for each field.
C     IDPTR_INT - Initial value of IDPTR(=76)
C     IPTR   - Starting address in ID for storing beam location information:
C              Three values stored for the current beam:
C                 Airborne     - mean track, mean drift, and number of beams
C                 Ground-based - fxang, scan direction, and number of beams
C     IPTR_INT - Initial value of IPTR = IDPTR_INT + 5*MAXFLD
C              Originally this was hardwired to 129.
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C             ICOB(IDIM,IDIM2,MAXYZ) is used to set aside enough memory 
C             for packed MAXFLD field values, each with length INITSZ.
C             Memory allocated with ICOB(IDIM,IDIM2) is (IDIMxIDIM2) 
C             32-bit words or MAXFLD 16-bit words.  
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     NRCBF  - Used to allocate memory for scratch arrays
C     IVDIM  - Used to allocate memory for scratch arrays
C     MAXIN  - Maximum buffer size needed for writing (reading) to (from)
C              Sprint's internal disk file.  Ten housekeeping words plus
C              MAXFLD*MAXRNG field values.
C     NIOB   - Maximum blocksize   needed for writing (reading) to (from) 
C              Sprint's internal disk file containing input radar data.  
C              Same as MAXIN.
C     MAXLEN - Maximum (buffer size)/4
C     BDVAL  - Missing data flag
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C              Other known parameters not included here, but set in 
C              routines.	    
C     NKOM   - Maximum number of commands allowed
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
      PARAMETER (WORDSZ=32,INTSZ=16)
      PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
      PARAMETER (IDIM2=4,MAXFLD=IDIM2*MAXWRD)
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
      PARAMETER (MXCRT=255,MAXPLN=MXCRT*MXCRT,MAXZ=35,MAXYZ=MAXPLN*MAXZ)
      PARAMETER (IDPTR_INT=76,IPTR_INT=IDPTR_INT+5*MAXFLD)
      PARAMETER (MAXRNG=1040,MAXEL=150,NID=IPTR_INT+3*MAXEL)
      PARAMETER (MAXSKP=27,MXCNT=500)
      PARAMETER (MXGDOR=1040,MXFDOR=20)
      PARAMETER (MXGNEX=1837,MXFNEX=3)
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
      PARAMETER (NRCBF=900000,IVDIM=(NRCBF+1)/(WORDSZ/8))
      PARAMETER (MAXIN=10+MAXRNG*MAXFLD,MAXLEN=MAXIN/4,NIOB=MAXIN)
c      PARAMETER (NIOB=85000)
      PARAMETER (BDVAL=-32768.)
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     See BEAMIN, DORSWP, DORVOL, NEXSWP, NEXVOL, RPNCAR, UFNCAR
C     Note:  Elevation angles are actually scaled by JRH7 in DORSWP and NEXSWP
C            and by ID(44) in RPNCAR and UFNCAR ==> angles are scaled by 100,
C            not 1000 in the 510-word header min/max [ID(170-171)]
C            This is done before the scaling by 1000 in HEDSET. Bummer!
C     JRH6 - Angle scaling factor
C     JRH7 - General scaling factor
C     IBAD - Integer bad (missing) value
C     JRH8 - Scaling factor for near-vertical axis (Z, E, or C)
C            LJM - not implemented yet until impacts elsewhere
C
C     Common block JUNK to allow variables to be initialized in the 
C     main program UFCART.
      COMMON /JUNK/LFTIM,JRH6,JRH7,JRH8,IBAD,DEBUG
      LOGICAL DEBUG
C     DEBUG  - Logical variable for turning on debugging printout to unit 8
C              Non-critical debugging statements are commented out.
C     See BEAMIN, CRTOUT, DOFILT, RDRYDK, TRPD, TRPVOL, TRPPPI, UFCART
C
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
