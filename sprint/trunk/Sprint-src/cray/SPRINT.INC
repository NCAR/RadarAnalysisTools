C
C     INCLUDE FILE FOR SPRINT COMPILATION
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     WORDSZ - Computer word size (either 64 or 32 set in SPRINT.INC)
C     INTSZ  - Word size used for interpolated field values (16)
C     IDIM   - First dimension of ICOB used in memory allocation for 
C              packing/unpacking INTSZ words into computer words
C              IDIM=64/WORDSZ
C     MAXWRD - Maximum number of INTSZ words contained in a computer word
C              MAXWRD=IDIM*WORDSZ/INTSZ
C     IDIM2  - Second dimension of ICOB used in memory allocation for 
C              packing/unpacking INTSZ words into computer words (4)
C     MAXFLD - Maximum number of input or output fields.
C              MAXFLD=IDIM2*MAXWRD
C     MXFLD  - Maximum number of fields set in dorade.h and nexrad.h
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     MXCRT  - Maximum number of grid points along any one axis (256)
C     MAXPLN - Maximum number of grid points in a plane (MXCRT*MXCRT)
C     MAXZ   - Maximum number of interpolated levels (10)
C     MAXYZ  - Maximum number of grid points in a volume
C              MAXYZ=MAXPLN*MAXZ
C     MAXRNG - Maximum number of input range gates
C     MXGAT  -Maximum number of gates set in dorade.h and nexrad.h
C     MAXEL  - Maximum number of input sweeps (scans)
C     NID    - Maximum storage needed in ID for sweep information
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C             ICOB(IDIM,IDIM2,MAXYZ) is used to set aside enough memory 
C             for packed MAXFLD field values, each with length INITSZ.
C             Memory allocated with ICOB(IDIM,IDIM2) is (IDIMxIDIM2) 
C             32-bit words or MAXFLD 16-bit words.  
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C     NRCBF  - Used to allocate memory for scratch arrays
C     IVDIM  - Used to allocate memory for scratch arrays
C     NIOB   - Maximum blocksize for writing (reading) to (from) Sprint's
C              internal disk file containing input radar data
C     MAXIN  - Maximum buffer size needed for writing (reading) to (from)
C              Sprint's internal disk file 
C     MAXLEN - Maximum (buffer size)/4
C     BDVAL  - Missing data flag
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
C              Other known parameters not included here, but set in 
C              routines.	    
C     NKOM   - Maximum number of commands allowed
C     MAXSKP - Maximum number of entries allowed for fixed table (FXTABLE)
C     MXCNT  - Maximum number of entries allowed in calibration file
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
      PARAMETER (WORDSZ=64,INTSZ=16)
      PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
      PARAMETER (IDIM2=4,MAXFLD=IDIM2*MAXWRD)
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
      PARAMETER (MXCRT=256,MAXPLN=MXCRT*MXCRT,MAXZ=10,MAXYZ=MAXPLN*MAXZ)
      PARAMETER (MAXRNG=1024,MAXEL=150,NID=129+3*MAXEL)
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
      PARAMETER (NRCBF=400000,IVDIM=(NRCBF+1)/(WORDSZ/8))
      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
      PARAMETER (BDVAL=-32768.)
C-----*-------*-------*-------*-------*-------*-------*-------*-------*-------
