      SUBROUTINE IVVUNP(IVVAR,IDIM,CNT,SUM,SUMSQ)
C
C     UNPACKS VARIANCE INFORMATION FROM A 64-BIT WORD
C
C-----SUBROUTINE GBYTES (NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C     NPACK
C       Address of the first word of the array to be unpacked.
C
C     ISAM
C       Array to receive the unpacked bit chunks.  They will be
C       right-justified with zero-fill in this array.  ISAM
C       should be dimensioned for ITER.
C
C     IBIT
C       A bit-count offset to be used before the first bit chunk is
C       unpacked.  For example, if IBIT=3 and NBITS=5, then
C       3 bits in NPACK will be skipped and the next 5 bits
C       will be unpacked into ISAM(1).
C
C     NBITS
C       The number of bits in each bit chunk to be unpacked.
C
C     NSKIP
C       The number of bits to skip between each bit chunk to be
C       unpacked (after the first bit chunk has been unpacked.)
C
C     ITER
C       The number of bit chunks to be unpacked.

      DIMENSION IVVAR(IDIM)
      DATA MASK16/O'177777'/
      INTEGER CVMGP

C      KCNT=ICEDAND(IVVAR,MASK16)
C      KSUM=ICEDAND(ICEDSHFT(IVVAR,-16),MASK16)
C      KSUM=CVMGP(KSUM-65536,KSUM,KSUM-32768)
C      KSUMSQ=ICEDSHFT(IVVAR,-32)

      CALL GBYTES(IVVAR,KSUMSQ,0,32,0,1)
      CALL GBYTES(IVVAR,KSUM,32,16,0,1)
      CALL GBYTES(IVVAR,KCNT,48,16,0,1)

      
      KSUM=CVMGP(KSUM-65536,KSUM,KSUM-32768)
      CNT=KCNT*0.0078125
      SUM=KSUM*0.0625
      SUMSQ=KSUMSQ*0.0625

c-----write(6,*)'IVVUNP: cnt,sum,sumsq=',cnt,sum,sumsq

      RETURN
      END
