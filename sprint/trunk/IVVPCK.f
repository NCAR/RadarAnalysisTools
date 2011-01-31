      SUBROUTINE IVVPCK(IVVAR,IDIM,CNT,SUM,SUMSQ)
C
C     PACKS VARIANCE INFORMATION INTO A 64-BIT WORD
C
C-----SUBROUTINE SBYTES (NPACK,ISAM,IBIT,NBITS,NSKIP,ITER)
C
C     This subroutine packs bits from the input array ISAM to the
C     output array NPACK.  After skipping "IBIT" bits in NPACK,
C     the "NBITS" rightmost bits from "ITER" successive words
C     in "ISAM" are packed into "NPACK" with "NSKIP" bits between each
C     moved block.
C
C
C     NPACK
C       Address of the first word of the array to be packed.
C
C     ISAM
C       Array to be packed into NPACK.  The right-most NBITS
C       bits of each word will be packed.  ISAM should be
C       dimensioned for at least ITER.
C
C     IBIT
C       A bit-count offset to be used before the first bits are
C       packed into NPACK.  For example, if IBIT=3 and NBITS=5,
C       3 bits in NPACK will be skipped before the right-most
C       5 bits of ISAM(1) are packed into it.
C
C     NBITS
C       The number of bits in each word of ISAM to be unpacked.
C       NBITS must not exceed the word size on the given machine.
C
C     NSKIP
C       The number of bits to skip in NPACK between packing each
C       bit chunk from ISAM.
C
C     ITER
C       The number of bit chunks to be packed.
      
      DIMENSION IVVAR(IDIM)
      INTEGER CVMGM

c-----write(6,*)'IVVPCK: ivvar,cnt,sum,sumsq=',ivvar,cnt,sum,sumsq

      KCNT=CNT*128.0
      KSUM=SUM*16.0
      IF (SUMSQ.GT.134217725.) STOP 802
      KSUMSQ=SUMSQ*16.0
      IF(KSUM.LT.-32767.OR.KSUM.GT.32767) STOP 801
      KSUM=CVMGM(KSUM+65536,KSUM,KSUM)

      CALL SBYTES(IVVAR,KSUMSQ,0,32,0,1)
      CALL SBYTES(IVVAR,KSUM,32,16,0,1)
      CALL SBYTES(IVVAR,KCNT,48,16,0,1)

C     IVVAR=ICEDOR(ICEDSHFT(  KSUM,16),KCNT)
C     IVVAR=ICEDOR(ICEDSHFT(KSUMSQ,32),IVVAR)

      RETURN
      END
