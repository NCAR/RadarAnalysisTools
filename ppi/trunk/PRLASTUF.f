c
c----------------------------------------------------------------------X
c
      SUBROUTINE PRLASTUF(ID,MXID,NAZZ,IPREC,N64,VNYQ)
C
C  UNIVERSAL FORMAT:
C     PRINTS HOUSKEEPING FROM THE PREVIOUS BEAM FOR END-OF-SWEEP
C
      DIMENSION ID(MXID)
      IDATH = ID(5)
      NRF   = ID(6)
      IVOL  = ID(7)
      IRY   = ID(8)
      IRC   = ID(9)
      ISWP  = ID(10)
      AZ    = ID(33)/64.0
      ELR   = ID(34)/64.0
      MODE  = ID(35)
      FXANG = ID(36)/64.0
      NF    = ID(IDATH)
      NFH   = ID(IDATH+4)
      RMIN  = FLOAT(ID(NFH+2))+0.001*FLOAT(ID(NFH+3))
      IGSP  = ID(NFH+4)
      NRNG  = ID(NFH+5)
      IF(ID(26).GT.99)ID(26)=ID(26)-100
      IDT   = ID(26)*10000+ID(27)*100+ID(28)
      IF(IDT.GT.992359)THEN
         JDT=1000000*(IDT/1000000)
         IDT=IDT-JDT
      END IF
      ITM   = ID(29)*10000+ID(30)*100+ID(31)
      IRN   = RMIN
      IRX   = RMIN+0.001*(NRNG-1)*IGSP+0.5
      IF(ELR.GT.180.0)ELR=ELR-360.0

C*****PATCHES FOR ELDORA - ROTATION ANGLE IN LOCAL USE HEADER BLOCK
C
c      LUHB  = ID(4)
c      ROT   = ID(LUHB+17)/64.
c      AZ    = ROT
C
      JPREC=IPREC-1
      WRITE(6,66)IDT,ITM,AZ,ELR,FXANG,IRN,IRX,IGSP,VNYQ,IVOL,
     +           NRF,ISWP,N64,IRC,MODE,NAZZ,JPREC
 66   FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +     ' F=',F5.1,' R=',I3,'-',I4,' Gs=',I4,' Ny=',F5.1,
     +     ' Vl=',I3,' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,
     +     ' Md=',I1,' Na=',I4,' Tr=',I5,' *')
      call sflush
      RETURN
      END




