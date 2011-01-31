      SUBROUTINE UFREC(IB,ITER)
C
C     ASSIGN SOME BASIC HOUSEKEEPING FROM UNPACKED RECORD.
C     Borrowed code from RDUF routine within PPI program.
C
C     CALLED FROM UFNCAR:  IBUF --> IB
C                          NWCT --> ITER
      DATA IPREC/0/
      DIMENSION IB(ITER)

      IPREC=IPREC+1

      DO 50 I=1,ITER
         IF(IB(I).GT.32767)IB(I)=IB(I)-65536
 50   CONTINUE

      IDATE=IB(26)*10000+IB(27)*100+IB(28)
      IF(IDATE.GT.991231)THEN
         JDATE=1000000*(IDATE/1000000)
         IDATE=IDATE-JDATE
      END IF
      ITIME=IB(29)*10000+IB(30)*100+IB(31)
      ISEC=3600*IB(29)+60*IB(30)+IB(31)

C     OBTAIN SOME BASIC HOUSEKEEPING INFORMATION
C
      IDATH = IB(5)
      NRF   = IB(6)
      IVOL  = IB(7)
      IRY   = IB(8)
      IRC   = IB(9)
      ISWP  = IB(10)
      ALAT  = FLOAT(IB(19)) + IB(20)/60. + IB(21)/(64.*3600.)
      ALON  = FLOAT(IB(22)) + IB(23)/60. + IB(24)/(64.*3600.)
      IYR   = IB(26)
      IMON  = IB(27)
      IDAY  = IB(28)
      IBAD  = IB(45)
      AZ    = IB(33)/64.
      EL    = IB(34)/64.
      AZR   = AZ
      ELR   = EL
      IF(EL.GT.180.0)EL=EL-360.0
      ITP   = IB(35)
      FXANG = IB(36)/64.
      NFH   = IB(IDATH+4)
      RMIN  = FLOAT(IB(NFH+2))+0.001*FLOAT(IB(NFH+3))
      IGSP  = IB(NFH+4)
      NRNG  = IB(NFH+5)
      IRN   = RMIN
      IRX   = RMIN+0.001*(NRNG-1)*IGSP+0.5

      WRITE(6,773)IDATE,ITIME,AZR,ELR,FXANG,IRN,IRX,IGSP,
     +     VNYQ,IVOL,NRF,ISWP,N64,IRC,ITP,NAZZ,IPREC
 773  FORMAT(1X,'D=',I6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +     ' F=',F5.1,' R=',I3,'-',I4,' Gs=',I4,' Ny=',F5.1,
     +     ' Vl=',I3,' Vr=',I5,' Sw=',I4,' Ln=',I4,' Rr=',I1,
     +     ' Md=',I1,' Na=',I4,' Tr=',I5)

      RETURN
      END

