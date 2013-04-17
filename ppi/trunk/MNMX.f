c
c----------------------------------------------------------------------X
c
      SUBROUTINE MNMX(DRR)
C
C  DETERMINE RANGE LIMITS OF PLOT WINDOW
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL

      REAL NSMIN,NSMAX

      if(drr.le.0.0)then
         print *,'*** MNMX - something is wrong: drr .le. 0 ***'
         print *,'           itpold,drr=',itpold,drr
c         stop
         drr=0.250 
         print *,'*** MNMX - reset drr ***'
         print *,'           itpold,drr=',itpold,drr
      end if

C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C
      IF(ISW.EQ.2)THEN
         R1=RMNSW
         NRNG=NRSW
      ELSE
         R1=R0
         NRNG=NGTS
      END IF

      write(6,1770)itpold,xmin(itpold),xmax(itpold),
     +     ymin(itpold),ymax(itpold)
 1770 format(2x,'Itp=',i1,' XmnXmx=',2f8.2,' YmnYmx=',2f8.2)
      write(6,1771)isw,rn,rx,r1,r2,drr,nrng,mngate,mxgate,itergt
 1771 format(2x,'Isw=',i1,' Rglim: rn,rx,r1,r2,dr,ng,mn,mx,iter=',
     +     5f10.3,4i6)
      IF(IBSCAN.NE.1)THEN
         IF(ITPOLD.NE.3)THEN
            CALL RNGLIM(XMIN(ITPOLD),XMAX(ITPOLD),YMIN(ITPOLD),
     +      YMAX(ITPOLD),RX,RN)
            MNGATE=MAX0(NINT((RN-R1)/(DRR*COS(FXOLD*0.0174533))-
     +      ITERGT),1)
            MXGATE=MIN0(NINT((RX-R1)/(DRR*COS(FXOLD*0.0174533))+
     +      ITERGT),NRNG)
         ELSE
            CALL RNGLIM(XMIN(ITPOLD),XMAX(ITPOLD),YMIN(ITPOLD),
     +      YMAX(ITPOLD),RX,RN)
            MNGATE=MAX0(NINT((RN-R1)/DRR)-ITERGT,1)
            MXGATE=MIN0(NINT((RX-R1)/DRR)+ITERGT,NRNG)
         END IF
      ELSE
         MNGATE=MAX0(NINT((RNGMIN-R1)/DRR),1)
         MXGATE=MIN0(NINT((RNGMAX-R1)/DRR),NRNG)
      END IF

c-----debug (ljm)
      r2=r1+(nrng-1)*drr
      write(6,1770)itpold,xmin(itpold),xmax(itpold),
     +     ymin(itpold),ymax(itpold)
      write(6,1771)isw,rn,rx,r1,r2,drr,nrng,mngate,mxgate,itergt
c-----debug (ljm)
      if(drr.le.0.0)stop

      RETURN
      END




