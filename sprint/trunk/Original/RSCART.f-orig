      SUBROUTINE RSCART(ICART,MAXPLN,NRCBF,ICTAB,KZBM,IDS,JL,IC,IHI,
     X                  IBKNT,KZOUT,IS360,NST,IABOVE)
C
C     Looks like this routine likely returns pointers to first output
C     grid point within the radar scan.  IC seems to be the first grid
C     point in the horizontal grid.  LJM 12/11/98.
C
      LOGICAL IS360
      LOGICAL DEBUGIT
      DIMENSION ICART(MAXPLN),IBKNT(4),ICTAB(4)

      DEBUGIT = .FALSE.
      IABOVE=1
      ITST=(ICTAB(JL+2)-KZBM)*IDS
      if(debugit)write(8,*)'RSCART: jl,ids,kzbm,ictab,itst=',
     +     jl,ids,kzbm,ictab(jl+2),itst
      IF(ITST.GE.0) GO TO 10
C
C        NO MATCHUP BETWEEN DATA AND CARTESIAN GRID
C
      if(debugit)write(8,*)'RSCART before go to 701: is360=',is360
      IF(.NOT.IS360) GO TO 701
      if(debugit)write(8,*)'RSCART  after go to 701: is360=',is360
   10 CONTINUE
      IBKNT(1)=1
      IBKNT(2)=0
      IBKNT(3)=NRCBF
      IBKNT(4)=0
      IF(IDS.GT.0) THEN
         IC=1
         IHI=1
      ELSE
         IC=ICTAB(2)
         IHI=NRCBF
      END IF
      ISTRT=IC
   30 CONTINUE
      CALL IGETCP2(ICART(IC),KZCART,IX,IY)
      ITST=(KZCART-KZBM)*IDS
      IF(ITST.GE.0) GO TO 40
      IC=IC+IDS
      IF(IC.LE.0.OR.IC.GT.ICTAB(2)) THEN
         IF(IS360) THEN
            IC=ISTRT
            CALL IGETCP2(ICART(IC),KZCART,IX,IY)
            GO TO 40
         ELSE
            if(debugit)write(8,*)'RSCART: ic,ictab(2)=',ic,ictab(2)
            GO TO 701
         END IF
      END IF
      GO TO 30
   40 CONTINUE
      KZOUT=KZCART
      NST=0
      RETURN

  701 CONTINUE
      write(8,*)'RSCART: calling TPQERX(306)'
      NST=1
      CALL TPQERX(306,0)
      IABOVE=0
      RETURN
      END





