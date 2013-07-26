!>     ITERATIVE REFINEMENT OF WC AT A LEVEL TO MINIMIZE THE
!!     DIFFERENCE BETWEEN ADJUSTED VALUES OF THE HORIZONTAL CONVERGENCE
!!     AND VERTICAL CONVERGENCE COMPUTED USING THE VALUE OF WC (CALLED WP)
!!     FROM THE PREVIOUS LEVEL.
!!     
!!     @param EUC U ERROR TERM FROM THE SYNTHESIS
!!     @param EUP U ERROR TERM FROM PREVIOUS LEVEL
!!     @param EVC V   "    "    "    "     "
!!     @param EVP V   "    "    "    "     "
!!     @param CVGC CONVERGENCE AT CURRENT LEVEL
!!     @param CVGP CONVERGENCE AT PREVIOUS LEVEL
!!     @param WC W ESTIMATE AT THIS LEVEL   +++ MODIFIED BY THIS ROUTINE +++
!!     @param WP W FROM PREVIOUS LEVEL +++ MODIFIED BY THIS ROUTINE +++

!!     @param NX NUMBER OF POINTS ALONG X-AXIS
!!     @param NY NUMBER OF POINTS ALONG Y-AXIS
!!     @param XYDELI INVERSE OF HORIZONTAL AXES SPACINGS (X AND Y IN KM)
!!     @param DZH (1./DELZ) * SIGN (DIRECTION OF INTEGRATION)
!!     @param RHOC DENSITY AT CURRENT LEVEL
!!     @param RHOP    "     " PREVIOUS  "

!!     @param C2RD USER SPECIFIED ITERATION CONTROLS:
!!       1- TARGET DIFFERENCR BETWEEN CONSECUTIVE ITERATIONS;
!!       2- MAXIMUM NUMBER OF ITERATIONS
!!     @param BAD MISSING DATA FLAG
!!     @param ITER ACTUAL NUMBER OF ITERATIONS              (OUTPUT)
!!     @param DMN ACTUAL DIFFERENCE BETWEEN ITERATIONS        "
!!     @param KST STATUS FLAG:                                "
!!       0- CONVERGED
!!       1- DIVERGED OR MAXED OUT ON ITERATIONS
!!       2- NO DATA THIS LEVEL

SUBROUTINE DWITER(EUP,EUC,EVP,EVC,CVGP,CVGC,WP,WC,&
     NX,NY,XYDELI,DZH,RHOC,RHOP,C2RD,BAD,ITER,DMN,KST)
  implicit none    
    
  integer,intent(in)                  :: NX,NY
  integer,intent(out)                 :: KST,ITER
  real,intent(in)                     :: XYDELI(2),C2RD(2),DZH,RHOC,RHOP,BAD
  real,intent(out)                    :: DMN
  real,dimension(NX,NY),intent(in)    :: EUC ,EVC ,CVGC ,CVGP ,EUP ,EVP,WP
  real,dimension(NX,NY),intent(inout) :: WC

  !     Local
  integer I, J, JC, JM1, JP1, IM1, IP1, IC, ITMAX, NQUAD, MINPTS, ITRMAX, NYM1, NXM1
  real DSQR, DMPREV, DMAX, DERR, DGDY, CNT, CXFAC, CYFAC, ERRMAX, DFDX, DMIN, WCVG, WITER
  real DPDX,DPDY,DCDX,DCDY
  real WS(NX,NY)

  DATA ITMAX,NQUAD,MINPTS/5,0,15/

  !     PARAMETERIZATION FOR BOUNDED DATA FILL
  !     
  !     
  ITER  = 1
  ERRMAX= C2RD(1)
  ITRMAX= C2RD(2)
  NXM1  = NX-1
  NYM1  = NY-1
  DMPREV= 1.E+8
  CXFAC = -0.5*XYDELI(1)
  CYFAC = -0.5*XYDELI(2)

  WC(:,:) = BAD
  ! DETERMINE WHICH LOCATIONS ARE GOOD AND SET WC=0.0 THERE
  DO J=2,NYM1
     JM1=J-1
     JP1=J+1
     DO I=2,NXM1
        WC(I,J)=0.0
        if( ANY((/WC(I,J), WP(I,J), EUC(I,J), EUP(I,J), EVC(I,J), EVP(I,J), CVGC(I,J),CVGP(I,J)/)==BAD) ) then
           WC(I,J)=BAD
        endif

        IF(WC(I,J)/=BAD) THEN
           DO JC=JM1,JP1,2
              IF(ANY((/WP(I,JC), EUC(I,JC), EUP(I,JC), EVC(I,JC), EVP(I,JC)/)==BAD)) then
                 WC(I,J) = BAD
                 exit
              endif
           ENDDO
        endif
        if(WC(I,J)/=BAD) then
           IM1=I-1
           IP1=I+1
           DO IC=IM1,IP1,2
              IF(ANY((/WP(IC,J), EUC(IC,J), EUP(IC,J), EVC(IC,J), EVP(IC,J)/)==BAD)) then
                 WC(I,J) = BAD
                 exit
              endif
           ENDDO
        endif
     enddo
  enddo

  WS(:,:) = 0.
  do while(.True.)
     ! ITERATION LOOP
     CNT=0.0
     DMN=0.0
     DSQR=0.0
     DMAX=1.0E-35
     DMIN=1.0E+35
     DO J=2,NYM1
        DO I=2,NXM1
           IF(WC(I,J)==BAD) CYCLE
           DPDX  = (EUP(I+1,J)*RHOP*WP(I+1,J)-EUP(I-1,J)*RHOP*WP(I-1,J))*CXFAC
           DPDY  = (EVP(I,J+1)*RHOP*WP(I,J+1)-EVP(I,J-1)*RHOP*WP(I,J-1))*CYFAC
           DCDX  = (EUC(I+1,J)*RHOC*WS(I+1,J)-EUC(I-1,J)*RHOC*WS(I-1,J))*CXFAC
           DCDY  = (EVC(I,J+1)*RHOC*WS(I,J+1)-EVC(I,J-1)*RHOC*WS(I,J-1))*CYFAC
           WCVG  = DPDX+DPDY+DCDX+DCDY

!            DFDX  = ((EUP(I+1,J)*RHOP*WP(I+1,J)+EUC(I+1,J)*RHOC*WS(I+1,J))-(EUP(I-1,J)*RHOP*WP(I-1,J)+EUC(I-1,J)*RHOC*WS(I-1,J)))*CXFAC
!            DGDY  = ((EVP(I,J+1)*RHOP*WP(I,J+1)+EVC(I,J+1)*RHOC*WS(I,J+1))-(EVP(I,J-1)*RHOP*WP(I,J-1)+EVC(I,J-1)*RHOC*WS(I,J-1)))*CYFAC
!            WCVG  = DFDX+DGDY

           WITER = WP(I,J)*RHOP+ DZH*(RHOC*CVGC(I,J)+RHOP*CVGP(I,J)+WCVG)
           WITER = WITER/RHOC

           CNT =CNT+1.0
           DERR=ABS(WC(I,J)-WITER)
           DMN =DMN+DERR
           IF (ABS(DERR)<DMIN) DMIN=ABS(DERR)
           IF (ABS(DERR)>DMAX) DMAX=ABS(DERR)
           DSQR=DSQR+DERR**2
           WC(I,J)=WITER
        enddo
     enddo

     ! TEST CONVERGENCE
     IF(CNT==0.0) then
        KST =2
        exit
     endif

     DMN =DMN/CNT
     DSQR=DSQR/CNT
     DSQR=DSQR-DMN**2.0
     DSQR=SQRT(DSQR)
     IF((DMN<=ERRMAX .AND. ITRMAX>0)  .OR. (DMN<=ERRMAX .AND. ITRMAX<0 .AND. ITER>=IABS(ITRMAX)))  then
        kst = 0
        exit
     endif

     IF(ITER>=ABS(ITRMAX) .OR. ((DMN-DMPREV)>=ERRMAX .AND. ITRMAX>0) ) then
        KST = 1
     endif

     ITER=ITER+1
     DMPREV=DMN
     DO J=2,NYM1
        DO I=2,NXM1
           IF(WC(I,J)/=BAD) WS(I,J)=WC(I,J)
        ENDDO
     ENDDO
  enddo



  !     FINISHED WITH ITERATIONS
  if(KST==0 .OR. KST==1) then
     !     
     !     FILL IN W-VALUES AT DATA BOUNDARIES, WS IS DECISION FILED FOR BNDFIL

     WS(:,:) = 0.
     DO J=1,NY
        DO I=1,NX
           IF(WC(I,J)/=BAD) CYCLE
           IF(ANY((/WP(I,J), EUC(I,J), EUP(I,J), EVC(I,J), EVP(I,J), CVGC(I,J), CVGP(I,J)/)==BAD)) CYCLE
           !     
           !     WE WANT ESTIMATE OF W HERE
           !     
           WS(I,J)=BAD
           !     
        enddo
     enddo

     !     AT THIS POINT DATA FROM PREVIOUS
     !     LEVEL ARE NO LONGER NEEDED ---
     !     USE FOR DATA FILL
     !     
     CALL BNDFIL(WC,WC,WS,NX,NY,1,NX,1,NY,ITMAX,NQUAD,MINPTS,BAD)
  endif

END SUBROUTINE DWITER
