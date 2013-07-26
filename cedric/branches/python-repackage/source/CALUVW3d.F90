!> Two or three radar wind synthesis, distances are all KMs 
!! @param NX,NY,NZ 3D grid number of each axis
!! @param NVAR number of input variables. For ground-base radar only radial velocity is necessary for 'inbuf' array(radial velocity has to be the first array if more than one variable is passed), 
!! but center of each radar has to be set in 'RC' argument. While for airborne radar, except for radial velocity,
!! azimuth, elevation have to be passed in as the 3rd,4th array in 'INBUF'. The 2nd array is reserved for dBZ, but is not used for now.
!! azimuth and elevation are in radian.
!! @param NRADS number of radars
!! @param INBUF [nx,ny,nz,NVAR,NRADS] input data
!! @param OUTBUF [nx,ny,nz,8] output data
!! @param RC [3,NRADAR] coordinates (xr,yr,zr) of each radar. For airborne radar, this argument is ignored. For ground-based radar 
!! this argument is necessary.
!! @param GRIDINFO [3,3] grid information GRIDINFO(1,1)=X0, GRIDINFO(2,1)=DX, GRIDINFO(1,2)=Y0, GRIDINFO(2,2)=DY
!! @param VTEST [3] thresholds for U, V, W. Default value for each is: 1.0, 100., 100.
!! @param ITEQN 1:two-equations solution 0: try three-equations solution when data allows
!! @param WEIGHT [NRADS] weighting of data from each radar
!! @param IMOVING [NRADS] if volume is on moving platform
!! @param KEEPW,KEEPPE flag to output auxilirry data
!! @param FILLV fill_value of input and output data

SUBROUTINE CALUVW3D(NX, NY, NZ, NVAR, NRADS, INBUF, OUTBUF, RC, GRIDINFO,&
    VTEST, ITEQN, WEIGHT, IMOVING, KEEPW, KEEPPE, FILLV)
    !        CALCULATES THE U,V,W COMPONENTS FOR 2 OR MORE DOPPLER RADARS
    implicit none
    integer,intent(in) :: NX, NY, NZ, NVAR, NRADS, ITEQN, IMOVING(NRADS)
    logical,intent(in) :: KEEPW, KEEPPE

    real,intent(in)    :: FILLV, WEIGHT(NRADS), VTEST(3), RC(3,NRADS), GRIDINFO(3,3)
    real,intent(in)    :: INBUF(NX,NY,NZ,NVAR,NRADS)
    real,intent(out)   :: OUTBUF(NX, NY, NZ, 8)

    !! python wrapping created by Xiaowen Tang
    !!
    !f2py integer, optional, intent(in), check(shape(INBUF,0)==NX), depend(INBUF) :: NX = shape(INBUF,0)
    !f2py integer, optional, intent(in), check(shape(INBUF,1)==NY), depend(INBUF) :: NY = shape(INBUF,1)
    !f2py integer, optional, intent(in), check(shape(INBUF,2)==NZ), depend(INBUF) :: NZ = shape(INBUF,2)
    !f2py integer, optional, intent(in), check(shape(INBUF,3)==NVAR), depend(INBUF) :: NVAR = shape(INBUF,3)
    !f2py integer, optional, intent(in), check(shape(INBUF,4)==NRADS), depend(INBUF) :: NRADS = shape(INBUF,4)
    !f2py real, intent(in) :: INBUF(NX,NY,NZ,NVAR,NRADS)
    !f2py real, intent(out), depend(NX,NY,NZ) :: OUTBUF(NX,NY,NZ,8)
    !f2py real, intent(in) :: RC(3, NRAD), GRIDINFO(3,3)
    !f2py real, intent(in) :: VTEST(3)
    !f2py integer, intent(in) :: ITEQN, IMOVING(NRADS)
    !f2py logical, intent(in) :: KEEPW, KEEPE
    !f2py real, intent(in) :: FILLV

    !     local variables   
    real XR,YR,ZR,WT,VR,R,DT2,DT3,Eu,Ev,U,V,W,UA,VA,WA,VRBAD, &
        T1,T2,T3,P1,P2,P3,Q1,Q2,Q3,SUMWT,VRSAM,PEV

    LOGICAL IUNITY,ITWO
    integer IDENO, NEQN, N, M, I, J, KK
    real    X1, XD, Y1, YD, Z1, ZD, X, Y, Z, AZ, EL, VASAV, UASAV, WASAV
    real    A(3),B(3),C(3),D(3)
    real,dimension(NRADS) ::  XRSAV,YRSAV,ZRSAV,WTSAV,VRSAV   
    real,parameter :: EPS  = 1.E-8

!     OUTBUF(:,:,:,:) = FILLV
    NEQN=3-ITEQN
    X1=GRIDINFO(1,1)
    XD=GRIDINFO(3,1)
    Y1=GRIDINFO(1,2)
    YD=GRIDINFO(3,2)
    Z1=GRIDINFO(1,3)
    ZD=GRIDINFO(3,3)
!     print *,'CALUVW: nrads=',nrads
!     print *, 'VMIN',MINVAL(INBUF(:,:,:,1,1)),MINVAL(INBUF(:,:,:,1,2))
!     print *, 'VMAX',MAXVAL(INBUF(:,:,:,1,1)),MAXVAL(INBUF(:,:,:,1,2))
!     print *, 'VMIN',MINVAL(INBUF(:,:,:,2,1)),MINVAL(INBUF(:,:,:,2,2))
!     print *, 'VMAX',MAXVAL(INBUF(:,:,:,2,1)),MAXVAL(INBUF(:,:,:,2,2))
!     print *, 'VMIN',MINVAL(INBUF(:,:,:,3,1)),MINVAL(INBUF(:,:,:,3,2))
!     print *, 'VMAX',MAXVAL(INBUF(:,:,:,3,1)),MAXVAL(INBUF(:,:,:,3,2))
!     print *, 'VMIN',MINVAL(INBUF(:,:,:,4,1)),MINVAL(INBUF(:,:,:,4,2))
!     print *, 'VMAX',MAXVAL(INBUF(:,:,:,4,1)),MAXVAL(INBUF(:,:,:,4,2))       
!     print *, 'X0',X1,'DX',XD
!     print *, 'Y0',Y1,'DY',YD
!     print *, 'Z0',Z1,'DZ',ZD
!     print *, 'VTEST',VTEST
!     print *, 'ITEQN',ITEQN
!     print *, 'WEIGHT',WEIGHT
!     print *, 'IMOVE', IMOVING
!     print *, 'KEEPW', KEEPW
!     print *, 'KEEPE', KEEPPE
!     print *, 'FILLV', FILLV

    ! CALCULATE THE U,V,W COMPONENTS OF THIS COMBINATION
    DO KK=1,NZ
        Z=Z1+(KK-1)*ZD
        Y=Y1-YD
        DO J=1,NY
            Y=Y+YD
            X=X1-XD
            DO I=1,NX
                X=X+XD
                ITWO  = .FALSE.
                IUNITY= .FALSE.
                M=0

                DO N=1,NRADS
                    IF(INBUF(I,J,KK,1,N)==FILLV .OR. WEIGHT(N)==FILLV .OR. WEIGHT(N)==0.0) CYCLE
                    ! airborne radar, extra test for az, el
                    IF(IMOVING(N)==1) THEN
                        IF(INBUF(I,J,KK,3,N)==FILLV .OR. INBUF(I,J,KK,4,N)==FILLV) CYCLE
                    END IF
                    M=M+1
                END DO

                if(M>1 .OR. NRADS==2) then
                    IF(M<=3) IUNITY=.TRUE.

                    ! force two-equation solution
                    IF(M==2 .OR. ITEQN/=0) ITWO=.TRUE.

                    ! INITIALIZE LEAST SQUARES EQUATION
                    SUMWT=0.0
                    A(:)=0.0
                    B(:)=0.0
                    C(:)=0.0
                    D(:)=0.0

                    ! LOOP ON EACH RADAR
                    M=0
                    VRBAD=0.0
                    DO N=1,NRADS
                        VR = INBUF(I,J,KK,1,N)
                        WT = WEIGHT(N)

                        IF(NRADS==2) THEN
                            ! FOR FORCED 2 RADAR SOLUTION, WANT TO CALCULATE GEO FACTORS EVERYWHERE.
                            ! THUS, EVEN IF ONE OR BOTH OF THE VRs ARE FILLV, CALCULATE THEM
                            IF (WT==FILLV .OR. WT==0.0) CYCLE
                            IF (VR==FILLV) VRBAD=1.0
                        ELSE
                            ! FOR THREE OR MORE RADAR POSSIBILITY, DON'T CALCULATE GEO FACTORS EVERYWHERE
                            IF(VR==FILLV .OR. WT==FILLV .OR. WT==0.0) CYCLE
                        END IF
                        IF(IUNITY) WT=1.0

                        IF(IMOVING(N)==1) THEN
                            !AIRBORNE DOPPLER
                            AZ  = INBUF(I,J,KK,3,N)
                            EL  = INBUF(I,J,KK,4,N)
                            IF(AZ==FILLV .OR. EL==FILLV) CYCLE
                            XR  = SIN(AZ)*COS(EL)
                            YR  = COS(AZ)*COS(EL)
                            ZR  = SIN(EL)
                        ELSE
                            XR  = X-RC(1,N)
                            YR  = Y-RC(2,N)
                            ZR  = Z-RC(3,N)
                            R   = SQRT(XR**2+YR**2+ZR**2)
                            IF(R<=EPS) CYCLE
                            XR  = XR/R
                            YR  = YR/R
                            ZR  = ZR/R
                        END IF
                        !
                        M=M+1
                        SUMWT   =SUMWT+WT
                        XRSAV(M)=XR
                        YRSAV(M)=YR
                        ZRSAV(M)=ZR
                        WTSAV(M)=WT
                        VRSAV(M)=VR

                        A(1)=A(1)+WT*XR*XR
                        B(1)=B(1)+WT*XR*YR
                        C(1)=C(1)+WT*XR*ZR
                        B(2)=B(2)+WT*YR*YR
                        C(2)=C(2)+WT*YR*ZR
                        C(3)=C(3)+WT*ZR*ZR

                        D(1)=D(1)+WT*VR*XR
                        D(2)=D(2)+WT*VR*YR
                        D(3)=D(3)+WT*VR*ZR
                    END DO
                end if


                U    = FILLV
                V    = FILLV
                W    = FILLV
                UASAV= FILLV
                VASAV= FILLV
                WASAV= FILLV
                Eu   = FILLV
                Ev   = FILLV

                IF(M>1 .AND. SUMWT>0.0 ) then
                    A(2)=B(1)
                    A(3)=C(1)
                    B(3)=C(2)

                    !NORMALIZE THE MATRIX EQUATION
                    A(:)=A(:)/SUMWT
                    B(:)=B(:)/SUMWT
                    C(:)=C(:)/SUMWT
                    D(:)=D(:)/SUMWT

                    P1 = B(2)*C(3)-B(3)*C(2)
                    P2 = B(1)*C(3)-B(3)*C(1) 
                    P3 = B(1)*C(2)-B(2)*C(1)
                    Q1 = A(3)*C(2)-A(2)*C(3)
                    Q2 = A(3)*C(1)-A(1)*C(3)
                    Q3 = A(2)*C(1)-A(1)*C(2)
                    T1 = A(2)*B(3)-A(3)*B(2)
                    T2 = A(1)*B(3)-A(3)*B(1)
                    T3 = A(1)*B(2)-A(2)*B(1)

                    if(ITWO) then 
                        ! two equations solution
                        DT2=A(1)*B(2)-A(2)*B(1)
                        IF(ABS(DT2)>EPS) then
                            Eu=P3/DT2
                            Ev=Q3/DT2
                            ! CALCULATE VARIANCE OF TWO EQUATION U,V ESTIMATES
                            UA=0.0
                            VA=0.0
                            DO N=1,M
                                UA=UA+(WTSAV(N)/SUMWT*(B(2)*XRSAV(N)-B(1)*YRSAV(N))/DT2)**2
                                VA=VA+(WTSAV(N)/SUMWT*(A(1)*YRSAV(N)-A(2)*XRSAV(N))/DT2)**2                 
                            END DO
                            UA=SQRT(UA)
                            VA=SQRT(VA)
                            UASAV=UA
                            VASAV=VA
                            IF (VRBAD/=1.0) then
                                IF(ABS(Eu)<VTEST(1) .AND. ABS(Ev)<VTEST(1) .AND. UA<VTEST(2) .AND. VA<VTEST(2)) THEN
                                    U=(D(1)*B(2)-D(2)*B(1))/DT2
                                    V=(D(2)*A(1)-D(1)*A(2))/DT2
                                END IF
                            endif
                        endif
                    else
                        ! 3 OR MORE RADAR SOLUTION
                        DT3=C(1)*T1-C(2)*T2+C(3)*T3
                        IF(ABS(DT3)>EPS) then
                            UA=0.0
                            VA=0.0
                            WA=0.0
                            DO N=1,M
                                UA=UA+(WTSAV(N)/SUMWT*(P1*XRSAV(N)-P2*YRSAV(N)+P3*ZRSAV(N))/DT3)**2
                                VA=VA+(WTSAV(N)/SUMWT*(Q1*XRSAV(N)-Q2*YRSAV(N)+Q3*ZRSAV(N))/DT3)**2
                                WA=WA+(WTSAV(N)/SUMWT*(T1*XRSAV(N)-T2*YRSAV(N)+T3*ZRSAV(N))/DT3)**2
                            END DO
                            UA=SQRT(UA)
                            VA=SQRT(VA)
                            WA=SQRT(WA)
                            WASAV=WA
                            IF(UA<VTEST(2) .AND. VA<VTEST(2) .AND. WA<VTEST(3)) then
                                UASAV=UA
                                VASAV=VA
                                U=(D(1)*P1-D(2)*P2+D(3)*P3)/DT3
                                V=(D(1)*Q1-D(2)*Q2+D(3)*Q3)/DT3
                                W=(D(1)*T1-D(2)*T2+D(3)*T3)/DT3
                            end if
                        end if
                    end if
                endif

                !  W FIELD IS BEING CALCULATED
                IF(KEEPW) THEN
                    OUTBUF(I,J,KK,1)=U
                    OUTBUF(I,J,KK,2)=V
                    OUTBUF(I,J,KK,3)=W
                    OUTBUF(I,J,KK,4)=0.
                    OUTBUF(I,J,KK,5)=UASAV
                    OUTBUF(I,J,KK,6)=VASAV
                    OUTBUF(I,J,KK,7)=WASAV
                ELSE
                    OUTBUF(I,J,KK,1)=U
                    OUTBUF(I,J,KK,2)=V
                    OUTBUF(I,J,KK,3)=0.
                    OUTBUF(I,J,KK,4)=UASAV
                    OUTBUF(I,J,KK,5)=VASAV
                    OUTBUF(I,J,KK,6)=Eu
                    OUTBUF(I,J,KK,7)=Ev
                END IF

                ! CALCULATE MOST PROBABLE ERROR
                IF(KEEPPE) THEN
                    PEV=FILLV
                    IDENO=M-NEQN
                    IF(ITEQN/=0) W=0.0
                    ! GOOD VALUES AT THIS LOCATION
                    IF(IDENO>0 .AND. U/=FILLV .AND. V/=FILLV .AND. W/=FILLV) THEN
                        SUMWT=0.0
                        DO N=1,M
                            R=SQRT(XRSAV(N)**2+YRSAV(N)**2+ZRSAV(N)**2)
                            VRSAM=(U*xrsav(n)+V*yrsav(n)+W*zrsav(n))
                            SUMWT=SUMWT+(VRSAV(N)-VRSAM)**2
                        END DO
                        PEV=SQRT(SUMWT/IDENO)
                    END IF
                    OUTBUF(I,J,KK,8)=PEV
                END IF
            end do
        end do
    enddo

end subroutine CALUVW3D
