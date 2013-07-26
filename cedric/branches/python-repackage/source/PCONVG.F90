!>
!! CALCULATES THE CONVERGENCE USING U,V COMPONENTS
!! NDER CONSECUTIVE VALUES MUST BE PRESENT TO COMPUTE
!! THE PARTIAL ALONG A LINEAR AXIS. PARTIAL WILL
!! BE PRESENT AT NDER LOCATIONS SINCE EDGE WEIGHTING
!! IS PERFORMED.
!! @param U,V [nx,ny] input horizontal wind field
!! @param CONV [nx,ny] ouput convergence
!! @param NX,NY grid size of input/output fields
!! @param NDER number of point to evaluate the derivative
!! @param XYDELI [2] reciprocal of DX,DY
!! @param BAD fill value of input/output field
!!
!! MODIFIED TO PERFORM EDGE WEIGHTING-- C. MOHR 8/87

SUBROUTINE PCONVG(U, V, CONV, NX, NY, NDER, XYDELI, BAD)
    implicit none
    integer,intent(in) :: NX,NY,NDER
    real,intent(in)    :: BAD, XYDELI(2), U(NX,NY), V(NX,NY)
    real,intent(out)   :: CONV(NX,NY)

!! python wrapping
!!
!f2py integer,optional,intent(in),check(shape(U,0)==NX),depend(U) :: NX=shape(U,0)
!f2py integer,optional,intent(in),check(shape(U,1)==NY),depend(U) :: NY=shape(U,1)
!f2py integer,intent(in) :: NDER
!f2py real,intent(in) :: U(NX,NY), V(NX,NY), XYDELI(2), BAD
!f2py real,intent(out),depend(NX,NY) :: CONV(NX,NY)

    integer KR,KL,MPOS,IDX,IGO,M,NPTS,IP,IDEL,I,J,K,II,JJ,KHI,NDMAX,KLO
    real :: sum,KPOS(5),WX(5,5,5)
    DATA KPOS/0,1,2,0,3/
    DATA WX/25*0.,-1.,1.,0.,0.,0.,-1.,1.,0.,0.,0.,15*0.,&
        -1.5,2.,-.5,0.,0.,-.5,0.,0.5,0.,0.,0.5,-2.,1.5,0.,0.,&
        35*0.,&
        -2.08333333,4.,-3.,1.333333,-0.25,&
        -0.25,-0.8333333,1.5,-0.5,0.08333333,&
        0.08333333,-0.6666667,0.,0.6666667,-0.08333333,&
        -0.08333333,0.5,-1.5,0.8333333,0.25,&
        0.25,-1.333333,3.,-4.,2.08333333/
    DATA NDMAX/5/

    CONV(:,:) = 0.
    !PARTIAL ACROSS THE X-DIMENSION
    DO JJ=1,NY
        J=JJ
        KLO=0
        DO II=1,NX
            I=II
            IF(U(I,J)/=BAD) THEN
                IF(KLO==0) KLO=I
                IF(I/=NX) CYCLE
                I=NX+1
            END IF
            IF(KLO==0) THEN
                CONV(I,J) = BAD
                CYCLE
            END IF

            KHI=I-1
            K=I-KLO
            IF(K<=2) THEN
                !        MUST BE AT LEAST 3 CONSEC. PTS.
                CONV(KLO,J)=BAD
                CONV(KHI,J)=BAD
            ELSE
                !        GOT AT LEAST 3, 5 FALLS BACK TO 3 IF NDER=5.
                NPTS=MIN0(K,NDER)
                IF(NPTS==4)NPTS=3
                IDEL=NPTS-KPOS(NPTS)
                DO M=KLO,KHI
                    KL=M-KLO
                    KR=KHI-M
                    IF(KL<IDEL) THEN
                        !              LEFT  OF CENTER WEIGHTING
                        MPOS=KL+1
                    ELSE IF(KR<IDEL) THEN
                        !              RIGHT OF CENTER WEIGHTING
                        MPOS=NPTS-KR
                    ELSE
                        !              CENTER WEIGHTING
                        MPOS=KPOS(NPTS)
                    END IF
                    IGO=M-MPOS
                    SUM=0.0
                    DO IP=1,NPTS
                        IDX=IP+IGO
                        SUM=SUM+U(IDX,J)*WX(IP,MPOS,NPTS)
                    enddo
                    SUM=SUM*XYDELI(1)
                    CONV(M,J)=SUM
                enddo
            END IF
            KLO=0
        enddo
    enddo

    !     PARTIAL ACROSS THE Y-DIMENSION
    DO II=1,NX
        I=II
        KLO=0
        DO JJ=1,NY
            J=JJ
            IF(V(I,J)/=BAD) THEN
                IF(KLO==0)KLO=J
                IF(J/=NY) CYCLE
                J=NY+1
            END IF
            IF(KLO==0) THEN
                CONV(I,J) = BAD
                CYCLE
            END IF
            KHI=J-1
            K=J-KLO
            IF(K<=2) THEN
                !                                        MUST BE AT LEAST 3 CONSEC. PTS.
                CONV(I,KLO)=BAD
                CONV(I,KHI)=BAD
            ELSE
                !                                        GOT AT LEAST 3, 5 FALLS BACK TO
                !                                        3 IF NDER=5.
                NPTS=MIN0(K,NDER)
                IF(NPTS==4)NPTS=3
                IDEL=NPTS-KPOS(NPTS)
                DO M=KLO,KHI
                    KL=M-KLO
                    KR=KHI-M
                    IF(KL<IDEL) THEN
                        !                                        LEFT  OF CENTER WEIGHTING
                        MPOS=KL+1
                    ELSE IF(KR<IDEL) THEN
                        !                                        RIGHT OF CENTER WEIGHTING
                        MPOS=NPTS-KR
                    ELSE
                        !                                              CENTER WEIGHTING
                        MPOS=KPOS(NPTS)
                    END IF
                    IGO=M-MPOS
                    SUM=0.0
                    DO IP=1,NPTS
                        IDX=IP+IGO
                        SUM=SUM+V(I,IDX)*WX(IP,MPOS,NPTS)
                    END DO
                    IF(CONV(I,M)/=BAD) CONV(I,M)= - (CONV(I,M)+(SUM*XYDELI(2)))
                END DO
            END IF
            KLO=0
        enddo
    enddo
END SUBROUTINE PCONVG
