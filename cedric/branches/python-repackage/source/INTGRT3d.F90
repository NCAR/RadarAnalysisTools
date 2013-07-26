SUBROUTINE INTGRT3D(conv,ww,rho_pro,zh,nx,ny,nz,dzh,imethod,alpha,bnd_value,bad)
    implicit none
    integer,intent(in) :: nx,ny,nz,imethod
    real,intent(in)    :: dzh, alpha, bnd_value, bad, zh(nz), rho_pro(nz), conv(nx,ny,nz)
    real,intent(out)   :: ww(nx,ny,nz)

!! python wrapping created by Xiaowen Tang
!!
!f2py integer, optional, intent(in), check(shape(conv,0)==NX), depend(conv) :: NX = shape(conv,0)
!f2py integer, optional, intent(in), check(shape(conv,1)==NY), depend(conv) :: NY = shape(conv,1)
!f2py integer, optional, intent(in), check(shape(conv,2)==NZ), depend(conv) :: NZ = shape(conv,2)
!f2py integer, intent(in) :: imethod
!f2py real, intent(in) :: dzh, alpha, bnd_value, bad, zh(nz), rho_pro(nz), conv(nx,ny,nz)
!f2py real, intent(out):: ww(nx,ny,nz)

    integer kbegin,loop,i,j,k,idir,L
    real :: plev,pfac,zlev,rnum
    real :: bnd_flag(nx,ny),rho_scale(nx,ny),ww_scale(nx,ny)

    print *, 'CONV',MINVAL(conv),MAXVAL(conv)
    print *, 'rho_pro',MINVAL(rho_pro),MAXVAL(rho_pro)
    print *, 'zh',MINVAL(zh),MAXVAL(zh)      
    print *, 'dzh',dzh
    print *, 'imethod',imethod
    print *, 'alpha', alpha
    print *, 'bnd_value', bnd_value
    print *, 'bad', bad

    if(1==imethod .OR. 3==imethod) then
        idir = 1
        kbegin=1
    else 
        idir = -1
        kbegin=nz
    endif


    !  set boundary type      
    ww(:,:,kbegin) = BAD
    bnd_flag(:,:)  = BAD
    DO J=1,NY
        DO I=1,NX
            IF(conv(I,J,kbegin)/=BAD) THEN
                bnd_flag(I,J)=zh(kbegin)
                ww(I,J,kbegin)=bnd_value
            ENDIF
        enddo
    enddo

    !  LOOP FOR EACH REMAINING LEVEL TO BE INTEGRATED
    PFAC= rho_pro(kbegin)/rho_pro(kbegin+idir)

    k = kbegin-idir
    DO LOOP=1,NZ
        k = k+IDIR
        ZLEV= zh(k)

        IF(k==kbegin) CYCLE
        DO J=1,NY
            DO I=1,NX
                IF(conv(I,J,K)==BAD) THEN
                    !               ! convergence of current layer is missing
                    ww(I,J,K) = BAD
                else if( ww(I,J,K-idir)/=BAD ) then
                    !               ! convergence of current layer and w(convergence of last layer) is valid
                    !               ! if w is valid, convergence of last layer must always be valid
                    ww(I,J,K) = ww(I,J,K-idir)*PFAC + DZH*(conv(I,J,K-idir)*PFAC+conv(I,J,K))
                else if( bnd_flag(I,J)/=BAD ) then
                    !               ! convergence of current layer is valid, but w(convergence of last layer) is not
                    !               ! boundary has been initialized 
                    ww(I,J,K)=BAD
                else
                    !               ! convergence of current layer is valid, but w(convergence of last layer) is not
                    !               ! boundary has not been initialized
                    ww(I,J,K) = bnd_value
                    bnd_flag(I,J)=zh(k)
                endif
            enddo
        enddo
    END DO


    !  VARIATIONAL ADJUSTMENT-- PASS DOWNWARDS AND MODIFY INTEGRAL
    !  no horizontal information is used
    !  RBUF(:, 3) W estimation from upward integration
    !  RBUF(:, 4) boundary height of upward integration
    !  RBUF(:, 5) boundary value

    IF(3==imethod) THEN
        rho_scale = 0.0
        ww_scale  = BAD

        !        LOOP FOR EACH LEVEL REQUESTED
        L = NZ+1
        DO LOOP=1,NZ
            L=L-1
            ZLEV=zh(L)
            PLEV=rho_pro(L)

            DO J=1,NY
                DO I=1,NX
                    IF(ww(I,J,L)==BAD .OR. ZLEV<bnd_flag(I,J)) CYCLE
                    IF(rho_scale(I,J)==0.0) THEN
                        !                 UNITIALIZED variational integration path --CHECK FOR UPPER BOUNDARY
                        IF(bnd_flag(I,J)==ZLEV) THEN
                            !                    FLAG SINGLETON CONVERGENCE VALUE OR NO UPPER BOUNDARY CONDITION AT THIS LEVEL
                            !                       (THIS CAN ONLY HAPPEN WITH FIELD INIT)
                            ww(I,J,L)=BAD
                        ELSE
                            !                 GENERATE DENOMINATOR OF THE SCALING TERM --RBUF(-,1)
                            !                          AND THE ( WT -WB -WK ) TERM     --RBUF(-,2)
                            !                 SET RESULT TO UPPER BOUNDARY AND BRANCH TO LOOP END
                            rho_scale(I,J) = 1./(EXP(-2.0*ALPHA*(ZLEV-bnd_flag(I,J)))-1.0)
                            ww_scale(I,J)  = (bnd_value-ww(I,J,L))*PLEV
                            ww(I,J,L)      = bnd_value
                        END IF
                    ELSE IF(bnd_flag(I,J)/=ZLEV) THEN
                        !                 COMPUTE THE NUMERATOR OF THE SCALING TERM AND MODIFYTHE VALUE OF THE INTEGRAL
                        RNUM=EXP(-2.0*ALPHA*(ZLEV-bnd_flag(I,J)))-1.0
                        IF(rho_scale(I,J)/=BAD) THEN
                            ww(I,J,L) = ww(I,J,L)+(rho_scale(I,J)*ww_scale(I,J)*RNUM/PLEV)
                        ELSE
                            ww(I,J,L) = BAD
                        END IF
                    END IF
                enddo
            enddo
        enddo
    END IF
end subroutine intgrt3d
