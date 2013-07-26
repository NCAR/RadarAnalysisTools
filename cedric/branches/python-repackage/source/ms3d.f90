!> Three dimensional (compared to the original 2D rountine in CEDRIC) to 
!! param idir direction of integration. 1=up, -1=down
subroutine ms3d(u,v,eu,ev,conv,w,rho,nx,ny,nz,idir,bnd_value,dx,dy,dz,bad,C2RD)
    implicit none
    integer nx,ny,nz,idir,iter,kst
    real bnd_value,dx,dy,dz,bad,dmn,C2RD(2),rho(NZ),XYDELI(2)
    real,dimension(nx,ny,nz),intent(in)    :: eu,ev,conv
    real,dimension(nx,ny,nz),intent(inout) :: u,v
    real,dimension(nx,ny,nz),intent(out)   :: w

    integer I,J,K,loop,kbegin

!! python wrapping created by Xiaowen Tang
!!
!f2py integer, optional, intent(in), check(shape(u,0)==NX), depend(u) :: NX = shape(u,0)
!f2py integer, optional, intent(in), check(shape(u,1)==NY), depend(u) :: NY = shape(u,1)
!f2py integer, optional, intent(in), check(shape(u,2)==NZ), depend(u) :: NZ = shape(u,2)
!f2py integer, intent(in), :: idir
!f2py real, dimension(NX,NY,NZ), intent(in) :: eu,ev,conv
!f2py real, dimension(NX,NY,NZ), intent(inout) :: u,v
!f2py real, dimension(NX,NY,NZ), intent(out):: w
!f2py real, intent(in) :: rho(nz)
!f2py real, intent(in) :: bnd_value, dx, dy, dz, bad
!f2py real, intent(in) :: C2RD(2)

!     print *, 'u:',MINVAL(u),MAXVAL(u)
!     print *, 'v:',MINVAL(v),MAXVAL(v)
!     print *, 'eu:',MINVAL(eu),MAXVAL(eu)
!     print *, 'ev:',MINVAL(ev),MAXVAL(ev)
!     print *, 'conv:',MINVAL(conv),MAXVAL(conv)
!     print *, 'w:',MINVAL(w),MAXVAL(w)      
!     print *, 'rho:',MINVAL(rho),MAXVAL(rho) 
!     print *, 'idir',idir
!     print *, 'bnd_value',bnd_value
!     print *, 'dx,dy,dz',dx,dy,dz
!     print *, 'bad', bad
!     print *, 'C2RD', C2RD
    
    if(idir>0) then
        kbegin = 1
    else
        kbegin = nz
    endif
    XYDELI(1) = 1./dx
    XYDELI(2) = 1./dy

    k = kbegin-idir
    do loop=1,nz
        k = k+idir

        if(k/=kbegin) then
            CALL DWITER(eu(:,:,k-idir), eu(:,:,k), ev(:,:,k-idir), ev(:,:,k),&
                conv(:,:,k-idir), conv(:,:,k), w(:,:,k-idir), w(:,:,k),&
                NX,NY,XYDELI,dz*idir/2.,rho(k),rho(k-idir),C2RD,BAD,ITER,DMN,KST)
        endif

        !   INITIALIZE BOUNDARY
        DO J=1,NY
            DO I=1,NX
                IF(w(I,J,K)==BAD .AND. conv(I,J,K)/=BAD) THEN
                    w(I,J,K)=bnd_value
                END IF
            END DO
        END DO

        !   PUT EVERYTHING IN ITS PLACE FOR NEXT LEVEL
        DO J=1,NY
            DO I=1,NX
                IF(w(I,J,K)/=BAD) THEN
                    u(I,J,K)=u(I,J,K)+w(I,J,K)*eu(I,J,K)
                    v(I,J,K)=v(I,J,K)+w(I,J,K)*ev(I,J,K)
                ELSE
                    u(I,J,K)=BAD
                    v(I,J,K)=BAD
                END IF
            enddo
        enddo
    enddo
end subroutine ms3d

