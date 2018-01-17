Module tools

  Implicit none


CONTAINS

  ! Fonction flux suivant x
  function Fx(U,gamma) result(res)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: U
    double precision, dimension(4) :: res
    double precision :: E

    E = U(1)/(gamma-1) + U(2)*(U(3)**2+U(4)**2)/2

    res(1) = U(2)*U(3)
    res(2) = U(2)*U(3)**2 + U(1)
    res(3) = U(2)*U(3)*U(4)
    res(4) = U(3)*(E+U(1))

  end function Fx


  ! Fonction flux suivant y
  function Fy(U,gamma) result(res)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: U
    double precision, dimension(4) :: res
    double precision :: E

    E = U(1)/(gamma-1) + U(2)*(U(3)**2+U(4)**2)/2

    res(1) = U(2)*U(4)
    res(2) = U(2)*U(4)**2 + U(1)
    res(3) = U(2)*U(3)*U(4)
    res(4) = U(4)*(E+U(1))

  end function Fy

  subroutine Initialisation(U_total, X, Y, Nx, bmoins, bplus, gamma, p1, rho1, u1, v1, p2, rho2, u2, v2, &
    & p3, rho3, u3, v3, p4, rho4, u4, v4)

    Implicit none

    integer, intent(in) :: Nx
    double precision, intent(in) :: gamma
    double precision, dimension(:,:), intent(inout) :: U_total
    double precision, intent(in) :: p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4
    double precision, dimension(:), intent(in) :: X,Y
    double precision, dimension(2), intent(inout) :: bmoins, bplus
    double precision :: c
    integer :: i,j,k

    do i=1,size(X)-1
      do j=1,size(Y)-1
        k=(j-1)*Nx+i ! Indice global dans le maillage cartésien
        if ((X(i)>0.5) .and. (Y(j)>0.5)) then
          ! print*, "HELLO 1 : X et Y =", X(i), Y(j)
          c = sqrt(gamma*p1/rho1)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u1-c-0.*u1
          bmoins(2) = v1-c-0.*v1
          bplus(1) = u1+c+0.*u1
          bplus(2) = v1+c+0.*v1
          ! print*, "HELLO 1 : k =", k
          U_total(k,1) = p1
          U_total(k,2) = rho1
          U_total(k,3) = u1
          U_total(k,4) = v1
        end if
        if ((X(i)<=0.5) .and. (Y(j)>0.5)) then
          ! print*, "HELLO 2 : X et Y =", X(i), Y(j)
          c = sqrt(gamma*p2/rho2)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u2-c-0.*u2
          bmoins(2) = v2-c-0.*v2
          bplus(1) = u2+c+0.*u2
          bplus(2) = v2+c+0.*v2
          ! print*, "HELLO 2 : k =", k
          U_total(k,1) = p2
          U_total(k,2) = rho2
          U_total(k,3) = u2
          U_total(k,4) = v2
        end if
        if ((X(i)<=0.5) .and. (Y(j)<=0.5)) then
          ! print*, "HELLO 3 : X et Y =", X(i), Y(j)
          c = sqrt(gamma*p3/rho3)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u3-c-0.*u3
          bmoins(2) = v3-c-0.*v3
          bplus(1) = u3+c+0.*u3
          bplus(2) = v3+c+0.*v3
          ! print*, "HELLO 3 : k =", k
          U_total(k,1) = p3
          U_total(k,2) = rho3
          U_total(k,3) = u3
          U_total(k,4) = v3
        end if
        if ((X(i)>0.5) .and. (Y(j)<=0.5)) then
          ! print*, "HELLO 4 : X et Y =", X(i), Y(j)
          c = sqrt(gamma*p4/rho4)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u4-c-0.*u4
          bmoins(2) = v4-c-0.*v4
          bplus(1) = u4+c+0.*u4
          bplus(2) = v4+c+0.*v4
          ! print*, "HELLO 4 : k =", k
          U_total(k,1) = p4
          U_total(k,2) = rho4
          U_total(k,3) = u4
          U_total(k,4) = v4
        end if
      end do
    end do

  end subroutine Initialisation

  subroutine Reload_waves(U_total,bplus,bmoins,gamma, Nx, i, j)

    Implicit none

    integer, intent(in) :: Nx,i,j
    integer :: k
    double precision :: c
    double precision, intent(in) :: gamma
    double precision, dimension(2), intent(inout) :: bmoins, bplus
    double precision, dimension(:,:), intent(in) :: U_total

    k = (j-1)*Nx+i ! Indice global dans le maillage cartésien

    c = sqrt(gamma*U_total(k,1)/U_total(k,2))

    ! Définition des vitesses d'ondes mises à jour
    bmoins(1) = U_total(k,3)-c-0.*U_total(k,3)
    bmoins(2) = U_total(k,4)-c-0.*U_total(k,4)
    bplus(1) = U_total(k,3)+c+0.*U_total(k,3)
    bplus(2) = U_total(k,4)+c+0.*U_total(k,4)

  end subroutine


End module tools
