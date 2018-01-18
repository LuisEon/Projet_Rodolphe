Module tools

  Implicit none


CONTAINS

  ! Fonction flux suivant x
  function Fx(U,gamma) result(res)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: U
    double precision, dimension(4) :: res
    double precision :: p

    p = (gamma-1)*(U(4)-((U(2)**2)+(U(3)**2))/(2*U(1)))

    res(1) = U(2)
    res(2) = ((U(2)**2)/U(1))+p
    res(3) = (U(2)*U(3))/U(1)
    res(4) = (U(2)/U(1))*(U(4)+p)

  end function Fx


  ! Fonction flux suivant y
  function Fy(U,gamma) result(res)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: U
    double precision, dimension(4) :: res
    double precision :: p

    p = (gamma-1)*(U(4)-((U(2)**2)+(U(3)**2))/(2*U(1)))

    res(1) = U(3)
    res(2) = (U(2)*U(3))/U(1)
    res(3) = ((U(3)**2)/U(1))+p
    res(4) = (U(3)/U(1))*(U(4)+p)

  end function Fy

  subroutine Initialisation(U_total, X, Y, bmoins, bplus, gamma, p1, rho1, u1, v1, p2, rho2, u2, v2, &
    & p3, rho3, u3, v3, p4, rho4, u4, v4)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(:,:,:), intent(inout) :: U_total
    double precision, intent(in) :: p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4
    double precision, dimension(:), intent(in) :: X,Y
    double precision, dimension(2), intent(inout) :: bmoins, bplus
    double precision :: c, E1, E2, E3, E4
    integer :: i,j

    ! Energies initiales
    E1 = p1/(gamma-1) + rho1*((u1**2)+(v1**2))/2
    E2 = p2/(gamma-1) + rho2*((u2**2)+(v2**2))/2
    E3 = p3/(gamma-1) + rho3*((u3**2)+(v3**2))/2
    E4 = p4/(gamma-1) + rho4*((u4**2)+(v4**2))/2


    do j=1,size(Y)-1
      do i=1,size(X)-1
        if ((X(i)>0.5) .and. (Y(j)>0.5)) then
          c = sqrt(gamma*p1/rho1)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u1-c
          bmoins(2) = v1-c
          bplus(1) = u1+c
          bplus(2) = v1+c
          ! Vecteur U initial dans la partie 1
          U_total(i,j,1) = rho1
          U_total(i,j,2) = rho1*u1
          U_total(i,j,3) = rho1*v1
          U_total(i,j,4) = E1
        end if
        if ((X(i)<=0.5) .and. (Y(j)>0.5)) then
          c = sqrt(gamma*p2/rho2)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u2-c
          bmoins(2) = v2-c
          bplus(1) = u2+c
          bplus(2) = v2+c
          ! Vecteur U initial dans la partie 2
          U_total(i,j,1) = rho2
          U_total(i,j,2) = rho2*u2
          U_total(i,j,3) = rho2*v2
          U_total(i,j,4) = E2
        end if
        if ((X(i)<=0.5) .and. (Y(j)<=0.5)) then
          c = sqrt(gamma*p3/rho3)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u3-c
          bmoins(2) = v3-c
          bplus(1) = u3+c
          bplus(2) = v3+c
          ! Vecteur U initial dans la partie 3
          U_total(i,j,1) = rho3
          U_total(i,j,2) = rho3*u3
          U_total(i,j,3) = rho3*v3
          U_total(i,j,4) = E3
        end if
        if ((X(i)>0.5) .and. (Y(j)<=0.5)) then
          c = sqrt(gamma*p4/rho4)
          ! Définition des vitesses d'ondes initiales
          bmoins(1) = u4-c
          bmoins(2) = v4-c
          bplus(1) = u4+c
          bplus(2) = v4+c
          ! Vecteur U initial dans la partie 4
          U_total(i,j,1) = rho4
          U_total(i,j,2) = rho4*u4
          U_total(i,j,3) = rho4*v4
          U_total(i,j,4) = E4
        end if
      end do
    end do

  end subroutine Initialisation

  subroutine Reload_waves(U_total,bmoins,bplus,gamma, Nx, Ny)

    Implicit none

    integer, intent(in) :: Nx,Ny
    double precision :: cm, cd, ch, pm, pd, ph
    double precision, intent(in) :: gamma
    double precision, dimension(2), intent(inout) :: bmoins, bplus
    double precision, dimension(:,:,:), intent(in) :: U_total
    integer :: i,j

    do j=1,Ny
      do i=1,Nx

        ! Définition des vitesses d'ondes mises à jour
        ! Choix de front d'onde isentropique

        if (j==1) then ! Si on est sur le bord du bas
          pm = (gamma-1)*(U_total(i,j,4)-((U_total(i,j,2)**2)+(U_total(i,j,3)**2))/(2*U_total(i,j,1)))
          ph = (gamma-1)*(U_total(i,j+1,4)-((U_total(i,j+1,2)**2)+(U_total(i,j+1,3)**2))/(2*U_total(i,j+1,1)))
          cm = sqrt(gamma*pm/U_total(i,j,1))
          ch = sqrt(gamma*ph/U_total(i,j+1,1))
          bmoins(2) = min(0.,(U_total(i,j,3)/U_total(i,j,1))-cm)
          bplus(2) = max(0.,(U_total(i,j+1,3)/U_total(i,j+1,1))+ch)
        else if (j==Ny) then ! Si on est sur le bord du haut
          pm = (gamma-1)*(U_total(i,j,4)-((U_total(i,j,2)**2)+(U_total(i,j,3)**2))/(2*U_total(i,j,1)))
          cm = sqrt(gamma*pm/U_total(i,j,1))
          bmoins(2) = min(0.,(U_total(i,j,3)/U_total(i,j,1))-cm)
          bplus(2) = max(0.,(U_total(i,j,3)/U_total(i,j,1))+cm)
        else ! Sinon
          pm = (gamma-1)*(U_total(i,j,4)-((U_total(i,j,2)**2)+(U_total(i,j,3)**2))/(2*U_total(i,j,1)))
          ph = (gamma-1)*(U_total(i,j+1,4)-((U_total(i,j+1,2)**2)+(U_total(i,j+1,3)**2))/(2*U_total(i,j+1,1)))
          cm = sqrt(gamma*pm/U_total(i,j,1))
          ch = sqrt(gamma*ph/U_total(i,j+1,1))
          bmoins(2) = min(0.,(U_total(i,j,3)/U_total(i,j,1))-cm)
          bplus(2) = max(0.,(U_total(i,j+1,3)/U_total(i,j+1,1))+ch)
        end if

        if (i==1) then ! Si on est sur le bord de gauche
          pm = (gamma-1)*(U_total(i,j,4)-((U_total(i,j,2)**2)+(U_total(i,j,3)**2))/(2*U_total(i,j,1)))
          pd = (gamma-1)*(U_total(i+1,j,4)-((U_total(i+1,j,2)**2)+(U_total(i+1,j,3)**2))/(2*U_total(i+1,j,1)))
          cm = sqrt(gamma*pm/U_total(i,j,1))
          cd = sqrt(gamma*pd/U_total(i+1,j,1))
          bmoins(1) = min(0.,(U_total(i,j,2)/U_total(i,j,1))-cm)
          bplus(1) = max(0.,(U_total(i+1,j,2)/U_total(i+1,j,1))+cd)
        else if (i==Nx) then ! Si on est sur le bord de droite
          pm = (gamma-1)*(U_total(i,j,4)-((U_total(i,j,2)**2)+(U_total(i,j,3)**2))/(2*U_total(i,j,1)))
          cm = sqrt(gamma*pm/U_total(i,j,1))
          bmoins(1) = min(0.,(U_total(i,j,2)/U_total(i,j,1))-cm)
          bplus(1) = max(0.,(U_total(i,j,2)/U_total(i,j,1))+cm)
        else ! Sinon
          pm = (gamma-1)*(U_total(i,j,4)-((U_total(i,j,2)**2)+(U_total(i,j,3)**2))/(2*U_total(i,j,1)))
          pd = (gamma-1)*(U_total(i+1,j,4)-((U_total(i+1,j,2)**2)+(U_total(i+1,j,3)**2))/(2*U_total(i+1,j,1)))
          cm = sqrt(gamma*pm/U_total(i,j,1))
          cd = sqrt(gamma*pd/U_total(i+1,j,1))
          bmoins(1) = min(0.,(U_total(i,j,2)/U_total(i,j,1))-cm)
          bplus(1) = max(0.,(U_total(i+1,j,2)/U_total(i+1,j,1))+cd)
        end if

      end do
    end do

  end subroutine


End module tools
