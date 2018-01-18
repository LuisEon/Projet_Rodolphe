Module HLL

  use tools

  Implicit none

CONTAINS

  subroutine Calcul_flux_num_F(Ug,Ud,bmoins,bplus,F,gamma) ! Pour calculer le flux numérique selon x

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: Ug, Ud
    double precision, dimension(2), intent(in) :: bplus, bmoins
    double precision, dimension(4), intent(out) :: F

    ! Selon x
    if ((bmoins(1)<=0.) .and. (bplus(1)>=0.)) then
      F = (1/(bplus(1)-bmoins(1)))*(bplus(1)*Fx(Ug,gamma)-bmoins(1)*Fx(Ud,gamma)+bmoins(1)*bplus(1)*(Ud-Ug))
    end if
    if (bmoins(1)>0.) then
      F = Fx(Ug,gamma)
    end if
    if (bplus(1)<0.) then
      F = Fx(Ud,gamma)
    end if
    if (bmoins(1)>bplus(1)) then
      print*, "Vitesses mal définies !!!"
      call abort
    end if

  end subroutine

  subroutine Calcul_flux_num_G(Ub,Uh,bmoins,bplus,G,gamma) ! Pour calculer le flux numérique selon y

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: Ub, Uh
    double precision, dimension(2), intent(in) :: bplus, bmoins
    double precision, dimension(4), intent(out) :: G

    ! Selon y
    if ((bmoins(2)<=0.) .and. (bplus(2)>=0.)) then
      G = (1/(bplus(2)-bmoins(2)))*(bplus(2)*Fy(Ub,gamma)-bmoins(2)*Fy(Uh,gamma)+bmoins(2)*bplus(2)*(Uh-Ub))
    end if
    if (bmoins(2)>0.) then
      G = Fy(Ub,gamma)
    end if
    if (bplus(2)<0.) then
      G = Fy(Uh,gamma)
    end if
    if (bmoins(2)>bplus(2)) then
      print*, "Vitesses mal définies !!!"
      call abort
    end if

  end subroutine

  subroutine HLL_method(U_total, Nx, Ny, dx, dy, dt, bmoins, bplus, gamma, i, j)

    Implicit none

    integer, intent(in) :: i,j,Nx,Ny
    double precision, intent(in) :: dx, dy, dt, gamma
    double precision, dimension(2), intent(inout) :: bplus, bmoins
    double precision, dimension(:,:,:), intent(inout) :: U_total
    double precision, dimension(4) :: F_1,F_2,G_1,G_2 ! Flux numériques


    if (j==1) then ! Si on est sur le bord du bas
      call Calcul_flux_num_G(U_total(i,j,:),U_total(i,j,:),bmoins,bplus,G_1,gamma) ! G_(j-1/2)
      call Calcul_flux_num_G(U_total(i,j,:),U_total(i,j+1,:),bmoins,bplus,G_2,gamma) ! G_(j+1/2)
    else if (j==Ny) then ! Si on est sur le bord du haut
      call Calcul_flux_num_G(U_total(i,j-1,:),U_total(i,j,:),bmoins,bplus,G_1,gamma) ! G_(j-1/2)
      call Calcul_flux_num_G(U_total(i,j,:),U_total(i,j,:),bmoins,bplus,G_2,gamma) ! G_(j+1/2)
    else ! Sinon
      call Calcul_flux_num_G(U_total(i,j-1,:),U_total(i,j,:),bmoins,bplus,G_1,gamma) ! G_(j-1/2)
      call Calcul_flux_num_G(U_total(i,j,:),U_total(i,j+1,:),bmoins,bplus,G_2,gamma) ! G_(j+1/2)
    end if

    if (i==1) then ! Si on est sur le bord de gauche
      call Calcul_flux_num_F(U_total(i,j,:),U_total(i,j,:),bmoins,bplus,F_1,gamma) ! F_(i-1/2)
      call Calcul_flux_num_F(U_total(i,j,:),U_total(i+1,j,:),bmoins,bplus,F_2,gamma) ! F_(i+1/2)
    else if (i==Nx) then ! Si on est sur le bord de droite
      call Calcul_flux_num_F(U_total(i-1,j,:),U_total(i,j,:),bmoins,bplus,F_1,gamma) ! F_(i-1/2)
      call Calcul_flux_num_F(U_total(i,j,:),U_total(i,j,:),bmoins,bplus,F_2,gamma) ! F_(i+1/2)
    else ! Sinon
      call Calcul_flux_num_F(U_total(i-1,j,:),U_total(i,j,:),bmoins,bplus,F_1,gamma) ! F_(i-1/2)
      call Calcul_flux_num_F(U_total(i,j,:),U_total(i+1,j,:),bmoins,bplus,F_2,gamma) ! F_(i+1/2)
    end if

    ! U(n+1) = U(n) - (dt/dx)*(Flux_x(i+1/2)-Flux_x(i-1/2)) - (dt/dy)*(Flux_y(j+1/2)-Flux_y(j-1/2)))
    U_total(i,j,:) = U_total(i,j,:)-(dt/dx)*(F_2(:)-F_1(:))-(dt/dy)*(G_2(:)-G_1(:))

  end subroutine HLL_method
  
End module HLL
