Module HLL

  use tools

  Implicit none

CONTAINS

  subroutine Calcul_flux_num_F(Um,Ud,bmoins,bplus,F,gamma) ! Pour calculer F_(i+1/2)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: Um, Ud
    double precision, dimension(2), intent(in) :: bplus, bmoins
    double precision, dimension(4), intent(out) :: F

    ! Selon x
    if ((bmoins(1)<0) .and. (bplus(1)>0)) then
      F = (1/(bplus(1)-bmoins(1)))*(bplus(1)*Fx(Um,gamma)-bmoins(1)*Fx(Ud,gamma)+bmoins(1)*bplus(1)*Ud-bmoins(1)*bplus(1)*Um)
      print*, "F = ", F
    end if
    if ((bmoins(1)>0) .and. (bplus(1)>0)) then
      F = Fx(Um,gamma)
    end if
    if ((bmoins(1)<0) .and. (bplus(1)<0)) then
      F = Fx(Ud,gamma)
    end if
    if ((bmoins(1)>0) .and. (bplus(1)<0)) then
      print*, "Vitesses mal définies !!!"
      call abort
    end if

  end subroutine

  subroutine Calcul_flux_num_G(Um,Uh,bmoins,bplus,G,gamma) ! Pour calculer G_(j+1/2)

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: Um, Uh
    double precision, dimension(2), intent(in) :: bplus, bmoins
    double precision, dimension(4), intent(out) :: G

    ! Selon y
    if ((bmoins(2)<0) .and. (bplus(2)>0)) then
      G = (1/(bplus(2)-bmoins(2)))*(bplus(2)*Fy(Um,gamma)-bmoins(2)*Fy(Uh,gamma)+bmoins(2)*bplus(2)*Uh-bmoins(2)*bplus(2)*Um)
      print*, "G = ", G
    end if
    if ((bmoins(2)>0) .and. (bplus(2)>0)) then
      G = Fy(Um,gamma)
    end if
    if ((bmoins(2)<0) .and. (bplus(2)<0)) then
      G = Fy(Uh,gamma)
    end if
    if ((bmoins(2)>0) .and. (bplus(2)<0)) then
      print*, "Vitesses mal définies !!!"
      call abort
    end if

  end subroutine

  subroutine HLL_method(U_total, Nx, Ny, dx, dy, dt, bmoins, bplus, gamma, i, j)

    Implicit none

    integer, intent(in) :: i,j,Nx,Ny
    double precision, intent(in) :: dx, dy, dt, gamma
    double precision, dimension(2), intent(inout) :: bplus, bmoins
    double precision, dimension(:,:), intent(inout) :: U_total
    double precision, dimension(4) :: F_1,F_2,G_1,G_2 ! Flux
    integer :: k

    k=(j-1)*Nx+i ! Indice global dans le maillage cartésien

    if (i<Nx+1) then ! Si on est sur le bord du bas
      print*, "HIIIIIIIIIIIIIIIIIIIII"
      call Calcul_flux_num_G(U_total(k,:),U_total(k,:),bmoins,bplus,G_1,gamma) ! G_(j-1/2)
      call Calcul_flux_num_G(U_total(k,:),U_total(k+Nx,:),bmoins,bplus,G_2,gamma) ! G_(j+1/2)
    else if (i>(Ny-1)*Nx) then ! Si on est sur le bord du haut
      call Calcul_flux_num_G(U_total(k-Nx,:),U_total(k,:),bmoins,bplus,G_1,gamma) ! G_(j-1/2)
      call Calcul_flux_num_G(U_total(k,:),U_total(k,:),bmoins,bplus,G_2,gamma) ! G_(j+1/2)
    else ! Sinon
      call Calcul_flux_num_G(U_total(k-Nx,:),U_total(k,:),bmoins,bplus,G_1,gamma) ! G_(j-1/2)
      call Calcul_flux_num_G(U_total(k,:),U_total(k+Nx,:),bmoins,bplus,G_2,gamma) ! G_(j+1/2)
    end if

    if (mod(k,Nx)==1) then ! Si on est sur le bord de gauche
      call Calcul_flux_num_F(U_total(k,:),U_total(k,:),bmoins,bplus,F_1,gamma) ! F_(i-1/2)
      call Calcul_flux_num_F(U_total(k,:),U_total(k+1,:),bmoins,bplus,F_2,gamma) ! F_(i+1/2)
    else if (mod(k,Nx)==0) then ! Si on est sur le bord de droite
      call Calcul_flux_num_F(U_total(k-1,:),U_total(k,:),bmoins,bplus,F_1,gamma) ! F_(i-1/2)
      call Calcul_flux_num_F(U_total(k,:),U_total(k,:),bmoins,bplus,F_2,gamma) ! F_(i+1/2)
    else ! Sinon
      call Calcul_flux_num_F(U_total(k-1,:),U_total(k,:),bmoins,bplus,F_1,gamma) ! F_(i-1/2)
      call Calcul_flux_num_F(U_total(k,:),U_total(k+1,:),bmoins,bplus,F_2,gamma) ! F_(i+1/2)
    end if

    U_total(k,:) = U_total(k,:) - (dt/dx)*(F_2(:)-F_1(:)) - (dt/dy)*(G_2(:)-G_1(:))

    call Reload_waves(U_total,bplus,bmoins,gamma,Nx, i, j)

    print*, "i = ", i, " j = ", j
    print*, "bmoins = ", bmoins(1), bmoins(2)
    print*, "bplus = ", bplus(1), bplus(1)

  end subroutine HLL_method

End module HLL
