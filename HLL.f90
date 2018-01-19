Module HLL

  use HLL_tools

  Implicit none

CONTAINS

  subroutine HLL_Calcul_flux_num_F(Ug,Ud,b,F,gamma) ! Pour calculer le flux numérique selon x

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: Ug, Ud
    double precision, dimension(2), intent(in) :: b
    double precision, dimension(4), intent(out) :: F
    double precision :: bmoins,bplus

    bmoins = b(1)
    bplus = b(2)

    ! Selon x
    if ((bmoins<=0.) .and. (bplus>=0.)) then
      F = (1/(bplus-bmoins))*(bplus*Fx(Ug,gamma)-bmoins*Fx(Ud,gamma)+bmoins*bplus*(Ud-Ug))
    end if
    if (bmoins>0.) then
      F = Fx(Ug,gamma)
    end if
    if (bplus<0.) then
      F = Fx(Ud,gamma)
    end if
    if (bmoins>bplus) then
      print*, "Vitesses mal définies !!!"
      call abort
    end if

  end subroutine

  subroutine HLL_Calcul_flux_num_G(Ub,Uh,b,G,gamma) ! Pour calculer le flux numérique selon y

    Implicit none

    double precision, intent(in) :: gamma
    double precision, dimension(4), intent(in) :: Ub, Uh
    double precision, dimension(2), intent(in) :: b
    double precision, dimension(4), intent(out) :: G
    double precision :: bmoins,bplus

    bmoins = b(1)
    bplus = b(2)

    ! Selon y
    if ((bmoins<=0.) .and. (bplus>=0.)) then
      G = (1/(bplus-bmoins))*(bplus*Fy(Ub,gamma)-bmoins*Fy(Uh,gamma)+bmoins*bplus*(Uh-Ub))
    end if
    if (bmoins>0.) then
      G = Fy(Ub,gamma)
    end if
    if (bplus<0.) then
      G = Fy(Uh,gamma)
    end if
    if (bmoins>bplus) then
      print*, "Vitesses mal définies !!!"
      call abort
    end if

  end subroutine

  subroutine HLL_method(U_total, Nx, Ny, dx, dy, dt, bx, by, gamma, i, j)

    Implicit none

    integer, intent(in) :: i,j,Nx,Ny
    double precision, intent(in) :: dx, dy, dt, gamma
    double precision, dimension(:,:,:), intent(in) :: bx, by
    double precision, dimension(:,:,:), intent(inout) :: U_total
    double precision, dimension(4) :: F_1,F_2,G_1,G_2 ! Flux numériques


    if (j==1) then ! Si on est sur le bord du bas
      call HLL_Calcul_flux_num_G(U_total(i,j,:),U_total(i,j,:),by(i,j,:),G_1,gamma) ! G_(j-1/2)
      call HLL_Calcul_flux_num_G(U_total(i,j,:),U_total(i,j+1,:),by(i,j+1,:),G_2,gamma) ! G_(j+1/2)
    else if (j==Ny) then ! Si on est sur le bord du haut
      call HLL_Calcul_flux_num_G(U_total(i,j-1,:),U_total(i,j,:),by(i,j,:),G_1,gamma) ! G_(j-1/2)
      call HLL_Calcul_flux_num_G(U_total(i,j,:),U_total(i,j,:),by(i,j+1,:),G_2,gamma) ! G_(j+1/2)
    else ! Sinon
      call HLL_Calcul_flux_num_G(U_total(i,j-1,:),U_total(i,j,:),by(i,j,:),G_1,gamma) ! G_(j-1/2)
      call HLL_Calcul_flux_num_G(U_total(i,j,:),U_total(i,j+1,:),by(i,j+1,:),G_2,gamma) ! G_(j+1/2)
    end if

    if (i==1) then ! Si on est sur le bord de gauche
      call HLL_Calcul_flux_num_F(U_total(i,j,:),U_total(i,j,:),bx(i,j,:),F_1,gamma) ! F_(i-1/2)
      call HLL_Calcul_flux_num_F(U_total(i,j,:),U_total(i+1,j,:),bx(i+1,j,:),F_2,gamma) ! F_(i+1/2)
    else if (i==Nx) then ! Si on est sur le bord de droite
      call HLL_Calcul_flux_num_F(U_total(i-1,j,:),U_total(i,j,:),bx(i,j,:),F_1,gamma) ! F_(i-1/2)
      call HLL_Calcul_flux_num_F(U_total(i,j,:),U_total(i,j,:),bx(i+1,j,:),F_2,gamma) ! F_(i+1/2)
    else ! Sinon
      call HLL_Calcul_flux_num_F(U_total(i-1,j,:),U_total(i,j,:),bx(i,j,:),F_1,gamma) ! F_(i-1/2)
      call HLL_Calcul_flux_num_F(U_total(i,j,:),U_total(i+1,j,:),bx(i+1,j,:),F_2,gamma) ! F_(i+1/2)
    end if

    ! U(n+1) = U(n) - (dt/dx)*(Flux_x(i+1/2)-Flux_x(i-1/2)) - (dt/dy)*(Flux_y(j+1/2)-Flux_y(j-1/2)))
    U_total(i,j,:) = U_total(i,j,:)-(dt/dx)*(F_2(:)-F_1(:))-(dt/dy)*(G_2(:)-G_1(:))

  end subroutine HLL_method

End module HLL
