Program main

  use HLL
  use HLLC
  use MOOD
  use read_and_write
  use HLL_tools
  use HLLC_tools

  Implicit none

  integer :: Nx, Ny
  double precision :: tf, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4
  double precision :: CFL, gamma, dx, dy, dt
  double precision, dimension(:,:,:), allocatable :: bx, by
  double precision, dimension(:), allocatable :: X,Y
  double precision, dimension(:,:,:), allocatable :: U_total
  double precision :: t
  integer :: i,j,m
  character*22 :: name_file

  name_file = "condition_initiale.txt"

  ! On lit les conditions initiales dans un fichier de données
  call read_file(name_file, Nx, Ny, dt, tf, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4)
  print*, "Fichier de paramètres lu..."
  print*, "Taille du domaine : Nx*Ny = ", Nx*Ny
  print*, "Temps final fixé à : tf = ", tf

  Allocate(X(Nx+1))
  Allocate(Y(Ny+1))
  Allocate(U_total(Nx,Ny,4))
  ! Allocate(bx(Nx+1,Ny,2))
  ! Allocate(by(Nx,Ny+1,2))
  Allocate(bx(Nx+1,Ny,3))
  Allocate(by(Nx,Ny+1,3))

  gamma = 1.4
  dx = 1./Nx
  dy = 1./Ny
  t=0.

  ! On considère le domaine carré 2D : [0,1]x[0,1] et la position de chaque point définie par :
  do i=1,size(X)
    X(i)=(i-1)*dx
  end do
  do j=1,size(Y)
    Y(j)=(j-1)*dy
  end do

  ! Implémentation des conditions initiales HLL
  ! call HLL_Initialisation(U_total, X, Y, bx, by, gamma, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, &
  ! & u3, v3, p4, rho4, u4, v4)

  ! Implémentation des conditions initiales HLLC
  call HLLC_Initialisation(U_total, X, Y, bx, by, gamma, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, &
  & rho3, u3, v3, p4, rho4, u4, v4)

  ! Condition CFL
  CFL = max(maxval(bx(:,:,2)),maxval(by(:,:,2)))*2*dt*(dx+dy)/(dx*dy)
  print*, "CFL = ", CFL
  if (CFL >= 0.5) then
    print*, "Condition CFL non respectée"
    call abort
  end if

  ! Pression initiale
  call write_file("output/initi_",U_total,X,Y,Nx,Ny,gamma,1)
  ! Densité initiale
  call write_file("output/initi_",U_total,X,Y,Nx,Ny,gamma,2)
  ! Vitesse initiale
  call write_file("output/initi_",U_total,X,Y,Nx,Ny,gamma,3)

  print*, "t = ", t, "tf = ", tf

  ! Boucle en temps
  do while (t<tf)
  !do m=1,400
    do j=1,Ny
      do i=1,Nx
        ! call HLL_method(U_total, Nx, Ny, dx, dy, dt, bx, by, gamma, i, j)
        call HLLC_method(U_total, Nx, Ny, dx, dy, dt, bx, by, gamma, i, j)
      end do
    end do
    ! call HLL_Reload_waves(U_total,bx,by,gamma,Nx,Ny)
    call HLLC_Reload_waves(U_total,bx, by,gamma,Nx,Ny)
    t=t+dt
  end do

  ! do j=1,Ny
  !   do i=1,Nx
  !     print*, "U(", i, ", ", j, ") = ", U_total(i,j,:)
  !   end do
  ! end do

  ! Pression finale
  call write_file("output/final_",U_total,X,Y,Nx,Ny,gamma,1)
  ! Densité finale
  call write_file("output/final_",U_total,X,Y,Nx,Ny,gamma,2)
  ! Vitesse finale
  call write_file("output/final_",U_total,X,Y,Nx,Ny,gamma,3)

End program main
