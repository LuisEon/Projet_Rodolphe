Program main

use HLL
use MOOD
use read_and_write
use tools

Implicit none

integer :: Nx, Ny
double precision :: tf, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4
double precision :: CFL, gamma, dx, dy, dt
double precision, dimension(2) :: bplus, bmoins
double precision, dimension(:), allocatable :: X,Y
double precision, dimension(:,:), allocatable :: U_total
double precision :: E1, E2, E3, E4
double precision :: t
integer :: i,j
character*22 :: name_file

name_file = "condition_initiale.txt"

! On lit les conditions initiales dans un fichier de données
call read_file(name_file, Nx, Ny, dt, tf, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4)

print*, "Nx et Ny =", Nx, Ny

Allocate(X(Nx+1))
Allocate(Y(Ny+1))
Allocate(U_total(Nx*Ny,4))

gamma = 1.4
dx = 1./(Nx+1)
dy = 1./(Ny+1)
t=0.

! On considère le domaine carré 2D : [0,1]x[0,1] et la position de chaque point définie par :
do i=1,size(X)
  X(i)=i*dx
end do
do i=1,size(Y)
  Y(i)=i*dy
end do

! Energies initiales
E1 = p1/(gamma-1) + rho1*(u1**2+v1**2)/2
E2 = p2/(gamma-1) + rho2*(u2**2+v2**2)/2
E3 = p3/(gamma-1) + rho3*(u3**2+v3**2)/2
E4 = p4/(gamma-1) + rho4*(u4**2+v4**2)/2

! Implémentation des conditions initiales
call Initialisation(U_total, X, Y, Nx, bmoins, bplus, gamma, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4)

! Condition CFL
CFL = (1/dt*max(bplus(1),bplus(2)))*(dx*dy/(4*(dx+dy)))
print*, "CFL = ", CFL
if (CFL >= 0.5) then
  print*, "Condition CFL non respectée"
  call abort
end if


print*, "VECTEUR X : ", size(X)
print*, "VECTEUR U : ", size(U_total(:,1))

call write_file("initi_",U_total,X,Y,Nx,Ny,3)

print*, "t = ", t, "tf = ", tf
do while (t<tf)

  do i=1,Nx
    do j=1,Ny
      call HLL_method(U_total, Nx, Ny, dx, dy, dt, bmoins, bplus, gamma, i, j)
    end do
  end do
  t=t+dt
  print*, "t = ", t
  ! print*, "VECTEUR U temps t = ", t, " ------------------------------------------------"
  ! do i=1,Nx*Ny
  !   print*, U_total(i,1), U_total(i,2), U_total(i,3), U_total(i,4)
  ! end do

end do

call write_file("final_",U_total,X,Y,Nx,Ny,3)

End program main
