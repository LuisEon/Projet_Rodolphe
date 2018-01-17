Module read_and_write

  Implicit none

CONTAINS

  subroutine read_file(name_file, Nx, Ny, dt, tf, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4)

    Implicit none

    character*22, intent(in) :: name_file
    integer, intent(out) :: Nx, Ny
    double precision, intent(out) :: dt, tf, p1, rho1, u1, v1, p2, rho2, u2, v2, p3, rho3, u3, v3, p4, rho4, u4, v4
    character*20 :: titre
    character*20 :: test

    Open(unit=15,file=name_file)
    !Lire Nx
    Read(15,*) titre, test
    If (titre=="Nx=") then
      Read(test,*) Nx
    Else
      Nx=400
      Write(*,*) "Nx n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : Nx=400"
    End if
    !Lire Ny
    Read(15,*) titre, test
    If (titre=="Ny=") then
      Read(test,*) Ny
    Else
      Ny=400
      Write(*,*) "Ny n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : Ny=400"
    End if
    !Lire dt
    Read(15,*) titre, test
    If (titre=="dt=") then
      Read(test,*) dt
    Else
      dt=0.0002
      Write(*,*) "dt n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : dt=0.0002"
    End if
    !Lire tf
    Read(15,*) titre, test
    If (titre=="tf=") then
      Read(test,*) tf
    Else
      tf=0.3
      Write(*,*) "tf n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : tf=0.3"
    End if
    !Lire p1
    Read(15,*) titre, test
    If (titre=="p1=") then
      Read(test,*) p1
    Else
      p1=1.5
      Write(*,*) "p1 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : p1=1.5"
    End if
    !Lire rho1
    Read(15,*) titre, test
    If (titre=="rho1=") then
      Read(test,*) rho1
    Else
      rho1=1.5
      Write(*,*) "rho1 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : rho1=1.5"
    End if
    !Lire u1
    Read(15,*) titre, test
    If (titre=="u1=") then
      Read(test,*) u1
    Else
      u1=0.
      Write(*,*) "u1 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : u1=0."
    End if
    !Lire v1
    Read(15,*) titre, test
    If (titre=="v1=") then
      Read(test,*) v1
    Else
      v1=0.
      Write(*,*) "v1 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : v1=0."
    End if
    !Lire p2
    Read(15,*) titre, test
    If (titre=="p2=") then
      Read(test,*) p2
    Else
      p2=0.3
      Write(*,*) "p2 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : p2=0.3"
    End if
    !Lire rho2
    Read(15,*) titre, test
    If (titre=="rho2=") then
      Read(test,*) rho2
    Else
      rho2=0.5323
      Write(*,*) "rho2 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : rho2=0.5323"
    End if
    !Lire u2
    Read(15,*) titre, test
    If (titre=="u2=") then
      Read(test,*) u2
    Else
      u2=1.206
      Write(*,*) "u2 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : u2=1.206"
    End if
    !Lire v2
    Read(15,*) titre, test
    If (titre=="v2=") then
      Read(test,*) v2
    Else
      v2=0.
      Write(*,*) "v2 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : v2=0."
    End if
    !Lire p3
    Read(15,*) titre, test
    If (titre=="p3=") then
      Read(test,*) p3
    Else
      p3=0.029
      Write(*,*) "p3 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : p3=0.029"
    End if
    !Lire rho3
    Read(15,*) titre, test
    If (titre=="rho3=") then
      Read(test,*) rho3
    Else
      rho3=0.138
      Write(*,*) "rho3 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : rho3=0.138"
    End if
    !Lire u3
    Read(15,*) titre, test
    If (titre=="u3=") then
      Read(test,*) u3
    Else
      u3=1.206
      Write(*,*) "u3 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : u3=1.206"
    End if
    !Lire v3
    Read(15,*) titre, test
    If (titre=="v3=") then
      Read(test,*) v3
    Else
      v3=1.206
      Write(*,*) "v3 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : v3=1.206"
    End if
    !Lire p4
    Read(15,*) titre, test
    If (titre=="p4=") then
      Read(test,*) p4
    Else
      p4=0.3
      Write(*,*) "p4 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : p4=0.3"
    End if
    !Lire rho4
    Read(15,*) titre, test
    If (titre=="rho4=") then
      Read(test,*) rho4
    Else
      rho4=0.5323
      Write(*,*) "rho4 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : rho4=0.5323"
    End if
    !Lire u4
    Read(15,*) titre, test
    If (titre=="u4=") then
      Read(test,*) u4
    Else
      u4=0.
      Write(*,*) "u4 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : u4=0."
    End if
    !Lire v4
    Read(15,*) titre, test
    If (titre=="v4=") then
      Read(test,*) v4
    Else
      v4=1.206
      Write(*,*) "v4 n'a pas été détecté dans le fichier de données initiales"
      Write(*,*) "Valeur par défaut : v4=1.206"
    End if

    Close(15)

  End subroutine

  subroutine write_file(name_file,U_total,X,Y,Nx,Ny,l)

    Implicit none

    character*6, intent(in) :: name_file
    integer, intent(in) :: l,Nx,Ny
    double precision, dimension(:), intent(in) :: X,Y
    double precision, dimension(:,:), intent(in) :: U_total
    integer :: i,j,k

    ! if (l==1) then
    !   Open (unit=4,file=name_file//"pressure.dat",action="write",status="unknown")
    !
    !   write (unit=4,fmt=*) "# X, Y, P"
    !   Do i=1,Nx+1
    !     Do j=1,Ny+1
    !       k=(j-1)*Nx+i ! Indice global dans le maillage cartésien
    !       write (unit=4,fmt=*) X(i), Y(j), U_total(k,l)
    !     End Do
    !   End Do
    !
    !   Close(4)
    ! end if

    if (l==2) then
      Open (unit=5,file=name_file//"density.vtk",action="write",status="unknown")

      write (unit=5,fmt='(A)') "# vtk DataFile Version 2.0"
      write (unit=5,fmt='(A)') "Density"
      write (unit=5,fmt='(A)') "ASCII"
      write (unit=5,fmt='(A)') "DATASET RECTILINEAR_GRID"
      write (unit=5,fmt='(A,X,I4,X,I4,X,I1)') "DIMENSIONS", Nx+1, Ny+1, 1
      write (unit=5,fmt='(A,X,I4,X,A)') "X_COORDINATES", Nx+1, "double"
      Do i=1,Nx+1
        write (unit=5,fmt='(F16.14,X)',advance='no') X(i)
      End Do
      write(unit=5,fmt='(A)'), " "
      write (unit=5,fmt='(A,X,I4,X,A)') "Y_COORDINATES", Ny+1, "double"
      Do j=1,Ny+1
        write (unit=5,fmt='(F16.14,X)',advance='no') Y(j)
      end do
      write(unit=5,fmt='(A)'), " "
      write (unit=5,fmt='(A,X,I1,X,A)') "Z_COORDINATES", 1, "double"
      write (unit=5,fmt='(F2.0)') 0.
      write (unit=5,fmt='(A,X,I7)') "CELL_DATA", Nx*Ny
      write (unit=5,fmt='(A,X,A,X,A,X,I1)') "SCALARS", "Density", "double", 1
      write (unit=5,fmt='(A,X,A)') "LOOKUP_TABLE", "default"
      Do i=1,Nx
        Do j=1,Ny
          k=(j-1)*Nx+i ! Indice global dans le maillage cartésien
          write (unit=5,fmt='(F16.14,X)') U_total(k,l)
        end do
      end do
      Close(5)
    end if

    if (l==3) then
      Open (unit=5,file=name_file//"velocity_u.vtk",action="write",status="unknown")

      write (unit=5,fmt='(A)') "# vtk DataFile Version 2.0"
      write (unit=5,fmt='(A)') "Velocity_U"
      write (unit=5,fmt='(A)') "ASCII"
      write (unit=5,fmt='(A)') "DATASET STRUCTURED_GRID"
      write (unit=5,fmt='(A,X,I4,X,I4,X,I1)') "DIMENSIONS", Nx+1, Ny+1, 1
      write (unit=5,fmt='(A,X,I7,X,A)') "POINTS", (Nx+1)*(Ny+1), "double"
      Do i=1,Nx+1
        Do j=1,Ny+1
          write (unit=5,fmt='(F16.14,X,F16.14,X,F2.0)') X(i), Y(j), 0.
        End do
      End do
      write (unit=5,fmt='(A,X,I7)') "CELL_DATA", Nx*Ny
      write (unit=5,fmt='(A,X,A,X,A,X,I1)') "SCALARS", "Velocity_U", "double", 1
      write (unit=5,fmt='(A,X,A)') "LOOKUP_TABLE", "default"
      Do i=1,Nx
        Do j=1,Ny
          k=(j-1)*Nx+i ! Indice global dans le maillage cartésien
          write (unit=5,fmt='(F16.14,X)') U_total(k,l)
        end do
      end do
      Close(5)
    end if

    ! if (l==4) then
    !   Open (unit=7,file=name_file//"velocity_y.dat",action="write",status="unknown")
    !
    !   write (unit=7,fmt=*) "# X, Y, V"
    !   Do i=1,Nx
    !     Do j=1,Ny
    !       k=(j-1)*Nx+i ! Indice global dans le maillage cartésien
    !       write (unit=7,fmt=*) X(i), Y(j), U_total(k,l)
    !     End Do
    !   End Do
    !
    !   Close(7)
    ! end if

    if ((l>4) .or. (l<1)) then
      print*, "MAUVAISE ECRITURE !!!"
    end if

  end subroutine

End module read_and_write
