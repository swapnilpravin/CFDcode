subroutine channel()
    use settings
    use partitioner
	use io
	use schemes
	use solver
	use boundaryConditions
	use IBM

	implicit none

	
    !double precision ,dimension(Np,3) :: Yp, Vp, Vp0		! Particle variables

	double precision ,dimension(ny-2,1:m) :: D_CONV_U, D_CONV_V, DEL2U, DEL2V, DpDx, DpDy
	
    double precision :: R, E_P, E_U, E_V
	integer :: NITS_final_P, NITS_final_U

	integer :: i, j

    integer :: id, Nproc, ierr

    ! for io
    double precision :: Umax, Pmax, Umax_loc, Pmax_loc

    call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
    call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)

    
	! Initialize Flow Variables
	field%u=0; field%v=0; field%P=0
	!field%u_last=0; field%v_last=0;
	field%u_star=0; field%v_star=0
	field%u_star2=0; field%v_star2=0

    !if(id==0) print*, 'flow initialized.'

	! Pressure driven force
	forces%F = 8*nu*U0/Ly**2
    !forces%F = 0

    ! H: Sum of all IBM force
    ! B: Total body force (= F + H); F: Pressure force to drive the channel flow
	forces%Hx=0; forces%Hy=0
	forces%Bx=0; forces%By=0;

	! construct x,y,z matrices (mesh)
	do i=1,m; mesh%x(:,i) = (iStartx+i-1)*dx;	end do
	do i=1,ny; mesh%y(i,:) = (i-1)*dy;	end do

	! Velocity IC
	call setIC(field%u,field%v,field%P,mesh%x,mesh%y)

	![u,v] = setVelocityBC1(u,v,dx,dy,dt,nu);

	! write data before starting simulation
	call writeToTecplot2D_MPI(mesh%x,mesh%y,field%u,field%v,field%P,0)
	
	do i=1,nt
		
        ! Calculate force for IBM
		!call IBMForceCircle(forces%Hx,forces%Hy,field%u,field%v,mesh%x,mesh%y)
		call IBMForceOscillatingCircle(forces%Hx,forces%Hy,field%u,field%v,mesh%x,mesh%y,i)

		! Total body force
		forces%Bx = forces%F + forces%Hx
		forces%By = forces%Hy
		
		call calcUstar_Implicit(field%u_star,field%v_star,E_U,E_V,NITS_final_U,field%u,field%v,forces%Bx,forces%By)

        !if(id==0) print*, 'communication done'
        
        !call setVelocityBC_MPI(field%u_star,field%v_star)

		call PressureSolver_MPI(field%P,E_P,NITS_final_P,field%u_star,field%v_star)
		call PressureSolver_MPI_BiCGStab(field%P,E_P,NITS_final_P,field%u_star,field%v_star)

        !if(id==0) print*, 'Pressure solver done: ', NITS_final
		call GRAD(DpDx,DpDy,field%P)

        !if(id==0) print*, 'GRAD calculated'

		field%u_star2(2:ny-1,1:m) = field%u_star(2:ny-1,1:m) - dt/rho*DpDx
		field%v_star2(2:ny-1,1:m) = field%v_star(2:ny-1,1:m) - dt/rho*DpDy

        call setVelocityBC_MPI(field%u_star2,field%v_star2)

		call communicate(field%u_star2)
		call communicate(field%v_star2)


        !if(id==0) print*, 'u_star2 calculated'


		field%u = field%u_star2
		field%v = field%v_star2

        !call setVelocityBC_MPI(field%u,field%v)
        
        ! COMMUNICATE (u)
        !call communicate(field%u)
        !call communicate(field%v)


        !if(id==0) print*, 'field updated'

        Umax_loc = maxval(field%u(:,1:m))
        Pmax_loc = maxval(field%P(:,1:m))
        call mpi_reduce(Umax_loc,Umax,1,MPI_DOUBLE_PRECISION,MPI_MAX,0,MPI_COMM_WORLD,ierr)
        call mpi_reduce(Pmax_loc,Pmax,1,MPI_DOUBLE_PRECISION,MPI_MAX,0,MPI_COMM_WORLD,ierr)
        !if(id==0) print*, 'reduction for io done'

        if (id==0) then       
            if (mod(i,nLog)==0) then
                call writeLogToTerminal_MPI(i, &
                'Pressure error','double',E_P, &
                'Pressure solver iterations','integer',dble(NITS_final_P), &
				'Velocity U error','double',E_U, &
				'Velocity V error','double',E_V, &
				'Velocity solver iterations','integer',dble(NITS_final_U), &
				'Umax','double',Umax, &
                'Pmax','double',Pmax )
            end if
            
            
        end if

        if (mod(i,nWrite)==0) then
            call writeToTecplot2D_MPI(mesh%x,mesh%y,field%u,field%v,field%P,i)
        end if

        call mpi_barrier(MPI_COMM_WORLD,ierr)
		
	end do


end subroutine channel
