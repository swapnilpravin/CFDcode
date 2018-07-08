module boundaryConditions
use settings
use partitioner
use communicator

implicit none

contains

	subroutine setIC(u,v,p,x,y)
		double precision, dimension(ny,0:m+1) :: u,v,P
		double precision, dimension(ny,1:m) :: x,y
		
		double precision, dimension(ny,1:m) :: r
		
		integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
        call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)
    

		!u = U0
		u = 0
		v = 0
        p = 0

        if (id==0) p(5,5) = 1
		
		!u(:,1:m) = U0 * ( 1 - ((y-Ly/2)/(Ly/2))**2 ) 
            ! only the non-ghost points are set here; call sync function
            ! to set the ghost cells as well
        
		call communicate(u)
        call communicate(v)
        call communicate(p)

	end subroutine setIC
	
	
    !-------------------------------------------------------------
    ! setPressureBC_MPI:
    ! set dP/dn=0 at boundaries on y and z directions
    !-------------------------------------------------------------
    subroutine setPressureBC_MPI(P)
        double precision, dimension(ny,0:m+1) :: P

        integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
        call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)
    
        ! dP/dy = 0 at y=0,Ly (including edge nodes)
		P(1,:) = P(2,:)
		P(ny,:) = P(ny-1,:)

		! dP/dz = 0 at z=0,W (including edge nodes)
		!P(1,:,:) = P(2,:,:)
		!P(nz,:,:) = P(nz-1,:,:)
		
        ! dP/dx = 0 at x=0,Lx (including edge nodes, for cavity validation case)
		! This BC applies here for the oscilating IBM element within fixed walls
        !if (id==0)          P(:,1) = P(:,2)
        !if (id==Nproc-1)    P(:,m) = P(:,m-1)


    end subroutine setPressureBC_MPI



    !---------------------------------------------------------------
    ! setVelocityBC_MPI:
    ! set velocities at no-slip walls in y and z directions to 0
    !---------------------------------------------------------------
    subroutine setVelocityBC_MPI(u,v)
        double precision, dimension(ny,0:m+1) :: u,v
		
		integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
        call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)   
		
		!call setToZero(u)
        !call setToZero(v)
		
		! Top: no slip 
		u(1,:) = 0
        u(ny,:) = 0
		v(1,:) = 0
        v(ny,:) = 0
		
		! Left: fixed inlet
		if(id==0) then
			u(:,1) = U0
			v(:,1) = 0
		end if
		
		! Right: outlet (zero gradient for u and v)
		if(id==Nproc-1) then 
			u(:,m) = u(:,m-1)
			v(:,m) = v(:,m-1)
		end if
        
        !call setUCavity(u)

    end subroutine setVelocityBC_MPI

    subroutine setToZero(u)
        double precision, dimension(ny,0:m+1) :: u

        integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
        call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)      

        u(1,:) = 0
        u(ny,:) = 0

        ! for 2D cavity
		! Applies for the oscillating IBM element case
        !if(id==0)       u(:,1) = 0
        !if(id==Nproc-1) u(:,m) = 0


    end subroutine setToZero

	subroutine setZeroGradient(u)
        double precision, dimension(ny,0:m+1) :: u

        integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
        call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)      

		! Top and bottom walls
        u(1,:) = u(2,:)
        u(ny,:) = u(ny-1,:)

        ! Left and right walls
        if(id==0)       u(:,1) = u(:,2)
        if(id==Nproc-1) u(:,m) = u(:,m-1)

    end subroutine setZeroGradient


    subroutine setUCavity(u)
        double precision, dimension(ny,0:m+1) :: u

        integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
        call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)      

        u(1,:) = 0
        u(ny,:) = U0

        ! for 2D cavity
        if(id==0)       u(:,1) = 0
        if(id==Nproc-1) u(:,m) = 0


    end subroutine

end module boundaryConditions
