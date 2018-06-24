module settings

use mpi

    implicit none
    
    double precision, parameter :: pi = 4*atan(1.0)
    
    integer :: nx,ny
    double precision :: dx,dy,dt
    double precision :: rho,mu,nu
    integer :: nt,nit
    double precision :: omega_P, omega_U, omega_V, tol
    double precision :: Lx,Ly
    double precision :: U0
    integer:: nLog, nWrite		! nLog: write log to terminal every nLog timesteps
    							! write: results to file every nwrite timesteps
    double precision :: RADIUS	! for testing: circle radius
    
    double precision :: A, T	! Oscillating polygon parameters, amplitude and time period
    

	! IB points
    integer :: n_ib
	double precision, dimension(:), allocatable :: x_ib, y_ib

	! Monitor points
	integer :: n_mon
	double precision, dimension(:), allocatable :: x_mon, y_mon
    

    contains
        subroutine setup(filename)

	        character(len=*) :: filename
	        character(len=30) :: temp

            integer :: id, Nproc, ierr

			integer :: i

            call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)

            if (id==0) then
	        
                open(unit=10,file=filename,status='old',action='read')
                
                read (10,*) temp, Lx
                read (10,*) temp, Ly
                read (10,*) temp, dx
                read (10,*) temp, dy
                read (10,*) temp, dt
                read (10,*) temp, nt
                read (10,*) temp, nit
                read (10,*) temp, rho
                read (10,*) temp, nu
                read (10,*) temp, omega_P
                read (10,*) temp, omega_U
                read (10,*) temp, omega_V
                read (10,*) temp, tol
                read (10,*) temp, U0
                read (10,*) temp, nLog
                read (10,*) temp, nWrite
                read (10,*) temp, A
                read (10,*) temp, T

			    close(10)

            end if

            ! broadcast to all
            call mpi_bcast(Lx,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(Ly,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(dx,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(dy,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(dt,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(nt,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(nit,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(rho,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(nu,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(omega_P,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(omega_U,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(omega_V,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(tol,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(U0,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(nLog,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(nWrite,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(A,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
            call mpi_bcast(T,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

            !call mpi_barrier(MPI_COMM_WORLD,ierr)

			nx = floor(Lx/dx)
			ny = floor(Ly/dy)

            !print*, Lx, Ly, Lz
			
			mu = nu*rho

			! Read montor points from 'monitor.txt'
			if (id==0) then
				open(unit=20,file='monitor.txt',status='old',action='read')
				read (20,*) n_mon
			end if
			call mpi_barrier(MPI_COMM_WORLD, ierr)
			call mpi_bcast(n_mon,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
			allocate(x_mon(n_mon))
			allocate(y_mon(n_mon))

			if (id==0) then
				do i=1,n_mon
					read (20,*) x_mon(i), y_mon(i)
				end do
				close(20)
			end if
			call mpi_barrier(MPI_COMM_WORLD, ierr)
			call mpi_bcast(x_mon,n_mon,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
			call mpi_bcast(y_mon,n_mon,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

			! Read IB points from 'ib.txt'
			if (id==0) then
				open(unit=30,file='ib.txt',status='old',action='read')
				read (30,*) n_ib
			end if
			call mpi_barrier(MPI_COMM_WORLD, ierr)
			call mpi_bcast(n_ib,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
			allocate(x_ib(n_ib))
			allocate(y_ib(n_ib))

			if (id==0) then
				do i=1,n_ib
					read (30,*) x_ib(i), y_ib(i)
				end do
				close(30)
			end if
			call mpi_barrier(MPI_COMM_WORLD, ierr)
			call mpi_bcast(x_ib,n_ib,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
			call mpi_bcast(y_ib,n_ib,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

            
        end subroutine setup
        
        
        
        subroutine setTimestep(a)
        	double precision :: a
        	dt = a
        end subroutine
        

end module settings
