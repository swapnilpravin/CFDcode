module mpi_vars

	! mpi variables to store that can then be shared with all subroutines that need them

	use mpi
	implicit none

	integer :: Nproc		! size of MPI_COMM_WORLD
	integer :: id			! rank of process in MPI_COMM_WORLD

	integer :: sub_comm		! communicators created after splitting MPI_COMM_WORLD into Master and Slave

	integer :: sub_size		! size of sub_comm
	integer :: sub_id		! rank of process in sub_comm

	! color of sub comms created from MPI_COMM_WORLD
	integer :: color
	integer, parameter :: MASTER = 0
	integer, parameter :: SLAVE = 1

	contains

		subroutine set_mpi_vars()

			integer :: ierr

			call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)
			call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)

			! assign colors for master and slave processes
			if (id == 0) then
				color = MASTER
			else
				color = SLAVE
			end if

			! create new comms
			call mpi_comm_split(MPI_COMM_WORLD, color, id, sub_comm, ierr)

			call mpi_comm_size(sub_comm, sub_size, ierr)
			call mpi_comm_rank(sub_comm, sub_id, ierr)
		end subroutine set_mpi_vars

end module mpi_vars
