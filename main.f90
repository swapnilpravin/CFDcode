program main

    use mpi
	use mpi_vars
    use settings
    use partitioner
    use io, only : printHeader


	implicit none

    integer :: ierr!, Nproc, id

	!integer :: sub_size, sub_id
	!integer :: color, sub_comm
	!integer, parameter :: MASTER = 0, SLAVE = 1

    call mpi_init(ierr)
    !call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
    !call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)

	call set_mpi_vars()

    if (Nproc<3) then
        print*, ''
        print*, 'Error: Run with three or more processes!'
        print*, ''
		!call mpi_finalize(ierr)
        call exit()
    end if

	! assign colors for master and slave comms
	!if (id == 0) then
	!	color = MASTER
	!else
	!	color = SLAVE
	!end if

	! create new communicators
	!call mpi_comm_split(MPI_COMM_WORLD, color, id, sub_comm, ierr)

	!call mpi_comm_rank(sub_comm, sub_id, ierr)
	!call mpi_comm_size(sub_comm, sub_size, ierr)

	call setup('setup.txt')

	!if (color == MASTER) print*, 'SETUP DONE'
	!if (id == 0) print*, 'SETUP DONE'

    !call mpi_barrier(MPI_COMM_WORLD,ierr)

    if (color == MASTER) then
    !if (id == 0) then
        call printHeader()
    end if

	!print*, 'Comm color is: ', color

    call setPartitions()
	
	call channel()

    call mpi_finalize(ierr)

end program main
