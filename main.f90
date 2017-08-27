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

	call setup('setup.txt')

	!if (color == MASTER) print*, 'SETUP DONE'
	!if (id == 0) print*, 'SETUP DONE'

    !call mpi_barrier(MPI_COMM_WORLD,ierr)

    if (color == MASTER) call printHeader()

	if (color == SLAVE) call setPartitions()
	
	call channel()

    call mpi_finalize(ierr)

end program main
