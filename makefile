objects = settings.obj  partitioner.obj  io.obj  communicator.obj  IBM.obj  schemes.obj  boundaryConditions.obj  solver.obj  channel.obj  main.obj  

#FC = ~/apps/tools/openmpi/bin/mpif90
#FC = mpif90
FC = pgfortran
#FC = $(HOME)/apps/bin/mpif90
#FFLAGS = -ffree-line-length-none -O3 -w
FFLAGS = -Mmpi=msmpi -O3
# for profiling: -pg 
#
# intel: (for profiling: -pg) -check bounds
#FFLAGS = -no-wrap-margin -O3


channel: $(objects)
	$(FC) $(FFLAGS) -o channel $(objects)

settings.obj  : settings.f90
	$(FC) $(FFLAGS) -c settings.f90

io.obj  : io.f90
	$(FC) $(FFLAGS) -c io.f90

partitioner.obj  : partitioner.f90
	$(FC) $(FFLAGS) -c partitioner.f90

communicator.obj  : communicator.f90
	$(FC) $(FFLAGS) -c communicator.f90

schemes.obj  : schemes.f90
	$(FC) $(FFLAGS) -c schemes.f90

boundaryConditions.obj  : boundaryConditions.f90
	$(FC) $(FFLAGS) -c boundaryConditions.f90 

solver.obj  : solver.f90
	$(FC) $(FFLAGS) -c solver.f90

IBM.obj  : IBM.f90
	$(FC) $(FFLAGS) -c IBM.f90

channel.obj  : channel.f90
	$(FC) $(FFLAGS) -c channel.f90

main.obj  : main.f90
	$(FC) $(FFLAGS) -c main.f90

# platelet_count.exe: platelet_count.f90
# 	ifort -o platelet_count.exe platelet_count.f90

clean:
	del *.obj  *.mod *.exe *.obj ut *.dat *.plt *.tec *~ *pbs.e* *pbs_run.obj * *pbs_run.e* core.* channel

clean-data:
	del *.tec 
	
test.obj : test.f90 
	$(FC) $(FFLAGS) -c test.f90
	
test: settings.obj  partitioner.obj  IBM.obj  test.obj  
	$(FC) $(FFLAGS) settings.obj  partitioner.obj  IBM.obj  test.obj  -o test