F90=gfortran
OPTIM_FLAG=-O3
USUAL_FLAG=-Wall
DEBUG_FLAG=-fbounds-check

PROG=run
SRC=read_and_write.f90 tools.f90 HLL.f90 MOOD.f90 main.f90

usual:
	$(F90) $(USUAL_FLAG) $(SRC) -o $(PROG)
optim:
	$(F90) $(OPTIM_FLAG) $(SRC) -o $(PROG)
debug:
	$(F90) $(DEBUG_FLAG) $(SRC) -o $(PROG)
clean:
	@rm -f *.o *.mod *~ core a.out $(PROG)
