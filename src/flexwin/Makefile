#Directories
PWD    = $(shell pwd)
OBJDIR = ../../obj
SRCDIR = $(PWD)
#LIBDIR = $(PWD)/UTILS/lib
#IDIR   = $(PWD)/UTILS/include
#EVALLIB=/home/lei/ASDF/evalresp/.libs

#CFLAGS = -check noarg_temp_created

# new version with default sac libraries
SACLIBDIR = ${SACHOME}/lib
LIBS = -lsacio -lsac -lasdf
 #LIB = -L/opt/seismo/lib -lDRWFiles -lf90recipes -lDSacio -lDSacLib -lSacTools -lm

ASDFLIBDIR=$(ASDFHOME)/lib
ASDFINCDIR=$(ASDFHOME)/include

## set ADIOS_DIR here or before doing make
#override ADIOS_DIR:=/home/lei/bin/adios-1.5.0
#override ADIOS_INC:=`${ADIOS_DIR}/bin/adios_config -c -f`
#override ADIOS_FLIB:=`${ADIOS_DIR}/bin/adios_config -l -f`

ADIOS_FLIB=$(shell adios_config -lf)
ADIOS_INC=$(shell adios_config -cf)

############################
#compiler option
#OPT = -I${SHARED}
#OPT = -std03
FC = ftn
CC = cc
MPIFC = ftn
MPICC = cc
#CFLAGS= -g -O0 -check all

_OBJ = 	copy_float.o user_parameters.o \
			 	seismo_subs.o io_subs.o user_functions.o \
				maxima.o travel_times.o \
				select_windows_stalta2.o flexwin_interface_subs.o flexwin.o \
				flexwin_interface.o

OBJ = $(patsubst %, ${OBJDIR}/%, $(_OBJ))

##########################################################
PROG = TEST
default: ${PROG}

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	  $(MPIFC) ${CFLAGS} -c -o $@ $< -J $(OBJDIR) -I$(ASDFINCDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f
	  $(MPIFC) ${CFLAGS} -c -o $@ $< -J $(OBJDIR) -I$(ASDFINCDIR)
.F.o:
	$(OBJDIR)/%.o: $(SRCDIR)/%.F
	  $(MPIFC) ${CFLAGS} -c -o $@ $< -J $(OBJDIR) -I$(ASDFINCDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	  $(MPICC) -c -o $@ $< 

${PROG}: $(OBJ)


.PHONY:clean print_var cleanall

print_var:
	@echo $(OBJ)
	@echo $(SRCDIR)

clean:
	rm -f  ${LIB_ALL} ${PROGS} *.o *.mod *.a $(OBJDIR)/*

cleanall:
	rm -f  iasp91.*
	cd ${TAULIBDUR} ; make -f make_gfortran clean

