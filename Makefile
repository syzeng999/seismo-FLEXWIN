#Directories
PWD    = $(shell pwd)
OBJDIR = ./obj
SRCDIR = ./src/main
#LIBDIR = $(PWD)/UTILS/lib
#IDIR   = $(PWD)/UTILS/include
#EVALLIB=/home/lei/ASDF/evalresp/.libs

# new version with default sac libraries
TAULIBDIR=$(PWD)/ttimes_mod
SACLIBDIR = ${SACHOME}/lib
LIBS = -lsacio -lsac -lm -ltau -lasdf

#all_obj = $(shell find . -name obj/*.*)

## set ADIOS_DIR here or before doing make
#override ADIOS_DIR:=/home/lei/bin/adios-1.5.0
#override ADIOS_INC:=`${ADIOS_DIR}/bin/adios_config -c -f`
#override ADIOS_FLIB:=`${ADIOS_DIR}/bin/adios_config -l -f`

ASDFLIBDIR=$(ASDFHOME)/lib
ASDFINCDIR=$(ASDFHOME)/include

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

#_OBJ = var_main_mod.o main_subs.o main.o 
_OBJ = main_subs.o main.o 

OBJ = $(patsubst %, ${OBJDIR}/%, $(_OBJ))

##########################################################
PROG = FLEXWIN_ASDF
default: MK_OBJDIR ${PROG}

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	  $(MPIFC) ${CFLAGS} -c -o $@ $< -J $(OBJDIR) -I$(ASDFINCDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f
	  $(MPIFC) ${CFLAGS} -c -o $@ $< -J $(OBJDIR) $(ASDFINCDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	  $(MPICC) -c -o $@ $< 

#include shared/Makefile FLEXWIN/Makefile measure_adj/Makefile

MK_OBJDIR:
	mkdir -p $(OBJDIR)

make_asdf:
	cd src/asdf_util; make

make_shared:
	cd src/shared; make

make_flexwin:
	cd src/flexwin; make

all_obj = $(wildcard $(OBJDIR)/*.o)

${PROG}: make_shared make_flexwin $(OBJ)
	${MPIFC} ${CFLAGS} -o $@ $(all_obj) \
		-L${TAULIBDIR} -L${SACLIBDIR} -L${ASDFLIBDIR} ${LIBS} ${ADIOS_FLIB}


.PHONY:clean print_var cleanall

print_var:
	@echo $(OBJ)
	@echo $(SRCDIR)
	@echo $(all_obj)

clean:
	rm -f  ${LIB_ALL} ${PROG} *.o *.mod *.a $(OBJDIR)/*

cleanall:
	rm -f  iasp91.*
	cd ${TAULIBDUR} ; make -f make_gfortran clean

