module main_subs

  implicit none

contains

subroutine read_main_parfile_mpi(rank, comm, ierr)

  use var_main

  use mpi 
  integer :: rank, comm, ierr
  
  if(rank.eq.0)then
    print *, "Read in master node:"
    call read_main_parfile(ierr)
  endif

  print *,"Bcast the par..."
  call MPI_Bcast(OBSD_FILE,150,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(SYNT_FILE,150,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(FLEXWIN_OUTDIR,150,MPI_CHARACTER,0,comm,ierr)

	!if(rank.eq.1) then
	!	print *, "MPI_staff"
	!	print *, RUN_FLEXWIN, RUN_MEASURE_ADJ, WRITE_ADJ_ASDF,&
	!			ROTATE_COMP, WRITE_NORMAL_OUTPUT
	!		print *, trim(OBSD_FILE), 	
	!   PRINT *, trim(MEASURE_ADJ_OUTDIR)
	!endif

end subroutine read_main_parfile_mpi


subroutine read_main_parfile(ierr)

  !read the parfile for the main(some flags)
  use var_main

  integer :: dummy_row
  integer :: ierr
  integer :: IIN=21
  integer :: i

  character(len=30) :: dummy_string

  !print *,"Read main par"
  dummy_row = 8
  
  open(UNIT=IIN,FILE="PAR_FILE_MAIN",iostat=ierr)
  if(ierr.ne.0)then
    print *,"Can't find PAR_FILE_MAIN. Stop! "
    stop
  endif

  do i=1,dummy_row
    read(IIN,*)
  enddo

  !print *,"HERE"

	read(IIN,*)

	read(IIN,2) dummy_string, OBSD_FILE
	print *, "OBSD_FILE: ", trim(OBSD_FILE)
	read(IIN,2) dummy_string, SYNT_FILE
	print *, "SYNT_FILE: ", trim(SYNT_FILE)
	read(IIN,2) dummy_string, FLEXWIN_OUTDIR
	print *, "FLEXWIN_OUTDIR: ", trim(FLEXWIN_OUTDIR)

2 format(a,a)
3 format(a,l20)
4 format(a,i)

  close(IIN)
	!stop

end subroutine read_main_parfile

subroutine read_flexwin_parfile_mpi(flexwin_par_all, min_period, &
                      max_period, event_dpt, nrecords, &
											rank, comm, ierr)

	use flexwin_struct
	use flexwin_subs

  use mpi

	type(flexwin_par_struct_all) :: flexwin_par_all
	real :: min_period, max_period, event_dpt(:)
	integer :: rank, comm, ierr
  integer :: nrecords

	integer :: oldtype(6), newtype, offset(6), blockcount(6)
	integer :: extent

	integer :: tag=1, i, source, loc
	integer :: stat(MPI_STATUS_SIZE)

	character(len=150) :: fn

  real :: aver_event_dpt


  aver_event_dpt=sum(event_dpt(1:nrecords))/nrecords
  !check if the event_dpt are simliar
  !cause current version of FLEXWIN can only deal with asdf from one earthquake
  if(abs(aver_event_dpt-event_dpt(1)).gt.5.0) then
    print *, "Check if the event_dpt(:) array are the same"
    print *, "This version FLEXWIN only takes asdf from one earthquake"
    stop
  endif
	!print *,"SET UP"
	!setup description of the flexwin_par
	!call read_flexwin_parfile_mpi(flexwin_par, fstart, fend, rank, nproc, comm)

  do i=1,3
    loc=2*i-1
    if(loc.eq.1)then
      offset(loc)=0
    else
      call MPI_TYPE_EXTENT(MPI_DOUBLE_PRECISION, extent, ierr)
	    offset(loc) = offset(loc-1)+23*extent
    endif
	  oldtype(loc) = MPI_LOGICAL
	  blockcount(loc) = 6

   	loc=2*i
	  call MPI_TYPE_EXTENT(MPI_LOGICAL, extent, ierr)
	  offset(loc) = offset(loc-1)+6*extent
	  oldtype(loc) = MPI_DOUBLE_PRECISION
	  blockcount(loc) = 23
		!if(rank.eq.0) then
		!	print *,"blockcount",loc,blockcount(loc)
		!endif
		!if(rank.eq.1) then
		!	print *,"blockcount:",blockcount(loc)
		!endif

		!print *,i, blockcount(1), blockcount(2), blockcount(3), blockcount(4),&
		!					blockcount(5), blockcount(6)
  enddo

	!now define and commit
	call MPI_TYPE_STRUCT(6, blockcount, offset, oldtype, newtype, ierr)
	call MPI_TYPE_COMMIT(newtype, ierr)

	!print *, "SET UP finished!"

	if(rank.eq.0) then
    print *,"Period Band(s):", min_period, max_period
    print *,"event_dpt:", aver_event_dpt
		call read_flexwin_parfile(flexwin_par_all, min_period, &
                      max_period, aver_event_dpt)
	endif

  call MPI_Bcast(flexwin_par_all, 1, newtype, 0, comm, ierr)
	
	!if(rank==1) then
	!	print *, flexwin_par_all%T%DEBUG
	!	print *, flexwin_par_all%T%CC_BASE
	!	print *, flexwin_par_all%T%WEIGHT_N_WINDOWS
	!endif
	
	print *, "Finalize reading flexwin parfile"

end subroutine read_flexwin_parfile_mpi

end module main_subs
