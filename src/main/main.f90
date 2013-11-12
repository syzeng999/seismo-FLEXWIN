!> @file
!! Program: Global_Tomography_Data_Processing
!! Developer: Princeton Global Tomography Group(PGTG)
!! Group Member: Wenjie Lei(lei@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!! James A. Smith(jas11@princeton.edu)
!! Bug Report: lei@princeton.edu

program main

  use asdf_data
  use flexwin_struct
  use var_main

  use win_io
  use asdf_subs

  use flexwin_subs
	use main_subs

  use mpi
  implicit none

  type(asdf_event)        :: synt_all, obsd_all
  type(win_info),allocatable      :: win_all(:)
  type(flexwin_par_struct_all) 		:: flexwin_par_all

  !mpi_var
  integer                 :: nproc,comm,rank
  integer                 :: ierr,adios_err

  integer                 :: i

	real :: t1, t2, t3, t4

	call CPU_TIME(t1)
	!----------.
  !init mpi  !
	!----------'
  call mpi_init(ierr)
  call mpi_comm_dup(mpi_comm_world,comm,ierr)
  call mpi_comm_rank(comm,rank,ierr)
  call mpi_comm_size(comm,nproc,ierr)
  if(rank.eq.0) print *, "Start FLEXWIN..."
  if(rank.eq.0) print *, "NPROC:", nproc

  !--------------------------.
  !read main parfile         !
  !--------------------------'
  if(rank.eq.0) print *,"Read in main Parfile..."
  call read_main_parfile_mpi(rank,comm,ierr)
	!stop

  !--------------------------.
  !read in asdf data         !
  !--------------------------'
	if(rank.eq.0) then
   	print *,"-----------------"
   	print *,"Read ASDF file"
   	print *,"-----------------"
	endif
	if(rank.eq.0) then
  	print *, "OBSD_FILE: ",trim(OBSD_FILE)
		print *, "SYNT_FILE: ",trim(SYNT_FILE)
	endif
  call read_asdf_file(OBSD_FILE,obsd_all,rank,nproc,comm,ierr)
  print *, "read obsd finished!"
  call read_asdf_file(SYNT_FILE,synt_all,rank,nproc,comm,ierr)
  print *, "read synt finished!"
	if(rank.eq.0) then
  	print *, "/event:", trim(obsd_all%event)
	endif
  !stop

  !need to be removed in the future
  obsd_all%min_period=17.0
  obsd_all%max_period=60.0

  !--------------------------.
  !read parfile              !
  !--------------------------'
	if(rank.eq.0) then
   	print *,"-----------------"
   	print *,"Read Flexwin PAR_FILE"
   	print *,"-----------------"
	endif
  if(rank.eq.0) print *,"Read in flexwin Parfile..."
  call read_flexwin_parfile_mpi(flexwin_par_all, obsd_all%min_period,&
        obsd_all%max_period, obsd_all%event_dpt, obsd_all%nrecords, &
        rank, comm, ierr)

	call MPI_Barrier(comm,ierr)
 	print *, "rank, number of records: ", rank, obsd_all%nrecords

  allocate(win_all(obsd_all%nrecords))

  !--------------------------.
  !FLEXWIN                   !
  !--------------------------'
	if(rank.eq.0) then
   	print *,"-----------------"
   	print *,"RUNNING FLEXWIN"
   	print *,"-----------------"
	endif

  do i=1, obsd_all%nrecords
   !call flexwin subroutine
     call flexwin(obsd_all%records(i)%record,obsd_all%npoints(i),obsd_all%sample_rate(i),obsd_all%begin_value(i),&
				synt_all%records(i)%record,synt_all%npoints(i),synt_all%sample_rate(i),synt_all%begin_value(i),&
     		obsd_all%event_lat(i), obsd_all%event_lo(i), obsd_all%event_dpt(i),&
				obsd_all%receiver_lat(i),obsd_all%receiver_lo(i),&
     		obsd_all%receiver_name_array(i),obsd_all%network_array(i),obsd_all%component_array(i),&
				obsd_all%P_pick(i),obsd_all%S_pick(i),&
     		flexwin_par_all,win_all(i))
  enddo

  if(rank.eq.0) print *, "Write out WIN"
  call win_write(FLEXWIN_OUTDIR, obsd_all%event, obsd_all%min_period,&
          obsd_all%max_period, obsd_all%nrecords,&
          obsd_all%receiver_name_array, obsd_all%network_array,&
          obsd_all%component_array, obsd_all%receiver_id_array,&
          win_all, rank)

  !--------------------------.
  !finalize mpi              !
  !--------------------------'
  call MPI_Barrier(comm,ierr)
  call mpi_finalize(ierr)

	call CPU_TIME(t2)

	open(unit=22, file='cpu_time')
	write(22, *) "rank, time:", rank, t2-t1
	close(22)

end program main
