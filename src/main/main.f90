!> @file
!! Program: Global_Tomography_Data_Processing
!! Developer: Princeton Global Tomography Group(PGTG)
!! Group Member: Wenjie Lei(lei@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!! James A. Smith(jas11@princeton.edu)
!! Bug Report: lei@princeton.edu

program main

  use asdf_data
  use asdf_read_subs

  use flexwin_struct
  use var_main
  use fw_interface
  use main_subs

  use mpi
  implicit none

  type(asdf_event)        :: synt_all, obsd_all
  type(win_info),allocatable      :: win_all(:)
  type(flexwin_par_struct_all)    :: flexwin_par_all

  integer :: nrecords
  character(len=20) :: station(MAXDATA_PER_PROC), network(MAXDATA_PER_PROC)
  character(len=20) :: component(MAXDATA_PER_PROC), receiver_id(MAXDATA_PER_PROC)

  !mpi_var
  integer                 :: nproc,comm,rank
  integer                 :: ierr,adios_err

  integer                 :: i
  double precision        :: t1, t2, t3, t4

  !t1 = MPI_Wtime()
  !----------.
  !init mpi  !
  !----------'
  call mpi_init(ierr)
  call mpi_comm_dup(mpi_comm_world,comm,ierr)
  call mpi_comm_rank(comm,rank,ierr)
  call mpi_comm_size(comm,nproc,ierr)
  if(rank.eq.0) print *, "Start FLEXWIN...NPROC:", NPROC

  !--------------------------.
  !read main parfile         !
  !--------------------------'
  if(rank.eq.0) print *,"Read in main Parfile..."
  call read_main_parfile_mpi(rank,comm,ierr)

  !--------------------------.
  !read in asdf data         !
  !--------------------------'
  if(rank.eq.0) then
    print *,"-----------------"
    print *,"Read ASDF file"
    print *,"-----------------"
    print *, "OBSD_FILE: ",trim(OBSD_FILE)
    print *, "SYNT_FILE: ",trim(SYNT_FILE)
  endif
  call read_asdf_file(OBSD_FILE, obsd_all, nrecords, &
    station, network, component, receiver_id, 0, &
    rank, nproc, comm, ierr)
  call read_asdf_file(SYNT_FILE, synt_all, nrecords, &
    station, network, component, receiver_id, 1, &
    rank, nproc, comm, ierr)

  if(rank.eq.0) then
    print *, "/event:", trim(obsd_all%event)
  endif
  print *, "rank, number of records: ", rank, obsd_all%nrecords

  !WJ:need to be removed in the future
  obsd_all%min_period=PERIOD_BEGIN
  obsd_all%max_period=PERIOD_END
  print *, "PERIOD:", obsd_all%min_period, obsd_all%max_period

  !flexwin interface(file: flexwin_interface.f90)
  call flexwin_interface(obsd_all, synt_all, win_all, &
        rank, nproc, comm, ierr)

  !--------------------------.
  !finalize mpi              !
  !--------------------------'
  call MPI_Barrier(comm,ierr)
  call mpi_finalize(ierr)

  !t2 = MPI_Wtime()
  !open(unit=22, file='cpu_time')
  !write(22, *) "rank, time:", rank, t2-t1
  !close(22)

end program main
