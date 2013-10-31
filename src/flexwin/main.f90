!==============================================================================
!Program: Global_Tomography_Data_Processing
!Developer: Princeton Global Tomography Group(PGTG)
!Group Member: Wenjie Lei(lei@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!James A. Smith(jas11@princeton.edu)
!===================
!Functions:
!1) Read ADIOS
!2) Processing: rtrend, rmean, taper, inst_remove(filter)
!3) data_quality: generate the useful data list
!4) flexwin: window selection
!5) write out the new ADIOS file(filtered, selected and window selected)
!===================
!Bug Report: lei@princeton.edu
!===============================================================================

!-------------------------------------------------------------
!main: mimicking the whole work flow
program main

  !use adios_read_mod
  !use adios_write_mod
  use asdf_data
  use asdf_subs
  use win_io
  !use asdf_subs
	use flexwin_subs
  implicit none
  include 'mpif.h'

  integer :: i
  !this is the input file name
  !character(len=120) :: input_fn='input_cmt_LHZ_17_60'
  character(len=150) :: OBSD_FILE, SYNT_FILE
  !this is the output directory
  character(len=120) :: OUTDIR
  !variables
  type(asdf_event) :: synt_all,obsd_all
  type(win_info),allocatable  :: win(:)
  type(flexwin_par_struct_all) :: flexwin_par_all

  integer :: myid,numprocs,comm
  integer :: ierr

  !init mpi
  call mpi_init(ierr)
  call mpi_comm_dup(mpi_comm_world,comm,ierr)
  call mpi_comm_rank(comm,myid,ierr)
  call mpi_comm_size(comm,numprocs,ierr)
  !split the job for every processor

  OBSD_FILE='./DATA/200801151752A_obsd.bp'
  SYNT_FILE='./DATA/200801151752A_synt.bp'
  OUTDIR='./OUTPUT'

  !call ADIOS_read(obsd_all,synt_all,input_fn)
  call read_asdf_file(OBSD_FILE,obsd_all,comm)
  print *,"Reading OBSD finished"
  call read_asdf_file(SYNT_FILE,synt_all,comm)
  print *,"Reading SYNT finished"
  !print *,"=============================="
  !print *,"The number of processor:", myid
  !print *,"The number of seis read in:",n_seis
  !print *,"=============================="

  allocate(win(obsd_all%nrecords))

  !call read_flexwin_parfile(flexwin_par_all, obsd_all%min_period, &
  !        obsd_all%max_period)
  call read_flexwin_parfile(flexwin_par_all,17.0,60.0)
  print *, 'Reading FLEXWIN PAR finished'
  !stop

  do i=1,obsd_all%nrecords
    !----
    !----
    !data process
    !call data_process(obsd(i),synt(i))
    !data selection:see if the obs is usable
    !call data_quality_check(obsd(i),dq(i))
    !flexwin: output is the window file for each processor
    !if(dq(i)%good) then
  
    call flexwin(obsd_all%records(i)%record,obsd_all%npoints(i),obsd_all%sample_rate(i),obsd_all%begin_value(i),&
    synt_all%records(i)%record, synt_all%npoints(i),synt_all%sample_rate(i),synt_all%begin_value(i),&
    obsd_all%event_lat, obsd_all%event_lo, obsd_all%event_dpt,obsd_all%receiver_lat(i),obsd_all%receiver_lo(i),&
    obsd_all%receiver_name_array(i),obsd_all%network_array(i),obsd_all%component_array(i),&
    obsd_all%P_pick(i),obsd_all%S_pick(i),&
    flexwin_par_all,win(i))

  enddo

  print *,"flexwin finished"
    !else
      !win%num_win=0
    !endif
  !enddo
  print *,"=============================="
  print *,"The number of processor:",myid
  print *,"FLEXWIN finished"
  print *,"=============================="

  !write out: output is the window information
  call win_write(OUTDIR,obsd_all%nrecords,obsd_all%receiver_name_array,&
                  obsd_all%network_array, obsd_all%component_array,&
                  obsd_all%receiver_id_array,win,myid)

  !finalize mpi
  call mpi_finalize(ierr)

end program main
