module flexwin_subs

contains
!----------------------------------------------------------------------
!> Modifiedy by Wenjie Lei
!> Change flexwin into a subroutine, called by the workflow
!> start from the very basic, meanign the input and output variables are basic

!> @param obs synt, obs and all the other info
!> @param npts2 num_win, win_start, win_end

!> @author Alesia(Original) Wenjie Lei()
  subroutine flexwin(obs_in, npts2_in, dt2_in, b2_in, &
                      synt_in, npts1_in, dt1_in, b1_in, &
                      evla_in, evlo_in, evdp_in, stla_in, stlo_in, &
                      kstnm_in, knetwk_in, kcmpnm_in, &
                      P_pick_in, S_pick_in, &
                      flexwin_par_all,win)

    use flexwin_struct 
    use user_parameters
    use seismo_variables
    implicit none

    real,intent(in) :: synt_in(:), obs_in(:)
    real,intent(in) :: dt2_in, dt1_in, b2_in, b1_in
    integer,intent(in) :: npts2_in, npts1_in
    character(len=*),intent(in) :: kstnm_in, knetwk_in, kcmpnm_in
    real :: evla_in, evlo_in, stla_in, stlo_in, evdp_in
    real :: P_pick_in, S_pick_in

    type(win_info) :: win
    type(flexwin_par_struct_all) :: flexwin_par_all

    integer :: i
    integer :: ierr
    
    print *, "============"
    call copy_flexwin_par_to_local(flexwin_par_all, kcmpnm_in)
    
    if(DEBUG) write(*,*) "DEBUG: reading sac file"
    call copy_var_to_module_var(obs_in, npts2_in, dt2_in, b2_in, &
                      synt_in, npts1_in, dt1_in, b1_in, &
                      evla_in, evlo_in, evdp_in, stla_in, stlo_in, &
                      kstnm_in, knetwk_in, kcmpnm_in, &
                      P_pick_in, S_pick_in)

    write(*,*) "FLEXWIN--station, network, cmp: ", trim(kstnm), ".", trim(knetwk),&
          ".", trim(kcmpnm)
    if(DEBUG) then
      write(*,*) "obs npts:", npts2, " synt npts:", npts1
    endif

    !do this outside of this subroutine
    !call read_parameter_file()

    !do i=1,obsd_all%nrecords

     !check the consistency between obs and synt
     call check_and_filter_data(ierr)
     if(ierr.eq.1)then
       print *,"Data check failed. Skip this record now!"
       win%num_win=0
       return
     endif

     if (DEBUG) write(*,*) 'DEBUG : selecting windows'
     call select_windows_stalta2()

        !if(MAKE_SEISMO_PLOTS) then
        !  if (DEBUG) write(*,*) 'DEBUG : writing output seismos'
        !  call write_seismos_gmt(basename(i))
        !endif

        !if(MAKE_WINDOW_FILES) then
        !  if (DEBUG) write(*,*) 'DEBUG : writing mt input'
        !  call write_mt_input_whole(OON,obs_name(i),syn_name(i))
        !endif

        !print *,"=============================="
        !print *,"window selection finished,write out win begins"
        !print *,num_win
        !print *,win_start(1:num_win)
        !print *,win_end(1:num_win)
    
        !print *,win%num_win
      win%num_win=num_win
      !print *,"write out num"
      if(num_win/=0)then
        allocate(win%t_start(num_win))
        allocate(win%t_end(num_win))
        win%t_start(1:num_win)=win_start(1:num_win)
        !print *,"write out start time"
        win%t_end(1:num_win)=win_end(1:num_win)
        !print *,"write out end time"
      endif

    !enddo
    !print *,"write out end"
    !print *,"=============================="

  end subroutine flexwin

!-----------------------------------------------------------------------
  subroutine copy_var_to_module_var(obs_in, npts2_in, dt2_in, b2_in, &
                  synt_in, npts1_in, dt1_in, b1_in, &
                  evla_in, evlo_in, evdp_in, stla_in, stlo_in, &
                  kstnm_in, knetwk_in, kcmpnm_in, &
                  P_pick_in, S_pick_in)

    use seismo_variables
    implicit none

    real,intent(in) :: synt_in(*), obs_in(*)
    real ,intent(in) :: dt2_in, dt1_in, b2_in, b1_in
    integer,intent(in) :: npts2_in, npts1_in
    character(len=*),intent(in) :: kstnm_in, knetwk_in, kcmpnm_in 
    real :: evla_in, evlo_in, stla_in, stlo_in, evdp_in
    real :: P_pick_in, S_pick_in

    integer :: string_len
        
    obs(1:npts2_in)=dble(obs_in(1:npts2_in))
    npts2=npts2_in
    dt2=dble(dt2_in)
    b2=dble(b2_in)
    synt(1:npts1_in)=dble(synt_in(1:npts1_in))
    npts1=npts1_in
    dt1=dble(dt1_in)
    b1=dble(b1_in)

    evla=evla_in
    evlo=evlo_in
    evdp=evdp_in
    stla=stla_in
    stlo=stlo_in

    P_pick=P_pick_in
    S_pick=S_pick_in

    string_len=min(len(kstnm),len(kstnm))
    !print *,"string_len station",string_len
    kstnm(1:string_len)=kstnm_in(1:string_len)
    string_len=min(len(knetwk),len(knetwk_in))
    !print *,"string_len network",string_len
    knetwk(1:string_len)=knetwk_in(1:string_len)
    string_len=min(len(kcmpnm),len(kcmpnm_in))
    !print *,"string_len component",string_len
    kcmpnm(1:string_len)=kcmpnm_in(1:string_len)
    !print *,
    !print *,trim(kcmpnm)

  end subroutine copy_var_to_module_var

  !subroutine copy_global_to_local(obsd_all,synt_all,loc)
  
  !type(asdf_event),intent(in) :: obsd_all,synt_all
  !integer :: loc
  !copy
  !synt data info
  !dt1=synt_all%sample_rate(loc)
  !npts1=synt_all%npoints(loc)
  !b1=synt_all%begin_value(loc)
  !synt(1:npts1)=synt_all%records(loc)%record(1:npts1)
  
  !obs data info
  !dt2=obsd_all%sample_rate(loc)
  !npts2=obsd_all%npoints(loc)
  !b2=obsd_all%begin_value(loc)
  !obs(1:npts2)=obsd_all%records(loc)%record(1:npts2)

  !location info
  !evla=obsd_all%event_lat
  !evlo=obsd_all%event_lo
  !evdp=obsd_all%event_dpt

  !stla=obsd_all%receiver_lat(loc)
  !stlo=obsd_all%receiver_lo(loc)

  !station info
  !kstnm=obsd_all%receiver_name(loc)
  !knetwk=obsd_all%network(loc)
  !kcmpnm=obsd_all%component(loc)


  !P_arrival and S_arrival
  !P_pick=obsd_all%P_pick(loc)
  !S_pick=obsd_all%S_pick(loc)

  !end subroutine copy_global_to_local

  subroutine copy_flexwin_par_to_local(flexwin_par_all,kcmpnm_in)

    use flexwin_struct 
    implicit none

    type(flexwin_par_struct_all) :: flexwin_par_all
    character(len=*),intent(in) :: kcmpnm_in

    !print *, "kcmpnm:", trim(kcmpnm)
    if(kcmpnm_in(3:3)=="Z") then
      call copy_flexwin_par_to_local_sub(flexwin_par_all%Z)
    elseif(kcmpnm_in(3:3)=="R")then
      call copy_flexwin_par_to_local_sub(flexwin_par_all%R)
    elseif(kcmpnm_in(3:3)=="T")then
      call copy_flexwin_par_to_local_sub(flexwin_par_all%T)
    else
      stop
    endif

  end subroutine copy_flexwin_par_to_local

  subroutine copy_flexwin_par_to_local_sub(flexwin_par)
   
    use flexwin_struct 
    use seismo_variables
    implicit none

    type(flexwin_par_struct) :: flexwin_par

    !copy
    DEBUG             = flexwin_par%DEBUG
    print *, "FLEXWIN DEBUG:", DEBUG
    MAKE_SEISMO_PLOTS = flexwin_par%MAKE_SEISMO_PLOTS
    MAKE_WINDOW_FILES = flexwin_par%MAKE_WINDOW_FILES
    BODY_WAVE_ONLY    = flexwin_par%BODY_WAVE_ONLY
    RUN_BANDPASS      = flexwin_par%RUN_BANDPASS
    WIN_MIN_PERIOD    = flexwin_par%WIN_MIN_PERIOD
    WIN_MAX_PERIOD    = flexwin_par%WIN_MAX_PERIOD
    FSTART   = flexwin_par%FSTART
    FEND     = flexwin_par%FEND
    STALTA_BASE       = flexwin_par%STALTA_BASE
    TSHIFT_BASE       = flexwin_par%TSHIFT_BASE
    TSHIFT_REFERENCE  = flexwin_par%TSHIFT_REFERENCE
    DLNA_BASE         = flexwin_par%DLNA_BASE
    DLNA_REFERENCE    = flexwin_par%DLNA_REFERENCE
    CC_BASE           = flexwin_par%CC_BASE
    DATA_QUALITY      = flexwin_par%DATA_QUALITY
    SNR_INTEGRATE_BASE = flexwin_par%SNR_INTEGRATE_BASE
    SNR_MAX_BASE      = flexwin_par%SNR_MAX_BASE
    WINDOW_S2N_BASE   = flexwin_par%WINDOW_S2N_BASE
    C_0  = flexwin_par%C_0
    C_1  = flexwin_par%C_1
    C_2  = flexwin_par%C_2
    C_3a = flexwin_par%C_3a
    C_3b = flexwin_par%C_3b
    C_4a = flexwin_par%C_4a
    C_4b = flexwin_par%C_4b
    WEIGHT_SPACE_COVERAGE = flexwin_par%WEIGHT_SPACE_COVERAGE
    WEIGHT_AVERAGE_CC = flexwin_par%WEIGHT_AVERAGE_CC
    WEIGHT_N_WINDOWS  = flexwin_par%WEIGHT_N_WINDOWS

  end subroutine copy_flexwin_par_to_local_sub
  
  !>read flexwin parameter file
  !!read in three component and store them into flexwin_par_all
  subroutine read_flexwin_parfile(flexwin_par_all, fstart, fend, event_dpt)

    use flexwin_struct 
    implicit none

    !here, fstart and fend are actually the period(min_period, max_period) here
    type(flexwin_par_struct_all) :: flexwin_par_all
    real :: fstart, fend, event_dpt

    integer :: fstart_int, fend_int
    character(len=20) :: fstart_string, fend_string

    integer :: i
    character(len=150) :: parfile_dir, fn
    
    !print *, fstart, fend
    write(fstart_string,'(i10)')int(fstart)
    write(fend_string,'(i10)')int(fend)
    !print *, fstart_string, fend_string
    fstart_string=adjustl(fstart_string)
    fend_string=adjustl(fend_string)
    
    parfile_dir="./PAR_FILES"

    !read in three component into flexwin_par_all
    fn=trim(parfile_dir)//"/PAR_FILE_"//trim(fstart_string)//"_"//&
              trim(fend_string)//"_Z"
    print *, "Reading in flexwin par in Z component"
    print *, "parfile name:", trim(fn)
    call read_flexwin_parfile_comp(fn, flexwin_par_all%Z)
    !fn="./FLEXWIN/PAR_FILE"
    fn=trim(parfile_dir)//"/PAR_FILE_"//trim(fstart_string)//"_"//&
              trim(fend_string)//"_R"
    print *, "Reading in flexwin par in R component"
    print *, "parfile name:", trim(fn)
    call read_flexwin_parfile_comp(fn, flexwin_par_all%R)
    !fn="./FLEXWIN/PAR_FILE"
    fn=trim(parfile_dir)//"/PAR_FILE_"//trim(fstart_string)//"_"//&
              trim(fend_string)//"_T"
    print *, "Reading in flexwin par in T component"
    print *, "parfile name:", trim(fn)
    call read_flexwin_parfile_comp(fn, flexwin_par_all%T)

    !stop

  end subroutine read_flexwin_parfile

  subroutine read_flexwin_parfile_comp(fn, flexwin_par)

    use flexwin_struct 
    use seismo_variables
    implicit none

    character(len=*) :: fn
    type(flexwin_par_struct) :: flexwin_par

    integer, parameter :: IIN = 11
    integer, parameter :: NHEAD = 12

    integer :: idummy
    character(len=34) junk

    open(unit=IIN,file=fn,status='old')

    ! ignore header
    do idummy=1,NHEAD
      read(IIN,*)
    enddo

    !--------------------------------------------------------
    ! read parameters, skipping empty lines and comment lines

    ! boolean parameters
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,3) junk,flexwin_par%DEBUG
    read(IIN,3) junk,flexwin_par%MAKE_SEISMO_PLOTS
    read(IIN,3) junk,flexwin_par%MAKE_WINDOW_FILES
    read(IIN,3) junk,flexwin_par%BODY_WAVE_ONLY
    if (flexwin_par%DEBUG) then
      write(*,*) 'DEBUG: ------------------------------------:'
      write(*,*) 'DEBUG: PARAMETERS read in from PAR_FILE are:'
      write(*,*) '       DEBUG',flexwin_par%DEBUG
      write(*,*) '       MAKE_SEISMO_PLOTS',flexwin_par%MAKE_SEISMO_PLOTS
      write(*,*) '       MAKE_WINDOW_FILES',flexwin_par%MAKE_WINDOW_FILES
      write(*,*) '       BODY_WAVE_ONLY',flexwin_par%BODY_WAVE_ONLY
    endif

    ! period min/max for filtering
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,3) junk,flexwin_par%RUN_BANDPASS
    read(IIN,2) junk,flexwin_par%WIN_MIN_PERIOD
    read(IIN,2) junk,flexwin_par%WIN_MAX_PERIOD
    FSTART = 1./flexwin_par%WIN_MAX_PERIOD
    FEND   = 1./flexwin_par%WIN_MIN_PERIOD
    if (flexwin_par%DEBUG) then
      write(*,*) '       WIN_MIN_PERIOD',flexwin_par%WIN_MIN_PERIOD
      write(*,*) '       WIN_MAX_PERIOD',flexwin_par%WIN_MAX_PERIOD
    endif

    ! E(t) water level
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%STALTA_BASE
    if (flexwin_par%DEBUG) write(*,*) '       STALTA_BASE',flexwin_par%STALTA_BASE

    ! Tshift
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%TSHIFT_BASE
    read(IIN,2) junk,flexwin_par%TSHIFT_REFERENCE
    if (flexwin_par%DEBUG) write(*,*) '       TSHIFT_BASE',flexwin_par%TSHIFT_BASE
    if (flexwin_par%DEBUG) write(*,*) '       TSHIFT_REFERENCE',flexwin_par%TSHIFT_REFERENCE

    ! limit on dlnA for window acceptance
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%DLNA_BASE
    read(IIN,2) junk,flexwin_par%DLNA_REFERENCE
    if (flexwin_par%DEBUG) write(*,*) '       DLNA_BASE',flexwin_par%DLNA_BASE
    if (flexwin_par%DEBUG) write(*,*) '       DLNA_REFERENCE',flexwin_par%DLNA_REFERENCE

    ! limit on CC
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%CC_BASE
    if (flexwin_par%DEBUG) write(*,*) '       CC_BASE',flexwin_par%CC_BASE

    ! boolean switch for check_data_quality
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,3) junk,flexwin_par%DATA_QUALITY

    ! if DATA_QUALITY = .true. and if two different measurements of
    ! signal-to-noise ratios exceeds these two base levels,
    ! then the data time series (and syn) is kept
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%SNR_INTEGRATE_BASE
    read(IIN,2) junk,flexwin_par%SNR_MAX_BASE

    ! limit on signal to noise ratio in a particular window.
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%WINDOW_S2N_BASE

    ! Fine tuning constants
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%C_0
    read(IIN,2) junk,flexwin_par%C_1
    read(IIN,2) junk,flexwin_par%C_2
    read(IIN,2) junk,flexwin_par%C_3a
    read(IIN,2) junk,flexwin_par%C_3b
    read(IIN,2) junk,flexwin_par%C_4a
    read(IIN,2) junk,flexwin_par%C_4b
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%WEIGHT_SPACE_COVERAGE
    read(IIN,2) junk,flexwin_par%WEIGHT_AVERAGE_CC
    read(IIN,2) junk,flexwin_par%WEIGHT_N_WINDOWS

    if (flexwin_par%DEBUG) then
      write(*,*) 'DEBUG: ------------------------------------:'
    endif

    !--------------------------------------------------------
    ! close parameter file
    close(IIN)


    !--------------------------------------------------------
    ! line formats
2   format(a,f20.8)
3   format(a,l20)

    ! unused formats
    ! 1 format(a,i20)
    ! 4 format(a,a)

  end subroutine
!----------------------------------------------------------------------
end module flexwin_subs
