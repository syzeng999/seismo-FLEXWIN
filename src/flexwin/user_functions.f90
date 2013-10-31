! -------------------------------------------------------------
! edit here to change T0 and T1 on some condition 
! Note, this function is called AFTER the seismogram has been 
! read but before it is filtered.
! -------------------------------------------------------------

subroutine modify_T0_T1_on_condition
  use seismo_variables

  ! do nothing

  ! adjust fstart and fend accordingly
  FSTART=1./WIN_MAX_PERIOD
  FEND=1./WIN_MIN_PERIOD

  end subroutine


subroutine set_up_criteria_arrays
	use seismo_variables
	implicit none

	!based on component info and period info to choose specific criteria
	if(WIN_MAX_PERIOD.le.80.0 .and. WIN_MIN_PERIOD.ge.5.0)then
		!body wave
		select case (kcmpnm(3:3))
			case ("Z")
				call set_up_criteria_arrays_bw_Z
			case ("R")
				call set_up_criteria_arrays_bw_RT
			case ("T")
				call set_up_criteria_arrays_bw_RT
			case DEFAULT
				write(*,*) "Specific set_up_criteria subroutine missing."
				write(*,*) "COMPONENT: ", kcmpnm(3:3), &
						" PERIOD: ", WIN_MAX_PERIOD, WIN_MIN_PERIOD
				write(*,*) "Create your own if you want to continuew"
				stop
		end select
	else if(WIN_MAX_PERIOD.le.140.0 .and. WIN_MIN_PERIOD.ge.50.0) then
		!surface wave
		select case (kcmpnm(3:3))
			case ("Z")
				call set_up_criteria_arrays_sw_Z
			case ("R")
				call set_up_criteria_arrays_sw_RT
			case ("T")
				call set_up_criteria_arrays_sw_RT
			case DEFAULT
				write(*,*) "Specific set_up_criteria subroutine missing."
				write(*,*) "COMPONENT: ", kcmpnm(3:3), &
						" PERIOD: ", WIN_MAX_PERIOD, WIN_MIN_PERIOD
				write(*,*) "Create your own if you want to continuew"
				stop
		end select
	else
		!none file found
		write(*,*) "Specific set_up_criteria subroutine missing."
		write(*,*) "COMPONENT: ", kcmpnm(3:3), &
				" PERIOD: ", WIN_MAX_PERIOD, WIN_MIN_PERIOD
		write(*,*) "Create your own if you want to continuew"
		stop
	endif

end subroutine


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_bw_RT
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! This is the basic version of the subroutine - no variation with time
! -----------------------------------------------------------------
  do i = 1, npts
    time=b+(i-1)*dt
    DLNA_LIMIT(i)=DLNA_BASE
    CC_LIMIT(i)=CC_BASE
    TSHIFT_LIMIT(i)=TSHIFT_BASE
    STALTA_W_LEVEL(i)=STALTA_BASE
    S2N_LIMIT(i)=WINDOW_S2N_BASE
  enddo

  ! these values will be used for signal2noise calculations
  ! if DATA_QUALITY=.true.
  if (DATA_QUALITY) then
   ! noise_end=ph_times(1)-WIN_MAX_PERIOD
    noise_end=S_pick-(WIN_MAX_PERIOD+50)
    noise_start=b
    signal_end=b+(npts-1)*dt
    signal_start = noise_end
  endif


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.2
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 !Q_vel=4.2
 Q_vel=4.8
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 if (DATA_QUALITY) then
   signal_end=R_time
 endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  ! noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  !noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
   if (time.gt.R_time) then
     S2N_LIMIT(i)=2*WINDOW_S2N_BASE    ! only pick big signals
     !CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     !TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     !DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   endif
   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   !if (time.lt.(S_pick - 0.2*S_pick)) then
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   ! select only body waves
   !if (time.gt.Q_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   ! if an intermediate depth event
   !!UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS
!   if (evdp.ge.100 .and. evdp.lt.300) then
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
  ! ! if a deep event
  ! elseif (evdp.ge.300) then
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo
!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------



! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_bw_Z
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! This is the basic version of the subroutine - no variation with time
! -----------------------------------------------------------------
  do i = 1, npts
    time=b+(i-1)*dt
    DLNA_LIMIT(i)=DLNA_BASE
    CC_LIMIT(i)=CC_BASE
    TSHIFT_LIMIT(i)=TSHIFT_BASE
    STALTA_W_LEVEL(i)=STALTA_BASE
    S2N_LIMIT(i)=WINDOW_S2N_BASE
  enddo

  ! these values will be used for signal2noise calculations
  ! if DATA_QUALITY=.true.
  if (DATA_QUALITY) then
    noise_end=ph_times(1)-WIN_MAX_PERIOD
    noise_start=b
    signal_end=b+(npts-1)*dt
    signal_start = noise_end
  endif


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.2
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 Q_vel=4.8
 !Q_vel=5.0
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 if (DATA_QUALITY) then
   signal_end=R_time
 endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  !noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  ! noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
   if (time.gt.R_time) then
     S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/3.0    ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   endif
   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   !select only body waves
   if (time.gt.R_time) then
     STALTA_W_LEVEL(i)=1
   endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   !! UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS!!
   ! if an intermediate depth event
!   if (evdp.ge.100 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
   ! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo

!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_sw_RT
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! This is the basic version of the subroutine - no variation with time
! -----------------------------------------------------------------
  do i = 1, npts
    time=b+(i-1)*dt
    DLNA_LIMIT(i)=DLNA_BASE
    CC_LIMIT(i)=CC_BASE
    TSHIFT_LIMIT(i)=TSHIFT_BASE
    STALTA_W_LEVEL(i)=STALTA_BASE
    S2N_LIMIT(i)=WINDOW_S2N_BASE
  enddo

  ! these values will be used for signal2noise calculations
  ! if DATA_QUALITY=.true.
  if (DATA_QUALITY) then
    !noise_end=ph_times(1)-WIN_MAX_PERIOD
    !noise_end=ph_times(1)-WIN_MAX_PERIOD/2
    noise_end=S_pick-(WIN_MAX_PERIOD+50)
    noise_start=b
    signal_end=b+(npts-1)*dt
    signal_start = noise_end
  endif


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.0
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 !Q_vel=4.2
 Q_vel=4.8
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
! if (DATA_QUALITY) then
!   signal_end=R_time
! endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  ! noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  !noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !! if (time.gt.Q_time+WIN_MAX_PERIOD) then
  !! if (time.gt.Q_time) then
    !! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    !! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
    !! TSHIFT_LIMIT(i)=TSHIFT_BASE/3.0    ! only pick small timeshifts
    !! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
    !! STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   !!endif
   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !if (time.gt.(R_time+WIN_MAX_PERIOD)) then
  if (time.gt.(R_time+2*WIN_MAX_PERIOD)) then
   !if (time.gt.R_time) then
     S2N_LIMIT(i)=3*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= CC_LIMIT(i)+0.03     ! only pick very similar signals
     !TSHIFT_LIMIT(i)=TSHIFT_BASE      ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/2.0       ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*2.5 ! pick only distinctive arrivals
   endif
   ! --------------------------------
   ! Before Rayleigh waves, then make the all criteria stronger
   ! ratio criterion stronger
  if (time.lt.(Q_time-2*WIN_MAX_PERIOD)) then
  ! if (time.gt.R_time) then
    ! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    ! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
    ! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
    ! STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
  endif
   ! --------------------------------

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   !if (time.lt.(S_pick - 0.2*S_pick)) then
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

    !select only surface waves
   !if (time.lt.Q_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   ! if an intermediate depth event
   !!UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS
!   if (evdp.ge.70 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
!   ! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo
!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------



! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_sw_Z
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! This is the basic version of the subroutine - no variation with time
! -----------------------------------------------------------------
  do i = 1, npts
    time=b+(i-1)*dt
    DLNA_LIMIT(i)=DLNA_BASE
    CC_LIMIT(i)=CC_BASE
    TSHIFT_LIMIT(i)=TSHIFT_BASE
    STALTA_W_LEVEL(i)=STALTA_BASE
    S2N_LIMIT(i)=WINDOW_S2N_BASE
  enddo

  ! these values will be used for signal2noise calculations
  ! if DATA_QUALITY=.true.
  if (DATA_QUALITY) then
    !noise_end=ph_times(1)-WIN_MAX_PERIOD
    noise_end=ph_times(1)-WIN_MAX_PERIOD/2
    noise_start=b
    signal_end=b+(npts-1)*dt
    signal_start = noise_end
  endif


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 !R_vel=3.7
 R_vel=4.2
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
! Q_vel=4.0
 Q_vel=4.8
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 !if (DATA_QUALITY) then
 !  signal_end=R_time
 !endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  !noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  ! noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !if (time.gt.(R_time+WIN_MAX_PERIOD)) then
  if (time.gt.(dist_km/3.7+2*WIN_MAX_PERIOD)) then
   !if (time.gt.R_time) then
     S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE        ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals

   endif
   ! --------------------------------
   ! Before Rayleigh waves, then make the all criteria stronger
   ! ratio criterion stronger
  if (time.lt.(R_time-2*WIN_MAX_PERIOD)) then
  ! if (time.gt.R_time) then
    ! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    ! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
    ! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
    ! STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   endif
   ! --------------------------------

   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   !select only surface waves
   !if (time.lt.R_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif
  
   ! --------------------------------
   ! modulate criteria according to event depth
   !
   !! UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS!!
   ! if an intermediate depth event
!   if (evdp.ge.70 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
   !! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo

!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------
subroutine set_up_criteria_arrays_default
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time

! -----------------------------------------------------------------
! This is the basic version of the subroutine - no variation with time
! -----------------------------------------------------------------
  do i = 1, npts
    time=b+(i-1)*dt
    DLNA_LIMIT(i)=DLNA_BASE
    CC_LIMIT(i)=CC_BASE
    TSHIFT_LIMIT(i)=TSHIFT_BASE
    STALTA_W_LEVEL(i)=STALTA_BASE
    S2N_LIMIT(i)=WINDOW_S2N_BASE
  enddo

  ! these values will be used for signal2noise calculations
  ! if DATA_QUALITY=.true.
  if (DATA_QUALITY) then
    !noise_end=ph_times(1)-WIN_MAX_PERIOD
    noise_end=ph_times(1)-WIN_MAX_PERIOD/2
    noise_start=b
    signal_end=b+(npts-1)*dt
    signal_start = noise_end
  endif

!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------

