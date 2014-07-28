module var_main

  logical :: WRITE_SINGLE_WIN_FILE
  logical :: REMOVE_SURFACE_WAVE

  character(len=150) :: OBSD_FILE, SYNT_FILE
  character(len=150) :: FLEXWIN_OUTDIR

  double precision :: PERIOD_BEGIN, PERIOD_END

end module var_main
