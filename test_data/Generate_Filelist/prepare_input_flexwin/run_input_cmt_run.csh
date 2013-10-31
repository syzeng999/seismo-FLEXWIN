#!bin/csh

set j = `ls /tigress-hsm/lei/CMT/2010*`
foreach i ($j)
  set cmt = `echo $i | awk 'BEGIN { FS = "CMT/" } ; { print $2 }' | awk -F. '{print ($1)}'`
  echo $cmt
  csh run_input_cmt.csh $cmt
end
#
