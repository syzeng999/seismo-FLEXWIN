#!bin/csh

#set j = `ls /tigress/lei/CMT/201210091232A`
#foreach i ($j)
#  set cmt = `echo $i | awk 'BEGIN { FS = "CMT/" } ; { print $2 }' | awk -F. '{print ($1)}'`
#
  set cmt = 201210091232A
  echo $cmt
  csh run_input_cmt.csh $cmt
#end
#
