#!/bin/csh
 set k="/tigress/lei/tigress-hsm/tier1/CMT/2010*"
 set outdir="/tigress/lei/tigress-hsm/tier1/SOURCE/input_files/"
 foreach i ($k)
   echo $i
   set cmt=`echo $i | awk 'BEGIN { FS = "CMT/" } ; { print $2 }'`
   echo $cmt
   ./seis_plot_sw.sh $cmt
	set outdir = "$outdir""$cmt"/plot
	echo $outdir 
	mkdir -p $outdir
  pdcat $outdir/*pdf $outdir/all
 end  
#
