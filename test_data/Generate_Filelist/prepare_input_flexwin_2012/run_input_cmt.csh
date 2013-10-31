#!\bin\csh
#set kk = $1 
set cmt = $1
set stype = `cat seis_type`
foreach seis ($stype)
	echo $seis
	#echo $stype

	set dir = `pwd`

# NOTE: First make sure that you have extracted OBSERVED and SYNTHETIC seismograms in the defined directories below. 
#       You should modify the directories of seismograms and where you would like to store the input files.

# VERTICAL COMPONENT
	set z = `ls "../9s-seismograms/OBSD_final/"$cmt"_"$seis"/CMT/select"/*LHZ* | wc -l` 
	echo $z
	echo $z > /tigress/lei/SOURCE/input_files/$cmt"/input_cmt_LHZ_"$seis
	foreach i ("/tigress/lei/9s-seismograms/OBSD_final/"$cmt"_"$seis"/CMT/select/"*LHZ*)
	set sta = `echo $i | awk 'BEGIN { FS = "select/" } ; { print $2 }' | awk -F. '{print ($1)}'`
   	set nw = `echo $i | awk -F. '{print ($2)}'` 
   	#set nm = `echo $i | awk -F. '{print ($3)}'` 
   	set cmp = `echo $i | awk -F. '{print ($3)}'` 

   	set a = "/tigress/lei/9s-seismograms/SYNT_final/"$cmt"_"$seis"/CMT/select/"$sta"."$nw"."$cmp".sem"
     # echo $a
   	set b = "/tigress/lei/SOURCE/input_files/"$cmt"/MEASURE_cmt_"$cmp"_"$seis"/"$sta"."$nw"."$cmp
   	echo $i >> /tigress/lei/SOURCE/input_files/$cmt"/input_cmt_"$cmp"_"$seis
   	echo $a >> /tigress/lei/SOURCE/input_files/$cmt"/input_cmt_"$cmp"_"$seis
   	echo $b >> /tigress/lei/SOURCE/input_files/$cmt"/input_cmt_"$cmp"_"$seis
	end

#
