#!/bin/sh

gmtset PAPER_MEDIA "a4" MEASURE_UNIT inch HEADER_FONT_SIZE 14p LABEL_FONT_SIZE 16p PLOT_DEGREE_FORMAT ddd:mm:ssF


if [ $# != 1 ]; then
	echo ERROR
	echo run ./seis_plot.sh $CMT_file 
	exit
else 
	cmtid=$1
  dir="/tigress/lei/tigress-hsm/tier1/SOURCE/input_files"

	cmtfile="/tigress/lei/tigress-hsm/tier1/CMT/"$cmtid 
	if [ ! -f $cmtfile ] ; then
		echo WRONG! NO $cmtfile 
		exit
	fi 

	stafile="/tigress/lei/tigress-hsm/tier1/STATIONS/STATIONS"
	if [ ! -f $stafile ]; then
		echo WRONG!  NO $stafile
		exit
	fi 
#	exit

	# Parameters can be changed
	nhead=4            # headers for seismograms
	proj=X6.5/1.8        # size of plot
	origin="-Y5"       
	shift="-Y-1.8"

	# CONSTANTS
	orient="-P"
	red=255/0/0
	black=0/0/0
	blue=0/0/255
	green=00/200/00
	paleblue=220/220/255


  z=0
  zz=0
        
	while read line 
	do
		echo $line 
    z=`expr $z + 1` #index to choose each station from  STATIONS file
		sta=`echo $line | awk '{print $1}'| awk -F"." '{print $1}'`
		net=`echo $line | awk '{print $2}'`
		nw=`echo $line | awk '{print $1}'| awk -F"." '{print $3}'`
#		echo $nw
#		exit



		# OUTPUT PS-PDF FILES
    out=$dir'/'$cmtid'/'$sta'.'$net'.ps'
    pdf=$dir'/'$cmtid'/'$sta'.'$net'.pdf'

		label=$sta'.'$net


		for BP in LHZ LHR LHT
#		for BP in LHZ
		do
			obs1=$dir'/'$cmtid'/MEASURE_cmt_'$BP'_bw/'$sta'.'$net'.'
			obs2=$BP'.obs'
                        obs=`ls $obs1*$obs2`
			syn1=$dir'/'$cmtid'/MEASURE_cmt_'$BP'_bw/'$sta'.'$net'.'
			syn2=$BP'.syn'
      syn=`ls $syn1*$syn2`
      echo "obs="$obs
      echo "syn="$syn
			win1=$dir'/'$cmtid'/MEASURE_cmt_'$BP'_bw/'$sta'.'$net'.'
			win2=$BP'.win.qual'
      win=`ls $win1*$win2`
      win_bos=$win1'..'$win2
      echo "win_bos="$win_bos
      syn_bos=$syn1'..'$syn2
      obs_bos=$obs1'..'$obs2
      echo "syn_bos="$syn_bos
      echo "obs_bos="$obs_bos
      a=`ls $obs | wc -l`
      echo "a="$a
      if [ $a != 1 ]; then
        t_start=0
        t_end=1000
        t_step=1
        max=1
        nwin=0
			  region=$t_start/$t_end/-$max/$max
        cp /tigress/lei/tigress-hsm/tier1/SOURCE/input_files/ext_files/empty.win.qual $win_bos
        cp /tigress/lei/tigress-hsm/tier1/SOURCE/input_files/ext_files/empty_syn $syn_bos
        cp /tigress/lei/tigress-hsm/tier1/SOURCE/input_files/ext_files/empty_obs $obs_bos
        win=$win_bos 
        syn=$syn_bos
        obs=$obs_bos
      elif [ $a == 1 ]; then
	   		t_start=`grep T_START ${obs} | awk '{print $NF}'`
		 		t_end=`grep T_END ${obs} | awk '{print $NF}'`
	    	t_step=`echo $t_end $t_start | awk '{print int(($1-$2)/10)}'`
				max=`grep PLOT_MAX ${obs} | awk '{print $NF}'`
				nwin=`grep NUM_WIN ${win} | awk '{print $NF}'`
	 	   	region=$t_start/$t_end/-$max/$max
      fi
      echo "max" $max
      echo "nwin" $nwin
      echo "region" $region

			if [ $BP == 'LHZ' ]; then
				psbasemap -R$region -J$proj $orient $origin -B${t_step}::/:vertical:/Wesn -K  > $out
			elif [ $BP == 'LHR' ]; then
				psbasemap -R$region -J$proj $shift -B${t_step}::/:radial:/Wesn -O -K  >> $out
			else 
				psbasemap -R$region -J$proj $shift -B${t_step}:"time (s)":/:transverse:/WeSn -O -K >> $out
			fi 
			tail -$nwin $win | awk '{printf "%f %e\n%f %e\n%f %e\n%f %e\n>\n", $2,-1*m,$2,m,$3,m,$3,-1*m}' m=$max | psxy -R$region -J$proj -M -W1 -G$paleblue -K -O>> $out
			psxy $obs -R$region -J$proj -H$nhead -W4 -O -K >> $out
			psxy $syn -R$region -J$proj -H$nhead -W4/$red -O -K >> $out
			tail -$nwin $win | awk '{printf "> %f %f 8 90 0 RT 10p 0.5i l\nCC=%.2f\ndT=%.2f\ndA=%.2f\n",$2,m,$5,$4,$6}' m=$max | pstext -R$region -J$proj -M -N -O -K >> $out
		done 


    stla=`grep "$sta"  $stafile | awk '{print $3}'` 
    stlo=`grep "$sta" $stafile | awk '{print $4}'` 
    echo "STATION:" $stla $stlo
	  #exit
		
		ename=`grep "event name" $cmtfile | awk '{printf ("%s",$NF)}'`
		evlo=`grep longitude $cmtfile | awk '{printf ("%.2f",$NF)}'`
		evla=`grep latitude $cmtfile | awk '{printf ("%.2f",$NF)}'`
		evdp=`grep depth $cmtfile | awk '{printf ("%.2f",$NF)}'`
		Mrr=`grep Mrr $cmtfile | awk '{print $NF}'`
		Mtt=`grep Mtt $cmtfile | awk '{print $NF}'`
		Mpp=`grep Mpp $cmtfile | awk '{print $NF}'`
		Mrt=`grep Mrt $cmtfile | awk '{print $NF}'`
		Mrp=`grep Mrp $cmtfile | awk '{print $NF}'`
		Mtp=`grep Mtp $cmtfile | awk '{print $NF}'`
		grdmath -Rg -I1 $evlo $evla SDIST 111.13 MUL = dist.grd

		pscoast -R-180/180/-90/90 -JW150/4.5i -Bg0/g0 -X1.2 -Y6.5 -Ggrey -Swhite -Wthinnest -O -K >> $out
		awk '{print $4,$3}' $stafile | psxy -R -J -St0.1 -Gyellow -Wthinnest -K -O >> $out 
		(echo $evlo $evla; echo $stlo $stla) | psxy -R -J $orient -Wthickest -K -O >> $out
		(echo $stlo $stla) | psxy -R -J $orient -St0.2 -Gblue -Wthinnest -K -O >>$out
		psmeca -R -J -Sm0.2 -Gblue -K -O -P << EOF >> $out
		$evlo $evla $evdp $Mrr $Mtt $Mpp $Mrt $Mrp $Mtp 1 0 0 
EOF
		pstext -R0/10/0/10 -Jx1 -X-0.8 -Y-3.0 -O -V << EOF >> $out
		3 6 15 0 15 CM $ename
		3 5.7 15 0 15 CM $label
EOF

		rm -f dist.grd
                ps2pdf $out $pdf
                rm $out
	done < $stafile
fi 


