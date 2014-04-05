#!/usr/bin/env bash

CUTE=/home/nicu/cutechess-cli
TOUR=/home/nicu/Tour
ENGD=/home/nicu/Engines

cd $TOUR
mkdir running aborted take archive 2>/dev/null

# Some functions we need:
error() {
	name=$1
	what=$2
	echo "Error:" $what >&2
	cd $TOUR
	mv running/$name aborted
	exit 1
}

# This should check if the engine is there and, if not, build it
# Now is just place holder
check_engine() {
	eng=$1
	cd $ENGD
	ename=Barbarossa-0.2.0-$e
	return 0
	if [ -f $eng.making ]
	then
		while [ -f $eng.making ]
		do sleep 5
		done
	else
		touch $eng.making
		builden.sh $eng
		cd $ENGD
		rm $eng.making
	fi
}

move_result() {
	name=$1
	cd $TOUR
	mv running/$name take
	# cutechess-cli writes sometimes another pgn where the
	# engines reside move it too
	cd $ENGD
	if [ -f ${name}.pgn ]
	then
		adir=$TOUR/take/${name}a
		mkdir $adir
		mv ${name}.pgn $adir/${name}a.pgn
	fi
}

while getopts ":r:t:T:g" o
do
	#echo DEBUG $o $OPTIND $OPTARG
	case "$o" in
	'?')
		echo "Wrong option: $OPTARG"
		exit 1
		;;
	g)	a_ttype=gauntlet
		;;
	r)	a_rounds=$OPTARG
		;;
	t)	a_threads=$OPTARG
		#echo DEBUG set a_threads $a_threads
		;;
	T)	a_time=$OPTARG	# Time in hours
		;;
	*)	echo What?
		echo $o $OPTARG $OPTIND
		exit 1
		;;
	esac
done

# end of options, what follows is the list of engines
shift $(( $OPTIND - 1 ))
a_engines=$*

# Read the config file and find defaults
if [ -r ~/tour.rc ]
then
	sed '/^#/d;s/#.*//;/[ \t]+$/d;s/=/ /' ~/tour.rc | while read lhs rhs
	do
		#echo DEBUG $lhs=$rhs
		echo DEBUG declare cf_$lhs=$rhs
		v=cf_$lhs
		declare $v=$rhs
		#echo ${!v}
	done
fi

threads=${a_threads:-${cf_threads:-1}}

if [[ -z $a_engines ]]
then engines=$cf_engines
else engines=$*
fi

if [[ -z $engines ]]
then
	echo No engines specified in command line or config file
	exit 1
fi

if [[ $a_ttype ]]
then
	ttype=$a_ttype
else
	ttype=round-robin
fi

if [[ $a_rounds ]]
then rounds=$a_rounds
elif [[ $a_time ]]
then
	en=$(echo $engines | wc -w)
	minutes=4	# per game, should be computed from time parameters
	# Games per round:
	if [[ $ttype == gauntlet ]]
	then gpr=$(( 2 * ($en - 1) ))
	else gpr=$(( $en * ($en - 1) ))
	fi
	# time=$(( $gpr * rounds * $minutes / $threads / 60 ))
	rounds=$(( $a_time * $threads * 60 / ($gpr * $minutes) ))
else
	rounds=${cf_rounds:-200}
fi

echo $ttype, $rounds rounds, $threads threads

timestamp=$(date +%Y%m%d%H%M%S)
experiment=TEST
location=$HOSTNAME
name=$experiment-$location-$timestamp

cd $TOUR
mkdir running/$name || error $name "Cannot create running directory"
cd running/$name

echo "[" > engines.json

econf=""
for e in $engines
do
	check_engine $e || error $name "Cannot find or build engine $e"

	# Now create the entry in engines.json:
	cd $TOUR/running/$name
	if [ ! -z "$econf" ]
	then
		echo -e "\t," >> engines.json
	fi
	echo -e "\t{\n\t\t\"name\" : \"$e\"," >> engines.json
	echo -e "\t\t\"command\" : \"./$ename\"," >> engines.json
	echo -e "\t\t\"workingDirectory\" : \"$ENGD\"," >> engines.json
	echo -e "\t\t\"protocol\" : \"uci\"" >> engines.json
	echo -e "\t}" >> engines.json

	econf="$econf -engine conf=$e"
done

echo "]" >> engines.json

exec >$name.log 2>&1

echo "Experiment $experiment"
echo "Location   $location"
echo "Timestamp  $timestamp"
echo "Engines    $engines"
echo "Rounds: $rounds, threads: $threads"

nohup $CUTE/cutechess-cli.sh -concurrency $threads -draw movenumber=20 movecount=5 score=5 -resign movecount=5 score=800 -tournament $ttype -event $name -games 2 -rounds $rounds -pgnout $name.pgn -recover -each option.Hash=512 tc=60+1 arg=-l arg=5 $econf &&
	move_result $name &
