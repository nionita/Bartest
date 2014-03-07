timestamp=$(date +%Y%m%d%H%M%S)
experiment=TEST
location=$HOSTNAME
name=$experiment-$location-$timestamp

# These will have to be command line parameters with defaults in $HOME/.tour
threads=30
rounds=$1
if [ -z "$rounds" ]
then rounds=200
fi
engines="addw clts clts1 cltv"

econf=""
for e in $engines
do
	econf="$econf -engine conf=$e"
done

exec >$name.log 2>&1

echo "Experiment	$experiment"
echo "Location	$location"
echo "Timestamp	$timestamp"
echo "Engines	$engines"

nohup ./cutechess-cli.sh -concurrency $threads -draw movenumber=20 movecount=5 score=5 -resign movecount=5 score=800 -tournament round-robin -event $name -games 2 -rounds $rounds -pgnout $name.pgn -recover -each option.Hash=512 tc=60+1 arg=-l arg=5 $econf &
