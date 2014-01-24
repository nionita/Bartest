n=$(date +%Y%m%d)

p=$1

if [ -z "$p" ]
then p=200
fi

exec >tour-$n.log 2>&1

date

nohup ./cutechess-cli.sh -concurrency 7 -draw movenumber=20 movecount=5 score=5 -resign movecount=5 score=800 -tournament round-robin -event TEST-$n -games 2 -rounds $p -pgnout TEST-$n.pgn -recover -each option.Hash=512 tc=60+1 arg=-l arg=5 \
	-engine conf=addw \
	-engine conf=exts \
	-engine conf=clse \
	-engine conf=addr \
	&
