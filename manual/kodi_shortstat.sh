#!/bin/bash
################################################
# Check the status of the playing item on Kodi #
################################################
# Uses `curl' and `jq'

# I've hardcoded player id = 1 here
# and not sure why but "id" is necessary, didn't care enough to check why
FILE=`curl -g -o - 'http://jessandmorgan.com:7654/jsonrpc?request={"jsonrpc":"2.0","method":"Player.GetItem","params":{"properties":["streamdetails"],"playerid":1},"id":"dunno_what_this_is"}' 2> /dev/null | jq '.result.item.label'`
PROPS=`curl -g -o - 'http://jessandmorgan.com:7654/jsonrpc?request={"jsonrpc":"2.0","method":"Player.GetProperties","params":{"properties":["percentage","speed","time","totaltime","position"],"playerid":1},"id":"x"}' 2> /dev/null`
PERCENT=`echo $PROPS | jq '.result.percentage'`

CUR_H=`echo $PROPS | jq '.result.time.hours'`
CUR_M=`echo $PROPS | jq '.result.time.minutes'`
CUR_S=`echo $PROPS | jq '.result.time.seconds'`
TOT_H=`echo $PROPS | jq '.result.totaltime.hours'`
TOT_M=`echo $PROPS | jq '.result.totaltime.minutes'`
TOT_S=`echo $PROPS | jq '.result.totaltime.seconds'`

if [ "$FILE" = "null" ] ; then
	echo "<idle>"
	exit
fi

FILE=${FILE#\"}

# get rid of torrent-style extensions
FILE=${FILE%%[. ]720*}
FILE=${FILE%%[. ]1080*}
FILE=${FILE%%[. ]HDTV*}
FILE=${FILE%%[. ]XviD*}
FILE=${FILE%%[. ][xX]264*}
FILE=${FILE%%[. ][Dd][Vv][Dd][Rr][Ii][Pp]*}
FILE=${FILE%%[. ][Bb][Rr][Rr][Ii][Pp]*}
FILE=${FILE%%[. ]INTERNAL*}

echo -n "[$FILE]"
if [ "$TOT_H" = "0" ] ; then
	# print without hours
	echo $PERCENT $CUR_M $CUR_S $TOT_M $TOT_S | awk '{printf " (%d%%) %02d:%02d/%02d:%02d\n", $1, $2, $3, $4, $5}'
else
	echo $PERCENT $CUR_H $CUR_M $CUR_S $TOT_H $TOT_M $TOT_S | awk '{printf " (%d%%) %02d:%02d:%02d/%02d:%02d:%02d\n", $1, $2, $3, $4, $5, $6, $7}'
fi

