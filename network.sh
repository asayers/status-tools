#! /bin/bash

WIFI=$(nmcli dev | grep wlan0 | sed 's/^[a-z,0-9]* *[a-z,0-9,-]* *//' | sed 's/ *$//')
ETH=$(nmcli dev | grep eth0 | sed 's/^[a-z,0-9]* *[a-z,0-9,-]* *//' | sed 's/ *$//')

if [ "$ETH" == "connected" ]; then
     echo "<fc=yellow>eth</fc>"
     exit 0
fi

if [ "$WIFI" == "connected" ]; then
#     CUR=$(iwconfig wlan0 | grep Quality | cut -d'=' -f2 | cut -d'/' -f1)
#     TOT=$(iwconfig wlan0 | grep Quality | cut -d'/' -f2 | cut -d' ' -f1) 
#     PERCENT=$((100*$CUR/$TOT))
#     if [ $PERCENT -gt 20 ]; then
#          COL="green"
#     else
#          COL="red"
#     fi
#     echo "<fc=$COL>$PERCENT</fc>%"
     echo "<fc=green>wlan</fc>"
elif [ "$WIFI" == "unavailable" ]; then
     echo "<fc=darkred>$WIFI</fc>"
elif [ "$WIFI" == "disconnected" ]; then
     echo "<fc=darkred>$WIFI</fc>"
elif [[ "$WIFI" == connecting* ]]; then
     echo "<fc=orange>$WIFI</fc>"
else
     echo "<fc=blue>$STATE</fc>"
fi
