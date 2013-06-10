#! /bin/bash
#
# Writes the title and artist of the song currently being played by MPD or MOC to STDOUT, formatted for xmobar
# If MPD is playing a song or is paused, its information will be written. If not, MOC will be checked similarly.
# If neither are playing a song or are paused, nothing will be written.

TCOL="cyan"         # The colour to be used to draw the song title when playing
ACOL="lightblue"    # The colour to be used to draw the song artist when playing
PCOL="darkred"      # The colour to be used to draw both the song title and artist when paused

MOCSTATE=$(mocp --info 2> /dev/null | head -n 1 | tail -c +8)

if [ $MOCSTATE == "PLAY" ]; then
  # MOC is playing
  ARTIST=$(mocp --info | grep "Artist" | sed 's/^[^ ]* //' | head -c 20)
  SONG=$(mocp --info | grep "SongTitle" | sed 's/^[^ ]* //' | head -c 20)
  echo "<fc=$ACOL>$ARTIST</fc> - <fc=$TCOL>$SONG</fc>"
elif [ $MOCSTATE == "PAUSE" ]; then
  # MOC is paused
  ARTIST=$(mocp --info | grep "Artist" | sed 's/^[^ ]* //' | head -c 20)
  SONG=$(mocp --info | grep "SongTitle" | sed 's/^[^ ]* //' | head -c 20)
  echo "<fc=$PCOL>$ARTIST - $SONG</fc>"
fi


#MPDSTATE=$(mpc 2> /dev/null | sed -e '2 !d' -e 's/^.*\[//' -e 's/\].*$//')
#if [ $MPDSTATE == "playing" ]; then
#  # MPD is playing
#  echo "<fc=$ACOL>$(mpc current | head -c 50 | sed "s/ - /\<\/fc\> - \<fc=$TCOL>/")</fc>"
#elif [ $MPDSTATE == "paused" ]; then
#  # MPD is paused
#  echo "<fc=$PCOL>$(mpc current | head -c 50)</fc>"
#fi
