#!/bin/sh
# Extract comments from youtube 
# url and print them pretty

tmp="tmp"
yt-dlp --write-comments --no-download -o $tmp $1 
jq '.comments.[].text' $tmp.info.json > coms
rm -rf $tmp.info.json
