#!/bin/bash

echo Please enter URL:
read url
echo Please enter time start[HH:MM:SS.mm]
read start 
echo Please enter time stop[HH:MM:SS.mm]
read stop 
yt-dlp -f mp4 $url --downloader ffmpeg --downloader-args "ffmpeg_i:-ss $start -to $stop" 
echo "Thanks :)"
