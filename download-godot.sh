#!/bin/bash
set -e

link="https://downloads.tuxfamily.org/godotengine/3.2.1/Godot_v3.2.1-stable_x11.64.zip"
outputfile="Godot_v3.2.1-stable_x11.64.zip"
echo "Downloading godot from: $link to: $outputfile"
curl $link --output $outputfile
echo "Unzipping file $outputfile"
unzip $outputfile
echo "Deleting file $outputfile"
rm $outputfile
echo "Finished"
