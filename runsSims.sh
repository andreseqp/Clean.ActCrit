#!/bin/bash

cd Simulations
cd $1


echo find . -name "*.json"
find . -name "*.json" | xargs -n 1 -P $2 nohup ../.././CleanAC.exe &
