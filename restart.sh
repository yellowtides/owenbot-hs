#!/bin/bash
# Script to restart Owen
screen -S Owen -X quit
screen -S Owen -dm "./.restartWithin.hs"
echo "done"