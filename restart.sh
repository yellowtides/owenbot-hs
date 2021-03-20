#!/bin/bash
# Script to restart Owen
screen -S Owen -X quit
#screen -S Owen -dm "./.restartWithin.sh"
screen -S Owen -dm "owenbot-exe"
echo "Restart complete."
