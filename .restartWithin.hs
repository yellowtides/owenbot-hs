#!/bin/bash
# Script to restart Owen within screen
pkill Owen-exe;
cd ~/owenbot-hs;
git pull;
stack run;
"
