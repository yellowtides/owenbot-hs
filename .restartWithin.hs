#!/bin/bash
# Script to restart Owen within screen
pkill owenbot-exe;
cd ~/owenbot-hs;
git pull;
stack run;
"
