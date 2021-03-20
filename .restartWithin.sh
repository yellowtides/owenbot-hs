#!/bin/bash
# Script to restart Owen within screen
pkill owenbot-exe;
owenbot-exe & disown;
