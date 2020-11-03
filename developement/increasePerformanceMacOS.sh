#!/usr/bin/env bash

# maybe you will need to tweak the numbers to your own context
# observe the current lmits on your Mac OS : > sysctl -a
# ulimit -S -n 12288 > set The maximum number of open file descriptors to max (12288)
sudo sysctl -w kern.ipc.somaxconn=20000
sudo sysctl -w kern.maxfiles=20480
sudo sysctl -w kern.maxfilesperproc=65536
sudo launchctl limit maxfiles 65536 200000
ulimit -S -n 12288
