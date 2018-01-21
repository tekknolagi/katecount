#!/bin/sh

if test "$#" -ne 1; then
	echo "Usage: $0 HOSTNAME"
	exit 1
fi

ssh "$1" 'yes | nc 10.247.140.103 5000 >/dev/null &'
