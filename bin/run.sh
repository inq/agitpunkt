#!/bin/sh

killall manicure
root="$( cd "$( dirname "$0" )" && pwd )/.."
command="$root/dist/build/manicure/manicure"

cd $root
mkdir -p pids
mkdir -p logs
rm manicure.sock
nohup $command >logs/stdout 2>logs/stderr &
echo $! >pids/manicure.pid
sleep 1
chmod 666 manicure.sock
