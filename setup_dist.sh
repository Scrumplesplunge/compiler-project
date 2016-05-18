#!/usr/bin/zsh

BASE=$PWD/`dirname $0`

if [ -z "$1" ]; then
  echo "Expected program name as first argument."
  exit 1
fi

PROGRAM=$1

# Control sequence for tmux.
MOD='CTRL+A'

# Create a new window.
xdotool key $MOD
xdotool key 'c'

# Create the panes.
xdotool type "cd $BASE/compiler"
xdotool key Return
xdotool type "./dist.sh $PWD/$PROGRAM --verbose"
xdotool key 'CTRL+l'

# Split horizontally.
xdotool key $MOD
xdotool key 'Shift+5'

xdotool type "cd $BASE/vm"
xdotool key Return
xdotool type 'bin/worker --verbose'
xdotool key 'CTRL+l'

# Split vertically.
xdotool key $MOD
xdotool key 'Shift+2'

xdotool type "cd $BASE/vm"
xdotool key Return
xdotool type 'bin/worker --verbose --port 17995'
xdotool key 'CTRL+l'
