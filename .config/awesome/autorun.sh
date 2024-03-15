#!/bin/sh

run() {
  if ! pgrep -f "$1";
  then
    "$@" &
  fi
}

run "fcitx5"

nitrogen --restore &
