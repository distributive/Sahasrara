#!/bin/bash
if [ ! -d "./backup" ]; then
  mkdir "./backup"
fi
if test -f "./backup/$1.3.bak"; then
  rm "./backup/$1.3.bak"
fi
if test -f "./backup/$1.2.bak"; then
  mv "./backup/$1.2.bak" "./backup/$1.3.bak"
fi
if test -f "./backup/$1.1.bak"; then
  mv "./backup/$1.1.bak" "./backup/$1.2.bak"
fi
cp "./$1" "./backup/$1.1.bak"
