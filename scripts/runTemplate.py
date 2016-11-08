#!/usr/bin/python

import os, glob, sys
dirs = [u for u in os.listdir('.') if os.path.isdir(u) is True]
for d in dirs:
  os.chdir(d)
  execfile('template.py')
  os.chdir('..')

