#!/usr/bin/python
# This file automatically transfers files from local directory to helix

import os, shutil, glob, re
from subprocess import call

workingdir = os.path.join('E:\\','Work','Ward','Studies','LupusMeta')
datadir = os.path.join(workingdir,'data','mcmc')
outdir = 'Z:\\lupus'

localdirs = glob.glob(os.path.join(datadir,'*')) # omits dotfiles
localdirs = [d for d in localdirs if os.path.isdir(d) is True]
#localdirs = glob.glob(os.path.join(datadir,'HQ')) 
localdirs = [os.path.basename(d) for d in localdirs]

for d in localdirs:
	if os.path.isdir(os.path.join(outdir,d)) is False:
		os.makedirs(os.path.join(outdir, d))
	os.chdir(os.path.join(datadir, d)) # local directory
	datfiles = glob.glob('*.txt')
	for f in datfiles:
		shutil.copy(f, os.path.join(outdir,d))
	os.chdir(os.path.join(workingdir,'scripts'))
	shutil.copy('template.py',os.path.join(outdir,d))
	os.chdir(os.path.join(outdir,d)) # biowulf directory
	[os.remove(f) for f in glob.glob('*.o')]
	[os.remove(f) for f in glob.glob('*.e')]
	[os.remove(f) for f in glob.glob('*.rda')]
	execfile('template.py')
	os.chdir(workingdir)








