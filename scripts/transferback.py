#!/usr/bin/python
# This script transfers necessary .rda files from helix back to the 
# local directory

import os, shutil, glob, re
workingdir = os.path.join('E:\\','Work','Ward','Studies','LupusMeta')
datadir = os.path.join(workingdir,'data','mcmc')
outdir = 'Z:\\'

localdirs = os.listdir(os.path.join(outdir,'lupus')) # omits dotfiles
localdirs = [os.path.basename(d) for d in localdirs if os.path.isdir(d)]

for d in localdirs:
	os.chdir(os.path.join(outdir,'lupus',d))
	rdafiles = glob.glob('*.rda')
	for r in rdafiles:
		shutil.copy(r, os.path.join(datadir,d))

