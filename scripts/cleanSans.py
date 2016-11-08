#!/usr/bin/env python

import os, glob, shutil, zipfile
workingdir=os.path.join('E:\\','Work','Ward','Studies','LupusMeta')
datadir=os.path.join(workingdir,'data','mcmc')

z = zipfile.ZipFile('sansData.zip','w')
sans = glob.glob('sans*')
sans = [s for s in sans if os.path.isdir(s) is True]
for s in sans:
    for root, dirs, files in os.walk(s):
        for f in files:
            z.write(os.path.join(root, f))
z.close()
for s in sans:
    shutil.rmtree(s)

