#!/usr/bin/python
import re, glob, os, shutil, posixpath
d = os.getcwd()
bname = os.path.basename(d)
#localdir = os.path.join('E:\\','Work','Ward','Studies','LupusMeta')
#shutil.copy(os.path.join(localdir,'Rfile.R'),'.')

f = open('Rfile.R','r')
x = f.readlines()
f.close()

datfiles = glob.glob('fullmodel.mixed*.txt')
ind = [int(re.search('([0-9]+)', f).group(1)) for f in datfiles]
for i in ind:
	x1 = [u.replace('mixed','mixed'+str(i)) for u in x]
	x1 = [u.replace('Rfile','Rfile'+str(i)) for u in x1]
	x1 = [u.replace('Rfile',posixpath.join('//data','dasgupab','lupus',bname,'Rfile')) for u in x1]
	f = open('Rfile'+str(i)+'.R','w')
	f.writelines(x1)
	f.close()

f = open('Rfile.sw','w')
for i in ind:
	f.write('R --vanilla < '+posixpath.join('//data','dasgupab','lupus',bname,'Rfile')+str(i)+'.R\n')
f.close()

#shutil.copy(os.path.join(localdir,'fullmodelcts.bug'),'.')
#shutil.copy(os.path.join(localdir,'fullmodelcts2.bug'),'.')
