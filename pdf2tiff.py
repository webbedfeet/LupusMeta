#!/usr/bin/python
import os, glob
import sys, getopt

options, remainder = getopt.getopt(sys.argv[1:], 'c',['compress','dpi='])
cmd =['gs','-q','-dNOPAUSE','-dBATCH','-sDEVICE=tiff24nc']
for opt,arg in options:
    if opt in ('-c','--compress'):
        cmd.append('-sCompression=lzw')
    if opt=='--dpi':
        cmd.append('-r'+arg+'x'+arg)
if any([f.find('-r')==0 for f in cmd]) is False:
        cmd.append('-r300x300')

print cmd
pdffiles = glob.glob('*.pdf')
tiffiles = [f.replace('pdf','tif') for f in pdffiles]

for i in range(len(pdffiles)):
    cmd2 = ' '.join(cmd) + ' -sOutputFile=%s %s' % (tiffiles[i], pdffiles[i])
    print(cmd2)
    os.popen(cmd2)


