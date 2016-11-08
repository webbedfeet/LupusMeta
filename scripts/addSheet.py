import sys
import csv,xlrd, xlwt,xlutils
from xlutils.copy import copy

csvfile = sys.argv[1]
xlwb = sys.argv[2]
sheetName = sys.argv[3]
outfile = sys.argv[4]

def addSheet(xlwb,sheetName, csvfile,outfile):
    #f = open(xlwb,'rb')
    rb = xlrd.open_workbook(xlwb, formatting_info=True)
    wb = copy(rb)
    ws = wb.add_sheet(sheetName)
    f2 = open(csvfile,'rb')
    sourceCSV = csv.reader(f2, delimiter=',')
    converters = (float, int)
    row_count = 0
    for row in sourceCSV:
        for col in range(len(row)):
            if row_count == 0:
                ws.write(row_count,col,row[col])
            else:
                ws.write(row_count, col, converters[col](row[col]))
        row_count +=1
    wb.save(outfile)

addSheet(xlwb,sheetName,csvfile,outfile)
