# Compile CSV files of Kaplan Meier data into a single spreadsheet
import glob, csv, xlwt, os
os.chdir('E:/Work/Ward/Studies/LupusMeta/data/fromPapers')
wb = xlwt.Workbook()

for filename in glob.glob('*.csv'):
    (f_path,f_name) = os.path.split(filename)
    (f_short_name, f_extension) = os.path.splitext(f_name)
    ws = wb.add_sheet(f_short_name)
    spamReader = csv.reader(open(filename, 'rb'))
    converters = (float,float)
    for rowx, row in enumerate(spamReader):
        for colx, value in enumerate(row):
            ws.write(rowx, colx, value)
wb.save('E:/Work/Ward/Studies/LupusMeta/data/KMcompiled.xls')

