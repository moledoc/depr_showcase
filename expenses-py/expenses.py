
from datetime import date,datetime
import sys

# Change path according to the directory,
# where this program and 'data.csv' is.
path = 'path/to/repository'

cur = date.today()
option=""
if(len(sys.argv) > 1):
    
    for i in range(1,len(sys.argv)):
        if "--meta" == sys.argv[i]:
            option="meta"
        elif "--date=" in sys.argv[i]:
            cur = datetime.strptime(sys.argv[i].strip("--date="),'%Y-%m').date()
        elif "--help" == sys.argv[i]:
            readme=open(path+'README.md',mode='r')
            print(readme.read())
            readme.close()
            sys.exit()
            #raise SystemExit()
        else:
            #raise SystemExit("ERROR: Unknown option.")
            sys.exit("ERROR: Unknown option.")


dict = {}
dict_types = {}

total_sum = 0

sup_date = date(cur.year + max(cur.month-11,0),12 if (cur.month+1)%12==0 else (cur.month+1)%12,1)
inf_date = date(cur.year + max(min(cur.month-3,0),-1),12 if (cur.month-2)%12==0 else (cur.month-2)%12,1) 

# Read in data
#f=open("data.csv")
f = open(path+"data.csv",encoding = "UTF-8")
while True:
    line = f.readline().strip()
    if line == "":
        break

    #print(line) #for debugging data
    line_elem = line.split(";")

    if "Date" == line_elem[0]:
        continue
    
    type = line_elem[2]
    desc = line_elem[3]

    # If option is meta, read only types and descs
    if option == 'meta':
        if type not in dict_types:
            dict_types[type] = []
        if desc not in dict_types[type]:
            dict_types[type].append(desc)
        continue

    # Pseudo code
    # if the expense is older than 2 months, skip expense.
    date_pieces = line_elem[0].split("-")
    date_exp = datetime(int(date_pieces[0]),int(date_pieces[1]),int(date_pieces[2])).date()
    if date_exp >= sup_date or date_exp <= inf_date:
        continue

    ym = date_exp.strftime("%Y-%m")
    expense = float(line_elem[1])

    if ym not in dict:
        dict[ym] = {'Total': 0}

    if type not in dict[ym]:
        dict[ym][type] = {'Total': 0}
        dict_types[type] = []

    if desc not in dict[ym][type]:
        dict[ym][type][desc] = 0
        dict_types[type].append(desc)

    dict[ym][type][desc]=dict[ym][type][desc]+expense
    dict[ym][type]['Total'] = dict[ym][type]['Total'] + expense
    dict[ym]['Total'] = dict[ym]['Total'] + expense
    total_sum=total_sum+expense

f.close()

if option == "meta":
    for type in sorted(dict_types.keys()):
        print("\n" + type)
        for desc in dict_types[type]:
            print("--"+desc)
    sys.exit()

print("Total expenses of the quarter: " + str(total_sum))
for period in sorted(dict.keys()):
    print("\n\nPeriod: " + period)
    print("Total expenses in " + period + ": " + str(dict[period]['Total']))
    for type in dict[period]:
        if type == 'Total':
            continue
                
        print("\n"+type+": "+str(dict[period][type]['Total']))
        for desc in dict[period][type]:
            if desc == 'Total':
                continue
            print("--" + desc + ": " + str(dict[period][type][desc]))




