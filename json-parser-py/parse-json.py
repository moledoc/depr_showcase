import json
import pandas as pd
# import csv
import sys
import os


filename=''
outfile='parsed_json.csv'

for argument in sys.argv:
    if '--help' in argument:
        print(
        '--help                 prints this help\n--file=<filename>      json file to parse into table; either one json or multiple json\'s on one line\n--outfile=<filename>   parsed json is written to this file; default is parsed_json.csv'
        )
        exit()

    if '--file=' in argument:
        filename=argument.split('=')[1]
    if '--outfile=' in argument:
        outfile=argument.split('=')[1]

if not (os.path.isfile(filename)):
    raise FileNotFoundError(filename + ' does not exits')

try:
    with open(filename,'r',encoding='utf-8') as f:
        input_jsons = [json.load(f)]
except:
    line_json =  [line.strip().replace('\n','') for line in open(filename, 'r',encoding='utf-8')]
    if line_json == []:
        raise ValueError('Empty file, nothing to parse')

    input_jsons = [json.loads(line) if line.strip() != '' else json.loads('{}') for line in line_json]

print('input file read')

def traverse(json,parent_path,level=1):
    # get dict keys
    keys=list(json)
    # for each key, traverse tree.
    for key in keys:
        # add level to the path dict.
        parent_path['level'+str(level)] = key
        subtree = json[key]
        # If branch is a dict, traverse that branch.
        if type(subtree) is dict:
            traverse(subtree,parent_path,level+1)
        # branch is a list, two options:
        # 1) list contains dict(s)
        # 2) list contains value
        elif type(subtree) is list: 
            # print all list elements
            for i in range(len(subtree)):
                # if list element was a dict, traverse that subree.
                if type(subtree[i]) is dict:
                    # add block to the path dict.
                    parent_path['level'+str(level)+'_block'] = 'block'+str(i+1)
                    traverse(subtree[i],parent_path,level+1)
                # if it was value, then we have reached a leaf; print leaf value.
                else:
                    # Add value to path dict and save the path to a list.
                    parent_path['value'] = subtree[i]
                    leaf_paths.append(parent_path.copy())
        # if branch is value, then it means we have reached leaf; print leaf value.
        else:
            # Add value to path dict and save the path to a list.
            parent_path['value'] = subtree
            leaf_paths.append(parent_path.copy())

# Collect paths to leaf nodes
leaf_paths = []
for i in range(len(input_jsons)):
    traverse(input_jsons[i],{})

print('json traversed')
# Sort paths descending
leaf_paths = sorted(leaf_paths, key=lambda k: len(k),reverse=True)

# add missing keys with empty values to the paths, that miss a key.
# All keys in the longest path are added to other paths.
for key in list(leaf_paths[0]):
    for path in leaf_paths[1::]:
        if key not in path:
            path[key] = ''

## # Write out json in a table format using pandas.
# Make dataframe, that reflects the datastructure.
df = pd.DataFrame(leaf_paths).sort_index(axis=1)
# write out the dataframe as csv.
df.to_csv(outfile,header=True,mode='w',index=False,sep='|')

## # Write out json in a table format using csv.
## sort dicts by key value
#for i in range(len(leaf_paths)):
#    leaf_paths[i] = dict(sorted(leaf_paths[i].items()))

## write data out as csv.
#columns = list(leaf_paths[0].keys())
    
## writing to csv file  
#with open(outfile, 'w',encoding='utf-8') as csvfile:  
#    # creating a csv dict writer object  
#    writer = csv.DictWriter(csvfile, fieldnames = columns,delimiter='|')
#    # writing headers (field names)  
#    writer.writeheader()
#    # writing data rows  
#    writer.writerows(leaf_paths)

print('parsed json written out to '+outfile)
