##################
#Imports and paths
##################
from pathlib import Path
import mysql.connector
import json
import sys

#Get the connection functions from connection.py from other directory.
conn_path = str(Path().absolute())+"/Connection"
sys.path.insert(1,conn_path)
from connection import *

# Get the logs directory path and (delete old and) create new log file for create tables and init load.
# Also create new file for action_log.log and backup.csv.
logs_path = str(Path().absolute())+"/Logs"
sys.path.insert(1,logs_path)
log = open(logs_path+"/create_and_init_load.log","w",encoding = "UTF-8")
log.close()
log = open(logs_path+"/actions_log.log","w",encoding = "UTF-8")
log.close()
log = open(logs_path+"/backup.csv","w",encoding = "UTF-8")
log.close()

# Set log as create_and_init_load.log file path
log = logs_path+"/create_and_init_load.log"
# #Get logging function
from logging_fun import * 

################
#Open connection
################
conn_info = open_conn()
conn = conn_info["conn"]
cursor = conn_info["cursor"]
#########################
#Create schema and tables
#########################

#Create database/schema
create_schema = "create schema Expenses;"
cursor.execute(create_schema)
conn.commit()
logging_fun(log,"Schema 'Expenses' created")

#Create expense types table
create_expense_types_tbl = "create table Expenses.expense_types(ID int not null auto_increment, EXPENSE_TYPE  varchar(50),EXPENSE_SUBTYPE  varchar(100), primary key (ID));"
cursor.execute(create_expense_types_tbl)
conn.commit()
logging_fun(log,"Table 'Expenses.expense_types' created")

#Create expenses table
create_expenses_tbl = "create table Expenses.expenses(ID int not null auto_increment, EXPENSE_TYPE int,SUM DECIMAL(7,2),DESCRIPTION varchar(100),TIME_OF_EXPENSE DATE, VALID_FROM timestamp default now(),primary key (ID), foreign key(EXPENSE_TYPE) references expense_types(ID));"
cursor.execute(create_expenses_tbl)
conn.commit()
logging_fun(log,"Table 'Expenses.expenses' created")

#########################
#Make init load to tables
#########################

# Open inital loading json
init_expense_types_json = open(str(Path().absolute())+"/Create_and_init_load_tbls/init_expense_types.json","r",encoding ="UTF-8")
types_info = json.load(init_expense_types_json)

#Initial loading
for type in types_info["Type"].keys():
    for subtype in types_info["Type"][type]:
        cursor.execute("insert into Expenses.expense_types (EXPENSE_TYPE,EXPENSE_SUBTYPE) values ('"+type+"','"+subtype+"');")
        conn.commit()
        logging_fun(log,"Inserted into 'Expenses.expense_type' values: '"+type+"','"+subtype+"'")

#renew expense_types.json
expense_types_json = str(Path().absolute())+"/Create_and_init_load_tbls/expense_types.json"
with open(expense_types_json,'w') as active_json:
    json.dump(types_info,active_json)
active_json.close()

#Close open files
init_expense_types_json.close()
#################
#Close connection
#################
close_conn(conn_info)

