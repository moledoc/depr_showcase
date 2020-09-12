##################
#Imports and paths
##################
import os
import sys
from pathlib import Path
import mysql.connector
import json

#Get the main directory path.
main_path = Path().absolute().parent 

#Get the connection functions from connection.py from other directory.
conn_path = str(main_path)+"/Connection"
sys.path.insert(1,conn_path)
import connection as connection

#Get the logs directory path and (delete old and) create new log file for create tables and init load.
logs_path = str(main_path)+"/Logs"
log = open(logs_path+"/create_and_init_load.log","w",encoding = "UTF-8")
log.close()
log = open(logs_path+"/create_and_init_load.log","a",encoding = "UTF-8")
#Get logging function
sys.path.insert(1,logs_path)
import logging_fun as logging

################
#Open connection
################
conn_info = connection.open_conn()
conn = conn_info["conn"]
cursor = conn_info["cursor"]
#########################
#Create schema and tables
#########################
#Start create schema and tables
logging.logging_fun(log,"Create schema and tables")

#Create database/schema
create_schema = "create schema Expenses;"
cursor.execute(create_schema)
conn.commit()
logging.logging_actions_fun(log,"Schema 'Expenses' created")

#Create expenses table
create_expenses_tbl = "create table Expenses.expenses(ID int not null auto_increment, EXPENSE_TYPE  varchar(50),SUM DECIMAL(7,2),DESCRIPTION  varchar(100),TIME_OF_EXPENSE timestamp, primary key (ID));"
cursor.execute(create_expenses_tbl)
conn.commit()
logging.logging_actions_fun(log,"Table 'Expenses.expenses' created")

#Create expense types table
create_expense_types_tbl = "create table Expenses.expense_types(ID int not null auto_increment, EXPENSE_TYPE  varchar(50),EXPENSE_SUBTYPE  varchar(100), primary key (ID));"
cursor.execute(create_expense_types_tbl)
conn.commit()
logging.logging_actions_fun(log,"Table 'Expenses.expense_types' created")

#End create schema and tables
logging.logging_fun(log,"Schema and tables created")

#########################
#Make init load to tables
#########################
#Start init load
logging.logging_fun(log,"Start initial data loading")

#Open inital loading json
expense_types_json = open("expense_types.json","r",encoding ="UTF-8")
types_info = json.load(expense_types_json)

#Initial loading
for type in types_info["Type"].keys():
    for subtype in types_info["Type"][type]:
        cursor.execute("insert into Expenses.expense_types (EXPENSE_TYPE,EXPENSE_SUBTYPE) values ('"+type+"','"+subtype+"');")
        conn.commit()
        logging.logging_actions_fun(log,"Inserted into 'Expenses.expense_type' values: '"+type+"','"+subtype+"'")

#End init load
logging.logging_fun(log,"Initial data loading complete")

#Close open files
expense_types_json.close()
log.close()
#################
#Close connection
#################
connection.close_conn(conn_info)

