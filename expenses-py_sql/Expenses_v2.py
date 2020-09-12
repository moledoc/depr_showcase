##################
#Imports and paths
##################
from pathlib import Path
import mysql.connector

# Load connection.py and CLI.py.
from Connection.connection import * 
from CLI.CLI import *

################
#Open connection
################

conn_info = open_conn()
conn = conn_info["conn"]
cursor = conn_info["cursor"]

###################
#Initialize program
###################

cli(conn,cursor)

#################
#Close connection
# and files
#################
close_conn(conn_info)
