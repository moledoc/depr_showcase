import sys 
from pathlib import Path

main_path = Path().absolute()#.parent
logs_path = str(main_path)+"/Logs"
#sys.path.insert(1,logs_path)
from Logs.logging_fun import *

### This function inserts new expenses to table Expenses.expenses. 
### Also, the action is logged and backup insert is done to .csv file.
# conn - connection parameter
# cursor - cursor parameter
# expense_info - a dict, that contains expense_type(int), sum(decimal(7,2)), decription(varchar) and time(timestamp) of the new expense.
def add_expense_fun(conn,cursor,expense_info):
    # Make the insert query
    query = "insert into Expenses.expenses (EXPENSE_TYPE, SUM,DESCRIPTION,TIME_OF_EXPENSE) values ({expense_type},{expense_sum},'{description}','{time}');"
    # Excecute the insert query
    cursor.execute(query.format(expense_type = expense_info['expense_type'],
        expense_sum = expense_info['expense_sum'],
        description = expense_info['description'],
        time = expense_info['time'])
        )
    # Commit to database
    conn.commit()
    # Log and backup the insert
    log = logs_path+"/actions_log.log"
    backup = logs_path+"/backup.csv"
    logging_bridge_fun(table_name = "expenses",log = log,backup = backup,cursor = cursor)


### This function inserts new expense types to table Expenses.expense_types. 
### Also, the action is logged and .json file is updated, that is used when init loading db.
# conn - connection parameter
# cursor - cursor parameter
# expense_info - a dict, that contains expense_type(varchar), expense_subtype(varchar)
def add_expense_type(conn,cursor,expense_type_info):
    # Make the insert query
    query = "insert into Expenses.expense_types (EXPENSE_TYPE,EXPENSE_SUBTYPE) values ('{expense_type}','{expense_subtype}');"
    # Excecute the insert query
    cursor.execute(query.format(expense_type = expense_type_info['expense_type'],
        expense_subtype = expense_type_info['expense_subtype']
        ))
    # Commit to database
    conn.commit()
    # Log and backup the insert
    log = logs_path+"/actions_log.log"
    json = str(main_path)+"/Create_and_init_load_tbls/expense_types.json"
    logging_bridge_fun(table_name = "expense_types",log = log,backup = json,cursor = cursor) 
