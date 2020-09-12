import json
import datetime

separator = "\n===================================================\n"

### This function is to log individual action
# log_filename - name of the log file with full path.
# message - the message, what is logged into the file.
def logging_fun(log_filename,message):
    log = open(log_filename,"a",encoding = "UTF-8")
    print(message)
    log.write(message+"\n")
    log.close()

### This function is to backup the data in a local file outside of db
# backup_file - The name of the backup file with full path.
# contents - the contents of backup file.
def backup_fun(backup_filename,contents):
    if ".csv" in backup_filename:
        backup = open(backup_filename,"a",encoding = "UTF-8")
        backup.write(contents+'\n')
        backup.close()
    if ".json" in backup_filename:
        # Get (new) type and new subtype
        content_parts = contents.split(",")
        expense_type = content_parts[1]
        expense_subtype = content_parts[2]
        # Read json file contents in and add new expense type/subtype
        init_load_json = open(backup_filename,"r",encoding ="UTF-8")
        init_load_info = json.load(init_load_json)
        init_load_json.close()
        #Try if the type exists. If yes, add to the list, if not then first create the type and then add to the list.
        try:
            init_load_info['Type'][expense_type].append(expense_subtype)
        except:
            init_load_info['Type'][expense_type] = []
            init_load_info['Type'][expense_type].append(expense_subtype)
        # Add new info to json file.
        with open(backup_filename,'w') as init_load_json:
                json.dump(init_load_info, init_load_json)
        init_load_json.close()

### This function is used as a bridge for logging actions and backing up data in a local file.
# table_name - the name of the table where the new insert was made.
# log - name of the log file with full path.
# backup - name of the backupfile with full path.
# cursor - cursor parameter
def logging_bridge_fun(table_name,log,backup,cursor):
    # Write to the log the last line that was added to the database.
    cursor.execute("select * from Expenses."+table_name+" order by ID desc limit 1")
    inserted = ','.join(map(str,cursor.fetchone()))
    logging_fun(log,str(datetime.datetime.now())+" - New "+table_name+" inserted: "+ str(inserted)) # need to import datetime
    # Write a backup.
    backup_fun(backup, inserted)


