from decimal import * 
import datetime
import sys
#from pathlib import Path

#main_path = Path().absolute().parent 
#logic_path = str(main_path)+"/Logic/"
#sys.path.insert(1,logic_path)
from Logic.logic import *

### Function for commands in CLI.
def help_fun():
    print("================================")
    print("Add new expense: a")
    print("Add new expense type: a_t")
    print("Help: h")
    print("Quit: q")
    print("================================")

### Function to print out the whole table from a query.
# cursor - cursor parameter
def print_fetchall(cursor):
    for row in cursor.fetchall():
        print(row)

### CLI implementation
# conn - connection parameter
# cursor - cursor parameter
# help_dict - dict, that contains necessary imported files or opened files.
def cli(conn,cursor):
    #Show help
    help_fun()
    # Create dict for expense info
    expense_info = {}
    # Find the valid types, so we don't need to find them every time
    cursor.execute("select ID from Expenses.expense_types;")
    valid_types_id = [item[0] for item in cursor.fetchall()]
    # Motor
    while True:
        usr_input = input("Select action: ")
        if usr_input == 'q':
            print("You have closed the program")
            break
        if usr_input == 'a':
            # Ensure that user selects valid type.
            while True:
                # Instruct what to choose and show the options.
                print("Select expense type:")
                cursor.execute("select * from Expenses.expense_types;")
                print_fetchall(cursor)
                expense_type = input(": ")
                #If input equals 'q' then end
                if expense_type == 'q':
                    print("You have closed the program")
                    return
                try:
                    # Check, that user selected valid ID.
                    if int(expense_type) in valid_types_id:
                        expense_info['expense_type'] = int(expense_type)
                        break                    
                except Exception as e:
                    print(e)
                    print("Chosen type is not suitable, try again.")
            # Ensure, that user inserts positive number, as sum
            while True:
                inserted_sum = input("Insert expense sum: ")
                # if input equals 'q' then end the cycle.
                if inserted_sum == 'q':
                    print("You have closed the program")
                    return
                try:
                    inserted_sum = Decimal(inserted_sum)
                    if inserted_sum > 0:
                        expense_info['expense_sum'] = inserted_sum
                        break
                    else:
                        print("Either sum is 0, which is pointless or it's negative, which is not accepted, try again.")
                except Exception as e:
                    print(e)
                    print("Chosen sum is not suitable, try again.")

            # Get the description of expense
            description = input("Expense description: ")
            #If input equals 'q' then end
            if description == 'q':
                print("You have closed the program")
                return
            expense_info['description'] = description 
            # Get expense time
            # Ensure, that date exist
            while True:
                inserted_date = input("Insert expense date in a form yyyy.mm.dd: ")
                #If input equals 'q' then end
                if inserted_date == 'q':
                    print("You have closed the program")
                    return
                try:
                    date_parts = inserted_date.split(".")
                    year,month,day = date_parts[0],date_parts[1], date_parts[2]
                    if(int(year) >= 2000):
                        expense_info['time'] = datetime.datetime(int(year),int(month),int(day))
                        break
                    print("Inserted date did not match the criterion, try again.")
                except Exception as e:
                    print(e)
                    print("Inserted date did not match the criterion, try again.")

            #print(expense_info)
            add_expense_fun(conn,cursor,expense_info)
        if usr_input == 'a_t':
            # Since the selection of type and subtype has same process, then we use index to guide te loop to correct way
            pass_index = 1
            type_str = "type"
            for pass_index in range(1,3):
                # Show current expense types
                cursor.execute("select * from Expenses.expense_types")
                print_fetchall(cursor)
                # Let user input type
                new_insert = input("Insert a (new) "+ type_str +": ")
                if new_insert == 'q':
                    print("You have closed the program")
                    return
                if pass_index == 1:
                    expense_info['expense_type'] = new_insert.title()
                    type_str = "subtype"
                if pass_index == 2:
                    expense_info['expense_subtype'] = new_insert.title()
            # Make a query, to see, if inserted type and subtype exist. In this case capitalization do not matter.
            query = "select count(*) from Expenses.expense_types where (lower(EXPENSE_TYPE),lower(EXPENSE_SUBTYPE)) in (('{expense_type}','{expense_subtype}'));"
            cursor.execute(query.format(expense_type = expense_info['expense_type'],
                expense_subtype = expense_info['expense_subtype']
                ))
            if cursor.fetchone()[0] == 0:
                # add the new type/subtype to table 'expense_types' and back it up in expense_types.json file.
                add_expense_type(conn,cursor,expense_info)
            else:
                print("Inserted type and subtype exists. Type/subtype not added.")
        if usr_input == 'h':
            help_fun()
        if usr_input not in ['a','a_t','h','q']:
            print("Sorry, I do not know this action.\n")
        


