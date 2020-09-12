
============
Project tree
============

Expenses_v2
--CLI
----CLI.py
--Connection
----connection.py
--Create_and_init_load_tbls
----create_tables.py
----expense_types.json
--Documentation
----README.txt
--Logic
----logic.py
--Logs
----create_and_init_load.log
----logging_fun.py
--Statistics
--Expenses_v2.py

==================
How I set up MySQL
==================

* First I installed MySQL with command 
	sudo apt-get install mysql
* I ran MySQL with root user
	sudo mysql
* Created new user in table mysql.user. To see existing users and
  authentication method, it's good to use the following command:
	select User, Host, plugin from mysql.user;
  To create a new user, I used the following commands:
	flush privileges;
	create user '<username>'@'localhost' identified with
mysql_native_password by '<password>';
	grant all privileges on *.* to '<username>'@'localhost';
	flush privileges;

=============================
Initializing database, tables
=============================

* Running the code 'python /Create_and_init_load_tbls/create_and_init_load_tbls.py' from 
  Expenses_v2/master creates new schema named 'Expenses' and tables named 'expenses' and 'expense_types'.
* Results of the program are logged in /Logs/create_and_init_load.log

Logic of create_and_init_load_tbls.py:
* Create schema and tables.
* Read the initial values from init_expense_types.json for table 'expense_types'.
* Insert the read values from previous point to the table mentioned.


