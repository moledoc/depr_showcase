# Synopsis

This is a simple python script, that summaries expenses from a text file. Summary is printed out for the running (or given month, see Options) month and two previous months. Data is read from file named data.csv. The separator in that file is expected to be ';' and the data must be arrange as folllows: Date;Expense;Type;Description. Date column should contain dates in format yyy-mm-dd; decimal separator in Expenses should be '.'. Sample data file is also included in the repository (data is randomly generated).

# Options

	--meta		Print out all types and corresponding descriptions.
	--date=<date>	Print out the summary of expenses using given date as the current date. The date is expected to be given in format yyyy-mm.
	--help 		Shows this help

# Notes

* The path to data.csv is hardcoded. User needs to change the path according to user's own system.
