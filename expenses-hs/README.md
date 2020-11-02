## Name 

expenses-hs

## Synopsis 

Interactive CLI tool to add and report expenses. Written in Haskell.

## Download

Download the whole repository and navigate to expenses-hs:

```sh
git clone https://github.com/moledoc/showcase.git
```

or make new directory and checkout only expenses-hs:

```sh
mkdir showcase
cd showcase
git init
git remote add origin https://github.com/moledoc/showcase.git
git fetch --all
git checkout origin/master -- expenses-hs
```

## Dependecies

Used Haskell packages:

* Control.Exception
* Data.Text
* Data.Text.IO
* Text.Read
* Control.Monad
* Data.List
* System.IO

This repository includes the binary of the program (compiled on GNU/Linux). However, GHCI is recommended, if one is interested in playing around with the source code.
To compile the source code

```sh
ghc --make-dynamic Expenses_v4
```

## Overview

Interactive commands in the program:

q  - exits the program

h  - shows help

a  - adds new expense. 
  
  The program asks the date, expense, category and description. 
  If invalid date/expense is given, program asks them until valid date/expense is given.
  
d  - deletes expense.

  Program asks how many latest expenses user wants to see. User can choose among those expenses, which one he/she wants to delete.
  
rl - show last <nr> inserted expenses

  Program asks how many latest expenses user wants to see and shows them.

rr - show expenses between range

  Program asks start and end date in format yyyy-mm-dd. If invalid date values are given, program asks until valid values are given or program is exited.
  
rn - report <nr> month expenses

  Program asks format type (yyyy-mm or yyyy-mm-dd), number of months to be reported and starting date. Shows total grouped by category expenses during given period.

rc - report expenses of chosen category (in descending order)

  Program asks user which category he/she wants to report (gives a list of available categories). 
  Also asks whether user wants to select specific description or not. If yes, then shows available descriptions for given category. Shows all expenses with chosen category (and chosen description) in descending order.

Data is read from file 'data.csv', that needs to exist in the same directory as the program.
If file 'data.csv' does not exist in the same directory as the running file, the program will create the file with necessary heading.
User can import his/hers data simply by coping the file into the directory the binary/source code is.
In this repository, there is test data given.

* data.csv
	
	* structure: Date,Expense,Category,Description
	* header needed to be included in data.csv, otherwise one expense is not reported.
	* data needs to be comma separated
  * Category and Description can not contain commas (currently)
	

## Notes/TODO

* When data.csv contains incorrect format, the program crashes. In this case, user needs to review his/hers data.csv file.
* User input 'c' cancels running command.
* Do similar functionality to option 'rn', only show report for each month.

## Author

Written by Meelis Utt
