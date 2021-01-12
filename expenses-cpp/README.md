<!-- README: this project was written as a project in C++ course and is meant to be run in a GNU/Linux commandline. This program semi interactive. -->

## Name

expenses - Tool for summarizing expenses written in C++

## Description

This is a project for a C++ course.

This program summarizes expenses in the given time period
defined in the $HOME/.config/expenses/data directory.

## Download

Download the whole repository and navigate to expenses-cpp:

```sh
git clone https://github.com/moledoc/showcase.git
```

or make new directory and checkout only expenses-cpp:

```sh
mkdir showcase
cd showcase
git init
git remote add origin https://github.com/moledoc/showcase.git
git fetch --all
git checkout origin/master -- expenses-cpp
```

## Installation

It is advised to do 

```sh
$ make setup
```

This creates the necessary folders

```sh
$HOME/.config/expenses/data
$HOME/.config/expenses/data/store
```

Additionally, example data files are copied from program/examples
to $HOME/.config/expenses/data.

If 'make setup' is not run, the necessary directories must be created manually. 
Otherwise the program can't find data.

Install the program:

```sh
$ sudo make install
```

Installing the program also installs the man page for the program.
For man page run

```sh
$ man expenses
```

Uninstall the program:

```sh
$ sudo make uninstall
```

Uninstall cleans the program directory and removes man page.
Directories $HOME/.config/expenses/data and $HOME/.config/expenses/data/store are
not deleted, since they might contain valuable data.

There is also possible to compile and run the program without installing it.

Compile:

```sh
$ make [build]
```

Clean:

```sh
$ make clean
```

Run:

```sh
$ ./bin/expenses
```

Doxygen documentation:

```sh
$ make doc
```

## Options

Summarizes expenses data for given period.

Usage: expenses [options [yyyy-mm | yyyy-mm-dd] [yyyy-mm | yyyy-mm-dd]]

**-c --current**

	Summarizes the running month. 
	This flag is used when there are no flags given.

**-f --from** (yyyy-mm | yyyy-mm-dd)

	Summarizes expenses from the given date up to the end of running month.

**-h --help**

	Shows help.

**-m --month** (yyyy-mm | yyyy-mm-dd)

	Summarizes one month expenses starting from the given date. 
	If date is omitted, the first day of the month is used.

**--monthly**

	Summarizes each month expenses starting with the earliest
	and ending with the latest defined month in the data.

**--path** (path)

	Path to data files. 
	Default location for data is $HOME/.config/expenses/data.

**--period** (yyyy-mm | yyyy-mm-dd) (yyyy-mm | yyyy-mm-dd)
	
	Summarizes between given dates. 
	If date is omitted, the first day of the month is used.

**-p --previous**

	Summarizes the previous month.

**-s --store**

	Stores summaries in $HOME/.config/expenses/data/store.
	If --path flag is used, then summaries are stored in <path to data>/store.
	If the directory <path to data>/store doesn't exist, it will be created.
	For each period a subdirectory is made.
	The subdirectory contains one summary file for each data file 
	+ one summary file containing info about all the files.

**-t --total**

	Summarizes all expenses.
	
## Notes

* If there are multiple period flags given (such as --previous, --current, --total, --period etc), then only the last such flag is used.
* If the end of the month is given for the --month flag as the starting date, then expenses are summarized to the end of the next month.  Note that in case of February, if it's a leap year and the given date has the day value as 28, then expenses are summarized to 28th of March.

## Examples

```sh
$ expenses -m 2020-01-07
$ expenses --month 2020-9
$ expenses --period 2019-12 2020-03-05
$ expenses --period 2019-12-8 2020-03 --store
$ expenses -p -s
$ expenses -f 2019-05-05
$ expenses --current --path $HOME/finances/data1
$ expenses --monthly --store
```

## Problems/issues

* Currently descriptions in data do not allow spaces. One should substitute them with some other character, such as underscore (\_), point (.) etc.
* User inputs for dates are not currently checked and incorrect inputs will result in errors.

## Author

Written by Meelis Utt
