## Name 

expenses-scala

## Synopsis 

Interactive GUI tool to manage and report expenses. Written in Scala using ScalaFX.

## Download

Download the whole repository and navigate to expenses-scala:

```sh
git clone https://github.com/moledoc/showcase.git
```

or make new directory and checkout only expenses-scala:

```sh
mkdir showcase
cd showcase
git init
git remote add origin https://github.com/moledoc/showcase.git
git fetch --all
git checkout origin/master -- expenses-scala
```

## Dependecies

ScalaFX must be set up. 
sbt must be installed. It is a build tool for Scala and Java.
I used IntelliJ for easier setup. The necessary .sbt file is included in the repository.

## Overview

There are three tabs in the program: Report, Add and Remove.

* In 'Report' tab, user can select a period and when button 'Report' is pressed, a report of expenses for selected period is shown. At startup, running month expenses are reported.

    * ![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-scala/README_images/report.png)

* In 'Add' tab, user can add a new expense by filling all text boxes and pressing the button 'Add'. The button 'Refresh' refreshes the view of expenses. 

    * ![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-scala/README_images/add.png)

* In 'Remove' tab, user can remove expense(s) by filling the text box with expenses ID\'s and pressing the button 'Remove'. Valid ID\'s are following:

    * single integer number;
    * comma separated list of integer numbers;
    * semi-colon separated list of integer numbers;
    * space separated list of integer numbers.

The button 'Refresh' refreshes the view of expenses. 

    * ![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-scala/README_images/remove.png)


Data is read from file 'data.csv', that needs to exist in the root directory of the scala project. If it doesn't exist, the program will not run.
In this repository, there is test data given.

* data.csv
	
	* structure: Date,Expense,Type,Description
	* header needed to be included in data.csv, otherwise one expense is not reported.
	* data needs to be comma separated
  * Category and Description can not contain commas
	

## Notes/TODOs

* When data.csv contains incorrect format, the program crashes. In this case, user needs to review his/hers data.csv file.
* When there is no data.csv file in projects root directory, the program will not run

## Author

Written by
Meelis Utt
