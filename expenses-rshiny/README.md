## Name 

expenses-rshiny

## Synopsis 

Tool to manage and visualize expenses. Written in R using Shiny Dashboard.

## Download

Download the whole repository and navigate to expenses-rshiny:

```sh
git clone https://github.com/moledoc/showcase.git
```

or make new directory and checkout only expenses-rshiny:

```sh
mkdir showcase
cd showcase
git init
git remote add origin https://github.com/moledoc/showcase.git
git fetch --all
git checkout origin/master -- expenses-rshiny
```

## Installation

There is no need for installation, except for installing R itself.
Necessary packages are checked and installed if necessary.


### Note

	* When running GNU/Linux or other Unix operating system, then user might need to resolve some dependencies themself.

## Overview

The program listens to port 4004. If running the program does not open a window by itself, then user can use url http://127.0.0.1:4004 in the browser to view the application. This program expects data/ directory to be in the same directory as app_v2.R. The directory data/ needs to contain the following files:

	* data.csv
	
		* structure: Date,Expense,Type,Description
		* this file requires brought out header
		* data needs to be comma separated
	
	* data_types.csv

		* structure: Type,Description
		* this file requires brought out header
		* data needs to be comma separated
	
	* data_scratchpad.csv
	
		* structure: does not need special structure, can hold notes to self on separate lines.
		* the header in this file does not matteer.
		
If directory data/ does not exits, then it is created with empty files mentioned above. If the directory exists, but the files do not, then currently the program should get an error.
User can import his/hers data simply by coping the file into data/ directory.
In this repository, there is test data given.

### Screenshots

#### Add/delete expense:

![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-rshiny/README_images/add_expense.png)

#### Make a temporary scratch for self:

![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-rshiny/README_images/scratchpad.png)

#### Add/delete type/description:

![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-rshiny/README_images/add_type_desc.png)

#### Scatterplot demo:

![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-rshiny/README_images/scatterplot.png)
		
#### Barplot demo:

![Screenshot](https://github.com/moledoc/showcase/blob/master/expenses-rshiny/README_images/barplot.png)

## Author

Written by Meelis Utt
