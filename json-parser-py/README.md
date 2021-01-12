## Name 

json-parser-py

## Synopsis 

This is a python script that takes in a json file and parses each level json to a table format.

## Dependecies

Used packages:

* os
* sys
* json
* pandas or csv

To install dependencies:

```sh
pip install pandas
#or
python3 -m pip install pandas
```

## Setup

```sh
git clone https://github.com/moledoc/showcase.git
```

or make new directory and checkout only json-parser-py:

```sh
mkdir showcase
cd showcase
git init
git remote add origin https://github.com/moledoc/showcase.git
git fetch --all
git checkout origin/master -- expenses-cpp
```

## Overview

Parse json into a table format to .csv file (separator='|').
Program has the following parameters:

*  --help                 prints this help and exits;
*  --file=<filename>      json file to parse into table; either one json or multiple json's on one line;
*  --outfile=<filename>   parsed json is written to this file; default is parsed_json.csv'

Either '--help' or '--file' must be given.

## Examples

Example .json and the corresponding .csv files are given in this repository.

## Notes/Issues/TODOs

* Notes: currently none
* Issues: currently none
* TODO: currently none

## Author

Written by
Meelis Utt
