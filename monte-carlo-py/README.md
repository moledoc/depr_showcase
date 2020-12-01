
## Name

Serial and parallel code in python, using Monte-Carlo method as an example.

## Purpose

In this project I demonstraite simple use of Message Passing Interface (MPI) in python.
I compare parallel solutions to serial solutions to see when it is good idea to parallelize and how much speedup I get.
I also use different number of processes.
The solutions are formulated into a report using Rmarkdown.

## Dependencies 

If one wants to run the report/programs oneself.\
The programming language python needs to be installed.
Furthermore, this project uses MPI libraries, so the necessary MPI packages must also be installed.
Also, corresponding python package must be installed (mpi4py).
Other used packages

* numpy
* pandas
* time
* matplotlib
* sys

A python package can be installed with command 

```{r}
pip install <package>
```

Since the report is compiled using Rmarkdown, then when one wants to run the report oneself,
R needs to be installed.
Necessary packages for R are

* reticulate (for running python in rmarkdown)
* rmarkdown
* knitr (should come with rmarkdown)

A R package can be installed with command 

```{r}
install.packages("<package name>")
```

in the R console.

## Execution

The .Rmd file can be run as any other rmarkdown file.
However, if one wants to compile the pdf from the commandline, then one can use the command

```sh
echo "require(rmarkdown);render('Report.Rmd')" | R --vanilla
```

## Report

[Report.pdf](https://github.com/moledoc/showcase/tree/master/monte-carlo-py/Report.pdf)

## Author

Written by
Meelis Utt

