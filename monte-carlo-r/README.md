## Name

Serial and parallel code in R, using Monte-Carlo method as an example.

## Purpose

In this project I explore different ways of parallel programming in R. I compare parallel solutions to serial solutions to see when it is good idea to parallelize and how much speedup I would get.
At the end of the generated report, there is visualization of the walltimes of the functions.

## Dependencies

The programming language R needs to be installed.
Furthermore, this project uses the following R packages:

  * magrittr
  * data.table
  * parallel
  * foreach
  * magrittr
  * ggplot2
  * doParallel
  * dplyr

A package can be installed with command 

```{r}
install.packages("<package name>")
```

in the R console.
In the Report.Rmd, there is a function _check_packages_ provided, that downloads the necessary packages, that do not exist.

## Execution

The .Rmd file can be run as any other rmarkdown file.
However, if one wants to compile the pdf from the commandline, then one can use the command

```sh
echo "require(rmarkdown);render('Report.Rmd')" | R --vanilla
```

## Report

[Report.pdf](https://github.com/moledoc/showcase/tree/master/monte-carlo-r/Report.pdf)

## Author

Written by
Meelis Utt

