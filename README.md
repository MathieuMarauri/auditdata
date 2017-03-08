# auditdata

__auditdata__ is an R package to perform a quality check on a table.

### Installation

You can install __auditdata__ from __github__ using the __devtools__ package : 

``` r
require(devtools)
install_github('MathieuMarauri/auditdata)
```

---

### Description

The main function of this package, _qualityCheck_, computes the number of missing values by variables and depending on the type of the variable some other key indicators.

Quantiles are computed for numeric variables and for categorical variables a frequency table of the unique values is returned. Date variables are also managable. 

An excel report is rendered using the [__xlsx__](https://cran.r-project.org/web/packages/xlsx/xlsx.pdf "Title") package. The styles are defined within the function, if the user wants to modify the style he or she has to modify the core of the function. Please go to [this site](http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r "Title") for a detailed tutorial on how to style a excel file.


### Usage

This package is typically used before an analysis. After a table has been loaded into R, it may be necessary to perform a quality check on the data, namely to know the number of missing values by columns, the levels of categorical columns and their frequency ...

The function _qualityCheck_ computes some indicators of the global quality of a dataset and produces an excel report with all the information. 


