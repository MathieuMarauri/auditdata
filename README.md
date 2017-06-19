# auditdata

__auditdata__ is an R package to perform a quality check on a table.

### Installation

You can install __auditdata__ from __github__ using the __devtools__ package : 

``` r
require(devtools)
install_github('MathieuMarauri/auditdata)
```

If you cannot install the package due to __rJava__ you should first check that you have a 64-bit Java version installed on your computer with the command line `java -d64 -version` or the R command `system("java -version")`. If you do not have Java installed with te proper architecture then go to [this site](https://www.java.com/en/download/manual.jsp "Title") to download the correct version of Java. Try again installing the __audidata__ package, you should have have to restart your computer. 

Should you have other issues regarding __rJava__ installation please refer to [this answer](https://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r/7604469#7604469 "Title") on stackoverflow. 

---

### Description

The main function of this package, _qualityCheck_, computes the number of missing values by variable and, depending on the type of the variable, other data quality key indicators.

Percenttiles are computed for numeric variables. For categorical variables a frequency table of the unique values is returned. 

An excel report is rendered using the [__xlsx__](https://cran.r-project.org/web/packages/xlsx/xlsx.pdf "Title") package. The styles are defined within the function, To modify the styles, edit the function and refer to [this site](http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r "Title") for a detailed tutorial on how to style an excel file.

Use `?qualityCheck` to see the documentation of the function.

### Usage

``` r
library("auditdata")
# load your data
qualityCheck(data)
```

This package is typically used before an analysis. After a table has been loaded into R, it may be necessary to perform a quality check on the data, namely to know the number of missing values by columns, the levels of categorical columns and their frequency ...

The function _qualityCheck_ computes some indicators of the global quality of a dataset and produces an excel report with all the information. 


