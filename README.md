# auditdata

Performs a quality check on a table.

### Description

The main function of this package, _qualityCheck_, computes the number of missing values by variables and depending on the type of the variable some other key indicators.

Quantiles are computed for numeric variables and for categorical variables a frequency table of the unique values is returned. Date variables are also managable. 

An excel report is rendered using the [__xlsx__](https://cran.r-project.org/web/packages/xlsx/xlsx.pdf "Title") package. The styles are defined within the function, if the user wants to modify the style he or she has to modify the core of the function. Please go to [this site](http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r "Title") for a detailed tutorial on how to modify the style in the excel file.


### Usage

This package is typically used before an analysis. After a table has been received it is always necessary to perform a quality check on the data, namely to know the number of missing values by columns, the levels of categorical columns ...

The function _qualityCheck_ computes some indicators of the global quality of a dataset and produces an excel report with all the information. 
