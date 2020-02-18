# auditdata

__auditdata__ is an R package to perform an audit on the quality of a table. 

### Installation

You can install __auditdata__ from __github__ using the __devtools__ package : 

``` r
devtools::install_github('MathieuMarauri/auditdata)
```

<!--
If you cannot install the package due to __rJava__ you should first check that you have a 64-bit Java version installed on your computer with the command line `java -d64 -version` or the R command `system("java -version")`. If you do not have Java installed with te proper architecture then go to [this site](https://www.java.com/en/download/manual.jsp "Title") to download the correct version of Java. Try again installing the __audidata__ package, you should have have to restart your computer. 

Should you have other issues regarding __rJava__ installation please refer to [this answer](https://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r/7604469#7604469 "Title") on stackoverflow. 
-->

---

### Description

The main function of this package are `audit_report_html_global()`, `audit_report_html()` and `audit_report_excel()`. They produce a document (html or excel file) containing information about the table given in input. The number of rows, of columns, of unique values and missing values are given as global information. More details are given on inidividual columns depending on the type (numeric, categorical or date).

### Usage

``` r
library("auditdata")
# Generation of fake data
data <- data.frame(
  cat1 = sample(month.name, 1000, replace = TRUE),
  cat2 = sample(letters, 1000, replace = TRUE),
  cat3 = sample(c("apple", "orange", "banana", "pear", "grapefruit", "cherry"), 1000, replace = TRUE),
  num1 = runif(1000, 100, 150),
  num2 = rnorm(1000, 37, 8),
  num3 = rexp(1000, 2),
  bool1 = sample(c(TRUE, FALSE), 1000, replace = TRUE),
  date1 = seq(from = as.Date("2010-01-01"), to = as.Date("2017-01-01"), length.out = 1000),
  date2 = seq(from = as.Date("2010-01-01"), to = as.Date("2017-01-01"), length.out = 1000)
)
while (sum(is.na(data) == TRUE) < (nrow(data) * ncol(data) * 10 / 100)) {
  data[sample(nrow(data), 1), sample(ncol(data), 1)] <- NA
}

# Audit of the data
audit_report_html_global(data)
audit_report_html(data)
```
