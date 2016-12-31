Cluster.OBeu <img src="obeu_logo.png" align="right" />
================
Kleanthis Koupidis, Charalampos Bratsas, Jaroslav Kuchar
November 9, 2016

[![Build Status](https://travis-ci.org/okgreece/Cluster.OBeu.svg?branch=master)](https://travis-ci.org/okgreece/Cluster.OBeu) [![Pending Pull-Requests](http://githubbadges.herokuapp.com/okgreece/Cluster.OBeu/pulls.svg)](https://github.com/okgreece/Cluster.OBeu/pulls) [![Github Issues](http://githubbadges.herokuapp.com/okgreece/Cluster.OBeu/issues.svg)](https://github.com/okgreece/Cluster.OBeu/issues)

This document describes the use of the functions implemented in Cluster.OBeu package in OpenCPU environment.

Install:
========

Load *devtools* library or install it if not already:

``` r
install.packages("devtools")
```

Then install *Cluster.OBeu* from [Github](https://github.com/okgreece/Cluster.OBeu)

``` r
# devtools::install_github("okgreece/Cluster.OBeu")
```

And load the library

``` r
# library(Cluster.OBeu)
```

### R Example

The package includes the following data:

``` r
# 1. a link to json file-openspending
# to add
```

``` r
# 2. a link to json file-rudolf
# to add
```

OpenCPU Short Guide - Cluster.OBeu
==================================

Go to: <http://okfnrg.math.auth.gr/ocpu/test/>

How to use functions:
---------------------

Type to the endpoint:

``` r
 ../library/ {name of the library} /R/ {function}
```

If you want to see the function parameters you should:

-   Select Method:

``` r
Get
```

and in order to run a function you should:

-   Select Method:

``` r
Post
```

Example \#1 - Open Spending-Cluster Analysis
--------------------------------------------

1 Go to <http://okfnrg.math.auth.gr/ocpu/test/>

2 Copy and paste the following function to the endpoint

``` r
../library/Cluster.OBeu/R/open_spending.cl
```

3 *Select Method*:

``` r
Post
```

4 **Add parameters** and set:

Define the input time series data:

-   *Param Name*:

``` r
json_data
```

-   *Param Value* -the **URL** of json data:

``` r
# a link with json file-openspending
# to add
```

Define the dimensions:

-   *Param Name*:

``` r
dimensions
```

-   *Param Value*:

``` r
# to add
```

Define the amount parameter:

-   *Param Name*:

``` r
amounts 
```

-   *Param Value*:

``` r
# to add
```

5 Ready! Click on **Ajax request**!

6 To see the results:

copy the */ocpu/tmp/{this}/R/.val* (the first choice on the right panel)

7 and paste <http://okfnrg.math.auth.gr/ocpu/tmp/> {this} /R/.val on a new tab.

Example \#2 - Rudolf Cluster Analysis
-------------------------------------

1 Go to <http://okfnrg.math.auth.gr/ocpu/test/>

2 Copy and paste the following function to the endpoint

``` r
../library/Cluster.OBeu/R/open_spending.cl
```

3 *Select Method*:

``` r
Post
```

4 **Add parameters** and set:

Define the input time series data:

-   *Param Name*:

``` r
json_data
```

-   *Param Value* -the following output from rudolf/ open spending api or you can provide the also **json URL**:

``` r
# 1. json link
# to add
```

Define the what:

-   *Param Name*:

``` r
dimensions
```

-   *Param Value*:

``` r
# to add
```

Define the measured dimensions parameter (e.g. budgetPhase dimension includes variables that are measurable/numeric variables):

-   *Param Name*:

``` r
measured.dimensions
```

-   *Param Value*:

``` r
# to add
```

Define the amount parameter:

-   *Param Name*:

``` r
amounts
```

-   *Param Value*:

``` r
# to add
```

5 Ready! Click on **Ajax request**!

6 To see the results:

copy the */ocpu/tmp/{this}/R/.val* (the first choice on the right panel)

7 and paste <http://okfnrg.math.auth.gr/ocpu/tmp/> {this} /R/.val on a new tab.

Further Details:
================

-   <https://www.opencpu.org/help.html>

-   <https://cran.r-project.org/web/packages/opencpu/vignettes/opencpu-server.pdf>

-   <https://www.opencpu.org/jslib.html>

Github:
=======

-   <https://github.com/okgreece/Cluster.OBeu>
