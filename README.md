# OBeU R package
##To install the package:
##install.packages("devtools") if not already
# install_github("okgreece/OBeU")


#OpenCPU Short Guide- OBEU in R 
(or open it with google doc:
https://docs.google.com/document/d/1Ee68KSOOoAbTzsVga8vzw7be7Qz-f9Pu7uyVW3Viiww)


Go to: http://okfnrg.math.auth.gr/ocpu/test/

##How to use functions:

type to the endpoint:
../library/{name of the library}/R/{function}

see the function parameters:
select method: get

to run a function:
select method: post


##Time Series Example:

Go to http://okfnrg.math.auth.gr/ocpu/test/

Copy and paste to the endpoint the following
../library/OBeU/R/ts.obeu

Select Method: Post

Add parameters and set:
Param Name: tsdata
Param Value one of the following:
Athens_draft_ts
Athens_revised_ts
Athens_reserved_ts
Athens_approved_ts
Athens_executed_ts



Add another one parameters and set:
Param Name: prediction_steps
Param Value: 4 (or another number)

Ready! Click on Ajax request!

To see the results: copy the /ocpu/tmp/{something}/R/.val (the first choice on the right panel)

and paste http://okfnrg.math.auth.gr/ocpu/tmp/{something}/R/.val on a new tab.


##Aggregation Example:

  ###Import/Read a dataset (from local repo):
Go to http://okfnrg.math.auth.gr/ocpu/test/

 Copy and paste to the endpoint the following:
../library/utils/R/read.csv

Select Method: Post

Add file and set:
Param Name: file
Select the csv file from your local directory

Add if necessary the appropriate parameters and

Ready! Click on Ajax request!

To see the dataset: copy the /ocpu/tmp/{something}/R/.val (the first choice on the right panel)

and paste http://okfnrg.math.auth.gr/ocpu/tmp/{something}/R/.val on a new tab. 
(***KEEP something’s name to use at next step)

  ###Aggregate
Copy and paste to the endpoint the following
../library/OBeU/R/aggregations.obeu

Select Method: Post

Add file and set:
Param Name: data
Param Value: copy and paste the {something} of the previous step

Add another parameters if necessary

Ready! Click on Ajax request!

To see the results: copy the /ocpu/tmp/{new something}/R/.val (the first choice on the right panel)

and paste http://okfnrg.math.auth.gr/ocpu/tmp/{new something}/R/.val on a new tab.


#Further Details:

https://www.opencpu.org/help.html

https://cran.r-project.org/web/packages/opencpu/vignettes/opencpu-server.pdf

https://www.opencpu.org/jslib.html


#About the package:
Three ways to install it in R or RStudio:

##Github directly:
https://github.com/okgreece/OBeU

##Source package:
https://github.com/okgreece/OBeU-source-pack

##Binary package:
https://github.com/okgreece/OBeu-binary-R-package

#This package is under development so some issues are obvious.
#More functions will be included.



