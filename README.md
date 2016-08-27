# OBeU R package
##To install the package:
a.) library(devtools) or 
install.packages("devtools") if not already 
and then library(devtools)

b.) install_github("okgreece/OBeU")


#OpenCPU Short Guide- OBEU in R 
(or open it with google doc:
https://docs.google.com/document/d/1Ee68KSOOoAbTzsVga8vzw7be7Qz-f9Pu7uyVW3Viiww)


Go to: http://okfnrg.math.auth.gr/ocpu/test/

##How to use functions:

1.) type to the endpoint:
../library/{name of the library}/R/{function}

2.) see the function parameters:
select method: get

3.) to run a function:
select method: post


##Time Series Example:

1.) Go to http://okfnrg.math.auth.gr/ocpu/test/

2.) Copy and paste to the endpoint the following
../library/OBeU/R/ts.obeu

3.) Select Method: Post

4.) Add parameters and set:
  a.)Param Name: tsdata
  b.)Param Value one of the following:
  
      Athens_draft_ts
      
      Athens_revised_ts
      
      Athens_reserved_ts
      
      Athens_approved_ts
      
      Athens_executed_ts



5.)Add another one parameters and set:
  a.)Param Name: prediction_steps
  b.)Param Value: 4 (or another number)

6.) Ready! Click on Ajax request!

7.) To see the results: copy the /ocpu/tmp/{something}/R/.val (the first choice on the right panel)

8.) and paste http://okfnrg.math.auth.gr/ocpu/tmp/{something}/R/.val on a new tab.


##Aggregation Example:

  ##Import/Read a dataset (from local repo):
1.) Go to http://okfnrg.math.auth.gr/ocpu/test/

2.) Copy and paste to the endpoint the following:
../library/utils/R/read.csv

3.) Select Method: Post

4.) Add file and set:
  a.)Param Name: file
  b.)Select the csv file from your local directory

5.) Add if necessary the appropriate parameters and

6.) Ready! Click on Ajax request!

7.) To see the dataset: copy the /ocpu/tmp/{something}/R/.val (the first choice on the right panel)

8.) and paste http://okfnrg.math.auth.gr/ocpu/tmp/{something}/R/.val on a new tab. 
(***KEEP something’s name to use at next step)

  ##Aggregate
1.) Copy and paste to the endpoint the following
../library/OBeU/R/aggregations.obeu

2.) Select Method: Post

3.) Add ile and set:
  a.) Param Name: data
  b.) Param Value: copy and paste the {something} of the previous step

4.) Add another parameters if necessary

5.) Ready! Click on Ajax request!

6.) To see the results: copy the /ocpu/tmp/{new something}/R/.val (the first choice on the right panel)

7.) and paste http://okfnrg.math.auth.gr/ocpu/tmp/{new something}/R/.val on a new tab.


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



