Cluster.OBeu <img src="okfgr2.png" align="right" />
================
Kleanthis Koupidis, Charalampos Bratsas, Jaroslav Kuchar
November 9, 2016

[![Build Status](https://travis-ci.org/okgreece/Cluster.OBeu.svg?branch=master)](https://travis-ci.org/okgreece/Cluster.OBeu) [![Pending Pull-Requests](http://githubbadges.herokuapp.com/okgreece/Cluster.OBeu/pulls.svg)](https://github.com/okgreece/Cluster.OBeu/pulls) [![Github Issues](http://githubbadges.herokuapp.com/okgreece/Cluster.OBeu/issues.svg)](https://github.com/okgreece/Cluster.OBeu/issues) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![packageversion](https://img.shields.io/badge/Package%20version-1.2.1-orange.svg?style=flat-square)](commits/master) [![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)

[Cluster.OBeu](https://okgreece.github.io/Cluster.OBeu/)
========================================================

Εstimate and return the necessary parameters for cluster analysis visualizations, used in [OpenBudgets.eu](http://openbudgets.eu/). It involves a set of techniques and algorithms used to find and divide into groups the Budget data of municipalities across Europe, described by the [OpenBudgets.eu data model](https://github.com/openbudgets/data-model).

The available clustering algorithms are hierarchical, kmeans from R base, pam, clara, fuzzy from [cluster package](https://CRAN.R-project.org/package=cluster) and model based algorithms from [mclust package](https://CRAN.R-project.org/package=mclust). It can be used to find the appropriate clustering algorithm and/or the appropriate clustering number of the input data according to the internal and stability measures from [clValid package](https://CRAN.R-project.org/package=clValid).

This package can generally be used to estimate clustering parameters, extract and convert them to JSON format and use them as input in a different graphical interface and also can be used in data that are not described by the [OpenBudgets.eu data model](https://github.com/openbudgets/data-model).

You can see detailed information [here](https://okgreece.github.io/Cluster.OBeu/).

``` r
# install Cluster.OBeu- cran stable version
install.packages(Cluster.OBeu) 
# or
# alternatively install the development version from github
devtools::install_github("okgreece/Cluster.OBeu")
```

Load library `Cluster.OBeu` <img src="obeu_logo.png" align="right" />

``` r
library(Cluster.OBeu)
```

Cluster Analysis in a call
==========================

`cl.analysis` can be used to estimate clustering model parameters and/or number of clusters needed for visualization of clusters and other clustering measures as list object.

    ## {
    ##     "cluster.method": [
    ##         "pam"
    ##     ],
    ##     "raw.data": [
    ##         {
    ##             "Approved": 75399.3175,
    ##             "Draft": 70000,
    ##             "Executed": 75399.3175,
    ##             "Revised": 70000
    ##         },
    ##         {
    ##             "Approved": 0,
    ##             "Draft": 0,
    ##             "Executed": 0,
    ##             "Revised": 0
    ##         },
    ##         {
    ##             "Approved": 572878.33,
    ##             "Draft": 853750,
    ##             "Executed": 375284.3275,
    ##             "Revised": 853750
    ##         },
    ##         {
    ##             "Approved": 96020276.865,
    ##             "Draft": 94005500,
    ##             "Executed": 62440620.7475,
    ##             "Revised": 98484342.5
    ##         },
    ##         {
    ##             "Approved": 0,
    ##             "Draft": 6976000,
    ##             "Executed": 0,
    ##             "Revised": 6976000
    ##         },
    ##         {
    ##             "Approved": 1712009.7425,
    ##             "Draft": 1837500,
    ##             "Executed": 1712009.7425,
    ##             "Revised": 1837500
    ##         },
    ##         {
    ##             "Approved": 0,
    ##             "Draft": 0,
    ##             "Executed": 0,
    ##             "Revised": 0
    ##         },
    ##         {
    ##             "Approved": 60769900.2525,
    ##             "Draft": 61847000,
    ##             "Executed": 54295598.9825,
    ##             "Revised": 61847000
    ##         },
    ##         {
    ##             "Approved": 0,
    ##             "Draft": 0,
    ##             "Executed": 0,
    ##             "Revised": 0
    ##         },
    ##         {
    ##             "Approved": 14832.88,
    ##             "Draft": 10000,
    ##             "Executed": 14832.88,
    ##             "Revised": 10000
    ##         },
    ##         {
    ##             "Approved": 1335546.3125,
    ##             "Draft": 2107000,
    ##             "Executed": 1159259.96,
    ##             "Revised": 2107000
    ##         },
    ##         {
    ##             "Approved": 854670.725,
    ##             "Draft": 6687750,
    ##             "Executed": 677145.9225,
    ##             "Revised": 6987000
    ##         },
    ##         {
    ##             "Approved": 2114998.8075,
    ##             "Draft": 2544000,
    ##             "Executed": 1922246.4025,
    ##             "Revised": 2544000
    ##         },
    ##         {
    ##             "Approved": 6.975,
    ##             "Draft": 0,
    ##             "Executed": 6.975,
    ##             "Revised": 0
    ##         },
    ##         {
    ##             "Approved": 0,
    ##             "Draft": 750,
    ##             "Executed": 0,
    ##             "Revised": 750
    ##         },
    ##         {
    ##             "Approved": 0,
    ##             "Draft": 2500,
    ##             "Executed": 0,
    ##             "Revised": 2500
    ##         }
    ##     ],
    ##     "data.pca": [
    ##         [
    ##             -0.7841,
    ##             -0.013
    ##         ],
    ##         [
    ##             -0.79,
    ##             -0.012
    ##         ],
    ##         [
    ##             -0.7388,
    ##             -0.0034
    ##         ],
    ##         [
    ##             6.0593,
    ##             0.2931
    ##         ],
    ##         [
    ##             -0.5341,
    ##             0.1609
    ##         ],
    ##         [
    ##             -0.6484,
    ##             -0.0295
    ##         ],
    ##         [
    ##             -0.79,
    ##             -0.012
    ##         ],
    ##         [
    ##             3.9504,
    ##             -0.4385
    ##         ],
    ##         [
    ##             -0.79,
    ##             -0.012
    ##         ],
    ##         [
    ##             -0.789,
    ##             -0.0123
    ##         ],
    ##         [
    ##             -0.6592,
    ##             -0.0014
    ##         ],
    ##         [
    ##             -0.5067,
    ##             0.1343
    ##         ],
    ##         [
    ##             -0.6098,
    ##             -0.0185
    ##         ],
    ##         [
    ##             -0.79,
    ##             -0.012
    ##         ],
    ##         [
    ##             -0.7899,
    ##             -0.012
    ##         ],
    ##         [
    ##             -0.7899,
    ##             -0.0119
    ##         ]
    ##     ],
    ##     "medoids": [
    ##         [
    ##             0,
    ##             750,
    ##             0,
    ##             750
    ##         ],
    ##         [
    ##             96020276.865,
    ##             94005500,
    ##             62440620.7475,
    ##             98484342.5
    ##         ],
    ##         [
    ##             854670.725,
    ##             6687750,
    ##             677145.9225,
    ##             6987000
    ##         ],
    ##         [
    ##             1712009.7425,
    ##             1837500,
    ##             1712009.7425,
    ##             1837500
    ##         ],
    ##         [
    ##             60769900.2525,
    ##             61847000,
    ##             54295598.9825,
    ##             61847000
    ##         ]
    ##     ],
    ##     "medoids.id": [
    ##         15,
    ##         4,
    ##         12,
    ##         6,
    ##         8
    ##     ],
    ##     "clusters": [
    ##         1,
    ##         1,
    ##         1,
    ##         2,
    ##         3,
    ##         4,
    ##         1,
    ##         5,
    ##         1,
    ##         1,
    ##         4,
    ##         3,
    ##         4,
    ##         1,
    ##         1,
    ##         1
    ##     ],
    ##     "compare": {
    ##         "cluster.size": [
    ##             9,
    ##             1,
    ##             2,
    ##             3,
    ##             1
    ##         ],
    ##         "cluster.max_diss": [
    ##             1387171.9098,
    ##             0,
    ##             1127917.4221,
    ##             1097672.1458,
    ##             0
    ##         ],
    ##         "cluster.av_diss": [
    ##             173710.1067,
    ##             0,
    ##             563958.711,
    ##             622474.5842,
    ##             0
    ##         ],
    ##         "cluster.diameter": [
    ##             1388094.3889,
    ##             0,
    ##             1127917.4221,
    ##             1253647.6791,
    ##             0
    ##         ],
    ##         "cluster.separation": [
    ##             2082678.8924,
    ##             60707449.4489,
    ##             6328476.5263,
    ##             2082678.8924,
    ##             60707449.4489
    ##         ],
    ##         "silhouette.info": {
    ##             "widths": [
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9495
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9495
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9495
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9495
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9495
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9493
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9468
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.9248
    ##                 ],
    ##                 [
    ##                     1,
    ##                     4,
    ##                     0.4574
    ##                 ],
    ##                 [
    ##                     2,
    ##                     5,
    ##                     0
    ##                 ],
    ##                 [
    ##                     3,
    ##                     4,
    ##                     0.8438
    ##                 ],
    ##                 [
    ##                     3,
    ##                     4,
    ##                     0.833
    ##                 ],
    ##                 [
    ##                     4,
    ##                     1,
    ##                     0.7343
    ##                 ],
    ##                 [
    ##                     4,
    ##                     1,
    ##                     0.7243
    ##                 ],
    ##                 [
    ##                     4,
    ##                     1,
    ##                     0.6928
    ##                 ],
    ##                 [
    ##                     5,
    ##                     2,
    ##                     0
    ##                 ]
    ##             ],
    ##             "clus.avg.widths": [
    ##                 0.8918,
    ##                 0,
    ##                 0.8384,
    ##                 0.7171,
    ##                 0
    ##             ],
    ##             "avg.width": [
    ##                 0.7409
    ##             ]
    ##         }
    ##     },
    ##     "cluster.ellipses": [
    ##         [
    ##             [
    ##                 -0.7331,
    ##                 -0.0026
    ##             ],
    ##             [
    ##                 -0.7835,
    ##                 -0.0091
    ##             ],
    ##             [
    ##                 -0.8339,
    ##                 -0.0197
    ##             ],
    ##             [
    ##                 -0.7835,
    ##                 -0.0132
    ##             ],
    ##             [
    ##                 -0.7331,
    ##                 -0.0026
    ##             ]
    ##         ],
    ##         [
    ##             6.0593,
    ##             0.2931
    ##         ],
    ##         [
    ##             [
    ##                 -0.1332,
    ##                 -0.2275
    ##             ],
    ##             [
    ##                 -0.5204,
    ##                 0.1476
    ##             ],
    ##             [
    ##                 -0.9076,
    ##                 0.5227
    ##             ],
    ##             [
    ##                 -0.5204,
    ##                 0.1476
    ##             ],
    ##             [
    ##                 -0.1332,
    ##                 -0.2275
    ##             ]
    ##         ],
    ##         [
    ##             [
    ##                 -0.4791,
    ##                 -0.0454
    ##             ],
    ##             [
    ##                 -0.6391,
    ##                 0.066
    ##             ],
    ##             [
    ##                 -0.7992,
    ##                 0.0125
    ##             ],
    ##             [
    ##                 -0.6391,
    ##                 -0.0989
    ##             ],
    ##             [
    ##                 -0.4791,
    ##                 -0.0454
    ##             ]
    ##         ],
    ##         [
    ##             3.9504,
    ##             -0.4385
    ##         ]
    ##     ],
    ##     "cluster.convex.hulls": [
    ##         [
    ##             [
    ##                 -0.7841,
    ##                 -0.013
    ##             ],
    ##             [
    ##                 -0.789,
    ##                 -0.0123
    ##             ],
    ##             [
    ##                 -0.79,
    ##                 -0.012
    ##             ],
    ##             [
    ##                 -0.79,
    ##                 -0.012
    ##             ],
    ##             [
    ##                 -0.7899,
    ##                 -0.0119
    ##             ],
    ##             [
    ##                 -0.7388,
    ##                 -0.0034
    ##             ],
    ##             [
    ##                 -0.7841,
    ##                 -0.013
    ##             ]
    ##         ],
    ##         [
    ##             0.2931,
    ##             6.0593
    ##         ],
    ##         [
    ##             -0.5067,
    ##             -0.5341
    ##         ],
    ##         [
    ##             [
    ##                 -0.6098,
    ##                 -0.0185
    ##             ],
    ##             [
    ##                 -0.6484,
    ##                 -0.0295
    ##             ],
    ##             [
    ##                 -0.6592,
    ##                 -0.0014
    ##             ],
    ##             [
    ##                 -0.6098,
    ##                 -0.0185
    ##             ]
    ##         ],
    ##         [
    ##             -0.4385,
    ##             3.9504
    ##         ]
    ##     ]
    ## }
    ## 

Cluster Analysis on OpenBudgets.eu platform
===========================================

`open_spending.cl` is designed to estimate and return the clustering model measures of [OpenBudgets.eu](http://openbudgets.eu/) datasets.

The input data must be a JSON link according to the [OpenBudgets.eu data model](https://github.com/openbudgets/data-model). There are different parameters that a user could specify, e.g. `dimensions`, `measured.dimensions` and `amounts` should be defined by the user, to form the dimensions of the dataset. `open_spending.cl` estimates and returns the json data that are described with the [OpenBudgets.eu data model](https://github.com/openbudgets/data-model), using `cl.analysis` function.

    ## {
    ##   "cluster.method": [
    ##     "fanny"
    ##   ],
    ##   "raw.data": [
    ##     {
    ##       "Draft": 280000,
    ##       "Executed": 301597.27,
    ##       "Approved": 301597.27,
    ##       "Revised": 280000
    ##     },
    ##     {
    ##       "Draft": 0,
    ##       "Executed": 0,
    ##       "Approved": 0,
    ##       "Revised": 0
    ##     },
    ##     {
    ##       "Draft": 40000,
    ##       "Executed": 59331.52,
    ##       "Approved": 59331.52,
    ##       "Revised": 40000
    ##     },
    ##     {
    ##       "Draft": 8428000,
    ##       "Executed": 4637039.84,
    ##       "Approved": 5342185.25,
    ##       "Revised": 8428000
    ##     },
    ##     {
    ##       "Draft": 26751000,
    ##       "Executed": 2708583.69,
    ##       "Approved": 3418682.9,
    ##       "Revised": 27948000
    ##     },
    ##     {
    ##       "Draft": 10176000,
    ##       "Executed": 7688985.61,
    ##       "Approved": 8459995.23,
    ##       "Revised": 10176000
    ##     },
    ##     {
    ##       "Draft": 0,
    ##       "Executed": 27.9,
    ##       "Approved": 27.9,
    ##       "Revised": 0
    ##     },
    ##     {
    ##       "Draft": 3000,
    ##       "Executed": 0,
    ##       "Approved": 0,
    ##       "Revised": 3000
    ##     },
    ##     {
    ##       "Draft": 10000,
    ##       "Executed": 0,
    ##       "Approved": 0,
    ##       "Revised": 10000
    ##     },
    ##     {
    ##       "Draft": 0,
    ##       "Executed": 0,
    ##       "Approved": 0,
    ##       "Revised": 0
    ##     },
    ##     {
    ##       "Draft": 3415000,
    ##       "Executed": 1501137.31,
    ##       "Approved": 2291513.32,
    ##       "Revised": 3415000
    ##     },
    ##     {
    ##       "Draft": 376022000,
    ##       "Executed": 249762482.99,
    ##       "Approved": 384081107.46,
    ##       "Revised": 393937370
    ##     },
    ##     {
    ##       "Draft": 27904000,
    ##       "Executed": 0,
    ##       "Approved": 0,
    ##       "Revised": 27904000
    ##     },
    ##     {
    ##       "Draft": 7350000,
    ##       "Executed": 6848038.97,
    ##       "Approved": 6848038.97,
    ##       "Revised": 7350000
    ##     },
    ##     {
    ##       "Draft": 0,
    ##       "Executed": 0,
    ##       "Approved": 0,
    ##       "Revised": 0
    ##     },
    ##     {
    ##       "Draft": 247388000,
    ##       "Executed": 217182395.93,
    ##       "Approved": 243079601.01,
    ##       "Revised": 247388000
    ##     }
    ##   ],
    ##   "data.pca": [
    ##     [
    ##       -0.7841,
    ##       0.013
    ##     ],
    ##     [
    ##       -0.79,
    ##       0.012
    ##     ],
    ##     [
    ##       -0.789,
    ##       0.0123
    ##     ],
    ##     [
    ##       -0.6592,
    ##       0.0014
    ##     ],
    ##     [
    ##       -0.5067,
    ##       -0.1343
    ##     ],
    ##     [
    ##       -0.6098,
    ##       0.0185
    ##     ],
    ##     [
    ##       -0.79,
    ##       0.012
    ##     ],
    ##     [
    ##       -0.7899,
    ##       0.012
    ##     ],
    ##     [
    ##       -0.7899,
    ##       0.0119
    ##     ],
    ##     [
    ##       -0.79,
    ##       0.012
    ##     ],
    ##     [
    ##       -0.7388,
    ##       0.0034
    ##     ],
    ##     [
    ##       6.0593,
    ##       -0.2931
    ##     ],
    ##     [
    ##       -0.5341,
    ##       -0.1609
    ##     ],
    ##     [
    ##       -0.6484,
    ##       0.0295
    ##     ],
    ##     [
    ##       -0.79,
    ##       0.012
    ##     ],
    ##     [
    ##       3.9504,
    ##       0.4385
    ##     ]
    ##   ],
    ##   "clusters": [
    ##     1,
    ##     1,
    ##     1,
    ##     1,
    ##     2,
    ##     1,
    ##     1,
    ##     1,
    ##     1,
    ##     1,
    ##     1,
    ##     3,
    ##     2,
    ##     1,
    ##     1,
    ##     3
    ##   ],
    ##   "compare": {
    ##     "membership": [
    ##       [
    ##         0.9793,
    ##         0.0197,
    ##         0.001
    ##       ],
    ##       [
    ##         0.9881,
    ##         0.0113,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.9873,
    ##         0.0121,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.5994,
    ##         0.3894,
    ##         0.0112
    ##       ],
    ##       [
    ##         0.0634,
    ##         0.9324,
    ##         0.0042
    ##       ],
    ##       [
    ##         0.5001,
    ##         0.4867,
    ##         0.0132
    ##       ],
    ##       [
    ##         0.9881,
    ##         0.0113,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.9881,
    ##         0.0113,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.9881,
    ##         0.0114,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.9881,
    ##         0.0113,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.8543,
    ##         0.14,
    ##         0.0057
    ##       ],
    ##       [
    ##         0.0444,
    ##         0.0469,
    ##         0.9087
    ##       ],
    ##       [
    ##         0.0748,
    ##         0.9202,
    ##         0.005
    ##       ],
    ##       [
    ##         0.613,
    ##         0.3752,
    ##         0.0118
    ##       ],
    ##       [
    ##         0.9881,
    ##         0.0113,
    ##         0.0006
    ##       ],
    ##       [
    ##         0.141,
    ##         0.1526,
    ##         0.7064
    ##       ]
    ##     ],
    ##     "coeff": [
    ##       0.8222,
    ##       0.7333
    ##     ],
    ##     "memb.exp": [
    ##       2
    ##     ],
    ##     "4": [
    ##       3
    ##     ],
    ##     "5": [
    ##       128800799.7235,
    ##       1e-015
    ##     ],
    ##     "6": [
    ##       37,
    ##       1,
    ##       500
    ##     ],
    ##     "7": {
    ##       "widths": [
    ##         [
    ##           1,
    ##           2,
    ##           0.8778
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8778
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8778
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8778
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8778
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8778
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8776
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.8741
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.804
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.6042
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.584
    ##         ],
    ##         [
    ##           1,
    ##           2,
    ##           0.4198
    ##         ],
    ##         [
    ##           2,
    ##           1,
    ##           0.876
    ##         ],
    ##         [
    ##           2,
    ##           1,
    ##           0.8729
    ##         ],
    ##         [
    ##           3,
    ##           2,
    ##           0.6433
    ##         ],
    ##         [
    ##           3,
    ##           2,
    ##           0.459
    ##         ]
    ##       ],
    ##       "clus.avg.widths": [
    ##         0.7859,
    ##         0.8744,
    ##         0.5511
    ##       ],
    ##       "avg.width": [
    ##         0.7676
    ##       ]
    ##     }
    ##   },
    ##   "cluster.ellipses": [
    ##     [
    ##       [
    ##         -0.5561,
    ##         0.0185
    ##       ],
    ##       [
    ##         -0.7474,
    ##         0.0312
    ##       ],
    ##       [
    ##         -0.9387,
    ##         0.0065
    ##       ],
    ##       [
    ##         -0.7474,
    ##         -0.0062
    ##       ],
    ##       [
    ##         -0.5561,
    ##         0.0185
    ##       ]
    ##     ],
    ##     [
    ##       [
    ##         -0.1332,
    ##         0.2275
    ##       ],
    ##       [
    ##         -0.5204,
    ##         -0.1476
    ##       ],
    ##       [
    ##         -0.9076,
    ##         -0.5227
    ##       ],
    ##       [
    ##         -0.5204,
    ##         -0.1476
    ##       ],
    ##       [
    ##         -0.1332,
    ##         0.2275
    ##       ]
    ##     ],
    ##     [
    ##       [
    ##         34.7927,
    ##         -10.2609
    ##       ],
    ##       [
    ##         5.0049,
    ##         0.0727
    ##       ],
    ##       [
    ##         -24.783,
    ##         10.4063
    ##       ],
    ##       [
    ##         5.0049,
    ##         0.0727
    ##       ],
    ##       [
    ##         34.7927,
    ##         -10.2609
    ##       ]
    ##     ]
    ##   ],
    ##   "cluster.convex.hulls": [
    ##     [
    ##       [
    ##         -0.6592,
    ##         0.0014
    ##       ],
    ##       [
    ##         -0.7388,
    ##         0.0034
    ##       ],
    ##       [
    ##         -0.7899,
    ##         0.0119
    ##       ],
    ##       [
    ##         -0.79,
    ##         0.012
    ##       ],
    ##       [
    ##         -0.79,
    ##         0.012
    ##       ],
    ##       [
    ##         -0.789,
    ##         0.0123
    ##       ],
    ##       [
    ##         -0.7841,
    ##         0.013
    ##       ],
    ##       [
    ##         -0.6484,
    ##         0.0295
    ##       ],
    ##       [
    ##         -0.6098,
    ##         0.0185
    ##       ],
    ##       [
    ##         -0.6592,
    ##         0.0014
    ##       ]
    ##     ],
    ##     [
    ##       -0.5341,
    ##       -0.5067
    ##     ],
    ##     [
    ##       6.0593,
    ##       3.9504
    ##     ]
    ##   ]
    ## }
    ##
