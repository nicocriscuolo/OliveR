
<!-- README.md is generated from README.Rmd. Please edit that file -->
OliveR 0.1.0
============

OliveR is an interactive statistical software realized through the features of the [shiny](https://shiny.rstudio.com) package. Its purpose is to implement functions of explorative data analysis and multivariate statistics (such as PCA and Cluster analysis) within a user friendly environment. Moreover, through the use of shapefiles, the application allows to view the results of the analysis directly on a geographical basis. OliveR was in fact designed to analyze and classify data of a continuous nature obtained from samples that are spatially distant, in order to quantify the differences in morphometric, biochemical and genetic nature. Its first use, from which the name derives, was made on olive trees present in southern Italy.

Installation
------------

You can install the released version of OliveR from [CRAN](https://CRAN.R-project.org) with:

``` r
library(devtools)

install_github(repo = "nicocriscuolo/OliveR")
```

Once the package is loaded and the dependencies installed, you can run the software in the default browser through the following functions:

``` r
library(OliveR)

runOliveR()
```

OliveR 0.1.0 works on all types of browsers (Internet Explorer, Safari, Chrome, etc.) and in its current version, if you do not use the features of Google Maps, it can also work locally and then offline. All you have to do is to install the updated versions of [R](https://www.r-project.org) and [RStudio](https://www.rstudio.com).

In this first version, in order for the software to be used, you need to import two types of files:

-   a user-defined .csv file: the first four columns must contain the statistical variables that indicate, in order, the identifier of the sample, the group to which it belongs, and the UTM East and North coordinates; therefore the names of these 4 columns are: "Sample\_ID", "Label", "UTM\_Est", "UTM\_Nord". The following columns are dynamic and will contain the variables on which the user wants to perform the analyzes;
-   a shapefile on which to display the spatial arrangement of the samples. The shapefile must be inside a folder (local path on the user's computer) in which there are also all the accessory files that are generally associated with a shapefile and that allow its operation (as in the GIS softwares) such as .cpg, .dbf, .prj, .sbn, .sbx, .shx.

Example
-------

Below is a link to the YouTube video of the application showing an example of using the software:

[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/WdUzk_eZjXU/0.jpg)](http://www.youtube.com/watch?v=WdUzk_eZjXU)

If you have a Google API to use the maps, you can view the disposition of your samples, and the information associated with it, including through the [Google Maps](https://www.google.com/maps) software. You can generate your API key at any time to access the software within the [Google Cloud Platform](https://cloud.google.com) and display the map directly on OliveR.

![oliver\_maps\_1](https://user-images.githubusercontent.com/35098432/44960136-3049cc80-aefa-11e8-9bf8-b3641b1d6e04.jpg)

![oliver\_maps\_2](https://user-images.githubusercontent.com/35098432/44960248-1f01bf80-aefc-11e8-8fd4-3acb9c9c9bba.jpg)
