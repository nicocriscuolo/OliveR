
<!-- README.md is generated from README.Rmd. Please edit that file -->
OliveR 0.1.0
============

OliveR is a R package containing a [shiny](https://shiny.rstudio.com) application that provides a wide series of statistical tools for exploring and analyzing both quantitative (such as morphometric or biochemical) and genetic data that can be associated to geographical coordinates and through the use of shapefiles, the application allows to visualize both data and results of a statistical analysis directly on a geographical basis.

It is a general-purpose interactive application that make available several functions for explorative data analysis as well as several standard methods for multivariate statistics (such as Linear models, Principal Component Analysis, Cluster analysis, etc) and therefore guides the user in visualizing, exploring and analyzing the data by a simple point-and-click approach.

In brief, OliveR contains the following main functionalities:

-   Interactive graphics, with scatter plots, barplots, geographical plots… cosa ancora? Statistical models such as
-   One way Anova, both parametric and non parameric (i.e, Kruskal-Wallis….
-   Statistical hypothesis tests (metti quali test si possono eseguire.. i.e., Shapiro-…
-   Principal Component Analysis
-   Cluster analysis (hierarchical clustering, K-means, PAM, and methods for assessing cluster quality such as silhutette… Qualcosa per l’analisi genetica….vedi

Installation
------------

You can install the released version of OliveR from [GitHub](https://CRAN.R-project.org) with:

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

Getting started
---------------

**In cui descrivi come farlo partire e metti uno screen shot della tua applicazione. Qui devi spiegare le due tipologie di dati i.e., quantitative e genetici**

Data input
----------

In this first version, in order for the software to be used, you need to import two types of files:

**A questo punto DOVRESTI DESCRIVERE LE DUE TIPOLOGIE DI DATI un po più in dettaglio: quantitativi e genetici, ovvero dovresti dire che a seconda del tipo di dato si aprono due funzionalità diverse e come devono essere formattate le tabelle**

-   a user-defined .csv file: the first four columns must contain the statistical variables that indicate, in order, the identifier of the sample, the group to which it belongs, and the UTM East and North coordinates; therefore the names of these 4 columns are: "Sample\_ID", "Label", "UTM\_Est", "UTM\_Nord". The following columns are dynamic and will contain the variables on which the user wants to perform the analyzes. [CSV data](https://github.com/nicocriscuolo/OliveR/tree/master/inst/CSV_data);

-   a shapefile on which to display the spatial arrangement of the samples. The shapefile must be inside a folder (local path on the user's computer) in which there are also all the accessory files that are generally associated with a shapefile and that allow its operation (as in the GIS softwares) such as .cpg, .dbf, .prj, .sbn, .sbx, .shx. [shapefile data](https://github.com/nicocriscuolo/OliveR/tree/master/inst/shpefile_data).

Example
-------

Below is a link to the YouTube video of the application showing an example of using the software:

[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/WdUzk_eZjXU/0.jpg)](http://www.youtube.com/watch?v=WdUzk_eZjXU)

If you have a Google API to use the maps, you can view the disposition of your samples, and the information associated with it, including through the [Google Maps](https://www.google.com/maps) software. You can generate your API key at any time to access the software within the [Google Cloud Platform](https://cloud.google.com) and display the map directly on OliveR.

![oliver\_maps\_1](https://user-images.githubusercontent.com/35098432/44960136-3049cc80-aefa-11e8-9bf8-b3641b1d6e04.jpg)

![oliver\_maps\_2](https://user-images.githubusercontent.com/35098432/44960248-1f01bf80-aefc-11e8-8fd4-3acb9c9c9bba.jpg)

Citation
--------

OliveR was designed to analyze data of a continuous and genetic (SSRs) nature obtained from samples that are spatially distributed in a given geographical area, in order to quantify the differences in morphometric, biochemical and genetic parameters. Its first use, from which the name derives, was made on olive trees present in southern of Italy.

**Quindi Inserire citazione paper (una volta definito il titolo e la lista degli autori) come rivista mettere submitted**

Contact
-------

For additional information regarding OliveR, please consult the documentation or [email us](mailto:nico.criscuolo981@gmail.com).
