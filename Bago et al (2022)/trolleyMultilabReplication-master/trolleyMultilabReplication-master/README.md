
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trolleyMultilabReplication

<!-- badges: start -->
<!-- badges: end -->

The goal of **trolleyMultilabReplication** is to provide a structure for
the data cleaning and analysis of the PSA006 trolley multilab
replication project. You can find out more about the project at the
projects’ [OSF repository](https://osf.io/efy2w/).

## Installation

The package is currently not on CRAN, but the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marton-balazs-kovacs/trolleyMultilabReplication")
```

## Description of the package

The package includes high-level data cleaning and and data analyzing
functions for the project. The functions are moderately documented. The
documentation will be expanded in the future. To check the documentation
of a given function use the following format
`?trolleyMultilabReplication::calculate_study1_stat`.

### Data

The code for data cleaning can be found in the ‘data-raw/’ folder of the
package. The data wrangling for the source data imported directly from
Qualtrics can be found in the ‘data-raw/trolley_raw.R’ file.

The raw datafile for the replication that contains the data from all of
the data collection sites can be found in a .csv format in the
‘extdata/’ folder and called ‘trolley_raw.csv’.

You can load this datafile if you are replicating the results of the
study by the following code:

``` r
# install.packages("readr")
# library(readr)
trolley_raw <- readr::read_csv(system.file("extdata", "trolley_raw.csv", package = "trolleyMultilabReplication"))
```

The steps for preprocessing the raw datafile can be found in the
‘raw-data/trolley_preprocessed.R’ file. The code for creating the final
processed datafile can be found in the ‘data-raw/trolley.R’ file.

The codebook for the processed datafile can be found by running the
following function: `?trolleyMultilabReplication::trolley`. This
codebook contains the description of the most important variables for
the raw data as well.

The preprocessed data, the processed data, and the datafile containing
the cultural distance scores for each country are saved as package data
files and can be loaded by the following functions:

``` r
# List the datafiles saved as package data
data(package = "trolleyMultilabReplication")
# Cultural distance data
trolleyMultilabReplication::cultural_distance
# Preprocessed data
trolleyMultilabReplication::trolley_preprocessed
# Processed data
trolleyMultilabReplication::trolley
```

### Analysis code

The analysis code that we used for this project including the code for
the creations of the tables and figures can be found in the ‘vignettes’
folder. The figures for the project in .pdf format can be found in the
‘vignettes/figures/’ folder.
