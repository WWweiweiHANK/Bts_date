This folder contains all code, data, and datasets necessary to follow and reproduce the analyses presented in the article 
"The globalizability of temporal discounting", submitted for its publication to Nature Human Behavior. 
There are five R files, two datasets, and two R.Data files. The order we recommend opening these is presented below (skipping those not necessary to open
but necessary to have in the folder for analyses).

a)	1_1_data_cleaning_and_preparation.Rmd: This file will load the initial responses to the questionnaire (999_2_data_original_1_10_2021.csv) 
	and the information at the country-level (999_3_country_information.csv). This script generates four different datasets to be used in 
	the posterior analyses. These datasets include information at item (dat_item), individual (dat_unique), and individual level but are 
	filtered for posterior analyses (dat_filter).
 
b)	1_2_data_files.Rmd: This R data file contains the three data frames mentioned above. These are requested at the beginning of the 
	following R code file. 

c)	2_1_model_estimation.R: This R file estimates all statistical models. 
	We caution that some of the analyses can take hours to return (particularly the Bayesian version of the main models). 
	All the resulting models are already estimated in the file 2_2_main_results.R

d)	3_1_submission_graphs. .Rmd: This R markdown file will generate all figures presented in the article's main text. 
	Remember that some of the graphs have minor aesthetic modifications conducted in alternative graphical software. 

e)	4_1_supplementary_analysis. This file contains all the instructions to compute all tables and figures presented in the supplementary file.

f)	999_1_auxiliary_functions.R: Collection of functions to perform the statistical analyses and graphics included in the remainder R code files. 
	This file is loaded at the beginning of all R code files.
