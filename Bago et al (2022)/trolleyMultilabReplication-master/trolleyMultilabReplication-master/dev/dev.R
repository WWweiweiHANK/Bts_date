# Add packages
usethis::use_package("dplyr")
usethis::use_package("kableExtra")
usethis::use_package("lmerTest")
usethis::use_package("jsonlite")
usethis::use_package("vroom")
usethis::use_package("countrycode")
usethis::use_package("gt")
usethis::use_package("janitor")
usethis::use_package("here")
usethis::use_package("ggrepel")
usethis::use_package("purrr")
usethis::use_package("ggplot2")
usethis::use_package("tidyr")
usethis::use_package("furrr")
usethis::use_package("tibble")
usethis::use_package("msm")
usethis::use_package("rlang")
usethis::use_package("BayesFactor")
usethis::use_package("broom")
usethis::use_package("broom.mixed")
usethis::use_package("lme4")
usethis::use_package("tidyr")
usethis::use_package("broomExtra")
usethis::use_package("lmerTest")
usethis::use_package("stringr")
usethis::use_package("tidyselect")
usethis::use_package("Rmisc")
usethis::use_package("effsize")
usethis::use_dev_package("papaja", remote = "crsh/papaja")


# Add pipe
usethis::use_pipe(export = TRUE)

# Add raw data files
usethis::use_data_raw("trolley_raw")
usethis::use_data_raw("trolley_preprocessed")
usethis::use_data_raw("trolley")
usethis::use_data_raw("cultural_distance")
usethis::use_data_raw("trolley_infosheet")

# Add data files
usethis::use_data(correct_answers, qualtrics_surveys, internal = TRUE, overwrite = TRUE)

# Document package
devtools::document()

# Load package
devtools::load_all()

# Add R functions
usethis::use_r("aggregate_inferences")
usethis::use_r("draw_values")
usethis::use_r("generate_dataset")
usethis::use_r("get_lmbf_inference")
usethis::use_r("get_ttestbf_inference")
usethis::use_r("simulate")
usethis::use_r("add_ind_col_scale")
usethis::use_r("summarise_study")
usethis::use_r("utils")
usethis::use_r("geom_flat_violin")
usethis::use_r("summarise_plot_data")
usethis::use_r("theme_trolley")
usethis::use_r("prepare_plot_data_study1")
usethis::use_r("create_plot_study1")
usethis::use_r("prepare_plot_data_study2")
usethis::use_r("create_plot_study2")
usethis::use_r("create_ind_col_plot")
usethis::use_r("prepare_plot_data_country")
usethis::use_r("calculate_study1_stat")
usethis::use_r("calculate_study2_stat")
usethis::use_r("calculate_intention_stat")
usethis::use_r("calculate_contact_stat")
usethis::use_r("create_plot_country")
usethis::use_r("my_bf_extractor")
usethis::use_r("calculate_interaction_stats")
usethis::use_r("tidy_interaction_stats")
usethis::use_r("calculate_additional_table")
usethis::use_r("calculate_culture_utilitarianism")
usethis::use_r("tidy_culture_utilitarianism")
usethis::use_r("prepare_plot_data_country_means")
usethis::use_r("create_plot_country_utilitarianism")


# Add vignettes
usethis::use_vignette("supplementary_materials")
rmarkdown::render()
usethis::use_vignette("manuscript")
devtools::build_vignettes()
build_rmd()
devtools::build_rmd("vignettes/supplementary_materials.Rmd")
devtools::build_rmd("vignettes/manuscript.Rmd")
devtools::build_rmd("vignettes/additional_analysis_R1.Rmd")

# Check the package
devtools::check()

# Build the package
# devtools::build(vignettes = FALSE)
