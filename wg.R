library(devtools)
library(roxygen2)
library(usethis)

usethis::use_r("tsconvert")

usethis::use_r("urtest")

usethis::use_r("tspanel_plot")

usethis::use_r("tsplot")

usethis::use_r("tsplot_ind")

usethis::use_r("acfplot")

usethis::use_r("pacfplot")

devtools::check()

usethis::use_readme_rmd()

usethis::use_git_config(user.name = "williamcantah", user.email = "william.cantah@ucc.edu.gh")
usethis::create_github_token()
credentials::set_github_pat()
usethis::use_git()
usethis::use_github()
