# Build Package Script
knitr::knit("README.Rmd")
codemetar::give_opinions()
styler::style_pkg()
goodpractice::gp()
devtools::document()
devtools::build()
devtools::check()
codemetar::write_codemeta()
