remotes::install_github("crsh/papaja@devel")

if("tinytex" %in% rownames(installed.packages()) == FALSE) {
  install.packages("tinytex")
}
tinytex::is_tinytex()


