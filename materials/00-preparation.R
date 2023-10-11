#------------------------------------------------------------------------------#
#                                                                              #
#                  Engaging & Reproducible Data Visualization                  #
#                  From Theory to Implementation with ggplot2                  #
#                                                                              #
#                         Preparation: Install Packages                        #
#                                                                              #
#                              Dr. Cedric Scherer                              #
#                       RTG-UGI Workshop // October 2023                       #
#                                                                              #
#------------------------------------------------------------------------------#

pkgs <- c("ggplot2", "readr", "dplyr", "tibble", "tidyr", "forcats", 
          "stringr", "lubridate", "purrr", "here", "scales", "ragg", 
          "systemfonts", "RColorBrewer", "rcartocolor", "scico", 
          "colorspace", "prismatic", "patchwork", "ggtext", "ggforce", 
          "ggrepel", "rnaturalearth", "rmapshaper", "magick", 
          "concavemen", "gapminder", "palmerpenguins", "remotes")

install.packages(setdiff(pkgs, rownames(installed.packages())))

remotes::install_github("ropensci/rnaturalearthhires")
remotes::install_github("AllanCameron/geomtextpath")
