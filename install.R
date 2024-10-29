# Required package names
packages <- c("dplyr", "ggplot2", "patchwork", "sf", "tibble")

# Install missing packages
install.packages(setdiff(packages, rownames(installed.packages())),
                 dependencies = TRUE
                 )
