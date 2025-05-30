# Define the list of required packages
required_packages <- c(
  "tidyverse", 
  "foreach", 
  "doParallel", 
  "rjags", 
  "coda", 
  "devtools", 
  "MASS", 
  "restriktor",
  "data.table",
  "kableExtra",
  "patchwork",
  "latex2exp",
  "grid",
  "gridExtra"
)

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  # Identify missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    message("The following packages are missing and will be installed: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  } else {
    message("All required packages are already installed.")
  }
}

# Run the function to ensure all packages are installed
install_missing_packages(required_packages)

# Load the packages
library(tidyverse)
library(foreach)
library(doParallel)
library(rjags)
library(coda)
library(devtools)
library(MASS)
library(restriktor)
library(grid)
library(gridExtra)

message("The packages have been installed and loaded.")
