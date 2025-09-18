# Overview
Analysis scripts and data for Aswamenakul et al (under review), "Lifelong creativity defies the dynamics of rapid innovation."

The project explores the nature of creativity and cognitive breakthroughs on a lifelong timescale using Picass as a case study.

There are 5 R-markdown files and 2 plain R scripts. The files "prep_img_features.R" and "bobross_img_features.R" contain image processing for Picasso's and Bob Ross' paintings respectively. The file "img_feature_extraction.Rmd" contain the PCA feature extraction. The files "art_foraging.Rmd" and "art_space_analysis.Rmd" contain the data processing and statistical analyses for Picasso's stylistic movements. The file "bob_ross_analysis.Rmd" contains the data processing and statistical analyses for Bob Ross' stylistic movements. The file "viz_compose.Rmd" generates the figures. Each of the markdown files can be knitted or executed in R Studio using the current file structure.

We included pre-processed data (PCA features) and cleaned up meta-data needed for the main analyses in the "processed_data" directory.

# Software Guide
System requirements, installation guide, demo, and instructions for R can be found at the following link: R 4.1.2: <https://www.r-project.org>

The file "packages.csv" include a list of packages on the machine that executed our analyses. To install missing packages, use the below code:

```
packages <- read.csv(file.path('packages.csv'))[, -1]
base_packages <- as.data.frame(installed.packages()) 
to_install <- setdiff(packages$Package, base_packages$Package) 
install.packages(to_install)
```