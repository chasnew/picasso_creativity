# Overview
Analysis scripts and data for Aswamenakul et al (under review), "Lifelong creativity in the artists Picasso, Monet, and Cézanne."

The project explores the nature of creativity and cognitive breakthroughs on a lifelong timescale using Picasso as a case study with Monet and Cézanne as comparative cases.

There are six R-markdown files and two plain R scripts. 

The two R scripts are for data preprocessing. You do not need to run them to reproduce the results in the paper. We share them in case researchers want to use the method for other projects. The file "prep_img_features.R" contain image feature extraction functions for painting images. The file "img_feature_extraction.Rmd" contain the pipeline that extract image embeddings and reduce embeddings dimensionality using Principle Component Analysis (PCA).

The five R-markdown files reproduce all results and figures for the Main Text and Supplemental Materials: 
1. "art_space_analysis.Rmd" reproduces the analyses of overall pairwise similarity of Picasso's paintings
2. "art_foraging.Rmd" reproduces the sliding-window analyses of Picasso
3. "cezanne_analysis.Rmd" reproduces the sliding-window analyses of Cézanne
4. "monet_analysis.Rmd" reproduces the sliding-window analyses of Monet
5. "viz_compose.Rmd" generates both main and supplemental figures.\
Each of the markdown files can be knitted or executed in R Studio using the current file structure.

The "scraping_scripts" folder contains R scripts used to retrieve Bob Ross', C\'ezanne's, and Monet's paintings and their metadata.\
The "results" folder contains pre-computed results used to produce the figures.\
The "img" folder contains images used to produce figures.\
The "processed_data" directory contains pre-processed data (PCA features) and cleaned up meta-data needed for the main analyses.

# Software Guide
System requirements, installation guide, demo, and instructions for R can be found at the following link: R 4.4.3: <https://www.r-project.org>

The file "packages.csv" include a list of packages on the machine that executed our analyses. To install missing packages, use the below code:

```
packages <- read.csv(file.path('packages.csv'))[, -1]
base_packages <- as.data.frame(installed.packages()) 
to_install <- setdiff(packages$Package, base_packages$Package) 
install.packages(to_install)
```
