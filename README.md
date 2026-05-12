# Overview
Analysis scripts and data for Aswamenakul et al (under review), "Lifelong creativity in the artists Picasso, Monet, and C\'ezanne."

The project explores the nature of creativity and cognitive breakthroughs on a lifelong timescale using Picasso as a case study with Monet and C\'ezanne as comparative cases.

There are 5 R-markdown files and 2 plain R scripts. The file "prep_img_features.R" contain image feature extraction functions for painting images. The file "img_feature_extraction.Rmd" contain the pipeline that extract image embeddings and reduce embeddings dimensionality using Principle Component Analysis (PCA).\
File "art_foraging.Rmd" contains data processing and sliding window analyses for Picasso's stylistic movements over his career. Similarly, files "bob_ross_analysis.Rmd", "cezanne_analysis.Rmd", and "monet_analysis.Rmd" contain the same analysis pipeline for Bob Ross, C\'ezanne, and Monet.\
File "art_space_analysis.Rmd" contains data processing and analyses for Picasso's overall stylistic steps using t-SNE and pairwise similarity.\
File "viz_compose.Rmd" generates both main and supplemental figures.\
Each of the markdown files can be knitted or executed in R Studio using the current file structure.

"scraping_scripts" folder contains R scripts used to retrieve Bob Ross', C\'ezanne's, and Monet's paintings and their metadata.\
"results" folder contains analysis results used to produce the figures.\
"img" folder contains manually created images used to produce figures.

We included pre-processed data (PCA features) and cleaned up meta-data needed for the main analyses in the "processed_data" directory.

# Software Guide
System requirements, installation guide, demo, and instructions for R can be found at the following link: R 4.4.3: <https://www.r-project.org>

The file "packages.csv" include a list of packages on the machine that executed our analyses. To install missing packages, use the below code:

```
packages <- read.csv(file.path('packages.csv'))[, -1]
base_packages <- as.data.frame(installed.packages()) 
to_install <- setdiff(packages$Package, base_packages$Package) 
install.packages(to_install)
```