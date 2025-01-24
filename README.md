# Temporal Communication Dynamics

## Overview
This repository contains the data, code, and figures for the research article:

Flores, P. M. & Hilbert, M. (2023). Temporal communication dynamics in the aftermath of large-scale upheavals: do digital footprints reveal a stage model? *Journal of Computational Social Science*. https://doi.org/10.1007/s42001-023-00218-7

## Repository Contents
- `data/`: Datasets for LA, Mexico, and Turkey cases
- `Supplemental Material.Rmd`: R Markdown source file for analysis replication
- `Supplemental-Material.md`: Markdown file for better Github visualization
- `renv.lock`: Package dependencies and versions used in the analysis

## Getting Started
1. Clone this repository
2. Open the project in RStudio
3. Install package management dependencies:
   ```r
   install.packages("renv", repos="https://cloud.r-project.org")
   renv::restore()
   ```
4. Open and run `Supplemental Material.Rmd` to replicate the analysis
   - The RMD file allows full replication of the analysis

## Dependencies
This project uses `renv` for package management to ensure reproducibility. All necessary packages and their versions are specified in `renv.lock` and will be automatically installed when running `renv::restore()`.

## Required Packages
Key packages used in this analysis:
- `here`: File path management
- `ggplot2`: Data visualization
- `dplyr`: Data manipulation
- `tseries`: Time series analysis
- `changepoint`: Change point detection
- And others (full list managed via `renv`)

## Contact
For questions about the code or paper, please contact the corresponding author Pablo M. Flores (https://pablomflores.com).

## License
This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png

You are free to:
- Share: copy and redistribute the material in any medium or format
- Adapt: remix, transform, and build upon the material for any purpose, even commercially

Under the following terms:
- Attribution: You must give appropriate credit, provide a link to the license, and indicate if changes were made.
