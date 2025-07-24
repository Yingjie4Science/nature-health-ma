[![Static Badge](https://img.shields.io/badge/Nature--Cities--doi-175E54)](https://doi.org/10.1038/s44284-025-00286-y)
![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=yingjie4science.nature-health-ma)


# Meta-analysis

> **Code and data for**:
> 
> Li, Y., Mao, Y., Mandle, L., Rydström, A., Remme, R., Lan, X., Wu, T., Song, C., Lu, Y., Nadeau, K., Meyer-Lindenberg, A., Daily, G., Guerry, A. Acute mental health benefits of urban nature. ***Nature Cities*** (2025). https://doi.org/10.1038/s44284-025-00286-y.
>
> Manuscript access through Stanford Digital Repository https://purl.stanford.edu/mb869ss2129
> 

## System Requirements

- R version: 4.3.2

- Operating system: Windows 11 Pro

- No special hardware or proprietary software is required.


## Quick Start / Installation
Instructions for getting started quickly, especially for non-R users.

* Clone the repository
If you're working from your Terminal or Git Bash, you can run:
```
git clone https://github.com/Yingjie4Science/nature-health-ma.git /path/to/your/target-folder
```

* Setting the Working Directory

This project is designed to be run directly from the project folder.

If you're using RStudio, opening the .Rproj file will automatically set the working directory.

If you are working manually, please set the working directory first:
```
setwd("/path/to/your/cloned/project")
```

* Load project environment

To set up the project environment, you can manually install all the necessary ***Required R Packages*** (see details in the next section), or

Use `renv` to automatically restore the environment:
```
# Install renv if not already installed
install.packages("renv")

# Restore the project-specific environment
renv::restore()
```

***Tip**: If you encounter errors restoring packages like `MASS` (base/recommended packages), 
set the following option before running* `renv::restore()`:
```
options(renv.config.ignore.installed.packages = TRUE)
renv::restore()
```

## Required R Packages

The following packages are required to run the analysis. 
Please install them prior to running the code using install.packages() or via a package manager like renv. 
Version numbers used in our analysis are shown below for reproducibility:

```         
library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)

library(meta)     # 7.0.0
library(metafor)  # 4.4.0
library(dmetar)   # 0.1.0; # for InfluenceAnalysis()
library(rstatix)  # 0.7.2; # for dunn_test(); wilcox_test(); add_xy_position()

library(ggplot2)
library(cowplot)
library(ggpubr)
library(rcartocolor)
library(RColorBrewer)
```


## Directory Structure

```
├── data/
│   ├── included_papers_for_systematic_review_SM.csv   # metadata for all 449 papers reviewed
│   ├── included_papers_for_MA_SM.csv                  # metadata for 78 papers used in meta-analysis
│   └── data_*.rds                                     # various data files used in generating final figures
│
├── code/
│   ├── fig_1a_1b_pattern.Rmd                          # Code for Fig. 1 (study distribution & tools)
│   ├── fig_2_3_effect_size.Rmd                        # Code for Fig. 2 and 3 (effect sizes)
│   ├── fig_ex1a_ex1b_tool_indicator.Rmd               # Extended Data Fig. 1
│   ├── fig_ex2_ex3_sm4_sig.Rmd                        # Extended Data Figs. 2–3 and Suppl. Fig. 4
│   ├── fig_sm2_publication_bias.Rmd                   # Funnel plot analysis
│   ├── fig_sm3_trim_fill.Rmd                          # Trim-and-fill publication bias analysis
│   ├── fig_sm5_sm6_sensitivity.Rmd                    # Random subset and leave-one-out sensitivity analysis
│   ├── fig_sm7_quality.Rmd                            # Quality assessment visualizations
│   └── func_*.R                                       # Various data processing functions
│      

```


## How to Run

You can replicate the main results and figures using the `.Rmd` files in the `code/` folder. 
Each script contains inline comments describing its purpose and required inputs. 


