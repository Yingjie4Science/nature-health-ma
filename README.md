![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=yingjie4science.nature-health-ma)

# Meta-analysis

## System requirements

- R version: 4.3.2

- Operating system: Windows 11 Pro

- No special hardware or proprietary software is required.


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
│   ├── included_papers_for_systematic_review.csv    # metadata for all 466 papers reviewed
│   ├── included_papers_for_MA.csv                   # metadata for 78 papers used in meta-analysis
│   └── data_*.rds                                   # various files used in final figures
│
├── code/
│   ├── fig_1a_1b_pattern.Rmd                        # Code for Fig. 1 (study distribution & tools)
│   ├── fig_2_3_effect_size.Rmd                      # Code for Fig. 2 and 3 (effect sizes)
│   ├── fig_ex1a_ex1b_tool_indicator.Rmd             # Extended Data Fig. 1
│   ├── fig_ex2_ex3_sm4_sig.Rmd                      # Extended Data Figs. 2–3 and Suppl. Fig. 4
│   ├── fig_sm2_publication_bias.Rmd                 # Funnel plot analysis
│   ├── fig_sm3_trim_fill.Rmd                        # Trim-and-fill publication bias analysis
│   ├── fig_sm5_sm6_sensitivity.Rmd                  # Random subset and leave-one-out sensitivity analysis
│   ├── fig_sm7_quality.Rmd                          # Quality assessment visualizations
│   └── func_*.R                                     # Various data processing functions
│      

```


## How to Run

You can replicate the main results and figures using the `.Rmd` files in the `code/` folder. 
Each script contains inline comments describing its purpose and required inputs. 


