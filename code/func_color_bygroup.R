
##' Define the colors of nature types
##'   add named colors to each category to keep the color consistent 

library(rcartocolor)
library(RColorBrewer)
# display_carto_all(colorblind_friendly = TRUE)



## https://personal.sron.nl/~pault/
color_bright <- c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
## Figure 1: Bright qualitative colour scheme that is colour-blind safe. The main scheme for lines and their labels.


## nature_type ---------------------------------------------------------------------------

##' color from https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2021_REL_NLCD#bands
nature_type_list <- c("Greenspace",               # '#68ab5f'	(Deciduous Forest)
                      "Greenspace - Forest",      # '#1c5f2c'	
                      "Greenspace - Shrub/scrub", # '#ccb879'	
                      "Greenspace - Grassland",   # '#dfdfc2'
                      "Greenspace - Farmland",    # '#ab6c28'
                      "Greenspace - Park",        # 
                      "Greenspace - Garden",      # '#aaff00'
                      "Greenspace - Street trees",   # was named "Greenspace - Corridors"
                      "Greenspace - Green roof/wall",# '#79ffd2'              (Shrub-Forest)
                      
                      "Bluespace - Open water",      # '#466b9f'
                      "Bluespace - Wetland",         # '#b8d9eb'
                      "Bluespace - Sea",             # '#0000ff'
                      "Bluespace - Beach/coastline", # '#0000ff'
                      "Bluespace",
                      "Overall",                     # '#000000'
                      "Other"                        # '#000000'	
)
nature_type_color <- c(
  '#68ab5f', # "Greenspace"
  '#1c5f2c', # "Greenspace - Forest"
  '#ccb879', # "Greenspace - Shrub/scrub"
  '#b2df8a', # "Greenspace - Grassland"
  '#ab6c28', # "Greenspace - Farmland"
  '#fb9a99', # "Greenspace - Park"
  '#f0c571', # "Greenspace - Garden" (forest disturbed before/at 2006)
  '#beaed4', # "Greenspace - Street trees"
  '#79ffd2', # "Greenspace - Green roof/wall"
  
  '#466b9f', # "Bluespace - Open water"
  '#b8d9eb', # "Bluespace - Wetland"
  '#4169E1', # "Bluespace - Sea"
  '#0096FF', # "Bluespace - Beach/coastline"
  '#6495ED', # "Bluespace"
  '#000000', # "Overall"
  '#000000'  # "Other"
)

nature_type_shape <- c(
  19, # "Greenspace"
  19, # "Greenspace - Forest"
  19, # "Greenspace - Shrub/scrub"
  19, # "Greenspace - Grassland"
  19, # "Greenspace - Farmland"
  10, # "Greenspace - Park"       # shape = 10, circle plus
  19, # "Greenspace - Garden"
  19, # "Greenspace - Corridors"
  19, # "Greenspace - Green roof/wall"
  
  19, # "Bluespace - Open water"
  19, # "Bluespace - Wetland"
  19, # "Bluespace - Sea"
  19, # "Bluespace - Beach/coastline"
  19, # "Bluespace"
  19, # "Overall"
  19  # "Other"
)
names(nature_type_color) <- nature_type_list



## exposure_group ------------------------------------------------------------------------
exposure_group <- c("L4 - physical activity in nature", 
                    "L4 - stay static in nature")
exposure_group_color <- NULL

## age_group -----------------------------------------------------------------------------

age_group_list <- c(
  'Adolescents', 'Young adults', 'Adults', 'Older adults'
  )

age_group_color <- rev(c(
  # '#009E73',
  # '#0072B2', 
  # '#D55E00', 
  # '#CC79A7'
  '#d35f27',
  '#e69f25',
  '#0773b2',
  '#5cb4e4'
))

# age_group_color <- c(
#   '#003f5c',
#   '#7a5195',
#   '#ef5675',
#   '#ffa600'
# )
names(age_group_color) <- age_group_list



## gender_group  -------------------------------------------------------------------------
gender_group_list <- c(
  "Female > 60%", 
  'Gender Balance', 
  "Male > 60%")


gender_group_color <- c(
  '#d73027',
  'gray60',
  '#4575b4'
)

names(gender_group_color) <- gender_group_list

## duration_group  -----------------------------------------------------------------------

duration_group_list <- c('<= 15', '16-45', '>45')
duration_group_color <- c(
  '#c2e699',
  '#78c679',
  '#238443'
)
names(duration_group_color) <- duration_group_list


## region --------------------------------------------------------------------------------
region_list <- c(
  'Africa', "Asia", "Europe", "North America", "South America", "Oceania"
)

# region_color <- brewer.pal(n = length(region_list), "Set1")
# region_color <- rcartocolor::carto_pal(n = length(region_list), name = "Safe")
region_color <- color_bright[1:length(region_list)]
names(region_color) <- region_list
  

## Function ==============================================================================
func_color_bygroup <- function(df, column_name, color_pal = NULL) {
  df <- as.data.frame(df) %>%
    # dplyr::mutate(!!column_name := factor(!!sym(column_name)), levels = age_group_list) %>%
    as.data.frame()
  
  element_list <- unique(df[,column_name]); element_list
  
  if (is.null(color_pal)) {
    color_bygroup <- brewer.pal(n = length(element_list), "Dark2")
  } else {
    color_bygroup <- color_pal[element_list]; 
  }
  
  
  
  
  ## return the nature types with assigned colors
  return(color_bygroup)
  
}

