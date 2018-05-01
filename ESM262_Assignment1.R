## ---------- ESM 262 -------------------------------- ##
## ---------- Data Wrangling - Assignment 1 ---------- ##
## ---------- Celine Mol ----------------------------- ##

## ---------- Step 1. Import Tidy -------------------- ##

## 1.1 Read the gazetteer data as-is (all columns; no type conversion) into a gaz_raw tibble

library(tidyverse)

gaz_raw <- read_delim("CA_Features_20180401.txt", delim = "|", col_names = TRUE)

# # 1.2 Copy only the following columns into a gaz tibble (you can rename them if you like):
# feature ID
# feature name
# feature class
# state alpha
# county name
# primary latitude (decimal)
# primary longitude (decimal)
# source latitude (decimal)
# source longitude (decimal)
# elevation in meters
# map name
# date created
# date edited

gaz <- gaz_raw %>% 
  select(FEATURE_ID, FEATURE_NAME, FEATURE_CLASS, STATE_ALPHA, COUNTY_NAME, PRIM_LAT_DEC, 
         PRIM_LONG_DEC, SOURCE_LAT_DEC, SOURCE_LONG_DEC, ELEV_IN_M, MAP_NAME, DATE_CREATED,
         DATE_EDITED)

## 1.3 Convert the gaz columns to the appropriate type. 
## Convert any placeholders for unknown data to NA

spec(gaz)

gaz <- type_convert(gaz, col_types = 
               cols (
                 DATE_CREATED = col_date(format = "%m/%d/%Y"),
                 DATE_EDITED = col_date(format = "%m/%d/%Y")
               ))


## 1.4 Delete from gaz rows where:
# the primary latitude or longitude are unknown
# the feature is not in California

sum(is.na(gaz$PRIM_LAT_DEC))
sum(is.na(gaz$PRIM_LONG_DEC))
# None of the primary lattude or longitude rows are unknown, so we can ignore deleting these

gaz <- gaz %>%
  filter(STATE_ALPHA == "CA")

## 1.5 Write the gaz tibble to a CSV file (using "|" as a delimiter).

write_delim(gaz, "gaz", delim = "|", na = "NA")

## ---------- Step 2. Analyze ------------------------ ##

## 2.1 What is the most-frequently-occuring feature name?

gaz$FEATURE_NAME <- as.factor(gaz$FEATURE_NAME)

frequent_name <- gaz %>%
  count(FEATURE_NAME) %>% 
  filter(n == max(n))

paste0("The most frequently occuring feature name is ", frequent_name$FEATURE_NAME)

## 2.2 What is the least-frequently-occuring feature class?

frequent_class <- gaz %>% 
  count(FEATURE_CLASS) %>% 
  filter(n == min(frequent_class$n))

paste0("The least frequently occuring feature classes are ", frequent_class$FEATURE_CLASS[1],
       " and ", frequent_class$FEATURE_CLASS[2])


## 2.3 What is the approximate center point of each county?
# Hint: Calculate the center of the bounding box of the county’s point features.


## 2.4 What are the fractions of the total number of features in each county that are natural? 
## man-made?
# Hint: Construct a tibble with two columns, one containing all possible feature classes 
# (see “Feature Class Definitions”), and another containing the string “natural” or “man-made”, 
# which you assign (it’s pretty obvious.) Then join this tibble to the gazetteer tibble.
library(stringr)

classes <- read_csv("class_definitions.csv")
colnames(classes) <- c("FEATURE_CLASS", "CLASS_DESCRIPTION")

gazetteer <- left_join(gaz, classes, by = "FEATURE_CLASS")

test <- gazetteer %>% 
  group_by(COUNTY_NAME) %>% 
  #mutate(natural = add_tally(CLASS_DESCRIPTION == "natural"))
            #manmade = add_tally(CLASS_DESCRIPTION == "manmade"))
  count(CLASS_DESCRIPTION) %>% 
  mutate(total = n())
  

