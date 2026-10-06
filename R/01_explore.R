# 01_explore.R — First look at the SBA loan data
library(tidyverse)

# Load the raw data (read everything as text first to avoid parsing problems)
sba <- read_csv("data/raw/SBAnational.csv",
                col_types = cols(.default = col_character()))

# Size and columns
dim(sba)
glimpse(sba)

# Outcome variable: paid in full vs charged off
sba %>% count(MIS_Status)

# How many loans per state? Where does Nebraska rank?
state_counts <- sba %>% count(State, sort = TRUE)
state_counts %>% filter(State == "NE")

# Midwest option, in case Nebraska is too small
midwest <- c("NE", "IA", "KS", "MO", "SD", "ND", "MN", "WI", "IL", "IN", "MI", "OH")
sba %>% filter(State %in% midwest) %>% count()
