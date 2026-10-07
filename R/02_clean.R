# 02_clean.R — Clean the SBA loan data and build the analysis variables
# Input:  data/raw/SBAnational.csv  (Kaggle, not tracked by Git)
# Output: data/clean/sba_clean.csv.gz

library(tidyverse)

raw <- read_csv("data/raw/SBAnational.csv",
                col_types = cols(.default = col_character()))

# Helper: turn "$60,000.00 " into 60000
parse_money <- function(x) parse_number(x)

# Industry names for the first two digits of NAICS
industry_names <- c(
  "11" = "Agriculture", "21" = "Mining & Oil", "22" = "Utilities",
  "23" = "Construction", "31" = "Manufacturing", "32" = "Manufacturing",
  "33" = "Manufacturing", "42" = "Wholesale Trade", "44" = "Retail Trade",
  "45" = "Retail Trade", "48" = "Transportation", "49" = "Transportation",
  "51" = "Information", "52" = "Finance & Insurance", "53" = "Real Estate",
  "54" = "Professional Services", "55" = "Management of Companies",
  "56" = "Admin & Support", "61" = "Education", "62" = "Health Care",
  "71" = "Arts & Recreation", "72" = "Accommodation & Food",
  "81" = "Other Services", "92" = "Public Administration"
)

sba <- raw %>%
  # 1. Keep loans with a known outcome
  filter(MIS_Status %in% c("P I F", "CHGOFF")) %>%
  # 2. Keep fiscal years 2001-2010:
  #    before 2001 industry and urban/rural codes are mostly missing,
  #    after 2010 many loans had not finished their term yet
  mutate(approval_fy = suppressWarnings(as.integer(ApprovalFY))) %>%
  filter(between(approval_fy, 2001, 2010)) %>%
  # 3. Build the analysis variables
  mutate(
    default        = if_else(MIS_Status == "CHGOFF", 1L, 0L),
    loan_amount    = parse_money(GrAppv),
    sba_amount     = parse_money(SBA_Appv),
    sba_portion    = sba_amount / loan_amount,
    log_loan       = log(loan_amount),
    term_months    = as.integer(Term),
    real_estate    = if_else(term_months >= 240, 1L, 0L),   # 20+ year loans are backed by real estate
    employees      = as.integer(NoEmp),
    new_business   = case_when(NewExist == "2" ~ 1L,
                               NewExist == "1" ~ 0L),        # 0 / blank -> NA
    area           = case_when(UrbanRural == "1" ~ "Urban",
                               UrbanRural == "2" ~ "Rural"), # 0 = undefined -> NA
    franchise      = if_else(FranchiseCode %in% c("0", "1"), 0L, 1L),
    revolving      = case_when(RevLineCr == "Y" ~ 1L,
                               RevLineCr == "N" ~ 0L),       # other codes -> NA
    low_doc        = case_when(LowDoc == "Y" ~ 1L,
                               LowDoc == "N" ~ 0L),
    industry       = unname(industry_names[str_sub(NAICS, 1, 2)]),
    nebraska       = if_else(State == "NE", 1L, 0L),
    state          = State
  ) %>%
  # 4. Drop rows missing the core variables
  filter(!is.na(new_business), !is.na(area), !is.na(industry),
         !is.na(state), loan_amount > 0, term_months > 0) %>%
  select(default, approval_fy, state, nebraska, industry, area,
         new_business, franchise, real_estate, term_months,
         loan_amount, log_loan, sba_portion, employees,
         revolving, low_doc)

# Quick checks
cat("Rows kept:", nrow(sba), "of", nrow(raw), "\n")
sba %>% summarise(default_rate = mean(default))
sba %>% group_by(nebraska) %>% summarise(n = n(), default_rate = mean(default))

write_csv(sba, "data/clean/sba_clean.csv.gz")
  
