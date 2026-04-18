# Rebuild the analytic dataset after downloading raw BRFSS XPT files
# into the repository root.
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tibble)
  library(purrr)
})

years <- 2020:2024

find_xpt <- function(y) {
  paths <- c(sprintf("LLCP%d.XPT ", y), sprintf("LLCP%d.XPT", y))
  for (p in paths) if (file.exists(p)) return(p)
  stop(sprintf("LLCP%d.XPT not found.", y))
}
files <- vapply(years, find_xpt, character(1))

canonical <- c("_MENT14D","MENTHLTH","_STATE","EXERANY2","SDLONELY",
               "EMTSUPRT","LSATISFY","GENHLTH","_AGEG5YR","_SEX",
               "_INCOMG1","EDUCA","_BMI5CAT","MEDCOST1","EMPLOY1",
               "MARITAL","_SMOKER3","DRNKANY6")

read_one <- function(path, year) {
  message(sprintf("Reading %s ...", path))
  raw <- read_xpt(path)
  message(sprintf("  -> %d rows, %d cols", nrow(raw), ncol(raw)))

  if ("_INCOMG" %in% names(raw) && !"_INCOMG1" %in% names(raw)) {
    raw[["_INCOMG1"]] <- raw[["_INCOMG"]]
  }
  if ("MEDCOST" %in% names(raw) && !"MEDCOST1" %in% names(raw)) {
    raw[["MEDCOST1"]] <- raw[["MEDCOST"]]
  }
  if ("DRNKANY5" %in% names(raw) && !"DRNKANY6" %in% names(raw)) {
    raw[["DRNKANY6"]] <- raw[["DRNKANY5"]]
  }

  present <- intersect(canonical, names(raw))
  d <- raw[, present]
  for (m in setdiff(canonical, present)) d[[m]] <- NA_real_
  d <- d[, canonical, drop = FALSE]
  names(d) <- sub("^_", "X_", names(d))
  d$year <- year
  d$income_scheme <- if (year == 2020) "5-level" else "7-level"
  d
}

raw_list <- map2(files, years, read_one)
all_raw <- bind_rows(raw_list)
message(sprintf("Combined raw: %d rows across %d years",
                nrow(all_raw), length(years)))

fips_lookup <- tibble(
  fips = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
           29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,
           54,55,56),
  state = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
            "Connecticut","Delaware","District of Columbia","Florida","Georgia",
            "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
            "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
            "New Jersey","New Mexico","New York","North Carolina","North Dakota",
            "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
            "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
            "Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
  abb = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL",
          "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
          "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD",
          "TN","TX","UT","VT","VA","WA","WV","WI","WY")
)

recode_factor <- function(x, codes, labels, missing = c(7, 9)) {
  x[x %in% missing] <- NA
  factor(x, levels = codes, labels = labels)
}

clean <- all_raw |>
  inner_join(fips_lookup, by = c("X_STATE" = "fips")) |>
  mutate(
    X_MENT14D = ifelse(X_MENT14D == 9, NA, X_MENT14D),
    fmd = X_MENT14D == 3,

    MENTHLTH = case_when(
      MENTHLTH == 88 ~ 0L,
      MENTHLTH %in% c(77, 99) ~ NA_integer_,
      TRUE ~ as.integer(MENTHLTH)
    ),

    exercise = recode_factor(EXERANY2, c(1, 2), c("Yes", "No")),

    loneliness = recode_factor(
      SDLONELY, 1:5,
      c("Always", "Usually", "Sometimes", "Rarely", "Never")
    ),

    support = recode_factor(
      EMTSUPRT, 1:5,
      c("Always", "Usually", "Sometimes", "Rarely", "Never")
    ),

    life_satisfy = recode_factor(
      LSATISFY, 1:4,
      c("Very satisfied", "Satisfied", "Dissatisfied", "Very dissatisfied")
    ),

    gen_health = recode_factor(
      GENHLTH, 1:5,
      c("Excellent", "Very good", "Good", "Fair", "Poor")
    ),

    age_group = recode_factor(
      X_AGEG5YR, 1:13,
      c("18-24","25-29","30-34","35-39","40-44","45-49","50-54",
        "55-59","60-64","65-69","70-74","75-79","80+"),
      missing = 14
    ),

    sex = recode_factor(X_SEX, 1:2, c("Male", "Female"), missing = integer(0)),

    income5 = case_when(
      income_scheme == "5-level" & X_INCOMG1 %in% 1:4 ~ as.integer(X_INCOMG1),
      income_scheme == "5-level" & X_INCOMG1 == 5 ~ 5L,
      income_scheme == "5-level" & X_INCOMG1 == 9 ~ NA_integer_,
      income_scheme == "7-level" & X_INCOMG1 %in% 1:4 ~ as.integer(X_INCOMG1),
      income_scheme == "7-level" & X_INCOMG1 %in% 5:7 ~ 5L,
      income_scheme == "7-level" & X_INCOMG1 == 9 ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    income = factor(income5, levels = 1:5,
                    labels = c("<$15k", "$15-25k", "$25-35k", "$35-50k", "$50k+")),

    education = recode_factor(
      EDUCA, 1:6,
      c("No school / K only", "Grades 1-8", "Some high school",
        "High school graduate", "Some college", "College graduate")
    ),

    bmi_cat = recode_factor(
      X_BMI5CAT, 1:4,
      c("Underweight", "Normal", "Overweight", "Obese"),
      missing = integer(0)
    ),

    med_cost = recode_factor(MEDCOST1, c(1, 2), c("Yes", "No")),

    employment = recode_factor(
      EMPLOY1, 1:8,
      c("Employed for wages", "Self-employed", "Out of work 1+ yr",
        "Out of work <1 yr", "Homemaker", "Student", "Retired",
        "Unable to work"),
      missing = 9
    ),

    marital = recode_factor(
      MARITAL, 1:6,
      c("Married", "Divorced", "Widowed", "Separated",
        "Never married", "Unmarried couple"),
      missing = 9
    ),

    smoker = recode_factor(
      X_SMOKER3, 1:4,
      c("Daily smoker", "Occasional smoker", "Former smoker", "Never smoker"),
      missing = 9
    ),

    drink_any = recode_factor(DRNKANY6, c(1, 2), c("Yes", "No"))
  ) |>
  select(year, state, abb, fips = X_STATE, fmd, mental_days = MENTHLTH,
         exercise, loneliness, support, life_satisfy, gen_health,
         age_group, sex, income, education, bmi_cat, med_cost,
         employment, marital, smoker, drink_any)

message(sprintf("Cleaned: %d rows (50 states + DC, all years)", nrow(clean)))
message(sprintf("  with valid FMD: %d", sum(!is.na(clean$fmd))))
message("  by year:")
print(table(clean$year, useNA = "ifany"))

saveRDS(clean, "brfss_subset.rds")
message("Wrote brfss_subset.rds (",
        round(file.info("brfss_subset.rds")$size / 1e6, 1), " MB)")
