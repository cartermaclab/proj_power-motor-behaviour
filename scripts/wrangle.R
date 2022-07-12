#> -------------------------------------------
#> POWER ANALYSES IN MOTOR BEHAVIOUR PROJECT
#> -- McKay, Corson, Vinh, Jeyarajan, Tandon, Brooks,
#> Hubley, and Carter
#>
#> Data wrangling
#>
#> Authors:
#>   Brad McKay
#>   Mike Carter
#>
#> Last update: June 26 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------


#> SCRIPT SETUP ----
#>
#> Load required libraries
source("scripts/libraries.R")

#> Read in data
data <- read.csv(
  "data/data.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


#> WRANGLE ----
#>
#> Convert to tibble and select only the consensus review
consensus_data <- data %>%
  dplyr::as_tibble() %>%
  dplyr::filter(Reviewer.Name == "Consensus")


#> Select required columns for primary analysis
consensus_data <- consensus_data %>%
  dplyr::select(
    'Covidence',
    'included',
    'exp1_has_pwr_analysis',
    'exp1_support',
    'exp1_sample_size',
    'exp1_effect_justification',
    'exp1_power_estimate',
    'exp2_has_pwr_analysis',
    'exp2_support',
    'exp2_sample_size',
    'exp2_effect_justification',
    'exp2_pwr_estimate',
    'exp3_has_pwr_analysis',
    'exp3_support',
    'exp3_sample_size',
    'exp3_effect_justification',
    'exp3_pwr_estimate',
    'exp4_has_pwr_analysis',
    'exp4_support',
    'exp4_sample_size',
    'exp4_effect_justification',
    'exp4_pwr_estimate'
  )

#> Count included and excluded studies and get other descriptives
dplyr::count(consensus_data, included)

#> Select only included studies
included_studies <- consensus_data %>%
  dplyr::filter(included == "Yes") %>%
  dplyr::select(-included)

included_long <- reshape(
  included_studies,
  varying = list(
    pwranal = c(2, 7, 12, 17),
    support = c(3, 8, 13, 18),
    sample = c(4, 9, 14, 19),
    just = c(5, 10, 15, 20),
    pwr = c(6, 11, 16, 21)
  ),
  v.names = c("pwranal",
              "support",
              "sample",
              "just",
              "pwr"),
  direction = ("long"),
  times = 1:4,
  timevar = "times",
  idvar = rep(1:2)
)

#> Count data for power analyses
#>
#> See a tibble with all values
dplyr::count(included_long, pwranal)

n_studies_pwr_yes <- dplyr::count(
  included_long, pwranal) %>%
  dplyr::filter(pwranal == "Yes") %>%
  pull(n)

n_studies_pwr_no <- dplyr::count(
  included_long, pwranal) %>%
  dplyr::filter(pwranal == "No") %>%
  pull(n)

#> Calculate proportion of studies with power analyses
prop_pwr <- n_studies_pwr_yes / (n_studies_pwr_yes + n_studies_pwr_no)


#> Count data for power analyses and support for main hypothesis
#>
#> See a tibble with all values
dplyr::count(included_long, pwranal, support)

n_pwr_yes_support_yes <- dplyr::count(
  included_long, pwranal, support) %>%
  dplyr::filter(pwranal == "Yes" & support == "Yes") %>%
  pull(n)

n_pwr_no_support_yes <- dplyr::count(
  included_long, pwranal, support) %>%
  dplyr::filter(pwranal == "No" & support == "Yes") %>%
  pull(n)

#> Calculate proportion of studies with and without
#> power analyses finding support
prop_support_yes_pwr_yes <- n_pwr_yes_support_yes / n_studies_pwr_yes
prop_support_yes_pwr_no <- n_pwr_no_support_yes / n_studies_pwr_no


#> Count data for power analyses based on each justification
#>
#> Filter for only studies with a power analysis
pwr_analysis <- included_long %>%
  tidyr::drop_na() %>%
  dplyr::filter(pwranal == "Yes")

#> Count data for each justification for studies with a reported
#> power analysis and create new column with proportion
#> Only four justifications were encountered:
#> 1. benchmarks, 2. no_explanation,
#> 3. pilot, and 4. previous study
#> ignore all others in script as they will be 0s
justification_pwr <- dplyr::count(pwr_analysis, just) %>%
  dplyr::mutate(
    prop = n / n_studies_pwr_yes
  )

#> Count data for each power target and add a new column
#> with the proportion for that target
target_pwr <- dplyr::count(pwr_analysis, pwr) %>%
  dplyr::mutate(
    prop = n / n_studies_pwr_yes
  )

#> Add some columns to pwr_analysis tibble that will be needed
#> for proportion tests in analysis script
#> First need to add a column converting to 1s and 0s
pwr_analysis <- pwr_analysis %>%
  dplyr::mutate(
    support_num = dplyr::if_else(support == "Yes", 1, 0),
    pwranal_num = dplyr::if_else(pwranal == "Yes", 1, 0)
  )

support_vector <- tapply(
  pwr_analysis$support_num,
  pwr_analysis$just,
  sum
)

total_vector <- tapply(
  pwr_analysis$pwranal_num,
  pwr_analysis$just,
  sum
)

#> Add columns to included_long that are copies of other columns
#> making some <fct> type and others <dbl>
#> a <fct> rather than <chr>
included_long <- included_long %>%
  dplyr::mutate(pwranal_fct = pwranal) %>%
  dplyr::mutate(pwranal_fct = forcats::as_factor(pwranal_fct)) %>%
  dplyr::mutate(dplyr::across(sample, as.numeric)) %>%
  dplyr::mutate(support_fct = support) %>%
  dplyr::mutate(support_fct = forcats::as_factor(support_fct))


#> Add a column to pwr_analysis that is a copy of pwr column but is
#> a <fct> rather than <chr>
pwr_analysis <- pwr_analysis %>%
  dplyr::mutate(pwr_fct = pwr) %>%
  dplyr::mutate(pwr_fct = forcats::as_factor(pwr_fct))

support_pwr_vector <- tapply(
  pwr_analysis$support_num,
  pwr_analysis$pwr_fct,
  sum
)

total_pwr_vector <- tapply(
  pwr_analysis$pwranal_num,
  pwr_analysis$pwr_fct,
  sum
)
