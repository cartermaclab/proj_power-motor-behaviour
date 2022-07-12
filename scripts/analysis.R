#> -------------------------------------------
#> POWER ANALYSES IN MOTOR BEHAVIOUR PROJECT
#> -- McKay, Corson, Vinh, Jeyarajan, Tandon, Brooks,
#> Hubley, and Carter
#>
#> Data analysis
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
source("scripts/wrangle.R")


#> ANALYSIS ----
#>
#> Test whether positive result rate is different between studies
#> with and without power analyses
prop_positivity_pwr_analysis <- prop.test(
  c(n_pwr_no_support_yes, n_pwr_yes_support_yes),
  c(n_studies_pwr_no, n_studies_pwr_yes)
)
prop_positivity_pwr_analysis

#> Calculate 95% confidence intervals for positive result rates in
#> both categories
binom_pwr_no_support_yes <- binom.test(
  x = n_pwr_no_support_yes,
  n = n_studies_pwr_no
)
min(binom_pwr_no_support_yes$conf.int)
max(binom_pwr_no_support_yes$conf.int)

binom_pwr_yes_support_yes <- binom.test(
  x = n_pwr_yes_support_yes,
  n = n_studies_pwr_yes
)
min(binom_pwr_yes_support_yes$conf.int)
max(binom_pwr_yes_support_yes$conf.int)

#> Test for equivalence against the smallest effect of interest (SESOI)
#> Borrowing from Scheel et al. (2021), we have selected a difference
#> in positivity rates of 6% as our SESOI.
#> Scheel et al. based their choice of SESOI on the difference between
#> positivity rates overall and the lowest positivity rates observed
#> within a sub-field of psychology as reported by Fanelli (2010)
sesoi <- .06

tost_pwr_support <- TOSTER::TOSTtwo.prop(
  prop1 = prop_support_yes_pwr_no,
  prop2 = prop_support_yes_pwr_yes,
  n1 = n_studies_pwr_no,
  n2 = n_studies_pwr_yes,
  low_eqbound = -sesoi,
  high_eqbound = sesoi,
  alpha = 0.05,
  verbose = TRUE
)


#> Test for difference in positivity rates based on effect
#> size justification
prop_es_justification <- prop.test(support_vector, total_vector)
prop_es_justification


#> Test for difference in positivity rates based on target power
prop_target_pwr <- prop.test(support_pwr_vector, total_pwr_vector)
prop_target_pwr


#> Test for difference in sample size between studies with and
#> without a power analysis
sample_diff <- t.test(
  sample ~ pwranal_fct,
  included_long,
  na.rm = TRUE
)
sample_diff

#> Calculate the median sample size for studies with and
#> without power analyses
aggregate(included_long$sample, list(included_long$pwranal_fct),
          FUN = median, na.rm = TRUE)


#> Test for difference in sample sizes of positive and negative results
sample_positivity_diff <- t.test(
  sample ~ support_fct,
  included_long,
  na.rm = TRUE
)
sample_positivity_diff

#> Calculate the median sample size for studies with and
#> without positive results
aggregate(included_long$sample, list(included_long$support_fct),
          FUN = median, na.rm = TRUE)


#> Run a shift function on the sample size data since they
#> were highly skewed
#> Create an object suitable for shift function
sample_size_sf <- included_long %>%
  tidyr::pivot_wider(
    names_from = pwranal_fct,
    values_from = sample
  )

no_pwr <- na.omit(sample_size_sf$No)
yes_pwr <- na.omit(sample_size_sf$Yes)

shift <- rogme::mkt2(no_pwr, yes_pwr)
shift$gr <- as.factor(shift$gr)

#> Create a scatter plot of the data then compute and plot
#> the shift function
#> Does not appear in manuscript so have placed the figure generation
#> within the analysis script instead
ps <- rogme::plot_scat2(
  data = shift,
  formula = obs ~ gr,
  xlabel = "",
  ylabel = "Sample (per group)",
  alpha = 1,
  shape = 21,
  colour = "grey10",
  fill = "grey90") +
  coord_flip()
ps

sf <- rogme::shifthd(
  data = shift,
  formula = obs ~ gr,
  nboot = 200)

psf <- rogme::plot_sf(sf, plot_theme = 2)

psf <- rogme::add_sf_lab(
  psf, sf,
  y_lab_nudge = .1,
  text_size = 4)

#> Change axis labels
psf[[1]] <- psf[[1]] +
  labs(x = "No power analysis (n)",
       y = "No power analysis - power analysis \nquantile differences (n)")

#> View shift function plot
psf[[1]]


#> Test for equivalence on sample size difference
#> NOTE. This is unnecessary given large difference between group means
#> Uncomment to run
# sample_size_diff_tib <- included_long %>%
#   dplyr::filter(pwranal == "Yes" | pwranal == "No")

# tost_sample_size_diff <- TOSTER::dataTOSTtwo(
#   data = sample_size_diff_tib,
#   deps = "sample",
#   group = "pwranal_fct",
#   var_equal = FALSE,
#   low_eqbound = -4,
#   high_eqbound = 4,
#   eqbound_type = "raw",
#   alpha = .05,
#   plots = TRUE
# )
# tost_sample_size_diff
