#> -------------------------------------------
#> POWER ANALYSES IN MOTOR BEHAVIOUR PROJECT
#> -- McKay, Corson, Vinh, Jeyarajan, Tandon, Brooks,
#> Hubley, and Carter
#>
#> Simulations
#>
#> Authors:
#>   Brad McKay
#>
#> Last update: June 26 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------


#> SCRIPT SETUP ----
#>
#> Default parameters used in each power simulation
set.seed(3) # for reproducible simulations
n1 <- 450
n2 <- 50
B <- 10000


#> POWER SIMULATIONS ----
#>
#> Based on proportions reported by Fanelli (2010)
#>
#> 90% power
sim90_pvalues <- numeric(B)

sim90_x1 <- rbinom(B, n1, 0.915)
sim90_x2 <- rbinom(B, n2, 0.75)

for (i in seq_len(B)) {
  sim90_pvalues[i] <- prop.test(
    x = c(sim90_x1[i], sim90_x2[i]),
    n = c(n1, n2),
    correct = FALSE,
    alternative = "two.sided",
    conf.level = .95
  )$p.value
}

#> Calculate power
mean(sim90_pvalues <= 0.05)


#> 80% power
sim80_pvalues <- numeric(B)

sim80_x1 <- rbinom(B, n1, 0.915)
sim80_x2 <- rbinom(B, n2, 0.777)

for (i in seq_len(B)) {
  sim80_pvalues[i] <- prop.test(
    x = c(sim80_x1[i], sim80_x2[i]),
    n = c(n1, n2),
    correct = FALSE,
    alternative = "two.sided",
    conf.level = .95
  )$p.value
}

#> Calculate power
mean(sim80_pvalues <= 0.05)


#> Smallest effect size of interest (or SESOI)
simSESOI_pvalues <- numeric(B)

simSESOI_x1 <- rbinom(B, n1, 0.915)
simSESOI_x2 <- rbinom(B, n2, 0.855)

for (i in seq_len(B)) {
  simSESOI_pvalues[i] <- prop.test(
    x = c(simSESOI_x1[i], simSESOI_x2[i]),
    n = c(n1, n2),
    correct = FALSE,
    alternative = "two.sided",
    conf.level = .95
  )$p.value
}

#> Calculate power
mean(simSESOI_pvalues <= 0.05)
