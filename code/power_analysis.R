# PM 513 Final Project - Part 1
# Michelle Navarrete Vega
# May 1, 2026

## ==== Libraries ====
library(readxl)
library(lme4)
library(lmerTest)
library(nlme)
library(tidyverse)
library(dplyr)

# ==== HYPOTHESES ====
#  H1 : The expected number of cigarettes smoked by each student is unaffected by the educational intervention; and
#  H2 : The expected number of cigarettes smoked by each student is unaffected by the support strategy.

# ==== Data ====
smoke_data_path <- "/Users/michellenavarretevega/Desktop/spring 2026/513/513-final-project/smokingdata.xlsx"

pilot1 <- read_excel(smoke_data_path, sheet = "Pilot 1")
# ID = student id
# RES = number of cigarretes smoked in prior week
# INV = educational intervention assigned 
    ## (1 = No presentation; 2 = Presentation)

head(pilot1)
#      ID   RES   INV
#   <dbl> <dbl> <dbl>
# 1     1     5     1
# 2     2     0     1
# 3     3     2     1
# 4     4    14     1
# 5     5     2     1
# 6     6     1     1

pilot2 <- read_excel(smoke_data_path, sheet = "Pilot 2")
# BLOCK = day strategy was assigned
# RES = number of cigarretes smoked in prior week
# TRT = support strategy assigned
    ## (1 = No support; 2 = Video; 3 = Meeting with an ex-smoker; 
            # 4 = Video plus Meeting with an ex-smoker)
n_students <- 10 # number of students


# ==== PROCESS ====

# starting with the whole plot (hypothesis 1)
## the whole plot error is sigma^2_ab
## a = # levels of the whole-plot factor (EDUC)
## b = # levels of the block factor (DIST)
## the F = MS_EDUC / MS_{DISTxEDUC}
## in the E(MS) table, the generalized MS_{BLOCKxWP} = sigma^2_e + nr*sigma^2_ab
## but that is for the raw observation level ^

# our parameters:
n_h <- 1 # homeroom per cell 
c_s <- 4 # support levels, = r
D <- 0 # placeholder for q = # districts/blocks
a <- 2 # education levels
## so -> MS_{DISTxEDUC} = sigma^2_e + 4*sigma^2_ab

## since we have homeroom means and not individual raw observation data
## we average CS / students and then adjust for the averaging
## sigma^_e -> sigma^_e / n_students
## 4*sigma^2_ab -> 4*sigma^2_ab / 4 -> sigma^2_ab
## so the MS_{BLOCKxWP} = sigma^_e / n_students + sigma^2_ab

# now how we are going to calculate the power
## F_crit <- qf(1 - alpha_level, df1, df2)
## for ^, df1 = a - 1 = 2 - 1 ||| a = # levels of the whole-plot factor (EDUC)
## df2 = (a - 1) (D - 1) = D - 1
## NCP = D x c_s x sum of effects squared / {sigma^_e / n_students + sigma^2_ab}

## power = P(F{df1, df2)NCP > F_crit})
## power <- pf(F_crit, df1, df2, ncp = ncp, lower.tail = FALSE)
## then we do an iterative process to find D
    # compute df2 by trying possible D
    # compute NCP
    # compute F_crit
    # compute power
    # stop when:
        ## power >= target power

# Power for the educational intervention is computed using a noncentral F distribution with numerator df = 1 and denominator df = D−1. The noncentrality parameter is determined by the intervention effect size and the whole-plot variance, which includes the district-by-intervention variance and the residual variance averaged across homerooms and students. The minimum number of districts is chosen as the smallest D that achieves the target power.

# ==== NEW ATTEMPT ====

#What we need: D x c_s x sum of effects squared / {sigma^2_e / n_students + sigma^2_ab}

model1 <- lm(RES ~ INV, data = pilot1) # educ with 2 factor levels
summary(model1)
anova(model1)
## sum of effects squared -> 2 factors so one effect is negative of the other
delta <- -3.17 
# those who did presentation did around 3 less cigs than those who did not have the presentation
alpha_educ <- delta/2
sum_alpha_educ <- alpha_educ^2 + alpha_educ^2


# now we need: sigma^2_e (variability after accounting for structure)
model2 <- lme(RES ~ TRT, random = ~1 | BLOCK, data = pilot2)
summary(model2)
#         (Intercept) Residual
# StdDev:    3.962027 3.698904
var_dist <- 3.962027^2 # 15.69766

## sigma^2_ab is not identifiable so we need to bound it
# interaction cannot vary more than the weaker main effect
# main effects = INV and DIST for this interaction
means_educ <- tapply(pilot1$RES, pilot1$INV, mean)
means_trt <- tapply(pilot2$RES, pilot2$TRT, mean)
sd_trt <- sqrt(mean((means_trt - mean(means_trt))^2))
sd_educ <- sqrt(mean((means_educ - mean(means_educ))^2))

sigma_ab <- 0.8 * min(sd_educ, sd_trt) 
var_ab <- sigma_ab^2 # 1.609566

# MS_{DISTxEDUC} =  (sigma^2_e / n_students) + sigma^2_ab
n_students <- 10
var_e_student <- 13.02   # from pilot 1 aov MSerror
var_e <- var_e_student / n_students  # = 1.302
ms_ab <- var_e + c_s * var_ab   # = 1.302 + 4 * var_ab

# === power calculation ====
    # compute df2 by trying possible D
    # compute NCP
    # compute F_crit
    # compute power
    # stop when:
        ## power >= target power


# ================================================
# H1: Educational Intervention (Whole Plot test)
# ================================================

alpha_level <- 0.05
target_power <- 0.90

results_h1 <- data.frame()
for (D in 2:50) {
  # degrees of freedom
  df_num <- a - 1              # = 1
  df_den <- (a - 1) * (D - 1)  # = D - 1
  # noncentrality parameter
  ncp <- (D * c_s * sum_alpha_educ) / ((a - 1) * ms_ab)
  # critical F value
  F_crit <- qf(1 - alpha_level, df_num, df_den)
  # power
  power <- pf(F_crit, df_num, df_den, ncp = ncp, lower.tail = FALSE)
  # store results
  results_h1 <- rbind(results_h1,
       data.frame(D = D,df_num = df_num, df_den = df_den, ncp = round(ncp, 3),
         F_crit = round(F_crit, 3), power = round(power, 3)))
  # stop when target reached
  if (power >= target_power) break}
results_h1 # D = 6 for both 80% and 7 for 90% power

# ================================================
# H2: Support Strategy (Subplot test)
# ================================================

summary(model2)
means_trt <- tapply(pilot2$RES, pilot2$TRT, mean)
grand_mean <- mean(pilot2$RES)
gamma_k <- means_trt - grand_mean
sum_gamma_trt <- sum(gamma_k^2)

target_power <- 0.80

results_h2 <- data.frame()
for (D in 2:50) {
  # degrees of freedom
  df_num <- c_s - 1              # = 3
  df_den <- a * (n_students * (D * c_s) - D - c_s + 1)  # = 2*(39D - 3)
  # noncentrality parameter
    ncp <- (a * D * sum_gamma_trt) / ((c_s - 1) * var_e)
  # critical F value
  F_crit <- qf(1 - alpha_level, df_num, df_den)
  # power
  power <- pf(F_crit, df_num, df_den, ncp = ncp, lower.tail = FALSE)
  # store results
  results_h2 <- rbind(results_h2,
       data.frame(D = D,df_num = df_num, df_den = df_den, ncp = round(ncp, 3),
         F_crit = round(F_crit, 3), power = round(power, 3)))
  # stop when target reached
  if (power >= target_power) break}
results_h2 # D = 3



