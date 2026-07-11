# PM 513 Final Project - Part 2
# Michelle Navarrete Vega
# May 11, 2026

## ==== Libraries ====
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)



# ==== HYPOTHESES ====
#  H1 : The expected number of cigarettes smoked by each student is unaffected by the educational intervention; and
#  H2 : The expected number of cigarettes smoked by each student is unaffected by the support strategy.

# ==== Data: definitive trial ====
smoke_data_path <- "/Users/michellenavarretevega/Desktop/spring 2026/513/513-final-project/513_final_data.xlsx"

smoke_data <- read_excel(smoke_data_path)

# ==== Variables ====

# DN = District Number

# EDUC = = educational intervention assigned 
    ## (1 = No presentation; 2 = Presentation)

# SUPP = support strategy assigned
    ## (1 = No support; 2 = Video; 3 = Meeting with an ex-smoker; 
            # 4 = Video plus Meeting with an ex-smoker)

# CS = total number of cigs smoked

# STUDENTS = Number of students in the EDUC/SUPP group

# Create the rate variable -> cigs per student
smoke_data$rate <- smoke_data$CS / smoke_data$STUDENTS
str(smoke_data)


# Factor the treatment variables
smoke_data$EDUC <- factor(smoke_data$EDUC, levels = c(1, 2),
                   labels = c("No Presentation", "Presentation"))
smoke_data$SUPP <- factor(smoke_data$SUPP, levels = c(1, 2, 3, 4),
                   labels = c("None", "Video", "Ex-Smoker", "Video + Ex-Smoker"))
smoke_data$DN <- factor(smoke_data$DN)
unique(smoke_data$DN) 
# quick look at the data
head(smoke_data)
#   DN    EDUC            SUPP                 CS STUDENTS  rate
# 1 1     No Presentation None                 59       11  5.36
# 2 1     No Presentation Video                72       10  7.2 
# 3 1     No Presentation Ex-Smoker            35       10  3.5 
# 4 1     No Presentation Video + Ex-Smoker    21       11  1.91
# 5 1     Presentation    None                 55        9  6.11
# 6 1     Presentation    Video                56       10  5.6 

summary(smoke_data)

smoke_data %>% count(DN)

educ_summary <- smoke_data %>%
  group_by(DN, EDUC) %>%
  summarise(mean_rate = mean(rate), .groups = "drop")

# Confirm: should be 22 rows (11 districts x 2 EDUC levels)
nrow(educ_summary)


# ================================================
# ── EDA! ────────────────
# ================================================

# ---- 1. Descriptive statistics by treatment group ----

# By educational intervention
smoke_data %>%
  group_by(EDUC) %>%
  summarise(
    n      = n(),
    mean   = round(mean(rate), 3),
    sd     = round(sd(rate), 3),
    median = round(median(rate), 3),
    min    = round(min(rate), 3),
    max    = round(max(rate), 3)
  )
#   EDUC                n  mean    sd median   min   max
# 1 No Presentation    44  4.33  2.02   3.74 0.7     8.7
# 2 Presentation       44  3.33  1.85   2.71 0.364   7  

# By support strategy
smoke_data %>%
  group_by(SUPP) %>%
  summarise(
    n      = n(),
    mean   = round(mean(rate), 3),
    sd     = round(sd(rate), 3),
    median = round(median(rate), 3),
    min    = round(min(rate), 3),
    max    = round(max(rate), 3)
  )

#   SUPP                  n  mean    sd median   min   max
# 1 None                 22  4.77  1.84   4.95 1.17   7.9 
# 2 Video                22  5.34  1.92   5.58 1.75   8.7 
# 3 Ex-Smoker            22  2.72  1.07   2.58 0.364  4.46
# 4 Video + Ex-Smoker    22  2.49  1.3    2.21 0.7    5.56



# Cell means: EDUC x SUPP
smoke_data %>%
  group_by(EDUC, SUPP) %>%
  summarise(
    n    = n(),
    mean = round(mean(rate), 3),
    sd   = round(sd(rate), 3),
    .groups = "drop"
  )

#   EDUC            SUPP                  n  mean    sd
#   <fct>           <fct>             <int> <dbl> <dbl>
# 1 No Presentation None                 11  5.24 1.88 
# 2 No Presentation Video                11  5.94 1.94 
# 3 No Presentation Ex-Smoker            11  3.13 0.933
# 4 No Presentation Video + Ex-Smoker    11  3.00 1.41 
# 5 Presentation    None                 11  4.29 1.75 
# 6 Presentation    Video                11  4.75 1.77 
# 7 Presentation    Ex-Smoker            11  2.31 1.07 
# 8 Presentation    Video + Ex-Smoker    11  1.97 0.989


library(knitr)

desc_table <- data.frame(
  Support = c("None", "Video", "Ex-Smoker", 
              "Video + Ex-Smoker", "Marginal Mean"),
  No_Presentation = c("5.24 (1.88)", "5.94 (1.94)", "3.13 (0.93)",
                      "3.00 (1.41)", "4.33"),
  Presentation    = c("4.29 (1.75)", "4.75 (1.77)", "2.31 (1.07)",
                      "1.97 (0.99)", "3.33"),
  Marginal_Mean   = c("4.77", "5.34", "2.72", "2.49", "")
)

kable(desc_table,
      col.names = c("Support Strategy", 
                    "No Presentation", 
                    "Presentation", 
                    "Marginal Mean"),
      caption   = "Table 1. Mean (SD) average cigarettes smoked per student 
                   by educational intervention and support strategy.")

# ---- 2. Boxplot by educational intervention ----

p1 <- ggplot(smoke_data, aes(x = EDUC, y = rate, fill = EDUC)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  labs(
    title = "Avg. Cigarettes per Student by Educational Intervention",
    x     = "Educational Intervention",
    y     = "Avg. Cigarettes per Student"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("boxplot_educ.png", plot = p1, width = 6, height = 5, dpi = 300)

# ---- 3. Boxplot by support strategy ----

p2 <- ggplot(smoke_data, aes(x = SUPP, y = rate, fill = SUPP)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  labs(
    title = "Avg. Cigarettes per Student by Support Strategy",
    x     = "Support Strategy",
    y     = "Avg. Cigarettes per Student"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("boxplot_supp.png", plot = p2, width = 6, height = 5, dpi = 300)

# ---- 4. Boxplot by EDUC x SUPP cell ----

p3 <- ggplot(smoke_data, aes(x = SUPP, y = rate, fill = EDUC)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  labs(
    title = "Avg. Cigarettes per Student by Intervention and Support Strategy",
    x     = "Support Strategy",
    y     = "Avg. Cigarettes per Student",
    fill  = "Educational Intervention"
  ) +
  theme_bw()

ggsave("boxplot_educ_by_supp.png", plot = p3, width = 10, height = 5, dpi = 300)

# ---- 5. Interaction plot (cell means, lines connect SUPP levels) ----

cell_means <- smoke_data %>%
  group_by(EDUC, SUPP) %>%
  summarise(mean_rate = mean(rate), .groups = "drop")

# Individual district lines (light) + cell mean lines (bold)
p4 <- ggplot() +
  # individual district trajectories sn shows variability across blocks
  geom_line(data = smoke_data,
            aes(x = SUPP, y = rate, group = interaction(DN, EDUC),
                color = EDUC),
            alpha = 0.3, linewidth = 0.6) +
  # cell means on top
  geom_line(data = cell_means,
            aes(x = SUPP, y = mean_rate, group = EDUC, color = EDUC),
            linewidth = 1.4) +
  geom_point(data = cell_means,
             aes(x = SUPP, y = mean_rate, color = EDUC),
             size = 3) +
  labs(
    title    = "Interaction Plot: Educational Intervention × Support Strategy",
    subtitle = "Light lines = individual districts; bold lines = cell means",
    x        = "Support Strategy",
    y        = "Avg. Cigarettes per Student",
    color    = "Educational Intervention"
  ) +
  theme_bw()
ggsave("int_connect_supp.png", plot = p4, width = 8, height = 5, dpi = 300)


# ---- 6. Boxplot by district , confirms block variability ----

p5 <- ggplot(smoke_data, aes(x = DN, y = rate, fill = DN)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  labs(
    title = "Avg. Cigarettes per Student by District (Block)",
    x     = "District",
    y     = "Avg. Cigarettes per Student"
  ) +
  theme_bw() +
  theme(legend.position = "none")
ggsave("boxplot_dist.png", plot = p5, width = 8, height = 5, dpi = 300)


# ================================================
# ── FITTING THE MODEL! ────────────────
# ================================================

smoke_aov <- aov(rate ~ EDUC + SUPP + EDUC:SUPP +
                   Error(DN/EDUC),       # DN = block, DN:EDUC = whole plot error
                 data = smoke_data)

summary(smoke_aov)
names(smoke_aov)

res <- residuals(smoke_aov$Within)

# 1. Q-Q plot
png("qq_plot.png", width = 6, height = 5, units = "in", res = 300)
qqnorm(res, main = "Normal Q-Q Plot of Residuals")
qqline(res, col = "red")
dev.off()

# 2. Residuals vs fitted
png("resid_fitted.png", width = 6, height = 5, units = "in", res = 300)
fitted_vals <- fitted(smoke_aov$Within)
plot(fitted_vals, res,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)
dev.off()

# 3. Cook's distance
png("cooks_distance.png", width = 6, height = 5, units = "in", res = 300)
model_lm <- lm(rate ~ DN + EDUC + SUPP + EDUC:SUPP, data = smoke_data)
plot(model_lm, which = 4)
dev.off()
###### doing one image:
png("diagnostics.png", width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))

# 1. Q-Q plot
qqnorm(res, main = "Normal Q-Q Plot of Residuals")
qqline(res, col = "red")

# 2. Residuals vs fitted
fitted_vals <- fitted(smoke_aov$Within)
plot(fitted_vals, res,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Cook's distance
model_lm <- lm(rate ~ DN + EDUC + SUPP + EDUC:SUPP, data = smoke_data)
plot(model_lm, which = 4,
     main = "Cook's Distance")

dev.off()
par(mfrow = c(1, 1)) # reset

# ================================================
# ── Question 6: Hypothesis Tests ────────────────
# ================================================

# hypothesis tests (mixed model for EDUC and SUPP effects)
 
# H1 test: EDUC vs MS{DN:EDUC} (whole plot error)
# H2 test: SUPP vs MS{Error}

summary(smoke_aov)



# Error: DN
#           Df Sum Sq Mean Sq F value Pr(>F)
# Residuals 10  34.51   3.451               

# Error: DN:EDUC - whole plot error
#           Df Sum Sq Mean Sq F value Pr(>F)
# EDUC       1  21.97  21.971   2.733  0.129
# Residuals 10  80.39   8.039               

# Error: Within - support
#           Df Sum Sq Mean Sq F value   Pr(>F)    
# SUPP       3 136.61   45.54  38.988 4.15e-14 ***
# EDUC:SUPP  3   0.40    0.13   0.114    0.952    
# Residuals 60  70.08    1.17                                   



# H1 (EDUC): F(1, 10) = 2.733, p = 0.129 -> fail to reject. No significant evidence that the educational intervention affects smoking rate.
# H2 (SUPP): F(3, 60) = 38.988, p < 0.001 -> reject. Strong evidence that support strategy affects smoking rate.

# The interaction (EDUC:SUPP) is also clearly non-significant (p = 0.952), which is good, it means the support strategy effect doesn't depend on which educational intervention was used, so interpreting the main effects cleanly is justified.




# ================================================
# ── Question 7: Point Estimates and CIs  ────────
# ================================================


# Extract MS values from aov output
# MS{DN:EDUC} = 8.039 (df = 10) -> used for EDUC CIs
# MS{Error}   = 1.168 (df = 60) -> used for SUPP CIs
 
MS_wp  <- 8.039  # whole plot error (DN:EDUC)
df_wp  <- 10
MS_err <- 1.168  # subplot error (Within residual)
df_err <- 60
 
# Cell means (averaging over districts)
cell_means <- tapply(smoke_data$rate, 
                     list(smoke_data$EDUC, smoke_data$SUPP), mean)
 
grand_mean <- mean(smoke_data$rate)
 
# --- EDUC effects (zero-sum: alpha1 + alpha2 = 0) ---
educ_means <- rowMeans(cell_means)          # mean over SUPP levels
alpha_hat  <- educ_means - grand_mean       # zero-sum estimates
 
# Var(alpha_hat_j) = (a-1)/(a*b*c) * MS_wp, where a=2 EDUC, b=11 DN, c=4 SUPP
# = (1/2) * (1/11) * (1/4) * MS_wp  ... but simpler:
# Var(Ybar_.j.. - Ybar_....) = ((a-1)/(a)) * MS_wp / (b*c)
a <- 2; b <- 11; c_sp <- 4
var_alpha <- ((a - 1) / a) * MS_wp / (b * c_sp)
se_alpha  <- sqrt(var_alpha)
t_wp      <- qt(0.975, df_wp)
 
cat("=== EDUC effects (zero-sum) ===\n")
cat(sprintf("grand mean: %.4f\n", grand_mean))
for (j in 1:a) {
  lo <- alpha_hat[j] - t_wp * se_alpha
  hi <- alpha_hat[j] + t_wp * se_alpha
  cat(sprintf("%s: est = %6.4f, 95%% CI = (%6.4f, %6.4f)\n",
              names(alpha_hat)[j], alpha_hat[j], lo, hi))
}

# No Presentation: est = 0.4997, 95% CI = (-0.1738, 1.1731)
# Presentation: est = -0.4997, 95% CI = (-1.1731, 0.1738)
 
# --- SUPP effects (zero-sum: gamma1+gamma2+gamma3+gamma4 = 0) ---
supp_means <- colMeans(cell_means)
gamma_hat  <- supp_means - grand_mean
 a    <- 2   # number of EDUC levels
b    <- 11  # number of districts
c_sp <- 4   # number of SUPP levels

# Var(gamma_hat_k - grand_mean) = ((c-1)/c) * MS_err / (a*b)
var_gamma <- ((c_sp - 1) / c_sp) * MS_err / (a * b)
se_gamma  <- sqrt(var_gamma)
t_err     <- qt(0.975, df_err)
 
results_supp <- data.frame(
  SUPP  = names(gamma_hat),
  gamma = round(gamma_hat, 4),
  lower = round(gamma_hat - t_err * se_gamma, 4),
  upper = round(gamma_hat + t_err * se_gamma, 4)
)
results_supp

# None: est = 0.9383, 95% CI = (0.5391, 1.3374)
# Video: est = 1.5146, 95% CI = (1.1155, 1.9138)
# Ex-Smoker: est = -1.1109, 95% CI = (-1.5101, -0.7118)
# Video + Ex-Smoker: est = -1.3420, 95% CI = (-1.7412, -0.9429)


# --- Pairwise differences between SUPP levels ---
# Var(gamma_k - gamma_k') = (2/c) * MS_err / (a*b)  ... simplifies to 2*MS_err/(a*b)
# Actually: Var(Ybar_..k - Ybar_..k') = 2 * MS_err / (a*b)
var_diff <- 2 * MS_err / (a * b)
se_diff  <- sqrt(var_diff)
 
supp_levels <- names(gamma_hat)
cat("\n=== Pairwise SUPP differences ===\n")
for (k in 1:(c_sp - 1)) {
  for (kp in (k + 1):c_sp) {
    diff <- supp_means[k] - supp_means[kp]
    lo   <- diff - t_err * se_diff
    hi   <- diff + t_err * se_diff
    cat(sprintf("%s - %s: est = %6.4f, 95%% CI = (%6.4f, %6.4f)\n",
                supp_levels[k], supp_levels[kp], diff, lo, hi))
  }
}

# None - Video: est = -0.5763, 95% CI = (-1.2281, 0.0755)
# None - Ex-Smoker: est = 2.0492, 95% CI = (1.3974, 2.7010)
# None - Video + Ex-Smoker: est = 2.2803, 95% CI = (1.6285, 2.9321)
# Video - Ex-Smoker: est = 2.6255, 95% CI = (1.9737, 3.2773)
# Video - Video + Ex-Smoker: est = 2.8566, 95% CI = (2.2048, 3.5085)
# Ex-Smoker - Video + Ex-Smoker: est = 0.2311, 95% CI = (-0.4207, 0.8829)

 
q7_table <- data.frame(
  Parameter = c(
    "EDUC: No Presentation",
    "EDUC: Presentation",
    "SUPP: None",
    "SUPP: Video",
    "SUPP: Ex-Smoker",
    "SUPP: Video + Ex-Smoker",
    "None - Video",
    "None - Ex-Smoker",
    "None - Video + Ex-Smoker",
    "Video - Ex-Smoker",
    "Video - Video + Ex-Smoker",
    "Ex-Smoker - Video + Ex-Smoker"
  ),
  Estimate = c(0.4997, -0.4997, 0.9383, 1.5146, -1.1109, -1.3420,
               -0.5763, 2.0492, 2.2803, 2.6255, 2.8566, 0.2311),
  Lower_95 = c(-0.1738, -1.1731, 0.5391, 1.1155, -1.5101, -1.7412,
               -1.2281, 1.3974, 1.6285, 1.9737, 2.2048, -0.4207),
  Upper_95 = c(1.1731, 0.1738, 1.3374, 1.9138, -0.7118, -0.9429,
                0.0755, 2.7010, 2.9321, 3.2773, 3.5085, 0.8829)
)
 
print(q7_table, row.names = FALSE)

#                      Parameter Estimate Lower_95 Upper_95
#          EDUC: No Presentation   0.4997  -0.1738   1.1731
#             EDUC: Presentation  -0.4997  -1.1731   0.1738
#                     SUPP: None   0.9383   0.5391   1.3374
#                    SUPP: Video   1.5146   1.1155   1.9138
#                SUPP: Ex-Smoker  -1.1109  -1.5101  -0.7118
#        SUPP: Video + Ex-Smoker  -1.3420  -1.7412  -0.9429
#                   None - Video  -0.5763  -1.2281   0.0755
#               None - Ex-Smoker   2.0492   1.3974   2.7010
#       None - Video + Ex-Smoker   2.2803   1.6285   2.9321
#              Video - Ex-Smoker   2.6255   1.9737   3.2773
#      Video - Video + Ex-Smoker   2.8566   2.2048   3.5085
#  Ex-Smoker - Video + Ex-Smoker   0.2311  -0.4207   0.8829


supp_means_vec <- colMeans(cell_means)  # marginal SUPP means
se_diff <- sqrt(2 * MS_err / (a * b))
t_err   <- qt(0.975, df_err)           # 95% CI, df = 60
supp_levels <- names(supp_means_vec)
pairs       <- combn(supp_levels, 2, simplify = FALSE)

results_pairs <- do.call(rbind, lapply(pairs, function(p) {
  diff  <- supp_means_vec[p[1]] - supp_means_vec[p[2]]
  lower <- diff - t_err * se_diff
  upper <- diff + t_err * se_diff
  data.frame(
    Comparison = paste(p[1], "vs", p[2]),
    Difference = round(diff, 4),
    Lower      = round(lower, 4),
    Upper      = round(upper, 4)
  )
}))

rownames(results_pairs) <- NULL
results_pairs


results_pairs$Significant <- ifelse(results_pairs$Lower > 0 | 
                                    results_pairs$Upper < 0, 
                                    "Significant", "Not Significant")

p_forest <- ggplot(results_pairs, 
                   aes(x = Difference, y = Comparison, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Significant" = "steelblue", 
                                "Not Significant" = "tomato")) +
  labs(
    x     = "Mean Difference in Avg. Cigarettes per Student",
    y     = "Comparison",
    color = NULL
  ) +
  theme_bw()

ggsave("forest_plot.png", plot = p_forest, width = 8, height = 5, dpi = 300)

