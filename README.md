# Split-Plot Analysis of Smoking Cessation Interventions

## Overview
Split-plot experimental design evaluating the effects of a school-based
educational intervention and follow-up support strategies on adolescent
cigarette smoking, using homeroom-level data collected across 11
school districts (N = 88 homerooms).

## Methods
- Split-plot ANOVA with district as blocking factor, educational
  intervention as whole-plot factor, support strategy as subplot factor
- 95% confidence intervals using stratum-appropriate error terms
  (whole-plot vs. subplot)
- Pairwise comparisons among support strategies
- Diagnostic assessment via Q-Q plots, residuals-vs.-fitted plots, and
  Cook's distance

## Software
R (v4.4.0) — readxl, dplyr, ggplot2, tidyverse

## Data
Homeroom-level average cigarettes smoked per student, aggregated for
privacy from individual-level responses. Two schools per district were
randomized to educational intervention (no presentation vs. two-hour
ex-smoker testimonial presentation); four homerooms per school were
randomized to support strategy (none, video, ex-smoker Q&A, or both).

## Key variables
- Outcome: average cigarettes smoked per student per homeroom
- Whole-plot factor: educational intervention (presentation vs. none)
- Subplot factor: support strategy (none, video, ex-smoker, video + ex-smoker)
- Blocking factor: district (n = 11)
