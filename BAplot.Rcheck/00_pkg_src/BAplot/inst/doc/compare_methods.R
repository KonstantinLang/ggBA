## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(magrittr)    # A Forward-Pipe Operator for R
library(tidyr)       # Tidy Messy Data
# library(knitr)       # A General-Purpose Package for Dynamic Report Generation in R
library(kableExtra)  # Construct Complex Table with 'kable' and Pipe Syntax

library(BAplot)

## ----options------------------------------------------------------------------
options(
  kable_styling_bootstrap_options = c("hover", "striped"),
  kable_styling_full_width = FALSE,
  kable_styling_position = "left"
)

## ----dataprep-----------------------------------------------------------------
tbl <- 
  temperature %>% 
  pivot_wider(names_from = method, values_from = temperature)

## ----simple_stats_table-------------------------------------------------------
ba_stat(data = tbl, var1 = infrared, var2 = rectal) %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  kable() %>% 
  kable_styling()

## ----simple_baplot------------------------------------------------------------
ba_plot(data = tbl, var1 = infrared, var2 = rectal)

