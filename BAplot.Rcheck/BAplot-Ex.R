pkgname <- "BAplot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "BAplot-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('BAplot')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ba_mean_diff")
### * ba_mean_diff

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ba_mean_diff
### Title: Derive mean and difference for Bland-Altman analysis
### Aliases: ba_mean_diff

### ** Examples

library(tidyr)
tbl <- temperature %>% pivot_wider(names_from = method, values_from = temperature)

# identity (default) - same as used inside ba_stat / ba_plot
ba_mean_diff(tbl, var1 = infrared, var2 = rectal)

# log transformation
ba_mean_diff(tbl, var1 = infrared, var2 = rectal, transform = "log")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ba_mean_diff", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ba_plot")
### * ba_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ba_plot
### Title: Bland-Altman plot
### Aliases: ba_plot

### ** Examples

library(tidyr)
tbl <- temperature %>% pivot_wider(names_from = method, values_from = temperature)

# simple example
ba_plot(data = tbl, var1 = infrared, var2 = rectal)

# with colors
ba_plot(data = tbl, var1 = infrared, var2 = rectal, colour = visit)

# with colors and faceting
ba_plot(data = tbl, var1 = infrared, var2 = rectal, group = treatment, colour = visit)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ba_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ba_stat")
### * ba_stat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ba_stat
### Title: Bland-Altman statistics
### Aliases: ba_stat

### ** Examples

library(tidyr)
tbl <- temperature %>% pivot_wider(names_from = method, values_from = temperature)

# simple example
ba_stat(data = tbl, var1 = infrared, var2 = rectal)

## example with grouping
ba_stat(data = tbl, var1 = infrared, var2 = rectal, group = treatment) %>%
  pivot_wider(names_from = parameter, values_from = value)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ba_stat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gen_temp_data")
### * gen_temp_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gen_temp_data
### Title: Generate rodents temperature data
### Aliases: gen_temp_data

### ** Examples

temperature <- BAplot:::gen_temp_data()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gen_temp_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
