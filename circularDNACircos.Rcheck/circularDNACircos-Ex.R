pkgname <- "circularDNACircos"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "circularDNACircos-Ex.timings", pos = 'CheckExEnv')
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
library('circularDNACircos')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dual_circos_plot")
### * dual_circos_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dual_circos_plot
### Title: dual_circos_plot
### Aliases: dual_circos_plot

### ** Examples

# Minimal runnable example: create two tiny datasets and plot to temp files
df1 <- data.frame(
  chromosome = c("chr1", "chr1","chr1","chr1"),
  start1     = c(1000, 1300,1050,1020),
  end1       = c(1150, 1450, 1100,1070),
  start2     = c(1350, 1650,1900,1930),
  end2       = c(1500, 1800, 1950,1980),
  orientation = c("FR", "RF","FF","RR"),
  stringsAsFactors = FALSE
)
df2 <- data.frame(
  chromosome = c("chr1", "chr1"),
  start1     = c(1050,1020),
  end1       = c(1100,1070),
  start2     = c(1900,1930),
  end2       = c(1950,1980),
  start3     = c(1300,1700),
  end3       = c(1450,1850),
  orientation = c("FFR","RRF"),
  stringsAsFactors = FALSE
)

f1 <- tempfile(fileext = ".csv")
f2 <- tempfile(fileext = ".csv")
utils::write.csv(df1, f1, row.names = FALSE)
utils::write.csv(df2, f2, row.names = FALSE)

out1 <- file.path(tempdir(), "circos1.png")
out2 <- file.path(tempdir(), "circos2.png")

dual_circos_plot(
  file1 = f1, file2 = f2,
  outer_track_height = 0.25, inner_track_height = 0.13,
  max_pairs_per_outer_bin = 20, max_pairs_per_inner_bin = 15,
  outer_track_arrow_head_width = 0.08, outer_track_arrow_head_length = 0.07,
  inner_track_arrow_head_width = 0.10, inner_track_arrow_head_length = 0.07,
  png1 = out1, png2 = out2
)
file.exists(out1) && file.exists(out2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dual_circos_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
