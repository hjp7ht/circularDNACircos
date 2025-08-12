## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(circularDNACircos)

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


