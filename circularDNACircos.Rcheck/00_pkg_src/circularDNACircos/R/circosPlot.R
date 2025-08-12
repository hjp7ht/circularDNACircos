#' dual_circos_plot
#'
#' @description Generates two circos plots from two files and saves as images in the same directory.
#' @param file1 First CSV or TXT file.
#' @param file2 Second CSV or TXT file.
#' @param outer_track_height can be in between(0 to 1 ideally 0.25)
#' @param inner_track_height can be in between( 0 to 1 ideally < 0.2)
#' @param max_pairs_per_outer_bin number of pairs you want to show in the bin ( ideally 20 for track height of 0.25)
#' @param max_pairs_per_inner_bin number of pairs you want to show in the bin ( ideally 20 for track height of 0.25)
#' @param outer_track_arrow_head_width arrow head width (ideally in between 0.05-0.2)
#' @param outer_track_arrow_head_length arrow head length( ideally in between 0.05-0.4)
#' @param inner_track_arrow_head_width arrow head width (ideally in between 0.05-0.2)
#' @param inner_track_arrow_head_length arrow head length( ideally in between 0.05-0.4)
#' @param png1 name of the first circos plot
#' @param png2 name of the second circos plot
#' @return Invisibly returns the widget object.
#'
#' @importFrom grDevices png dev.off
#' @importFrom graphics par legend
#' @examples
#' # Minimal runnable example: create two tiny datasets and plot to temp files
#' df1 <- data.frame(
#'   chromosome = c("chr1", "chr1","chr1","chr1"),
#'   start1     = c(1000, 1300,1050,1020),
#'   end1       = c(1150, 1450, 1100,1070),
#'   start2     = c(1350, 1650,1900,1930),
#'   end2       = c(1500, 1800, 1950,1980),
#'   orientation = c("FR", "RF","FF","RR"),
#'   stringsAsFactors = FALSE
#' )
#' df2 <- data.frame(
#'   chromosome = c("chr1", "chr1"),
#'   start1     = c(1050,1020),
#'   end1       = c(1100,1070),
#'   start2     = c(1900,1930),
#'   end2       = c(1950,1980),
#'   start3     = c(1300,1700),
#'   end3       = c(1450,1850),
#'   orientation = c("FFR","RRF"),
#'   stringsAsFactors = FALSE
#' )
#'
#' f1 <- tempfile(fileext = ".csv")
#' f2 <- tempfile(fileext = ".csv")
#' utils::write.csv(df1, f1, row.names = FALSE)
#' utils::write.csv(df2, f2, row.names = FALSE)
#'
#' out1 <- file.path(tempdir(), "circos1.png")
#' out2 <- file.path(tempdir(), "circos2.png")
#'
#' dual_circos_plot(
#'   file1 = f1, file2 = f2,
#'   outer_track_height = 0.25, inner_track_height = 0.13,
#'   max_pairs_per_outer_bin = 20, max_pairs_per_inner_bin = 15,
#'   outer_track_arrow_head_width = 0.08, outer_track_arrow_head_length = 0.07,
#'   inner_track_arrow_head_width = 0.10, inner_track_arrow_head_length = 0.07,
#'   png1 = out1, png2 = out2
#' )
#' file.exists(out1) && file.exists(out2)
#'
#' @export
dual_circos_plot <- function(file1, file2, outer_track_height = 0.3, inner_track_height = 0.13,
                             max_pairs_per_outer_bin = 20, max_pairs_per_inner_bin = 15,
                             outer_track_arrow_head_width=0.08, outer_track_arrow_head_length=0.07,
                             inner_track_arrow_head_width=0.1, inner_track_arrow_head_length=0.07,
                             png1 = "circos_plot1.png", png2 = "circos_plot2.png") {

  read_pairs <- function(file) {
    if (grepl("\\.csv$", file, ignore.case = TRUE)) {
      df <- readr::read_csv(file, show_col_types = FALSE)
    } else {
      first_line <- readLines(file, n = 1)
      first_items <- strsplit(first_line, "\t")[[1]]
      col_count <- length(first_items)

      expected_names <- c("chromosome", "start1", "end1", "start2", "end2", "orientation")
      is_header <- all(tolower(first_items[1]) == expected_names[1])

      if (is_header) {
        df <- readr::read_delim(file, delim = "\t", show_col_types = FALSE)
      } else {
        base_names <- expected_names
        if (col_count > length(base_names)) {
          col_names <- c("chromosome", "start1", "end1", "start2", "end2", "start3", "end3", "orientation")
        } else {
          col_names <- base_names[1:col_count]
        }
        df <- readr::read_delim(file, delim = "\t", col_names = col_names, show_col_types = FALSE)
      }
    }
    df
  }

  df1 <- read_pairs(file1)
  df2 <- read_pairs(file2)

    arrow_head_position_3_pair <- function(arrow_start,arrow_end,plot_start,plot_end,orientation){
      position = "start"
      if(orientation == "F"){
        if(plot_end - max(arrow_start,arrow_end) <= min(arrow_start,arrow_end) - plot_start){
          position="start"
        }
        else{
          position = "end"
        }
      }
      else if(orientation == "R"){
        if(plot_end - max(arrow_start,arrow_end) <= min(arrow_start,arrow_end) - plot_start){
          position="end"
        }
        else {
          position = "start"
        }
      }
      position
    }

    #function to find closest co-ordinates between two lines
    get_shortest_path <- function(plot_start, plot_end,
                                  l1_start, l1_end, l2_start, l2_end) {
      combos <- list(
        c(l1_start, l2_start),
        c(l1_start, l2_end),
        c(l1_end, l2_start),
        c(l1_end, l2_end)
      )
      names(combos) <- c("l1_start:l2_start", "l1_start:l2_end", "l1_end:l2_start", "l1_end:l2_end")
      circle_length <- plot_end - plot_start
      results <- list()
      for (nm in names(combos)) {
        a <- combos[[nm]][1]
        b <- combos[[nm]][2]
        clockwise <- abs(b - a)
        counterclockwise <- circle_length - clockwise
        results[[nm]] <- list(
          from = a,
          to = b,
          clockwise = clockwise,
          counterclockwise = counterclockwise
        )
      }
      # Find shortest path
      min_clockwise <- min(sapply(results, function(x) x$clockwise))
      min_counterclockwise <- min(sapply(results, function(x) x$counterclockwise))
      if (min_clockwise <= min_counterclockwise) {
        for (nm in names(results)) {
          if (results[[nm]]$clockwise == min_clockwise) {
            return(list(
              type = "clockwise",
              from = results[[nm]]$from,
              to = results[[nm]]$to
            ))
          }
        }
      } else {
        for (nm in names(results)) {
          if (results[[nm]]$counterclockwise == min_counterclockwise) {
            return(list(
              type = "counterclockwise",
              from = results[[nm]]$from,
              to = results[[nm]]$to,
              plot_start = plot_start,
              plot_end = plot_end
            ))
          }
        }
      }
    }


    #function to find the shortest arc co-ordinates and the direction of the minor arc
    find_shortest_between_lines <- function(plot_start, plot_end, l1, l2) {
      points <- expand.grid(
        a = c(l1$start, l1$end),
        b = c(l2$start, l2$end),
        stringsAsFactors = FALSE
      )
      min_arc <- Inf
      best <- NULL
      for (i in seq_len(nrow(points))) {
        res <- get_shortest_path(plot_start, plot_end,
                                 points$a[i], points$a[i],
                                 points$b[i], points$b[i])
        arc_len <- min(abs(res$to - res$from), (plot_end - plot_start) - abs(res$to - res$from))
        if (arc_len < min_arc) {
          min_arc <- arc_len
          best <- res
        }
      }
      return(best)
    }



    #function to find the shortest distance from line3
    find_shortest_from_line3 <- function(plot_start, plot_end, l3, l1, l2) {
      combos <- expand.grid(
        l3_pos = c(l3$start, l3$end),
        lX_pos = c(l1$start, l1$end, l2$start, l2$end),
        stringsAsFactors = FALSE
      )
      min_arc <- Inf
      best <- NULL
      for (i in seq_len(nrow(combos))) {
        res <- get_shortest_path(plot_start, plot_end,
                                 combos$l3_pos[i], combos$l3_pos[i],
                                 combos$lX_pos[i], combos$lX_pos[i])
        arc_len <- min(abs(res$to - res$from), (plot_end - plot_start) - abs(res$to - res$from))
        if (arc_len < min_arc) {
          min_arc <- arc_len
          best <- res
        }
      }
      return(best)
    }


    #function to check if the lines overlapping
    check_overlapping <- function(a1, a2, b1, b2, tol = 1) {
      !(a2 < b1 - tol || b2 < a1 - tol)
    }

    #function to select the pairs i.e. if there are more pairs it will be difficult to plot all of them on the circos plot, so selecting a few.
    select_pairs <- function(df, pretty_ticks, max_per_bin = 25) {
      n_bins <- length(pretty_ticks) - 1
      df$bin <- cut(df$start1, breaks = pretty_ticks, include.lowest = TRUE, labels = FALSE)

      result <- do.call(rbind, lapply(split(df, df$bin), function(bin_df) {
        n <- nrow(bin_df)
        if (n > max_per_bin) {
          bin_df <- bin_df[sample(n, max_per_bin), ]
        }
        bin_df
      }))

      if (!is.null(result) && nrow(result) > 0) {
        n_total <- as.integer(nrow(result))
        result$y <- seq(0.1, 0.95, length.out = n_total)
        rownames(result) <- NULL
        result$bin <- NULL
      } else {
        result <- data.frame()
      }

      result
    }



    plot_one_circos <- function(df, show_inner_track = TRUE,start,end, png_file) {
      png(filename = png_file, width = 6400, height = 6400, res = 1080)
      n_bins <- 9
      pretty_ticks <- seq(start, end, length.out = n_bins + 1)
      if (n_bins > 1) {
        pretty_ticks[2:n_bins] <- round(pretty_ticks[2:n_bins], -2)
      }
      pretty_ticks[1] <- start
      pretty_ticks[length(pretty_ticks)] <- end
      par(mar = rep(1, 4))
      circlize::circos.clear()
      circlize::circos.par(cell.padding = c(0.05, 0, 0.07, 0), start.degree = 90, gap.after = 0)
      circlize::circos.initialize(factors = "chr1",xlim = c(start, end))
      fr_rf_pairs <- df[df$orientation == "FR" | df$orientation=="RF",  ]
      rr_ff_pairs <- df[df$orientation == "FF" | df$orientation == "RR", ]
      fr_rf_pairs_sel <- select_pairs(fr_rf_pairs,pretty_ticks, max_pairs_per_outer_bin)
      rr_ff_pairs_sel <- select_pairs(rr_ff_pairs,pretty_ticks,15)

      pair_type_colors <- c(
        FR = "orange",
        RF = "blue",
        FF = "green",
        RR = "red"
      )
      pair_type_labels <- c(
        FR = "FR",
        RF = "RF",
        FF = "FF",
        RR = "RR"
      )
      circlize::circos.trackPlotRegion(
        factors = "chr1", ylim = c(0, 1), track.height = outer_track_height, bg.border = NA,
        panel.fun = function(x, y) {
          if(exists("fr_rf_pairs_sel") && is.data.frame(fr_rf_pairs_sel) && nrow(fr_rf_pairs_sel) > 0){
            print(fr_rf_pairs_sel)
            for (i in seq_len(nrow(fr_rf_pairs_sel))){
              y_pos <- fr_rf_pairs_sel$y[i]
              colr <- pair_type_colors[fr_rf_pairs_sel$orientation[i]]

              if(fr_rf_pairs_sel$start1[i] > fr_rf_pairs_sel$start2[i]){
                circlize::circos.arrow(
                  x1 = fr_rf_pairs_sel$start1[i], x2 = fr_rf_pairs_sel$end1[i], y = y_pos,
                  sector.index = "chr1", track.index = 1,
                  arrow.position = "start", col = colr, border = colr,
                  arrow.head.length = outer_track_arrow_head_length, arrow.head.width = outer_track_arrow_head_width, width = 0
                )

                circlize::circos.arrow(
                  x1 = fr_rf_pairs_sel$start2[i], x2 = fr_rf_pairs_sel$end2[i], y = y_pos,
                  sector.index = "chr1", track.index = 1,
                  arrow.position = "end", col = colr, border = colr,
                  arrow.head.length = outer_track_arrow_head_length, arrow.head.width = outer_track_arrow_head_width, width = 0
                )

              }
              else{
                circlize::circos.arrow(
                  x1 = fr_rf_pairs_sel$start1[i], x2 = fr_rf_pairs_sel$end1[i], y = y_pos,
                  sector.index = "chr1", track.index = 1,
                  arrow.position = "end", col = colr, border = colr,
                  arrow.head.length = outer_track_arrow_head_length, arrow.head.width = outer_track_arrow_head_width, width = 0
                )

                circlize::circos.arrow(
                  x1 = fr_rf_pairs_sel$start2[i], x2 = fr_rf_pairs_sel$end2[i], y = y_pos,
                  sector.index = "chr1", track.index = 1,
                  arrow.position = "start", col = colr, border = colr,
                  arrow.head.length = outer_track_arrow_head_length, arrow.head.width = outer_track_arrow_head_width, width = 0
                )

              }

              l1 <- list(start = fr_rf_pairs_sel$start1[i], end = fr_rf_pairs_sel$end1[i])
              l2 <- list(start = fr_rf_pairs_sel$start2[i], end = fr_rf_pairs_sel$end2[i])

              if(!check_overlapping(l1$start, l1$end, l2$start, l2$end)){
                # Dotted line between line1 and line2
                dotted1 <- get_shortest_path(start, end, l1$start, l1$end, l2$start, l2$end)
                # Draw dotted1
                if (dotted1$type == "clockwise") {
                  circlize::circos.lines(
                    c(dotted1$from, dotted1$to),
                    c(y_pos, y_pos),
                    sector.index = "chr1",
                    lty = 3, lwd = 1.2, col = colr
                  )
                }
                else {
                  if (dotted1$to < dotted1$from){
                    circlize::circos.lines(
                      c(dotted1$from, end),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )

                    circlize::circos.lines(
                      c(start, dotted1$to),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )
                  }
                  else {
                    circlize::circos.lines(
                      c(dotted1$to, end),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )

                    circlize::circos.lines(
                      c(start, dotted1$from),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )
                  }

                }

              }

            }
          }
        }
      )
      circlize::circos.trackPlotRegion(
        factors = "chr1", ylim = c(0, 1), track.height = inner_track_height, bg.border = NA,
        panel.fun = function(x, y) {
          if (exists("rr_ff_pairs_sel") && is.data.frame(rr_ff_pairs_sel) && nrow(rr_ff_pairs_sel) > 0) {
            print(rr_ff_pairs_sel)
            for (i in seq_len(nrow(rr_ff_pairs_sel))){
              y_pos <- rr_ff_pairs_sel$y[i]
              type <- rr_ff_pairs_sel$orientation[i]
              colr <- pair_type_colors[type]
              if(rr_ff_pairs_sel$start1[i]>rr_ff_pairs_sel$start2[i]){
                circlize::circos.arrow(
                  x1 = rr_ff_pairs_sel$start1[i], x2 = rr_ff_pairs_sel$end1[i], y = y_pos,
                  sector.index = "chr1", track.index = 2,
                  arrow.position = "end",
                  col = colr, border = colr,
                  arrow.head.length = inner_track_arrow_head_length, arrow.head.width = inner_track_arrow_head_width, width = 0
                )
                circlize::circos.arrow(
                  x1 = rr_ff_pairs_sel$start2[i], x2 = rr_ff_pairs_sel$end2[i], y = y_pos,
                  sector.index = "chr1", track.index = 2,
                  arrow.position = "start",
                  col = colr, border = colr,
                  arrow.head.length = inner_track_arrow_head_length, arrow.head.width = inner_track_arrow_head_width, width = 0
                )
              }
              else{
                circlize::circos.arrow(
                  x1 = rr_ff_pairs_sel$start1[i], x2 = rr_ff_pairs_sel$end1[i], y = y_pos,
                  sector.index = "chr1", track.index = 2,
                  arrow.position = "start",
                  col = colr, border = colr,
                  arrow.head.length = inner_track_arrow_head_length, arrow.head.width = inner_track_arrow_head_width, width = 0
                )
                circlize::circos.arrow(
                  x1 = rr_ff_pairs_sel$start2[i], x2 = rr_ff_pairs_sel$end2[i], y = y_pos,
                  sector.index = "chr1", track.index = 2,
                  arrow.position = "end",
                  col = colr, border = colr,
                  arrow.head.length = inner_track_arrow_head_length, arrow.head.width = inner_track_arrow_head_width, width = 0
                )
              }

              l1 <- list(start = rr_ff_pairs_sel$start1[i], end = rr_ff_pairs_sel$end1[i])
              l2 <- list(start = rr_ff_pairs_sel$start2[i], end = rr_ff_pairs_sel$end2[i])
              if(!check_overlapping(l1$start, l1$end, l2$start, l2$end)){
                # Dotted line between line1 and line2
                dotted1 <- get_shortest_path(start, end, l1$start, l1$end, l2$start, l2$end)
                # Draw dotted1
                if (dotted1$type == "clockwise") {
                  circlize::circos.lines(
                    c(dotted1$from, dotted1$to),
                    c(y_pos, y_pos),
                    sector.index = "chr1",
                    lty = 3, lwd = 1.2, col = colr
                  )
                }
                else {
                  if (dotted1$to < dotted1$from){
                    circlize::circos.lines(
                      c(dotted1$from, end),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )

                    circlize::circos.lines(
                      c(start, dotted1$to),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )
                  }
                  else {
                    circlize::circos.lines(
                      c(dotted1$to, end),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )

                    circlize::circos.lines(
                      c(start, dotted1$from),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )
                  }

                }

              }
            }
          }

        }
      )
      circlize::circos.axis(
        sector.index = "chr1",
        major.at = pretty_ticks,
        labels = format(pretty_ticks, scientific=FALSE, trim=TRUE),
        labels.cex = 0.6,
        labels.niceFacing = TRUE,
        minor.ticks = 0
      )
      track1_top    <- circlize::get.cell.meta.data("cell.top.radius",    sector.index="chr1", track.index=1)
      track1_bottom <- circlize::get.cell.meta.data("cell.bottom.radius", sector.index="chr1", track.index=1)
      track2_top    <- circlize::get.cell.meta.data("cell.top.radius",    sector.index="chr1", track.index=2)
      track2_bottom <- circlize::get.cell.meta.data("cell.bottom.radius", sector.index="chr1", track.index=2)

      circlize::draw.sector(0, 360, rou1 = track1_top,    rou2 = track1_top,    border = "black", lwd = 1, col = NA)
      circlize::draw.sector(0, 360, rou1 = track1_bottom, rou2 = track1_bottom, border = "black", lwd = 1, col = NA)
      circlize::draw.sector(0, 360, rou1 = track2_bottom, rou2 = track2_bottom, border = "black", lwd = 1, col = NA)

      legend("topleft",
             legend = pair_type_labels,
             col = pair_type_colors,
             lwd = 3,
             pch = c(NA, NA, NA, NA),
             seg.len = 2,
             bty = "n",
             cex = 0.6,
             text.col = pair_type_colors
      )

      dev.off()
    }

    plot_two_circos <- function(df, show_inner_track = FALSE,start,end,png_file) {
      png(filename = png_file, width = 6400, height = 6400, res = 1080)
      n_bins <- 9
      pretty_ticks <- seq(start, end, length.out = n_bins + 1)
      if (n_bins > 1) {
        pretty_ticks[2:n_bins] <- round(pretty_ticks[2:n_bins], -2)
      }
      pretty_ticks[1] <- start
      pretty_ticks[length(pretty_ticks)] <- end

      par(mar = rep(1, 4))
      circlize::circos.clear()
      circlize::circos.par(cell.padding = c(0.05, 0, 0.07, 0), start.degree = 90, gap.after = 0)
      circlize::circos.initialize(factors = "chr1",xlim = c(start, end))

      cirucular_n <- nrow(df)
      df$y <- seq(0.1,0.99, length.out = cirucular_n)
      circular_type_colors <- c(
        FFR = "green",
        RRF = "red"
      )
      circular_labels <- c(
        FFR = "FF-R",
        RRF = "RR-F"
      )
      circlize::circos.trackPlotRegion(
        factors = "chr1", ylim = c(0, 1), track.height = 0.3, bg.border = NA,
        panel.fun = function(x, y) {
          for (i in seq_len(cirucular_n)) {
            y_pos <- df$y[i]
            colr <- circular_type_colors[df$orientation[i]]

              circlize::circos.arrow(
                x1 = df$start1[i], x2 = df$end1[i], y = y_pos,
                sector.index = "chr1", track.index = 1,
                arrow.position = arrow_head_position_3_pair(df$start1[i],df$end1[i],start,end,substr(df$orientation[i],1,1)), col = colr, border = colr,
                arrow.head.length = 0.05, arrow.head.width = 0.1, width = 0
              )

              circlize::circos.arrow(
                x1 = df$start2[i], x2 = df$end2[i], y = y_pos,
                sector.index = "chr1", track.index = 1,
                arrow.position = arrow_head_position_3_pair(df$start2[i],df$end2[i],start,end,substr(df$orientation[i],2,2)), col = colr, border = colr,
                arrow.head.length = 0.05, arrow.head.width = 0.1, width = 0
              )
                circlize::circos.arrow(
                  x1 = df$start3[i], x2 = df$end3[i], y = y_pos,
                  sector.index = "chr1", track.index = 1,
                  arrow.position = arrow_head_position_3_pair(df$start3[i],df$end3[i],start,end,substr(df$orientation[i],3,3)), col = "blue", border = "blue",
                  arrow.head.length = 0.05, arrow.head.width = 0.1, width = 0
                )

                l1 <- list(start = df$start1[i], end = df$end1[i])
                l2 <- list(start = df$start2[i], end = df$end2[i])
                l3 <- list(start = df$start3[i], end = df$end3[i])

                if(!check_overlapping(l1$start, l1$end, l2$start, l2$end)){
                  # Dotted line between line1 and line2
                  dotted1 <- get_shortest_path(start, end, l1$start, l1$end, l2$start, l2$end)
                  # Draw dotted1
                  if (dotted1$type == "clockwise") {
                    circlize::circos.lines(
                      c(dotted1$from, dotted1$to),
                      c(y_pos, y_pos),
                      sector.index = "chr1",
                      lty = 3, lwd = 1.2, col = colr
                    )
                  }
                  else {
                    if (dotted1$to < dotted1$from){
                      circlize::circos.lines(
                        c(dotted1$from, end),
                        c(y_pos, y_pos),
                        sector.index = "chr1",
                        lty = 3, lwd = 1.2, col = colr
                      )

                      circlize::circos.lines(
                        c(start, dotted1$to),
                        c(y_pos, y_pos),
                        sector.index = "chr1",
                        lty = 3, lwd = 1.2, col = colr
                      )
                    }
                    else {
                      circlize::circos.lines(
                        c(dotted1$to, end),
                        c(y_pos, y_pos),
                        sector.index = "chr1",
                        lty = 3, lwd = 1.2, col = colr
                      )

                      circlize::circos.lines(
                        c(start, dotted1$from),
                        c(y_pos, y_pos),
                        sector.index = "chr1",
                        lty = 3, lwd = 1.2, col = colr
                      )
                    }
                  }
                }
                overlaps_l1 <- check_overlapping(l3$start, l3$end, l1$start, l1$end)
                overlaps_l2 <- check_overlapping(l3$start, l3$end, l2$start, l2$end)
                if (!(overlaps_l1 || overlaps_l2)) {
                                  # Dotted line between line3 and whichever of line1 or line2 is closest
                dotted2 <- find_shortest_from_line3(start, end, l3, l1, l2)

                # Draw dotted2 (similar)
                if (dotted2$type == "clockwise") {
                  circlize::circos.lines(
                    c(dotted2$from, dotted2$to),
                    c(y_pos, y_pos),
                    sector.index = "chr1",
                    lty = 3, lwd = 1.2, col = "blue"
                  )
                } else {
                  circlize::circos.lines(
                    c(dotted2$from, end),
                    c(y_pos, y_pos),
                    sector.index = "chr1",
                    lty = 3, lwd = 1.2, col = "blue"
                  )
                  circlize::circos.lines(
                    c(start, dotted2$to),
                    c(y_pos, y_pos),
                    sector.index = "chr1",
                    lty = 3, lwd = 1.2, col = "blue"
                  )
                }

                }

          }

          circlize::circos.segments(
            x0 = start, y0 = 0,
            x1 = start, y1 = 2,
            sector.index = "chr1", track.index = 1,
            col = "#cac800", lwd = 0.5, lty = 1
          )
        }
      )
      circlize::circos.axis(
        sector.index = "chr1",
        major.at = pretty_ticks,
        labels = format(pretty_ticks, scientific=FALSE, trim=TRUE),
        labels.cex = 0.9,
        labels.niceFacing = TRUE,
        minor.ticks = 0
      )
      track1_top    <- circlize::get.cell.meta.data("cell.top.radius", sector.index = "chr1", track.index = 1)
      track1_bottom <- circlize::get.cell.meta.data("cell.bottom.radius", sector.index = "chr1", track.index = 1)
      circlize::draw.sector(0, 360, rou1 = track1_top,    rou2 = track1_top,    border = "black", lwd = 1, col = NA)
      circlize::draw.sector(0, 360, rou1 = track1_bottom, rou2 = track1_bottom, border = "black", lwd = 1, col = NA)
      legend("topright",
             legend = circular_labels,
             col = circular_type_colors,
             lwd = 3,
             pch = c(NA, NA, NA, NA),
             seg.len = 2,
             bty = "n",
             cex = 0.6,
             text.col = circular_type_colors
      )
      dev.off()
    }

  start1 <- min(df1$start1, df1$end1, df1$start2, df1$end2, na.rm = TRUE)
  end1   <- max(df1$start1, df1$end1, df1$start2, df1$end2, na.rm = TRUE)
  start2 <- min(df2$start1, df2$end1, df2$start2, df2$end2, df2$start3, df2$end3, na.rm = TRUE)
  end2   <- max(df2$start1, df2$end1, df2$start2, df2$end2, df2$start3, df2$end3, na.rm = TRUE)
  plot_one_circos(df1, show_inner_track = TRUE,start1,end1,png1)
  plot_two_circos(df2, show_inner_track = FALSE,start2,end2,png2)

}


