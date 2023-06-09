#' Flux calculator
#'
#' Calculates CO2 flux from SEMACH raw data (.txt format). Returns vector with averages, dCO2 in ppm/min and flux in µmol m-2 s-1.
#' @param filepath Path from which data is loaded. E.g., "E:/Folder/measurement.txt"
#' @param start Starting point of evaluation in seconds after start of recording. Default is 61 s (or 1 min)
#' @param end End point of evaluation in seconds after start of recording. Default is 300 s (or 5 min)
#' @param V_chamber Chamber volume in m³. Useful e.g., when chamber volume is extended by base ring. Default is 0.01587 m³
#' @param A_chamber Chamber base area in m². Implemented for completion, usually not changed. Default is 0.0491 m³
#' @param draw Which variables should be included in plot? Default is c("all").
#' Other possible inputs are vector containing variable names or c("none") if no plotting is desired.
#' ("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"), or empty vector c().
#' @param save_plot Should the plot be saved? T/F Default is T.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.
flux_calc <- function(filepath,
                      start = 61,
                      #defaut for 1min to 5min of data
                      end = 300,
                      #
                      V_chamber = 0.01587,
                      # m³, defalut for no displacement / no additional tube space
                      A_chamber = .0491,
                      draw=c("all"),
                      save_plot=F,
                      show_plot=T,
                      draw.method="svg") {


  # testing ################################################################

  cat("\nstart: ",start,"\nend: ",end, "\t",class(end))



  # end of testing ###########################################################


  # m², default surface area
  require(readr)
  x <- read.delim(filepath)                             # reads input files using filepath given
  x <-
    x[1:11]                  # removes unwnted cols: charge (usually not of importance) and X13 (artefact column)
  names(x) <-
    # renaming columns
    c("Date_Time",
      "rH_i",
      "T_i",
      "P_i",
      "SM",
      "ST",
      "CO2",
      "rH_o",
      "T_o",
      "P_o",
      "PAR")

  dtparts = t(as.data.frame(strsplit(x[[1]], ' ')))      # date_time formatting (giving up on chron or POSIX, at least for the moment)
  x[[1]] <- NULL
  row.names(dtparts) = NULL
  x$date <- dtparts[, 1]
  x$time <- strtrim(dtparts[, 2], 8)
  x$t_sec <-
    as.numeric(c(0:(length(x[[1]]) - 1)))   # attaches column with seconds after start of recording (t0=0)

  xx <-
    subset(x, t_sec <= end &
             t_sec >= start)               # using data from (and including) start to (and including) end values
  slope <-
    lm(xx$CO2 ~ xx$t_sec)$coefficients[[2]] * 60     # calculates slope in ppm/min
  rH_i <-
    mean(xx$rH_i)                                 # calculates averages for each column (except CO2)
  T_i <- mean(xx$T_i)
  P_i <- mean(xx$P_i)
  SM <- mean(xx$SM)
  ST <- mean(xx$ST)
  rH_o <- mean(xx$rH_o)
  T_o <- mean(xx$T_o)
  P_o <- mean(xx$P_o)
  PAR <- mean(xx$PAR)
  P_i <- mean(xx$P_i)

  F_CO2 <-
    slope * V_chamber * P_i * 100 / (60 * 8.3145 * (T_i + 273.15) * A_chamber)  #flux calculation

  #                                  ppm/min  °C     hPa   %     %  °C   %     °C  hPa  µmol/(m²s)   µmol/(m²s)
  output <-
    c(x$date[[1]],
      x$time[[1]],
      slope,
      T_i,
      P_i,
      rH_i,
      SM,
      ST,
      rH_o,
      T_o,
      P_o,
      PAR,
      F_CO2,
      V_chamber,
      A_chamber,
      start,
      end)


  if(draw==c("all")|min(draw %in% # TRUE FALSE are read as 1 / 0... if all entries read TRUE, min is 1, else 0.
                 c("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"))==1){
   data_plot(xx,                       # uses data_plot function to draw raw data (loess for most parameters exect CO2 -->glm)
            filepath,
            draw = draw,
            save_plot = save_plot,
            show_plot = show_plot,
            draw.method=draw.method)
  }else if (draw!="none"){
    cat("\nWARNING:\t'draw' vector contains unknown values.\n\t\tSkipping plotting.")
  }

  return(output)
}
