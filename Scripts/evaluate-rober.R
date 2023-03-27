# Passo 1: Carrega os pacotes que serao usados
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(shiny, ggpubr, utils, readr, ggplot2, dplyr, SEMACHcalc)

# Passo 2: Carregar o directorio
# inclui o "directory" 2021-01-jul-floresta2, Vai mudando para cada data de analise. 

directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2021-01-jul-floresta2"
ls()
rm(directory)
directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2021-30-jun-floresta"
directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2021-30-jun-rec-arl"
directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2019-28-nov-rec-arl"
directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2019-28-nov-floresta"
directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2019-16-jul-floresta"
directory<-"C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2019-11-jul-rec-arl"

# Passo 3: Carregar as funcoes que serao usadas

# Funcao fluxcalc.R
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

########### Funcao bath_processor.R
#' Batch processor
#'
#' Allows quick processing of multiple SEMACH-output-files. Either default parameters (1min-5min)
#' or custom time interval and chamber dimension settings. All .txt-files have to be situated in the same folder. Returns dataframe with averages over selected measurement timeframe, CO2 accumulation rate (ppm/min) and CO2 flux (µmol m-2 s-1)
#' @param directory Path of folder containing data to be processed.
#' @param filenames Vector of actual filenames of the .txt-files to be processed. Only filename, do NOT add .txt (e.g., "filename.txt")
#' @param method Either "default" (also literally default), "fixed", or "individual". Default uses default chamber proportions (area, volume) and time intervals (1min-5min).
#' Fixed allows customisation of several or all parameters to a fixed value different to the default. Input through A_chamber, V_chamber, start and end. If no parameters are edited, use default instead.
#' Individual allows for individual parameter customisation for each .txt-file. Requires input of metadata file. If no parameters are edited, use default instead.
#' @param start Starting point of evaluation in seconds after start of recording. Used for method="fixed"
#' @param end End point of evaluation in seconds after start of recording. Used for method="fixed"
#' @param V_chamber Chamber volume in m³. Useful e.g., when chamber volume is extended by base ring. Used for method="fixed"
#' @param A_chamber Chamber base area in m². Implemented for completion, usually not changed. Used for method="fixed"
#' @param metafile data frame containing 5 columns:
#' 1 - 'filenames' As used in filenames parameter. Each measurement file has it's own parameter row.
#' 2 - 'V_chamber' Deviating chamber volume in m³ for each measurement file. If default, set to NA or enter default manually.
#' 3 - 'A_chamber' Deviating chamber area in m² for each measurement file. If default, set to NA or enter default manually.
#' 4 - 'start' Starting point of evaluation in seconds after start of recording for each measurement file. If default, set to NA or enter default manually.
#' 5 - 'end' Endpoint of evaluation in seconds after start of recording for each measurement file. If default, set to NA or enter default manually.
#' @param draw Which variables should be included in plot? Default is c("none").
#'  Other possible inputs are vector containing variable names
#'  ("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"). To plot all, use c("all") instead.
#' @param save_plot Should the plot be saved? T/F Default is F.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.

batch_processor <-
  function(directory,
           #path including folder name containing raw SEMACH data (as txt) as string
           filenames,
           # vector containing filenames as string (only names, no .txt necessary)
           method = "default",
           # default uses data from 1min to 5min and default chamber dimensions
           start = NA,
           # fixed value manipulation of parameters
           end = NA,
           A_chamber = NA,
           V_chamber = NA,
           metafile,
           draw=c("none"),
           save_plot=F,
           show_plot=F,
           draw.method = "svg") {
    # alternatively specific values for every filename
    
    if(substr(directory,nchar(directory),nchar(directory))!="/"){ # adds '/' if not last character of directory string already. Skipps if user already used '/' as last character
      directory<-paste(directory,"/",sep="")
    }
    
    if (method == "default") { ## default #####################################################################################################
      
      output <- data.frame()
      for (i in filenames) {
        output <-
          rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                         paste(directory, i, ".txt", sep = "")
          )))
      }
      names(output) <-
        c(
          "Measuring point",
          "Date",
          "Time",
          "CO2 [ppm/min]",
          "T_i [°C]",
          "P_i [hPa]",
          "rH_i [%]",
          "SM [%]",
          "ST [°C]",
          "rH_o [%]",
          "T_o [°C]",
          "P_o [hPa]",
          "PAR [µmol m-2 s-1]",
          "F_CO2 [µmol m-2 s-1]",
          "V_ch [m3]",
          "A_ch [m2]",
          "start [s]",
          "end [s]"
          
        )
      return(output)
      
      
    } else if (method == "fixed") { ## fixed ##########################################################################################################
      #fixed val manipul.
      output <- data.frame()
      if (is.na(A_chamber) == F &
          is.na(V_chamber) == F & is.na(start) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = A_chamber,
                        V_chamber = V_chamber,
                        start = start,
                        end = end
              )
            ))
        }
      } else if (is.na(V_chamber) == F &
                 is.na(start) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        V_chamber = V_chamber,
                        start = start,
                        end = end
              )
            ))
        }
      } else if (is.na(A_chamber) == F &
                 is.na(start) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = A_chamber,
                        start = start,
                        end = end
              )
            ))
        }
      } else if (is.na(A_chamber) == F &
                 is.na(V_chamber) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = A_chamber,
                        V_chamber = V_chamber,
                        end = end
              )
            ))
        }
      } else if (is.na(A_chamber) == F &
                 is.na(V_chamber) == F & is.na(start) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = A_chamber,
                        V_chamber = V_chamber,
                        start = start
              )
            ))
        }
      } else if (is.na(start) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""),
                                         start = start,
                                         end = end
            )))
        }
      } else if (is.na(A_chamber) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""),
                                         A_chamber = A_chamber,
                                         end = end
            )))
        }
      } else if (is.na(A_chamber) == F & is.na(V_chamber) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = A_chamber,
                        V_chamber = V_chamber
              )
            ))
        }
      } else if (is.na(V_chamber) == F & is.na(start) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        V_chamber = V_chamber,
                        start = start
              )
            ))
        }
      } else if (is.na(A_chamber) == F & is.na(start) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = A_chamber,
                        start = start
              )
            ))
        }
      } else if (is.na(V_chamber) == F & is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""),
                                         V_chamber = V_chamber,
                                         end = end
            )))
        }
      } else if (is.na(A_chamber) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), A_chamber = A_chamber
            )))
        }
      } else if (is.na(V_chamber) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), V_chamber = V_chamber
            )))
        }
      } else if (is.na(start) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), start = start
            )))
        }
      } else if (is.na(end) == F) {
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), end = end
            )))
        }
      } else{
        cat("Error, Method 'fixed' invalid.\nPlease use method 'default' or enter parameters.\n")
      }
      names(output) <-
        c(
          "Measuring point",
          "Date",
          "Time",
          "CO2 [ppm/min]",
          "T_i [°C]",
          "P_i [hPa]",
          "rH_i [%]",
          "SM [%]",
          "ST [°C]",
          "rH_o [%]",
          "T_o [°C]",
          "P_o [hPa]",
          "PAR [µmol m-2 s-1]",
          "F_CO2 [µmol m-2 s-1]",
          "V_ch [m3]",
          "A_ch [m2]",
          "start [s]",
          "end [s]"
          
        )
      return(output)
      
      
    } else if (method == "individual") { ## individual #################################################################################################################################
      # specified vals for each file
      output <- data.frame()
      
      for (i in filenames) {
        j<-as.numeric(row.names(metafile[which(metafile[1]==i),]))
        if (is.na(metafile[j,3]) == F &
            is.na(metafile[j,2]) == F & is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
          cat("A, V, start, and end given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = metafile[j,3],
                        V_chamber = metafile[j,2],
                        start = metafile[j,4],
                        end = metafile[j,5]
              )
            ))
          
        } else if (is.na(metafile[j,2]) == F &
                   is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
          cat("V, start, and end given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        V_chamber = metafile[j,2],
                        start = metafile[j,4],
                        end = metafile[j,5]
              )
            ))
          
        } else if (is.na(metafile[j,3]) == F &
                   is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
          cat("A, start, and end given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = metafile[j,3],
                        start = metafile[j,4],
                        end = metafile[j,5]
              )
            ))
          
        } else if (is.na(metafile[j,3]) == F &
                   is.na(metafile[j,2]) == F & is.na(metafile[j,5]) == F) {
          cat("A, V, and end given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = metafile[j,3],
                        V_chamber = metafile[j,2],
                        end = metafile[j,5]
              )
            ))
          
        } else if (is.na(metafile[j,3]) == F &
                   is.na(metafile[j,2]) == F & is.na(metafile[j,4]) == F) {
          cat("A, V, and start given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = metafile[j,3],
                        V_chamber = metafile[j,2],
                        start = metafile[j,4]
              )
            ))
          
        } else if (is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
          cat("start and end given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""),
                                         start = metafile[j,4],
                                         end = metafile[j,5]
            )))
          
        } else if (is.na(metafile[j,3]) == F & is.na(metafile[j,5]) == F) {
          cat("A and end given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""),
                                         A_chamber = metafile[j,3],
                                         end = metafile[j,5]
            )))
          
        } else if (is.na(metafile[j,3]) == F & is.na(metafile[j,2]) == F) {
          cat("A and V given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = metafile[j,3],
                        V_chamber = metafile[j,2]
              )
            ))
          
        } else if (is.na(metafile[j,2]) == F & is.na(metafile[j,4]) == F) {
          cat("V and start given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        V_chamber = metafile[j,2],
                        start = metafile[j,4]
              )
            ))
          
        } else if (is.na(metafile[j,3]) == F & is.na(metafile[j,4]) == F) {
          cat("A and start given\n")
          output <-
            rbind(output, c(
              i,
              flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                          paste(directory,  i, ".txt", sep = ""),
                        A_chamber = metafile[j,3],
                        start = metafile[j,4]
              )
            ))
          
        } else if (is.na(metafile[j,2]) == F & is.na(metafile[j,5]) == F) {
          cat("V and end given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""),
                                         V_chamber = metafile[j,2],
                                         end = metafile[j,5]
            )))
          
        } else if (is.na(metafile[j,3]) == F) {
          cat("A given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), A_chamber = metafile[j,3]
            )))
          
        } else if (is.na(metafile[j,2]) == F) {
          cat("V given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), V_chamber = metafile[j,2]
            )))
          
        } else if (is.na(metafile[j,4]) == F) {
          cat("start given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), start = metafile[j,4]
            )))
          
        } else if (is.na(metafile[j,5]) == F) {
          cat("end given\n")
          output <-
            rbind(output, c(i, flux_calc(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                           paste(directory,  i, ".txt", sep = ""), end = metafile[j,5]
            )))
          
        } else{
          cat("Error, Method 'individual' is invalid.\nPlease use method 'default' or enter parameters.\n")
        }
      }
      
      names(output) <-
        c(
          "Measuring point",
          "Date",
          "Time",
          "CO2 [ppm/min]",
          "T_i [°C]",
          "P_i [hPa]",
          "rH_i [%]",
          "SM [%]",
          "ST [°C]",
          "rH_o [%]",
          "T_o [°C]",
          "P_o [hPa]",
          "PAR [µmol m-2 s-1]",
          "F_CO2 [µmol m-2 s-1]",
          "V_ch [m3]",
          "A_ch [m2]",
          "start [s]",
          "end [s]"
          
        )
      return(output)
      
    } else{
      #wrong method
      print("Error, Method invalid.")
      return()
    }
  }


########### Funcao evaluate.R

#Evaluation of single folder measurement campaign with metafile as excel sheet (with specs as explained in SEMACHcalc::batch_processor)

#' Evaluate
#'
#' Takes folder directory containing SEMACH raw data files (.txt) AND a metadata file called "metadata" (.xlsx).
#' The metadata file should be constructed as described in SMEACHcalc::batch_processor and contain all raw data files in the
#' folder (which the user wants to evaluate) without the .txt ending. The sheet name in the excel file has to be "Tabelle1". The function calculates flux and averages as described in SMACHcalc::batch_processor
#' for each raw file and returns values as data frame. When save == TRUE also creates csv-file called "summary" in the directory.
#' @param directory Folder directory of folder that contains data and metafile. Example: "C:/Project/Data/Folder" or "C:/Project/Data/Folder/".
#' @param save Should summary.csv be created in directory? Default == TRUE.
#' @param draw Which variables should be included in plot? Default is c("none").
#'  Other possible inputs are vector containing variable names
#'  ("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"). To plot all, use c("all") instead.
#' @param save_plot Should the plot be saved? T/F Default is F.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.


evaluate <- function(directory,save=TRUE,
                     draw=c("none"),
                     save_plot=F,
                     show_plot=F,
                     draw.method="svg") {
  
  if (substr(directory, nchar(directory), nchar(directory)) != "/") {
    directory <- paste(directory, "/", sep = "")
  }
  # old read-in
  # metafile <- as.data.frame(readxl::read_excel(
  #  paste(directory, "metafile.xlsx", sep = ""), # be sure to create metafile
  # cell_cols(A:D),
  #  col_types = c("text", "numeric", "numeric", "numeric", "numeric"),
  #  na = "NA"
  #))
  
  # new read in
  metafile <- as.data.frame(readxl::read_excel(
    paste(directory, "metafile.xlsx", sep = ""), # be sure to create metafile
    sheet = "Tabelle1",  # be sure that metafile sheet is named Tabelle1
    cellranger::cell_cols(c("A","F")), # only required cols are read in
    col_types = c("text", "numeric", "numeric", "numeric", "numeric","text"),
    na = "NA"
  ))
  metafile<-metafile[1:5]  # cropping excess col (is used because cell_cols automatically applies rm_NA for 1st and last col)
  
  output <-
    batch_processor(
      directory = directory,
      filenames = metafile[[1]],
      metafile = metafile,
      method = "individual",
      draw=draw,
      save_plot=save_plot,
      show_plot=show_plot,
      draw.method=draw.method
    )
  if(save==TRUE){
    write.table(output,file=paste(directory,"summary.csv",sep=""),dec=".",sep=",",row.names = F) #because excel is weired
  }
  return(output)
}

############# Funcao dataplot.R

#'data_plot
#'
#'Plots raw SEMACH data and saves it (as svg) and / or displays it on console if desired.
#'Mostly for internal use.
#'
#' @param xx Subset of SEMACH raw data to be drawn as internally created by flux.calc. If the function is used
#' manually, pease be sure to add t_sec column, with the respective measurement time in seconds. Furthermore the columns
#' as described for draw should be included and named accordingly.
#' @param filepath Path from which data is loaded. E.g., "E:/Folder/measurement.txt"
#' @param draw Which variables should be included in plot? Default is c("all").
#' Other possible inputs are vector containing variable names.
#' ("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"), or empty vector c().
#' @param save_plot Should the plot be saved? T/F Default is T.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.

data_plot <- function(xx,
                      filepath,
                      draw = c("all"),   # if no plotting in flux.calc is desired, externally skipping (no "empty" case required)
                      save_plot = F,
                      show_plot = T,
                      draw.method = "svg") {
  ## plotter
  require(ggplot2)
  require(ggpubr)
  # for some reason .5 is rounded down. This give needed row amount
  
  if (draw == "all") {
    draw <- c("rH_i",
              "T_i",
              "P_i",
              "SM",
              "ST",
              "CO2",
              "rH_o",
              "T_o",
              "P_o",
              "PAR")
  }
  
  
  # kind of local dictionary for ylab names and units
  unit <- list(
    "%",
    "\u00b0C",
    "hPa",
    "%",
    "\u00b0C",
    "ppm",
    "%",
    "\u00b0C",
    "hPa",
    expression(paste(mu,"mol ",m^-2," ",s^-1))
  )
  names(unit)<-
    c(
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
  
  titles<-list(
    "rH in",
    expression(paste(T[air]," in")),
    "P in",
    "SM",
    "ST",
    expression(CO[2]),
    "rH out",
    expression(paste(T[air]," out")),
    "P out",
    "PAR"
  )
  names(titles)<-
    c(
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
  
  plotlist <- vector("list", length(draw)) #list with draw names to store individual plots (and use to create final plot later)
  names(plotlist) <- draw
  
  for (i in draw) {
    if (i == "CO2") { #"CO2" with glm, others with mean and loess
      slope<-paste("  ",format(lm(data = xx, CO2 ~ t_sec)$coefficients[[2]] * 60, digits = 3),
                   " ppm/min",
                   sep = "")
      plot <- ggplot(xx, aes(x = t_sec, y = .data[[i]])) +
        geom_point() +
        geom_smooth(method = "glm") +
        # geom_text(aes(                     # slope ppm/min as text in plot with positioning; maybe ajust for low or negative slopes
        #   x = .9 * max(t_sec),               # geom_text is original version, new as part of ggtitle
        #   y = quantile(CO2, .2, names = F),
        #   label = paste(format(
        #     lm(data = xx, CO2 ~ t_sec)$coefficients[[2]] * 60, digits = 3
        #   ),
        #   " ppm/min", sep = "")
        # ))
        xlab("s")+
        ylab(unit[[i]])+
        ggtitle(bquote(paste(CO[2],"  ",.(slope))))+    # bquote allows to include variable with .(), unlike expression
        theme(
          title=element_text(size=7)
        )#,axis.title = element_text(size=10))
      plotlist[[i]] <- plot
    } else{
      plot <- ggplot(xx, aes(x = t_sec, y = .data[[i]])) +
        geom_point() +
        geom_smooth(method = "gam") +
        geom_abline(slope = 0,
                    intercept = mean(xx[[i]]),
                    col = "red")+
        xlab("s")+
        ylab(unit[[i]])+
        ggtitle(titles[[i]])+
        theme(title=element_text(size=7))#,axis.title = element_text(size=10))
      plotlist[[i]] <- plot
    }
  }
  
  
  #  strtrim(basename(filepath),4) # extracting filename without .txt
  
  PLOT <- ggarrange(plotlist = plotlist,
                    ncol = 2,
                    nrow = round(length(draw) / 2 + .1))
  PLOT <-
    annotate_figure(
      PLOT,
      top = "",
      fig.lab = paste(basename(strtrim(
        filepath, nchar(filepath) - nchar(basename(filepath))
      )),
      strtrim(basename(filepath), 4)),
      fig.lab.face = "bold",
      fig.lab.size = 12
    )
  
  if (save_plot == T&"svg"%in%draw.method==T) {          # using %in% allows for use of charater or vector containing both
    svg(paste(
      strtrim(filepath, nchar(filepath) - nchar(basename(filepath))),
      strtrim(basename(filepath), 4),
      ".svg",
      sep = ""
    ))
    plot(PLOT)
    dev.off()
  }
  
  
  if (save_plot == T&"jpeg"%in%draw.method==T) {
    jpeg(paste(
      strtrim(filepath, nchar(filepath) - nchar(basename(filepath))),
      strtrim(basename(filepath), 4),
      ".jpeg",
      sep = ""
    ),quality = 100)  # a bit less compression than default
    plot(PLOT)
    dev.off()
  }
  
  
  
  
  if (show_plot == T) {
    plot(PLOT)
  }
}


########################
# Passo 4:  FAzer os procedimentos para os calculos
evaluate(directory) # o resultado vai para o "summary" no mesmo diretorio
result1<-evaluate(directory)
ls(result1)
#######################
##Para ver os graficos por anel e por repeticao
flux_calc("C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_semach\\R-analise-co2-Boca\\dados\\2019-11-jul-rec-arl\\anel1rep1.txt")

# data_plot   Nao consegui...

help(data_plot)


