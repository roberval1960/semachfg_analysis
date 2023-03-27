#' full.dataset function
#'
#' Creates one file from single campain data summarys. If desired evaluate can be rerun (summary only). Following structure is required:
#' Main folder containing campaign folders. Campaign folders contain raw data and metafile (as described in evaluate function), possiby already summary file.
#' All non-desired paths/subfolders/files in main folder need to be named in exclude, in form of a vector containing the paths.
#' Individual measurments must be named as Rx.y, where x is the ring number and y is the repetition. The location must be given as first letter of the campaign data folders.
#'
#'@param directory Main folder path (e.g., "C:/User/folder/MAIN_FOLDER/")
#'@param exclude Vector containing all undesired paths in main folder, default is empty. E.g., c("bad_file.csv","bad_folder")
#'@param evaluate Should the evaluate function be rerun for all campaign data (summary only)? Default is FALSE.
#'
#'



full.dataset<-function(directory,exclude=c("none"),evaluate=F){   #directory is folder contatining all data from a project (subfolders with campaign data, which was evaluated with SEMACHcalc::evaluate)
  require(readr)
  if(substr(directory,nchar(directory),nchar(directory))!="/"){ # adds '/' if not last character of directory string already. Skipps if user already used '/' as last character
    directory<-paste(directory,"/",sep="")
  }

  folders<-list.files(directory)
  folders<-subset(folders,folders%in%exclude==F) # exclude unwanted files

  dataset<-data.frame()

  for (i in folders){
    if (evaluate==T){

      evaluate(paste(directory,i,sep=""),draw=c("none"))
    }

    path<-paste(directory,i,"/summary.csv",sep="")
    print(path)

    campaign_data<-read.csv(path)
    #print(campaign_data)

    loc<-rep(strtrim(i,1),length(campaign_data[[1]]))
    ring<-substr(campaign_data$Measuring.point,2,2)
    campaign_data<-cbind(campaign_data,loc,ring)
    dataset<-rbind.data.frame(dataset,campaign_data)

  }

  names(dataset)<-c("MP","Date","Time","CO2_ppm","T_i","P_i","rH_i","SM","ST","rH_o",
                    "T_o","P_o","PAR","F_CO2","V_ch","A_ch","start","end","location","ring")
  dataset$Date<-parse_date(dataset$Date,format = "%d-%b-%y",locale=locale("en"))

  return(dataset)

}
