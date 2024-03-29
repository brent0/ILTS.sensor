## NOTE: install dependancies (see Readme)

library(devtools)

pkg.env = new.env(parent = emptyenv())
assign('manual.archive', "", pkg.env)
assign('oracle.server', oracle.personal.server, pkg.env)
assign('oracle.user', oracle.personal.user, pkg.env)
assign('oracle.password', oracle.personal.password, pkg.env)

#' @title  init.project.vars
#' @description  Set global database variables by user account
#' @import tcltk gWidgets2 gWidgets2tcltk
#' @export
init.project.vars = function() {
  if(exists("bio.datadirectory.ilts")){
    assign('manual.archive', bio.datadirectory.ilts , pkg.env)
  }
  else{
    assign('manual.archive', gWidgets2::gfile(text = "Select project work directory", type = "selectdir"), pkg.env)

  }

  #Take from snowcrab users .Rprofile.site
  if(exists("oracle.snowcrab.server")){
    assign('oracle.server', oracle.snowcrab.server, pkg.env)
    assign('oracle.user', oracle.snowcrab.user, pkg.env)
    assign('oracle.password', oracle.snowcrab.password, pkg.env)
  }
  #Take from lobster users .Rprofile.site, CHANGE TO WHAT IT IS ACTUALLY CALLED
  if(exists("oracle.personal.server")){
    assign('oracle.server', oracle.personal.server, pkg.env)
    assign('oracle.user', oracle.personal.user, pkg.env)
    assign('oracle.password', oracle.personal.password, pkg.env)
  }
  #If still not set prompt for values
  if(pkg.env$oracle.server == ""){
    options(guiToolkit="tcltk")
    assign('oracle.server',  gWidgets2::ginput("Enter your Oracle server (exa. ptran):"), pkg.env)
    assign('oracle.user',  gWidgets2::ginput("Enter your Oracle username:"), pkg.env)
    assign('oracle.password',  gWidgets2::ginput("Enter your Oracle passsword:"), pkg.env)

  }
}

#' @title  esonar2df
#' @description  Change esonar data format into useable dataframe
#' @param esonar The esonar data to convert
#' @import lubridate
#' @return dataframe
#' @export
esonar2df = function(esonar = NULL) {
  names(esonar)
  #colnames(esonar) = c("CPUDateTime","GPSDate","GPSTime","Latitude","Longitude","Speed","Heading","Validity","TransducerName","SensorName","SensorValue","ErrorCode","Hydrophone","SignalStrength", "setno", "latedit", "trip", "datetime")

  esonar$primary = NA  #Headline
  esonar$secondary = NA #Is nothing but may need in file
  esonar$wingspread = NA
  #esonar$depth = NA
  #esonar$temperature = NA
  esonar$STBDRoll = NA
  esonar$STBDPitch = NA

  #esonar$depth[which(esonar$SensorName == "Depth")] = esonar$SensorValue[which(esonar$SensorName == "Depth")]
  esonar$primary[which(esonar$SensorName == "Headline")] = esonar$SensorValue[which(esonar$SensorName == "Headline")]
  esonar$wingspread[which(esonar$SensorName == "STBDDoorMaster")] = esonar$SensorValue[which(esonar$SensorName == "STBDDoorMaster")]
  #esonar$temperature[which(esonar$SensorName == "Temperature")] = esonar$SensorValue[which(esonar$SensorName == "Temperature")]
  esonar$STBDRoll[which(esonar$SensorName == "STBDRoll")] = esonar$SensorValue[which(esonar$SensorName == "STBDRoll")]
  esonar$STBDPitch[which(esonar$SensorName == "STBDPitch")] = esonar$SensorValue[which(esonar$SensorName == "STBDPitch")]

  esonar$CPUDATETIME = NULL
  esonar$TRANSDUCERNAME = NULL
  esonar$SENSORNAME = NULL
  esonar$SENSORVALUE = NULL
  esonar$HYDROPHONE = NULL
  esonar$SIGNALSTRENGTH = NULL
  esonar$VALIDITY = NULL
  esonar$ERRORCODE = NULL
  esonar$HEADING = NULL


  esonar <- esonar %>% select(GPSDATE,GPSTIME,LATITUDE,LONGITUDE,SPEED,SET_NO,DDLAT,TRIP_ID,timestamp,primary,secondary,wingspread,STBDRoll,STBDPitch)
  #####NOTE: DDLAT is probably the wrong column but didn't know what "latedit" was supposed to be so used DDLAT to fill that space, but doesn't seem to affect running of function - Geraint E.

  colnames(esonar) = c("Date","Time","Latitude","Longitude","Speed", "Setno", "latedit","Trip","timestamp", "Primary","Secondary","WingSpread","Roll", "Pitch")

  return(esonar)
}

#' @title  get.acoustic.releases
#' @description  Get data from Oracle Database table
#' @import ROracle DBI
#' @param tn tablename to get
#' @param oracle.user your oracle username
#' @param oracle.password your oracle password
#' @return dataframe
#' @export
get.oracle.table = function(tn = "",server = pkg.env$oracle.server, user =pkg.env$oracle.user, password = pkg.env$oracle.password){
  if(tn == "")stop("You must provide a tablename to 'get.oracle.table'!!")


  drv <- ROracle::Oracle()
  con <- ROracle::dbConnect(drv, username = user, password = password, dbname = server)
  res <- ROracle::dbSendQuery(con, paste("select * from ", tn, sep=""))
  res <- ROracle::fetch(res)
  ROracle::dbDisconnect(con)
  return(res)
}


#' @title  ilts.format.merge
#' @description  Get all data, format and merge.
#' @param  update TRUE: Add new startion and overwite any previous. FALSE: Skip previously completed stations.
#' @param user Unique user. This is used to keep user files seperate
#' @param years single or vector of years to process
#' @import netmensuration lubridate
#' @return list of lists. Format (top to bottom) year-set-data
#' @export
ilts.format.merge = function(update = TRUE, user = "", years = ""){
  #Set up database server, user and password
  init.project.vars()

  cont=TRUE
  if(user == "")stop("You must call this function with a user. exa. ilts.format.merge(update = TRUE, user = 'John'" )

  if(file.exists(file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")))){
    current = read.csv(file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")))
    current$station = as.character(current$station)
    current$trip = as.character(current$trip)
  }else{
    current=NULL
  }
  #Need this to come from rdata file based on user
  if(file.exists(file.path(pkg.env$manual.archive,  paste("iltsStats_",user, ".RDATA", sep = "")))){
    load(file.path(pkg.env$manual.archive,  paste("iltsStats_",user, ".RDATA", sep = "")))
  }else{
    iltsStats = list()
  }
  Sys.setenv(TZ = "America/Halifax")
  Sys.setenv(ORA_SDTZ = "America/Halifax")

  plotdata = F #Alternative plotting that we do not require

  #Pull in the sensor data, this will be formatted and looped thru by trip then set.
  esona = get.oracle.table(tn = "LOBSTER.ILTS_SENSORS")
  esona$GPSTIME[which(nchar(esona$GPSTIME)==5)] = paste("0", esona$GPSTIME[which(nchar(esona$GPSTIME)==5)], sep="")
  esona$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(esona$GPSDATE)), esona$GPSTIME, sep=" "), tz="UTC" )
  esona = esona[ order(esona$timestamp , decreasing = FALSE ),]
  err = which(is.na(esona$timestamp))
  if(length(err)>0)esona = esona[-err,]

  ##### Addition to solve LATITUDE data logging issues - Geraint E.
  library(tidyr)
  library(dplyr)
  esona = esona %>% mutate(LATITUDE = ifelse(substr(LATITUDE,5,5) %in% " ", paste0(substr(LATITUDE,1,4),substr(LATITUDE,6,nchar(LATITUDE))),
                                             ifelse(substr(LATITUDE,6,6) %in% " ", paste0(substr(LATITUDE,1,5),substr(LATITUDE,7,nchar(LATITUDE))),LATITUDE)))
  esona <- esona %>% mutate(LATITUDE = gsub("n","N",LATITUDE)) %>% mutate(LATITUDE = gsub("F","N",LATITUDE))
  esona = separate(esona, LATITUDE, c("a","b","c"), " ", remove = FALSE)
  esona <- esona %>% filter(!(b %in% ""))
  #test4 = esona %>% filter(!(c %in% "N"))
  esona <- esona %>% select(-a,-b,-c)

  #####
  esona$LATITUDE = format.lol(x = esona$LATITUDE)
  esona$LONGITUDE = format.lol(x = esona$LONGITUDE)
  #If specific years desired filter unwanted
  if(years != ""){
    years = as.character(years)
    yind = which(as.character(lubridate::year(esona$timestamp)) %in% years)
    if(length(yind)>0)esona = esona[yind,]
    if(length(esona$timestamp)==0)stop("No data found for your year selection!")
  }
  # mini = get.oracle.table(tn = "FRAILC.MINILOG_TEMP")
  # #rebuild datetime column as it is incorrect and order
  # mini$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(mini$TDATE)), mini$TIME, sep=" "), tz="UTC" )
  # mini = mini[ order(mini$timestamp , decreasing = FALSE ),]

  #seab = get.oracle.table(tn = "LOBSTER.ILTS_TEMPERATURE")

  seabf = get.oracle.table(tn = "LOBSTER.ILTS_TEMPERATURE")
  #rebuild datetime column as it is incorrect and order
  seabf$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(seabf$UTCDATE)), seabf$UTCTIME, sep=" "), tz="UTC" )
  seabf = seabf[ order(seabf$timestamp , decreasing = FALSE ),]

  #Loop through each esonar file to convert and merge with temp
  eson = split(esona, esona$TRIP_ID)
  for(i in 1:length(eson)){
    if(cont){ #Condition fails if program exited
      trip = data.frame(eson[[i]])
      trip = split(trip, trip$SET_NO)
      for(j in 1:length(trip)){
        if(cont){ #Condition fails if program exited
          set = data.frame(trip[[j]])
          set = esonar2df(set)

          #Dont continue if update is false and station is already complete

          if((paste(unique(na.omit(set$Setno)), unique(na.omit(set$Trip)), sep = ".") %in% paste(current$station, current$trip, sep = ".")) & (update==FALSE)){
            message(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Already added call with update = TRUE to redo.", sep = ""))
          } else{
            # minisub = NULL
            # mini.ind.0 = which(mini$timestamp>set$timestamp[1])[1]
            # mini.ind.1 = which(mini$timestamp>set$timestamp[length(set$timestamp)])[1]-1
            # if(!(is.na(mini.ind.0) | is.na(mini.ind.0))){
            #   minisub = mini[c(mini.ind.0:mini.ind.1),]
            #   #### Only keep relevant data, If you encounter a minilog file
            #   #    depth, add that column to the following line to catch that case
            #   if("depth" %in% names(minisub)){
            #     minisub = minisub[,which(names(minisub) %in% c("datetime", "TEMP_C", "depth"))]
            #     names(minisub) = c("temperature","depth","timestamp")
            #   }
            #   else{
            #     minisub = minisub[,which(names(minisub) %in% c("datetime", "TEMP_C"))]
            #     names(minisub) = c("temperature","timestamp")
            #   }
            # }

            seabsub = NULL
            #Get seabird indicies and extend ?? mins on either side so that depth profile isn't cut off
            seab.ind.0 = which(seabf$timestamp>set$timestamp[1]-lubridate::minutes(15))[1]
            seab.ind.1 = which(seabf$timestamp>set$timestamp[length(set$timestamp)]+lubridate::minutes(15))[1]-1

            if(!(is.na(seab.ind.0) | is.na(seab.ind.0))){

              seabsub = seabf[c(seab.ind.0:seab.ind.1),]
              seabsub = seabsub[,which(names(seabsub) %in% c("timestamp", "TEMPC", "DEPTHM"))]
              names(seabsub) = c("temperature","depth","timestamp")
            }

            if(is.null(seabsub) && is.null(minisub))stop(paste("No temperature/depth file found for trip - set:  ", unique(na.omit(set$Trip)), " - ", unique(na.omit(set$Setno)), sep=""))
        #    if(is.null(seabsub)) seabsub = minisub
            #Remove depths = <0 #Not sure why but came accross stations with low depth values mixed in with real bottom depths.
            seabsub$depth[which(seabsub$depth <= 2)] = NA

            #Merge sensor data.
            mergset = merge(seabsub, set, "timestamp", all = TRUE)
            #Build the full, unbroken timeseries and merge
            timestamp = data.frame(seq(min(mergset$timestamp), max(mergset$timestamp), 1))
            names(timestamp) = c("timestamp")
            mergset = merge(mergset, timestamp, "timestamp", all = TRUE)
            mergset$timestamp = lubridate::ymd_hms(as.character(mergset$timestamp), tz="UTC" )
            #Find deepest point and extend possible data from that out to 20min on either side
            aredown = mergset$timestamp[which(mergset$depth == max(mergset$depth, na.rm = T))]
            time.gate =  list( t0=as.POSIXct(aredown)-lubridate::dminutes(20), t1=as.POSIXct(aredown)+lubridate::dminutes(20) )

            # Build the variables need for the proper execution of the bottom contact function from
            # the netmensuration package
            bcp = list(
              station = unique(na.omit(set$Setno)),
              trip = unique(na.omit(mergset$Trip)),
              YR=as.character(unique(lubridate::year(mergset$timestamp))),
              user.interaction = TRUE,
              from.manual.archive = pkg.env$manual.archive,
              from.manual.file = file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")),
              id=paste("Trip:", unique(na.omit(mergset$Trip)), "  -  Set:", unique(na.omit(set$Setno)),sep=""),
              datasource="lobster",
              nr=nrow(mergset),
              tdif.min=2,
              tdif.max=25,
              time.gate=time.gate,
              depth.min=3,
              depth.range=c(-20,30),
              depthproportion=0.6,
              eps.depth=.5, ##Must set this low due to shallow tows resulting in small depth variability
              smooth.windowsize=5,
              modal.windowsize=5,
              noisefilter.trim=0.025,
              noisefilter.target.r2=0.85,
              noisefilter.quants=c(0.025, 0.975))

            bcp = netmensuration::bottom.contact.parameters( bcp ) # add other default parameters .. not specified above

            names(mergset) = tolower(names(mergset))
            mergset$opening = mergset$primary
            #Fix missing position data by repeating the last know position. No NA positions allowed in bottom.contact function
            for(k in 1:nrow(mergset)){
              if(k==1 && is.na(mergset$latitude[k])){
                mergset$latitude[k] = mergset$latitude[!is.na(mergset$latitude)][1]
                mergset$longitude[k] = mergset$longitude[!is.na(mergset$longitude)][1]
              }
              if(is.na(mergset$latitude[k])){
                mergset$latitude[k] = mergset$latitude[k-1]
                mergset$longitude[k] = mergset$longitude[k-1]
              }

              #Fix missing depth data by repeating the last know depth. No NA depth allowed in bottom.contact function
              if(k==1 && is.na(mergset$depth[k])){
                mergset$depth[k] = mergset$depth[!is.na(mergset$depth)][1]
              }
              if(is.na(mergset$depth[k])){
                mergset$depth[k] = mergset$depth[k-1]
              }
            }

            mergset$doorspread = mergset$wingspread

            #Try to recover from user termination in order to write the current stat list to file

            tryCatch(
              {
               # print(mergset$depth)
               browser()
                bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )

                if ( is.null(bc) || ( !is.null(bc$res)  && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  bcp$noisefilter.target.r2=0.75
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  mergset$depth = jitter( mergset$depth, amount = bcp$eps.depth/10 )
                  bcp$noisefilter.inlah.h = 0.1
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  mergset$depth = jitter( mergset$depth, amount = bcp$eps.depth/10 )
                  bcp$noisefilter.inla.h = 0.25
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                for(i in 1:5){
                  if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  eps.depth.backup = bcp$eps.depth
                  bcp$eps.depth = bcp$eps.depth - 0.01
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                  }
                 }
                }
                eps.depth.final = bcp$eps.depth
                bcp$eps.depth = eps.depth.backup
                message(paste("Clicktouchdown file updated in: ", pkg.env$manual.archive, sep=""))

              },
              error=function(cond) {
                cont <<- FALSE
                message(cond)

              }
            )


            if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
              warning(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Touchdown metrics could not be calculated. Possible reason: tow may be too shallow, resulting in too little variation in depth values; check that sd(mergset$depth) > bcp$eps.depth, currently function tries reducing eps.depth as low as ",eps.depth.final," before quitting", sep = ""), immediate. = TRUE)
            }

            iltsStats[[paste(unique(na.omit(set$Trip)), unique(na.omit(set$Setno)),sep=".")]] = bc
          }#END Update clause
        }
      }#END Set subset
    }
  }#END Trip subset
  save(iltsStats, file = file.path(pkg.env$manual.archive, paste("iltsStats_",user, ".RDATA", sep = "")))
  message(paste("Stats file saved to: ", pkg.env$manual.archive, sep=""))
}



#' @title  format.lol
#' @description  Take latitude or longitude character vector and convert to decimal degrees.
#' @param x latitude ot longitude values to convert
#' @param lol Specify if you are converting latitude or longitude
#' @import stringr
#' @return list of correct latitude or longitude
#' @export
format.lol = function(x = NULL){
  if(is.null(x))stop("You must specify values: x")
  x = as.character(x)
  x = matrix(unlist(strsplit(x, " ")), ncol = 3, byrow = T)
  rx = as.numeric(x[,1]) + (as.numeric(x[,2])/60)
  if(any(grepl("S", x[1,])) || any(grepl("W", x[1,]))){
    rx = abs(rx)*-1
  }
  return(rx)
}

#####Execute

ilts.format.merge(update = FALSE, user = "geraint", years = "2020" )

