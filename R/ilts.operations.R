
pkg.env = new.env(parent = emptyenv())
assign('manual.archive', "", pkg.env)
assign('oracle.server', "", pkg.env)
assign('oracle.user', "", pkg.env)
assign('oracle.password', "", pkg.env)
options(stringsAsFactors = F)

#' @title rename.df
#' @description renaming columns
#' @export
rename.df = function(x, n0, n1) {
  if(!length(n0)== length(n1)) stop('length of names and renames need to be the same length')
  for(i in 1:length(n0)){
    names(x)[which(names(x)==n0[i])] = n1[i]
  }
  return(x)
}

#' @title  init.project.vars
#' @description  Set global database variables by user account
#' @import tcltk gWidgets2 gWidgets2tcltk
#' @export
init.project.vars = function() {
  if(exists("bio.datadirectory.ilts")){
    assign('manual.archive', bio.datadirectory.ilts , pkg.env)
      }
  if(!exists("bio.datadirectory.ilts")){
    print('A window will open that you need to choose the working folder.')
    assign('manual.archive', gWidgets2::gfile(text = "Select project work directory", type = "selectdir"), pkg.env)
    }
  dir.create(pkg.env$manual.archive, showWarnings = F)
  
  #Take from snowcrab users .Rprofile.site
  if(exists("oracle.snowcrab.server")){
    assign('oracle.server', oracle.snowcrab.server, pkg.env)
    assign('oracle.user', oracle.snowcrab.user, pkg.env)
    assign('oracle.password', oracle.snowcrab.password, pkg.env)
  }
  #Take from lobster users .Rprofile.site, CHANGE TO WHAT IT IS ACTUALLY CALLED
  if(exists("oracle.lobster.server")){
    assign('oracle.server', oracle.lobster.server, pkg.env)
    assign('oracle.user', oracle.lobster.user, pkg.env)
    assign('oracle.password', oracle.lobster.password, pkg.env)
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

  esonar = rename.df(esonar, c( 'CPUDATEANDTIME','GPSTIME','LATITUDE','LONGITUDE','SPEED','HEADING','VALIDITY','TRANSDUCERNAME','SENSORNAME','SENSORVALUE','ERRORCODE','HYDROPHONE','SIGNALSTRENGTH','SET_NO','LATEDIT','TRIP_ID','GPSDATE','timestamp'),
   c("CPUDateTime","GPSTime","Latitude","Longitude","Speed","Heading","Validity","TransducerName","SensorName","SensorValue","ErrorCode","Hydrophone","SignalStrength", "setno", "latedit", "trip", "GPSDate","datetime"))

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

  esonar$CPUDateTime = NULL
  esonar$TransducerName = NULL
  esonar$SensorName = NULL
  esonar$SensorValue = NULL
  esonar$Hydrophone = NULL
  esonar$SignalStrength = NULL
  esonar$Validity = NULL
  esonar$ErrorCode = NULL
  esonar$Heading = NULL
 esonar = rename.df(esonar, c('GPSTime','Latitude','Longitude','Speed','setno','latedit','trip','GPSDate','datetime','primary','secondary','wingspread','STBDRoll','STBDPitch'),
                            c("Time","Latitude","Longitude","Speed", "Setno", "latedit","Trip","Date","timestamp", "Primary","Secondary","WingSpread","Roll", "Pitch"))

  return(esonar)
}

#' @title  get.oracle.table
#' @description  Get data from Oracle Database table
#' @import ROracle DBI RODBC
#' @param tn tablename to get
#' @param oracle.user your oracle username
#' @param oracle.password your oracle password
#' @return dataframe
#' @export
get.oracle.table = function(tn = "",server = pkg.env$oracle.server, user =pkg.env$oracle.user, password = pkg.env$oracle.password, RODBC=F){
  if(tn == "")stop("You must provide a tablename to 'get.oracle.table'!!")
if(!RODBC){
  drv <- dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = user, password = password, dbname = server)
  res <- ROracle::dbSendQuery(con, paste("select * from ", tn, sep=""))
  res <- fetch(res)
  ROracle::dbDisconnect(con)
} 
if(RODBC)  {
  drv = odbcConnect(dsn = server ,uid = user, pwd = password)
  res = sqlQuery(drv,paste("select * from ", tn,";", sep=""))
  odbcCloseAll()
  }
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
ilts.format.merge = function(update = TRUE, user = "", years = "", use_RODBC=F, use_local=F, depth.only.plot=F, sensor.file='ILTS_SENSORS_TEMP.csv', minilog.file = 'MINILOG_TEMP.csv', seabird.file = 'ILTS_TEMPERATURE.csv'){
  #Set up database server, user and password
  init.project.vars()
  options(stringsAsFactors=F)

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
  dir.create(file.path(pkg.env$manual.archive,'raw'), showWarnings = F)
  
  if(!use_local) {  esona = get.oracle.table(tn = "FRAILC.ILTS_SENSORS_TEMP", RODBC=use_RODBC)
                    write.csv(esona,file=file.path(pkg.env$manual.archive, 'raw', 'ILTS_SENSORS_TEMP.csv'),row.names = F)
                  }      
  if(use_local)    esona = read.csv(file.path(pkg.env$manual.archive, 'raw', sensor.file))
  
  esona$GPSTIME[which(nchar(esona$GPSTIME)==5)] = paste("0", esona$GPSTIME[which(nchar(esona$GPSTIME)==5)], sep="")
  esona$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(esona$GPSDATE)), esona$GPSTIME, sep=" "), tz="UTC" )
  esona = esona[ order(esona$timestamp , decreasing = FALSE ),]
  err = which(is.na(esona$timestamp))
  if(length(err)>0){
    cat(paste('Errors in time stamps for TRIP_NO--SETS'),unique(paste(esona$TRIP_ID[err],'--',esona$SET_NO[err],sep="")))
     esona = esona[-err,]
    }
  esona$LATITUDE = format.lol(x = esona$LATITUDE)
  esona$LONGITUDE = format.lol(x = esona$LONGITUDE)
  #If specific years desired filter unwanted
  if(years != ""){
    years = as.character(years)
    yind = which(as.character(lubridate::year(esona$timestamp)) %in% years)
    if(length(yind)>0)esona = esona[yind,]
    if(length(esona$timestamp)==0)stop("No data found for your year selection!")
  }
  #if(!use_local)  {
  #    mini = get.oracle.table(tn = "FRAILC.MINILOG_TEMP", RODBC = use_RODBC)
  #    write.csv(mini,file=file.path(pkg.env$manual.archive, 'raw', 'MINILOG_TEMP.csv'),row.names = F)
  #  } 
  #if(use_local)    mini = read.csv(file.path(pkg.env$manual.archive, 'raw', minilog.file))
  
  #rebuild datetime column as it is incorrect and order
  #mini$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(mini$TDATE)), mini$TIME, sep=" "), tz="UTC" )
  #mini = mini[ order(mini$timestamp , decreasing = FALSE ),]

  #seab = get.oracle.table(tn = "LOBSTER.ILTS_TEMPERATURE")
  if(!use_local)    {
    seabf = get.oracle.table(tn = "frailc.ILTS_TEMPERATURE",RODBC = use_RODBC)
    write.csv(seabf,file=file.path(pkg.env$manual.archive, 'raw', 'ILTS_TEMPERATURE.csv'),row.names = F)
    }
  if(use_local)    seabf = read.csv(file.path(pkg.env$manual.archive, 'raw', seabird.file))
  
  #rebuild datetime column as it is incorrect and order
  seabf$UTCTIME[which(nchar(seabf$UTCTIME)==5)] = paste("0", seabf$UTCTIME[which(nchar(seabf$UTCTIME)==5)], sep="")
  seabf$UTCTIME[which(nchar(seabf$UTCTIME)==4)] = paste("00", seabf$UTCTIME[which(nchar(seabf$UTCTIME)==4)], sep="")
  seabf$UTCTIME[which(nchar(seabf$UTCTIME)==3)] = paste("000", seabf$UTCTIME[which(nchar(seabf$UTCTIME)==3)], sep="")
  seabf$UTCTIME[which(nchar(seabf$UTCTIME)==2)] = paste("0000", seabf$UTCTIME[which(nchar(seabf$UTCTIME)==2)], sep="")
  seabf$UTCTIME[which(nchar(seabf$UTCTIME)==1)] = paste("00000", seabf$UTCTIME[which(nchar(seabf$UTCTIME)==1)], sep="")
  
  seabf$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(seabf$UTCDATE)), seabf$UTCTIME, sep=" "), tz="UTC" )
  seabf = seabf[ order(seabf$timestamp , decreasing = FALSE ),]
  #Loop through each esonar file to convert and merge with temp
  eson = split(esona, esona$TRIP_ID)
  seabf$id = paste(seabf$TRIP_ID,seabf$SET_NO,sep="_")
  
  for(i in 1:length(eson)){
    if(cont){ #Condition fails if program exited
      trip = data.frame(eson[[i]])
      trip = split(trip, trip$SET_NO)
      for(j in 1:length(trip)){
        if(cont){ #Condition fails if program exited
          set = data.frame(trip[[j]])
          set = esonar2df(set)
          set$id = paste(set$Trip, set$Setno, sep="_")
          sseab = subset(seabf, id == na.omit(unique(set$id))) 
          sseab = sseab[order(sseab$timestamp),]
          #Dont continue if update is false and station is already complete

          if((paste(unique(na.omit(set$Setno)), unique(na.omit(set$Trip)), sep = ".") %in% paste(current$station, current$trip, sep = ".")) & (update==FALSE)){
            message(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Already added call with update = TRUE to redo.", sep = ""))
          } else{
         #   minisub = NULL
        #    mini.ind.0 = which(mini$timestamp>set$timestamp[1])[1]
        #    mini.ind.1 = which(mini$timestamp>set$timestamp[length(set$timestamp)])[1]-1
        #    if(!(is.na(mini.ind.0) | is.na(mini.ind.0))){
        #      minisub = mini[c(mini.ind.0:mini.ind.1),]
              #### Only keep relevant data, If you encounter a minilog file
              #    depth, add that column to the following line to catch that case
        #      if("depth" %in% names(minisub)){
        #        minisub = minisub[,which(names(minisub) %in% c("datetime", "TEMP_C", "depth"))]
        #        names(minisub) = c("temperature","depth","timestamp")
        #      }
        #      else{
         #       minisub = minisub[,which(names(minisub) %in% c("datetime", "TEMP_C"))]
        #        names(minisub) = c("temperature","timestamp")
         #     }
         #   }
             seabsub = NULL
            #Get seabird indicies and extend ?? mins on either side so that depth profile isn't cut off
            #seab.ind.0 = which(seabf$timestamp>set$timestamp[1]-lubridate::minutes(15))[1]
            #seab.ind.1 = which(seabf$timestamp>set$timestamp[length(set$timestamp)]+lubridate::minutes(15))[1]-1
            #if(is.na(seab.ind.1)) seab.ind.1 = which(seabf$timestamp>set$timestamp[length(set$timestamp)]+lubridate::minutes(4))[1]-1
            
            #   if(!(is.na(seab.ind.0) | is.na(seab.ind.0))){
            #   seabsub = seabf[c(seab.ind.0:seab.ind.1),]
            seabsub = sseab
               seabsub = seabsub[,which(names(seabsub) %in% c("timestamp", "TEMPC", "DEPTHM"))]
              names(seabsub) = c("temperature","depth","timestamp")
            #  }
            if(is.null(seabsub)){ #&& is.null(minisub)){
              print(paste("No temperature/depth file found for trip - set:  ", unique(na.omit(set$Trip)), " - ", unique(na.omit(set$Setno)), sep=""))
              next()
            }
            #if(is.null(seabsub)) seabsub = minisub
            #Remove depths = <0 #Not sure why but came accross stations with low depth values mixed in with real bottom depths.
            seabsub$depth[which(seabsub$depth <= 2)] = NA
            mergset = NULL
            #Merge sensor data.
            mergset = merge(seabsub, set, "timestamp", all = TRUE)
            #Build the full, unbroken timeseries and merge
             timestamp = data.frame(seq(min(mergset$timestamp), max(mergset$timestamp), 1))
            names(timestamp) = c("timestamp")
            mergset = merge(mergset, timestamp, "timestamp", all = TRUE)
            mergset$timestamp = lubridate::ymd_hms(as.character(mergset$timestamp), tz="UTC" )
            #Find deepest point and extend possible data from that out to 20min on either side
           print(paste(unique(mergset$Trip), unique(mergset$Setno)))
            
            if(all(na.omit(unique(mergset$Trip))=='100051989' && na.omit(unique(mergset$Setno))==102))browser()
             NoDeps = all(is.na(mergset$depth))
            if(NoDeps) cat("\n",paste('No depth info for Trip-Setno='),unique(paste(mergset$Trip,mergset$Setno,sep="-")))
            if(!NoDeps){
             aredown = mergset$timestamp[which(mergset$depth == max(mergset$depth, na.rm = T))]
            time.gate =  list( t0=as.POSIXct(aredown)-lubridate::dminutes(20), t1=as.POSIXct(aredown)+lubridate::dminutes(20) )

            if(depth.only.plot){
              pdf(file.path(pkg.env$manual.archive, paste(unique(na.omit(mergset$Trip)), unique(na.omit(mergset$Setno)), 'pdf',sep=".")))
              with(subset(mergset, !is.na(depth)),plot(timestamp,depth, type='l'))
              dev.off()
              write.csv(mergset,file=file.path(pkg.env$manual.archive, paste(unique(na.omit(mergset$Trip)), unique(na.omit(mergset$Setno)), 'csv',sep=".")))
              next()
            }

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
                bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )

                if ( is.null(bc) || ( !is.null(bc$res)  && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  bcp$noisefilter.target.r2=0.75
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  M$depth = jitter( M$depth, amount = bcp$eps.depth/10 )
                  bcp$noisefilter.inla.h = 0.1
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  M$depth = jitter( M$depth, amount = bcp$eps.depth/10 )
                  bcp$noisefilter.inla.h = 0.25
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )

                }
                message(paste("Clicktouchdown file updated in: ", pkg.env$manual.archive, sep=""))

              },
              error=function(cond) {
                cont <<- FALSE
                message(cond)

              }
            )


            if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
              warning(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Touchdown metrics could not be calculated.", sep = ""))

            }

            iltsStats[[paste(unique(na.omit(set$Trip)), unique(na.omit(set$Setno)),sep=".")]] = bc
           } #end if no depth
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
