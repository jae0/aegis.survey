
### NOTE: Needs considerable simplification .. dump of marfisci.* methods .. mostly useful for the SQL ... TODO 

landings_db = function(DS, p) {

  #\\wrapper around Mike McMahon's marfisci tools

  ldbdir = project.datadirectory("aegis", "landings", "raw_data" )

  if (DS %in% c("rawdata", "rawdata.redo")) {
    if (DS=="rawdata") {
      out = NULL
      for (yr in p$marfis.years) {
        fn = file.list( ldbdir, paste( "rawdata", yr, "rdz", sep="." ))
        if ( file.exists(fn)) {
          ldb = NULL
          ldb = read_write_fast(fn)
          out = rbind( out, ldb )
        }
      }
      return(out)
    }

    channel <- ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname="PTRAN", username = oracle.personal.username,  password = oracle.personal.password)

    #  ldb = marfissci.get.data(years=yr, get.nonlandings=TRUE, save.csv=FALSE)


    for (yr in p$marfis.years) {
      fn = file.list( ldbdir, paste("rawdata", yr, "rdz", sep="." ))

      spp.tweak = ""
      gear.tweak = ""
      years.tweak = ""

    if (!is.null(spp)) {
      spp.tweak = paste0("AND SPECIES_CODE IN (",paste(spp, collapse = ","),")")
    } 
    
    if (!is.null(gear)) {
      gear.tweak = paste0("AND GEAR_CODE IN (",paste(gear, collapse = ","),")")
    }
    
    if (!is.null(years)) {
      years.tweak = paste0("AND to_char(DATE_FISHED,'YYYY') IN (",paste(years, collapse =","),")")
    }

    if (get.nonlandings==T) {

      catch.usage.tweak = "UNION
/*Get all catch where CATCH_USAGE_CODE <> 'LANDED'*/
SELECT
S1.LOG_EFRT_STD_INFO_ID,
E1.FV_FISHED_DATETIME DATE_FISHED,
E1.FV_GEAR_CODE GEAR_CODE,
S1.SSF_SPECIES_CODE AS SPECIES_CODE,
S1.SSF_LANDED_FORM_CODE AS LANDED_FORM_CODE,
S1.SSF_SPECIES_SIZE_CODE AS SPEC_SIZE_CODE,
S1.CATCH_USAGE_CODE,
E1.FV_DURATION_IN_HOURS,
E1.FV_NUM_OF_GEAR_UNITS,
S1.UNIT_OF_MEASURE_ID,
ROUND(
CASE
WHEN S1.UNIT_OF_MEASURE_ID = 10
THEN S1.WEIGHT
WHEN S1.UNIT_OF_MEASURE_ID = 20
THEN S1.WEIGHT * 0.453592 #lbs->kg
WHEN S1.UNIT_OF_MEASURE_ID = 30
THEN S1.WEIGHT / 1000    #metric tons->kg
END, 2) WEIGHT_KG,
-999 RPT_WEIGHT_KG,
/*try to position data*/
ROUND(
CASE WHEN E1.ENT_LATITUDE IS NOT NULL
THEN SUBSTR(E1.ENT_LATITUDE, 1, 2) + SUBSTR(E1.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LATITUDE, 5, 2) / 3600
WHEN E1.DET_LATITUDE IS NOT NULL
THEN SUBSTR(E1.DET_LATITUDE, 1, 2) + SUBSTR(E1.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LATITUDE, 5, 2) / 3600
ELSE 0
END, 4) LAT,
ROUND(
CASE WHEN E1.ENT_LONGITUDE IS NOT NULL
THEN (-1 * (SUBSTR(E1.ENT_LONGITUDE, 1, 2) + SUBSTR(E1.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LONGITUDE, 5, 2) / 3600))
WHEN E1.DET_LONGITUDE IS NOT NULL
THEN (-1 * (SUBSTR(E1.DET_LONGITUDE, 1, 2) + SUBSTR(E1.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LONGITUDE, 5, 2) / 3600))
--WHEN E1.DET_NAFO_UNIT_AREA_ID IS NOT NULL
--THEN c1.LON
ELSE 0
END, 4) LON
FROM MARFISSCI.LOG_SPC_STD_INFO S1
INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E1
ON S1.LOG_EFRT_STD_INFO_ID = E1.LOG_EFRT_STD_INFO_ID
-- not 'landed' or 'live discard'
WHERE S1.CATCH_USAGE_CODE NOT IN (10,50)"

    } else {
      catch.usage.tweak = ""
    }
  
       Q = paste0(
"
SELECT * FROM
(
/*Get all catch where CATCH_USAGE_CODE= 'LANDED' */
SELECT
P.LOG_EFRT_STD_INFO_ID,
P.DATE_FISHED,
P.GEAR_CODE,
P.SPECIES_CODE,
P.LANDED_FORM_CODE,
P.SPECIES_SIZE_CODE,
P.CATCH_USAGE_CODE,
E.FV_DURATION_IN_HOURS,
E.FV_NUM_OF_GEAR_UNITS,
-999 UNIT_OF_MEASURE_ID, --allkg
P.RND_WEIGHT_KGS,
P.RPT_WEIGHT_KGS,
/*try to position data*/
ROUND(
CASE WHEN E.ENT_LATITUDE IS NOT NULL
THEN SUBSTR(E.ENT_LATITUDE, 1, 2) + SUBSTR(E.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LATITUDE, 5, 2) / 3600
WHEN E.DET_LATITUDE IS NOT NULL
THEN SUBSTR(E.DET_LATITUDE, 1, 2) + SUBSTR(E.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E.DET_LATITUDE, 5, 2) / 3600
WHEN P.LATITUDE IS NOT NULL
THEN SUBSTR(P.LATITUDE, 1, 2) + SUBSTR(P.LATITUDE, 3, 2) / 60 + SUBSTR(P.LATITUDE, 5, 2) / 3600
ELSE 0
END, 4) LAT,
ROUND(
CASE WHEN E.ENT_LONGITUDE IS NOT NULL
THEN (-1 * (SUBSTR(E.ENT_LONGITUDE, 1, 2) + SUBSTR(E.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LONGITUDE, 5, 2) / 3600))
WHEN E.DET_LONGITUDE IS NOT NULL
THEN (-1 * (SUBSTR(E.DET_LONGITUDE, 1, 2) + SUBSTR(E.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E.DET_LONGITUDE, 5, 2) / 3600))
WHEN P.LONGITUDE IS NOT NULL
THEN (-1 * (SUBSTR(P.LONGITUDE, 1, 2) + SUBSTR(P.LONGITUDE, 3, 2) / 60 + SUBSTR(P.LONGITUDE, 5, 2) / 3600))
ELSE 0
END, 4) LON
FROM MARFISSCI.pro_spc_info P
INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E
ON P.LOG_EFRT_STD_INFO_ID = E.LOG_EFRT_STD_INFO_ID
WHERE P.CATCH_USAGE_CODE = 10
", catch.usage.tweak , "
)
WHERE 1 = 1
", spp.tweak,"
", gear.tweak,"
", years.tweak,"
"
      )
      
      ldb = ROracle::dbGetQuery(channel, Q)
  
      read_write_fast( ldb, file=fn )
      print(fn)
    }
  
    ROracle::dbDisconnect(channel)
    return(ldbdir)
  }


  if (DS %in% c("aggregated", "aggregated.redo") ) {

  #  test = marfissci.process.data(df, agg.minutes=2, agg.by="SPECIES_CODE", save.RDS=F)


   agg.minutes=2
   agg.field = "RND_WEIGHT_KGS"
   agg.by = "SPECIES_CODE"
   agg.by.year =T
   name.det = NULL
   out.folder = "marfissci"
   
   
  agg = agg.minutes/60

  #SPATIAL IDENTIFICATION OF IMPROPERLY POSITIONED CATCH (i.e. on land)
  #use coastline to find those points that are inland
  #Anything other than NA is inland
  proj.metric = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96
                 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'
  if (!exists("coast.aea")) {

    writeLines("Building the coastline...
               ")
    coast.aea = as( aegis.coastline::coastline_db( project_to=proj.metric), "Spatial" )
    #coast.aea<<-spTransform(coast,CRS(proj.metric))
  }
  buff = agg*78847  #meters in 1 degree of longitude @45N
  if (!exists("coast.aea.buff")) {
    writeLines(paste0("With the selected aggregation level (",agg.minutes," minutes), the coastline is being
buffered internally by ",round(buff/1000,1), " km.  Data for this project is centered around ~45N,
and at this latitude, the aggregate values for any points within this buffer
will still overlay the ocean.  Beyond this distance, aggregate points will lie
entirely on the land.
"))
    coast.aea.buff <<- gBuffer(coast.aea, byid=TRUE, width=-1*buff)
  }


  df$VALIDITY = "VALID"  #default validity assumed good - overwrite if otherwise
  #flag data with unknown positions (assigned 0N,0W by marfissci.get.data
  noNA = c(c("LAT","LON"),agg.field) # fields where NA will be replaced with 0
  df[noNA][is.na(df[noNA])] <- 0

  df$VALIDITY[which(df$LAT == 0 | df$LON == 0)] = "NO COORDS"
  df$VALIDITY[which(df$LAT > 90 | df$LAT < -90)] = "BAD COORDS"
  df$VALIDITY[which(df$LON > 180 | df$LON < -180)] = "BAD COORDS"
  if (NROW(df[df$VALIDITY=='VALID',])==0) return(NULL) #can't position anything!
  #create temporary spatial df for terrestrial check - this loses the lat and
  #lon fields, so we add them back in for later convenience
  df.sp = df[which(df$VALIDITY=='VALID'),]
  coordinates(df.sp) = c("LON", "LAT")
  proj4string(df.sp) = CRS("+proj=longlat +datum=WGS84")
  df.sp@data$LON = coordinates(df.sp)[,1]
  df.sp@data$LAT = coordinates(df.sp)[,2]
  df.sp.proj.metric= spTransform(df.sp,CRS(proj.metric))
  writeLines(paste0("Finding points that exceed the allowable distance from the sea for this
aggregation level (",round(buff/1000,1), " km), and marking them as 'INLAND'.
"))

   df.sp.proj.metric@data$INLAND =over(df.sp.proj.metric,coast.aea.buff)
#   df = cbind(df,INLAND = df.sp.proj.metric@data$INLAND)

  df.sp.proj.metric@data$VALIDITY[which(!is.na(df.sp.proj.metric@data$INLAND))] = "INLAND"
  df.sp.proj.metric@data$INLAND = NULL
  #reorder columns for imminent rbind
  if (out.folder=="marfissci"){
    df.sp.proj.metric@data = df.sp.proj.metric@data[,c(1:12,16,15,13,14)]
  }else if (out.folder=="cl") {
    df.sp.proj.metric@data = df.sp.proj.metric@data[,c(8,7,1:6)]
    }else if (out.folder=="whalesitings") {
      df.sp.proj.metric@data = df.sp.proj.metric@data[,c(1,2,6,5,3,4)]
  }else{
    writeLines("unrecognized data columns")
  return(NULL)
    }
  df = rbind(df[!df$VALIDITY=='VALID',],df.sp.proj.metric@data)
  #  round the coordinates to the desired aggregation so we can group by the new
  # 'points', giving a lot less data to process
  df$LATAGG = (round(df$LAT/agg)*agg)-0.25*agg
  df$LONAGG = (round(df$LON/agg)*agg)+0.25*agg
  if(NROW(df[df$VALIDITY!="VALID",])>0) {
    writeLines(paste0(
      NROW(df[df$VALIDITY!="VALID",]), " of ", NROW(df)," the total records (a sum of ",sum(df[df$VALIDITY!="VALID",][,c(agg.field)])," of the
aggregated field) were poorly positioned.  These had either no positions, or were
determined to be terrestrial.  This total amount will be applied proportionately
across all other validly positioned data in a new field called 'WEIGHTED'
"))
  }else{
    writeLines(paste0("No invalid data was found (i.e. all positions were reasonably close to
the ocean). Despite this, a new field called 'WEIGHTED' will be output, but will
simply be populated with identical values as ",agg.field))
  }
  #Beyond this step, we will lose all initial details - e.g. catch usages,
  #reported units, gear types etc will be combined
  if (agg.by.year){
    df.agg = as.data.frame(as.list(aggregate(list("AGG_FIELD"=df[,c(agg.field)]),
                                             by=list(
                                               YEAR_FISHED = df$YEAR_FISHED,
                                               AGG_BY_FIELD = df[,c(agg.by)],
                                               VALIDITY = df$VALIDITY,
                                               LAT = df$LATAGG,
                                               LON = df$LONAGG),
                                             FUN=function(x) c(MEAN =round(mean(x),4),
                                                               CNT=round(length(x),4),
                                                               SUM = round(sum(x),4))
    )))
  }else{
    df.agg = as.data.frame(as.list(aggregate(list("AGG_FIELD"=df[,c(agg.field)]),
                                             by=list(
                                               AGG_BY_FIELD = df[,c(agg.by)],
                                               VALIDITY = df$VALIDITY,
                                               LAT = df$LATAGG,
                                               LON = df$LONAGG),
                                             FUN=function(x) c(MEAN =round(mean(x),4),
                                                               CNT=round(length(x),4),
                                                               SUM = round(sum(x),4))
    )))
  }
  #rm(df)


  if(NROW(df[df$VALIDITY!="VALID",])>0) {
    valid.data=df.agg[df.agg$VALIDITY=="VALID",]
    #get the proportion of the total sum of all attributable to each (valid) loc
    valid.data$PROP = prop.table(as.table(as.matrix(valid.data$AGG_FIELD.SUM)))[,1]
    invalid.data = df.agg[!df.agg$VALIDITY=="VALID",]
    invalid.data$PROP = 0
    invalid.total = sum(invalid.data$AGG_FIELD.SUM)
    valid.data$WEIGHTED = (valid.data$PROP * invalid.total) + valid.data$AGG_FIELD.SUM
    invalid.data$WEIGHTED = 0
    df.agg = rbind(valid.data,invalid.data)
    df.agg$PROP = NULL
    rm(valid.data)
    rm(invalid.data)
  } else {
    df.agg$PROP = df.agg$AGG_FIELD.SUM
  }


  #make a descriptive name so we know what we've got
  if (!is.null(name.det)){
    name.detail=name.det
  }else{
    name.detail=""
  }
  agg.type=paste0(substr(agg.by, 1, 4),"_")
  if (range(df.agg$AGG_BY_FIELD)[1] == range(df.agg$AGG_BY_FIELD)[2]) {
    agg.file = paste0(range(df.agg$AGG_BY_FIELD)[1],"_")
  }else{
    agg.file = paste(range(df.agg$AGG_BY_FIELD),collapse = "_")
  }
  if (agg.by.year){
    if (range(df.agg$YEAR_FISHED)[1] == range(df.agg$YEAR_FISHED)[2]) {
      years.file = paste0(range(df.agg$YEAR_FISHED)[1],"_")
    }else{
      years.file = paste0(paste(range(df.agg$YEAR_FISHED),collapse = "_"),"_")
    }
  }else{
    years.file =""
  }
  agg.amt=paste0(agg.minutes,"min")
  the.filename = paste0(name.detail,agg.type,agg.file,years.file,agg.amt)

  #do any field renaming before generating files
  colnames(df.agg)[colnames(df.agg) == 'AGG_BY_FIELD'] <- agg.by
  colnames(df.agg) <- gsub("AGG_FIELD\\.", "", colnames(df.agg))
  colnames(df.agg)[colnames(df.agg) == 'MEAN'] <- paste0("MEAN_",agg.field)
  colnames(df.agg)[colnames(df.agg) == 'CNT'] <- paste0("CNT_",agg.field)
  colnames(df.agg)[colnames(df.agg) == 'SUM'] <- paste0("SUM_",agg.field)

  df.agg.sp = df.agg[which(df.agg$VALIDITY!="BAD COORDS"),]
  coordinates(df.agg.sp) = c("LON", "LAT")
  proj4string(df.agg.sp) = CRS("+proj=longlat +datum=WGS84")

  ldir = project.datadirectory("aegis", "data", out.folder)

  if (save.RDS) {
    fn = file.path( ldir, "rds", paste(the.filename,"rds", sep="." ) )
    saveRDS(df.agg.sp, file=fn )
    writeLines(paste0("rds file written to ", fn))
  }

  if (save.CSV) {
    fn = file.path( ldir, "csv", paste(the.filename, "csv", sep="." ) )
    write.csv(df.agg, file=fn )
    writeLines(paste0("csv file written to ", fn) )
  }

  if(save.SHP) {
    fnloc = file.path( ldir, "shapes" )
    writeOGR(df.agg.sp, dsn=fnloc, layer=the.filename, driver='ESRI Shapefile', overwrite_layer=TRUE)
    writeLines(paste0("shp file written to ", fnloc, the.filename, ".*"))
  }
  if (output == "RDS"){
    the.output = df.agg.sp
  } else{
    the.output = df.agg
  }

#df = read.csv(paste0(project.datadirectory("mpa"),"/csv/raw_2010_612.csv")) #gets raw data
#test = marfissci.process.data(df, agg.minutes=2, agg.by="SPECIES_CODE", save.RDS=F) #aggregates it


marfissci.batch.process <- function(folder=file.path(project.datadirectory("aegis", "data"),"marfissci","raw_data"),
                                    out.folder="marfissci",
                                    combine=T){
  #' The purpose of this batch process function is to facilitate the mass
  #' generation of data products from marfis data.  It assumes that data has
  #' been extracted via marfissci.get.data(), and the resultant csv file(s) are
  #' saved locally.
  #'
  #' When called, this batch function will allow the user to select what data
  #' (i.e. years, species, and/or gears) should be aggregated together.  It will
  #' then generate rds files and figures for all the data.
  start.time <- Sys.time()

  do.it = function(all.data){
    library(lubridate)
    all.data$YEAR_FISHED=year(all.data$DATE_FISHED)
    if (range(all.data$YEAR_FISHED)[1] == range(all.data$YEAR_FISHED)[2]) {
      years.file = range(all.data$YEAR_FISHED)[1]
    }else{
      years.file = paste(range(all.data$YEAR_FISHED),collapse = "_")
    }

    agg.by=c("SPECIES_CODE","GEAR_CODE")

    channel <- ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname="PTRAN", username= oracle.personal.username, password = oracle.personal.password)
    for (a in 1:length(agg.by)){
      #get all of the unique values for the field we want to aggregate by
      combos = unique(all.data[agg.by[a]])
      if(agg.by[a] == "GEAR_CODE"){
        query = "SELECT GEAR_CODE, DESC_ENG FROM MARFISSCI.GEARS"
      }else{
        query = "SELECT SPECIES_CODE, DESC_ENG FROM MARFISSCI.SPECIES"
      }
      the.codes = ROracle::dbGetQuery(channel,query)
      combos=merge(combos,the.codes)

      for (i in 1:nrow(combos)){
        writeLines(paste0("Analysing: ", combos[i,2]))
        #print(paste0("working on ",all.data$DESC_ENG[all.data[agg.by]==combos[i,]]))
        this <- marfissci.process.data(all.data[which(all.data[agg.by[a]]==combos[i,1]),],
                                       agg.by =agg.by[a],
                                       save.RDS = T,
                                       save.CSV = T,
                                       save.SHP = T,
                                       agg.by.year =F,
                                       name.det=paste0(years.file,"_"),
                                       out.folder=out.folder,
                                       output="RDS")
        if (i==1) gearKeep<<-this
        if (!is.null(this)){
          writeLines("Generating a plot...")
          if(agg.by[a] == "GEAR_CODE") {
            colour.by = "CNT_RND_WEIGHT_KGS"
          }else{
            colour.by = "SUM_RND_WEIGHT_KGS"
          }
          marfissci.simple.map(this, agg.by = agg.by[a], colour.by = colour.by, save.plot = T, out.folder=out.folder,name.det=years.file, plot.title=paste0(combos[i,2]," ",  years.file))
        }else{
          writeLines(paste0("Insufficient data to plot a figure for ",combos[i,2]))
        }
      }

    }
  }
  ROracle::dbDisconnect(channel)


  if (combine){
  writeLines("Combining all of the csv files into a single one")
    all.data=do.call(rbind,lapply(file.path(folder,list.files(path=folder, pattern="\\.csv$")),
                                  read.csv, header=TRUE, sep=","))
    do.it(all.data)
  } else {
    file.names <- dir(folder, pattern ="\\.csv$")
    for(i in 1:length(file.names)){
      all.data <- read.csv(file.path(folder,file.names[i]),header=TRUE, sep=",")
      do.it(all.data)
    }
  }

  diff=difftime(Sys.time(),start.time, units = "secs")
  diff = format(.POSIXct(diff,tz="GMT"), "%H:%M:%S")
  writeLines(paste0(diff, " elapsed"))
  return(NULL)
}


cl.get.data = function(spp = NULL, gear=NULL, years = NULL, save.csv=T){

  channel <- ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname="PTRAN", username = oracle.cl.username, password = oracle.cl.password)

  if (!is.null(spp)) {
    spp.tweak = paste0("AND SPECIES IN (",paste(spp, collapse = ","),")")
  }else{
    spp.tweak = ""
  }
  if (!is.null(gear)) {
    gear.tweak = paste0("AND GEARCODE IN (",paste(gear, collapse = ","),")")
  }else{
    gear.tweak = ""
  }
  if (!is.null(years)) {
    years.tweak = paste0("AND substr(DATELAND,1, 4) IN (",paste(years, collapse =","),")")
  }else{
    years.tweak = ""
  }

  query.raw = paste0(
    "SELECT Z.LATITUDE LAT,
  Z.LONGITUDE LON,
  Z.CTCHDATE,
  Z.YEAR,
  Z.LIVE_WT,
  Z.SPECIES SPECIES_CODE,
  Z.GEARCODE GEAR_CODE
  FROM ZIFDB Z
   WHERE 1 = 1
    ", spp.tweak,"
    ", gear.tweak,"
    ", years.tweak,"
    "
  )
  data.raw =  ROracle::dbGetQuery(channel,query.raw)
  if (save.csv==T){
    #make a descriptive name so we know what we've got
    if (is.null(spp)){
      spp.file = ""
    }else if(range(spp)[1] == range(spp)[2]) {
      spp.file = paste0("_",range(spp)[1])
    }else{
      spp.file = paste0("_",paste(range(spp),collapse = "_"))
    }
    if (is.null(gear)){
      gear.file = ""
    }else if (range(gear)[1] == range(gear)[2]) {
      gear.file = paste0("_",range(gear)[1])
    }else{
      gear.file = paste0("_",paste(range(gear),collapse = "_"))
    }
    if (is.null(years)){
      years.file = ""
    }else if (range(years)[1] == range(years)[2]) {
      years.file = paste0("_",range(years)[1])
    }else{
      years.file = paste0("_",paste(range(years),collapse = "_"))
    }
    file.output = file.path(project.datadirectory("aegis", "data"), "cl", "raw_data", "cl_raw" , paste(years.file,gear.file,spp.file,".csv", sep="") )
    write.csv(data.raw, file.output, row.names = F)
    print(paste0("CSV written to ",file.output))
  }
  ROracle::dbDisconnect(channel)
  return(data.raw)

}
# years=seq(from = 1986, to = 2002) #2002 represents an overlap with marfissci
# #years=c(1986,1987)
# for (i in 1:length(years)){
#   cl.get.data(years=years[i])
# }





    marfissci.batch.process()
    cl.batch.process()
    marfissci.simple.map()

  }



}
