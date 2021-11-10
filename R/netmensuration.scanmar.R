

netmensuration.scanmar = function( DS, p, nm=NULL, YRS=NULL, setid=NULL, debugid=NULL){

  perley.years0 = c( 1990:1992 )
  perley.years1 = c( 2004:2009 )
  datalog.year0 = 2009    # note this overlaps with the last year in perley's database


  if (!file.exists( p$scanmar.dir )) {
   mk.Dir = readline("Directory not found, shall we create it? Yes/No (Default No) : ")
    if ( mk.Dir =="Yes" ) {
      dir.create( p$scanmar.dir, recursive=TRUE)
      print( paste("The directory -- ",  p$scanmar.dir, " -- has been created") )
    } else {
      warning( paste("Directory: ", p$scanmar.dir, " was not found." ))
    }
  }

  if(DS %in% c("perley", "perley.datadump", "perley.redo" )) {

    # fn1= old data, fn2= new data and fn3= merged data (old and new)
    fn1= file.path(p$scanmar.dir,"scanmar_rawdata_perley0.rdata")
    fn2= file.path(p$scanmar.dir,"scanmar_rawdata_perley1.rdata")
    fn3= file.path(p$scanmar.dir,"scanmar.perley.merged.rdata")

    if(DS=="perley"){
      nm = NULL
      if (file.exists(fn3)) load(fn3)
      return(nm)
    }

    if(DS=="perley.redo"){

      if (file.exists( fn1) ) {
        load(fn1)
      } else {
        require (ROracle)
        connect = dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.perley.db, username=oracle.perley.user, password=oracle.perley.password, believeNRows=F)
        scanmar = dbGetQuery(connect, "select * from   groundfish.perleyp_SCANMAR", as.is=T)
        dbDisconnect(connect)
        save(scanmar, file=fn1, compress=T)
      }

      if (file.exists( fn2) ) {
        load(fn2)
      } else {
        require (ROracle)
        connect = dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.perley.db, username=oracle.perley.user, password=oracle.perley.password, believeNRows=F)
        scanmarnew = dbGetQuery(connect, "select * from   groundfish.perleyp_NEWSCANMAR", as.is=T)
        dbDisconnect(connect)
        save(scanmarnew, file=fn2, compress=T)
      }

      nm = scanmar
      rm(scanmar)
      gc()

      names(nm)=tolower(names(nm))      #ac
      names(scanmarnew)=tolower(names(scanmarnew))

      # nm is the dataset which combines the old and new data (merged)
      # some variables were missing from scanmar to faciliate the merge of scanmarnew
      nm$fspd=NA
      nm$cspd=NA
      nm$latitude=NA
      nm$longitude=NA
      nm$depth=NA
      nm$empty=NA
      nm$time=NA

      # Correcting for data which contains NA in the time slot by identifying and deleting it
      strangedata = which(is.na(nm$logtime))
      if(length(strangedata)>0) nm=nm[-strangedata,]

      # fix some time values that have lost the zeros due to numeric conversion
      nm$logtime=gsub(":", "", nm$logtime)
      j=nchar(nm$logtime)
      tooshort=which(j==5); if (length(tooshort)>0) nm$logtime[tooshort]=paste("0",nm$logtime[tooshort],sep="")
      tooshort=which(j==4); if (length(tooshort)>0) nm$logtime[tooshort]=paste("00",nm$logtime[tooshort],sep="")
      tooshort=which(j==3); if (length(tooshort)>0) nm$logtime[tooshort]=paste("000",nm$logtime[tooshort],sep="")
      tooshort=which(j==2); if (length(tooshort)>0) nm$logtime[tooshort]=paste("0000",nm$logtime[tooshort],sep="")
      tooshort=which(j==1); if (length(tooshort)>0) nm$logtime[tooshort]=paste("00000",nm$logtime[tooshort],sep="")

      nm$hours=substring(nm$logtime,1,2)
      nm$min=substring(nm$logtime,3,4)
      nm$sec=substring(nm$logtime,5,6)
      nm$time = paste(nm$hours, nm$min, nm$sec, sep=":")

      # creating a matrix (nm2) with nm and scanmarnew
      nm2=matrix(NA,ncol=ncol(nm),nrow=nrow(scanmarnew))
      nm2= as.data.frame(nm2)
      names(nm2)=names(nm)

      # making the columns names of nm2 equal to those of scanmarnew
      o =names(scanmarnew)
      for(n in o){
        nm2[,n]=scanmarnew[,n]
      }

      # Combining rows of nm and nm2 to create the data frame nm
      nm=rbind(nm,nm2)
      rm(scanmarnew, nm2)
      gc()

      #This step is creating variables by pasting existing together
      #It is also changing character values to numeric
      nm$uniqueid=paste(nm$mission,nm$setno,sep="_")
      nm$ltspeed=as.numeric(nm$ltspeed)
      nm$ctspeed=as.numeric(nm$ctspeed)
      nm$doorspread=as.numeric(nm$doorspread)
      nm$wingspread=as.numeric(nm$wingspread)
      nm$clearance=as.numeric(nm$clearance)
      nm$opening=as.numeric(nm$opening)
      nm$depth=as.numeric(nm$depth)
      nm$latitude=as.numeric(nm$latitude)
      nm$longitude=as.numeric(nm$longitude)
      nm$year.mission= as.numeric(substring(nm$mission,4,7))
      nm$fspd=as.numeric(nm$fspd)
      nm$cspd= as.numeric(nm$cspd)

      # merge groundfish  timestamps and ensure that net mensuration timestamps are correct
      nm$id=paste(nm$mission, nm$setno, sep=".")
      nm$nm_id = paste( "Perley", nm$mission, nm$setno, sep=".")

      ii = which( nm$longitude > 0 )
      if (length(ii) > 0 ) nm$longitude[ii] = - nm$longitude[ii]

      # load groundfish inf table which has timestamps of start/stop times and locations
      gsinf = groundfish_survey_db( DS="gsinf" )
      gsinfvars=c("id", "sdate", "settype" )

      # merge
      nm = merge( nm, gsinf[,gsinfvars], by="id", all.x=TRUE, all.y=FALSE)

      nm$day = lubridate::day( nm$sdate )
      nm$mon = lubridate::month( nm$sdate )
      nm$yr = lubridate::year( nm$sdate )
      nm$date = paste(nm$yr, nm$mon, nm$day, sep="-")

      i = which(!is.finite(nm$day))
      if (length(i)>0) nm = nm[ -i, ]

      i = which( is.na( nm$time))
      if (length(i)>0) nm = nm[ -i, ]

      nm$timestamp = ymd_hms( paste( nm$date, nm$time ), tz="America/Halifax" ) ## need to verify if this is correct ... JC: yes, all data stored in this TZ
      nm$timestamp = with_tz( nm$timestamp, "UTC" )  # keep all internal operations in UTC

      keep=c("id", "nm_id", "vesel", "ltspeed", "ctspeed", "wingspread", "doorspread", "clearance",
             "opening", "fspd", "cspd", "latitude", "longitude", "depth", "settype", "timestamp", "yr"
             )
      nm=nm[,keep]

      # fix sets that cross midnight and list
      # some sets cross midnight and require days to be adjusted

      threshold.seconds = 1*60*60 # 1hr in seconds
      nmids = unique( nm$id )
      for (ii in nmids) {
        jj = which( nm$id == ii)
        tstamp = nm$timestamp[jj]
        r = range(tstamp, na.rm=TRUE)
        y = as.numeric( difftime(r[2], r[1]), units="secs")  # in seconds

        if ( y > threshold.seconds ) { # as duration is in seconds
          # if there is a timestamp problem, the problematic records are those with hour values that are soon after midnight
          # .. assume any values from midnight to 2 AM need to be recoded to the next day's value
          hrs = hour( tstamp )
          z = which( hrs < 2 )  # 2AM is the cutoff
          if ( length(z) > 0 ) {
            day( tstamp[z]) = day( tstamp[z])+1
          }
          nm$timestamp[jj] = tstamp
        }
      }

      w = which(!is.finite(nm$cspd))
      nm$ctspeed[w]=nm$cspd[w]
      v = which(!is.finite(nm$fspd))
      nm$ltspeed[v]=nm$fspd[v]
      nm$gyro=NA
      v.to.drop = c("vesel", "empty", "logtime", "cspd", "fspd", "settype", "dist", "edate" )
      for ( v in v.to.drop) nm[,v] = NULL

      save(nm, file=fn3,compress=TRUE)
    }
  }

  # -------------------------------

  if(DS %in% c("basedata", "basedata.redo"))  {

    if (is.null (YRS) ) YRS = p$netmensuration.years

    dir.create( file.path( p$scanmar.dir, "basedata"), recursive=TRUE, showWarnings=FALSE )

    if(DS == "basedata"){
      out = NULL
      for ( YR in YRS ) {
        basedata=NULL
        fn = file.path( p$scanmar.dir,  "basedata",  paste( "scanmar", "basedata", YR, "rdata", sep="." ))
        if ( file.exists(fn)) {
          load(fn)
          out = rbind( out, basedata )
        }
      }
      return(out)
    }

    varnames = c( "id", "nm_id", "ltspeed", "ctspeed", "wingspread", "doorspread", "clearance", "opening",
                  "latitude", "longitude", "depth", "gyro", "timestamp")

    rawdata.dir = file.path( p$scanmar.dir, "datalogs" )

    for ( YR in YRS ) {
      basedata = NULL
      fn = file.path( p$scanmar.dir, "basedata", paste( "scanmar", "basedata", YR, "rdata", sep="." ))

      if (YR %in% c( perley.years0, perley.years1)  ) {
        nm = netmensuration.scanmar( DS="perley", p=p )
        nm$yr =  lubridate::year(nm$timestamp)
        oo = which( nm$yr == YR )
        if (length( oo) > 0 ) {
          basedata = nm[oo,]
          basedata = basedata[ ,varnames ]
        }
        rm(nm); gc()
      }

      # if YR == 2009 .. then the next step will add the log data as well
      if (YR >= datalog.year0 ) {
        filelist = list.files(path=file.path( rawdata.dir, YR ), pattern="\\.log$",
                              full.names=T, recursive=TRUE, ignore.case=TRUE)
        unneeded = grep ("copy", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        unneeded = grep ("unsuccessful", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        unneeded = grep ("all.log", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        unneeded = grep ("aborted", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        unneeded = grep ("rejected", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        unneeded = grep ("gt\\.", filelist, ignore.case=TRUE)  # "gear trials"
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        if (length( filelist) > 0 ) {
          print( "Reading in scanmar log files:" )
          for ( fl in filelist ) {
            print(fl)
            j = load.scanmar.rawdata( fl, yr=YR )  # variable naming conventions in the past  .. always UTC
            if (is.null(j)) next()
            j$ctspeed = NA  # missing in modern data so add to permit merging with historical data
            j = j[ , varnames ]
            basedata = rbind( basedata, j)
          }
        }
      }
      if (!is.null( basedata) ) {
        save(basedata, file=fn, compress= TRUE)
        print(fn)
      }
    }
    return( YRS )
  }


  # -------------------------------------


  if (DS %in% c("basedata.lookuptable", "basedata.lookuptable.redo"))  {
    ## RAtionale: data from 2009 to 2014+ are missing mission/set inforamtion
    ## we need to match sets ("id") with scanmar data ("nm_id") using time and gpstrack / location information

    dir.create( file.path( p$scanmar.dir, "basedata.lookuptable"), recursive=TRUE, showWarnings=FALSE )

    if (is.null (YRS) ) YRS = p$netmensuration.years

    if(DS == "basedata.lookuptable"){
      out = NULL
      for ( YR in YRS ) {
        gf = NULL
        fnYR = file.path( p$scanmar.dir, "basedata.lookuptable", paste( "scanmar", "basedata.lookuptable", YR, "rdata", sep= "."))
        if ( file.exists(fnYR)) {
          load(fnYR)
          out = rbind( out, gf )
        }
      }
      return(out)
    }

    gf0 = groundfish_survey_db(DS="gsinf")

    gf0$nm_id = NA
    gf0$nm_id0 = NA   # this stores the initial id  ... perley data and >=2015 data have hand matched data .. but many are not matched correctly ..
    gf0$timestamp = gf0$sdate   # UTC
    gf0$min.distance = NA
    gf0$time.difference = NA
    gf0$depth.difference = NA
    gf0$match.level = NA  # -1 = not matched

    print( "Following print out are of potential mismatches:")

    for ( YR in YRS ) {
      print( YR )
      fnYR = file.path( p$scanmar.dir, "basedata.lookuptable", paste( "scanmar", "basedata.lookuptable", YR, "rdata", sep= "."))
      skipyear = FALSE
      # Incorporation of newer data, combining timestamp

      igf = which( gf0$yr==YR)
      if (length(igf)==0) {
        skipyear = TRUE
      } else {
        gf = gf0[igf,]
      }

      bd = netmensuration.scanmar( DS="basedata", p=p, YRS=YR )
      if (is.null( bd ))  skipyear = TRUE

      # extract data at modal depth locations for each profile/log
      # find deepest point (actually modal depth)  for each set
      # and estimate quantities for matching with gsinf
      out = NULL

      oo = which( bd$depth > 10 )
      x = bd [ oo, ]  # depth is a required field to determine bottom contact
      x = x[ order( x$timestamp), ]
      iid = sort( unique( x$nm_id ) )

      if (length( oo) > 30 ) {
        for ( ii in 1:length(iid) ) {
          v = w = NULL
          w = which( x$nm_id == iid[ii] )
          v = w[floor( length(w) / 2)]
          if ( length(w) > 30 )  {
            dd = x$depth[w]
            mm = modes( dd )
            if (is.finite( mm$lb2+mm$ub2 )) {
              ww = which( dd > mm$lb2 & dd < mm$ub2 )
              xx = which.min( x$timestamp[w[ww]]  )
              v = w[ww[xx]]
            }
          }
          out = c( out, v )
        }
      } else {
        for ( ii in 1:length(iid) ) {
          v = w = NULL
          w = which( x$nm_id == iid[ii] )
          v = w[floor( length(w) / 2)]
          if ( length(w) > 10 )  {
            mti = median( x$timestamp[w], na.rm=TRUE )
            dd = as.numeric( difftime( x$timestamp[w], mti, units="hours" ) )
            xx = which.min( abs(dd) )
            v = w[xx]
          }
          out = c( out, v )
        }
      }
      if ( length(out) > 1 ) {
        nm = x[out,]
      } else {
        skipyear = TRUE
      }

      if ( ! skipyear ) {
        nnm = nrow( nm)
        nm$lon=nm$longitude
        nm$lat=nm$latitude
        nm$longitude =NULL
        nm$latitude =NULL
        nm$id = toupper(nm$id)
        gf$id = toupper(gf$id) # needs to be used for matching with nm


        # match level 0 :: if there is already a misson.set identified in nm then record it
        # ... but there seem to be errors so go through the exhaustive process as well
        # store but do not use the id's ... they are unreliably matched (esp in the Perley data base)
        # but have been identified more reliably starting in 2015
        nm$nm_id0 = nm$nm_id

        # now switching focus upon gsinf: if gs$nm_id is missing then we require a matching nm identity (filename)
        # match level 1:: unique element with data within 1 km an 1 hr
        # criteria for exact matches in time (hr) and distance (km) :: 1 nm = 1.852 km, 3
        depth.eps = 25
        coords = c("lon", "lat")

        unmatched = which( is.na( gf$nm_id) & gf$geardesc=="Western IIA trawl" & gf$settype %in% c(1,2,5) )

        if (length(unmatched)>0) {

          for(igg in unmatched) {

            res = NULL  # container for matches

            # criteria for matching: distance, time and depth differences
            distance.km = try( abs( as.vector( geosphere::distCosine( nm[ , coords], gf[igg, coords])) )/1000 , silent=TRUE )
            if ( "try-error" %in% class( distance.km) ) distance.km = rep( NA, nnm )

            diftime.hr = as.numeric( difftime( gf$sdate[igg], nm$timestamp, units="hours" ) ) # gfsdate-nmtimestamp:low is earlier
            difdepth.m = nm$depth - gf$bottom_depth[igg]

            # first search for direct matches based upon encoded id's
            directMatches = which( nm$id == gf$id[igg] )
            if ( length(directMatches) > 0 ) {
              fd = data.frame( nm_id = nm$nm_id[directMatches], stringsAsFactors=FALSE )
              fd$nm_id0 = nm$nm_id0[directMatches]
              gf$match.level[igg] = 0 # direct is assumed correct
              gf$nm_id[igg] = nm$nm_id[directMatches]
              gf$nm_id0[igg] = nm$nm_id0[directMatches]
              gf$timestamp[igg] = nm$timestamp[directMatches]
              gf$min.distance[igg] = distance.km[directMatches]
              gf$time.difference[igg] = diftime.hr[directMatches]
              gf$depth.difference[igg] = difdepth.m[directMatches]
              next()
            }

            # time-based only: lower condifence as there are so many issues with timezones and unsynced clocks
            itime = which( diftime.hr > -1.75 & diftime.hr <= 1.75 )
            if ( length( itime ) > 0 ) {
              for ( m in itime ) {
                fno = NULL
                fno = data.frame( nm_id = nm$nm_id[m], stringsAsFactors=FALSE )
                fno$nm_id0 = nm$nm_id0[m]
                fno$time.hr = diftime.hr[m]
                fno$distance.km = distance.km[m]
                fno$depth.diff = difdepth.m[m]
                res = rbind( res, fno )
              }
            }

            # location-based only
            idist = which( distance.km < 10 )  # max = 4.5 km
            if ( length(idist) > 0 ) {
              for ( m in idist ) {
                fno = NULL
                fno = data.frame( nm_id = nm$nm_id[m], stringsAsFactors=FALSE )
                fno$nm_id0 = nm$nm_id0[m]
                fno$time.hr = diftime.hr[m]
                fno$distance.km = distance.km[m]
                fno$depth.diff = difdepth.m[m]
                res = rbind( res, fno )
              }
            }

            # depth based only
     #       idepth = which( abs(difdepth.m) < depth.eps )
     #       if ( length(idepth) > 0 ) {
     #         for ( m in idepth ) {
     #           fno = NULL
     #           fno = data.frame( nm_id = nm$nm_id[m], stringsAsFactors=FALSE )
     #           fno$nm_id0 = nm$nm_id0[m]
     #           fno$time.hr = diftime.hr[m]
     #           fno$distance.km = distance.km[m]
     #           fno$depth.diff = difdepth.m[m]
     #           res = rbind( res, fno )
     #         }
     #       }

            res = unique(res)
            reasonable = which( res$time.hr > -1.75 & res$time.hr <= 1.75 )  # tow should be 45 min (0.75) adding 1 hour in case of DST
            if ( length(reasonable) == 0 ) next()
            if ( length(reasonable) > 0 ) {
              # make some decisions:
              res = res[reasonable, ]
              res$match.level = NA

              ntime = length( which( is.finite( res$time.hr)) )
              ndist = length( which( is.finite( res$distance.km)) )
              ndepth = length( which( is.finite( res$depth.diff )) )

              if ( ndepth>0 ) {
                # must keep separated from "reasonable" as it is not always present
                jdepth = which( abs( res$depth.diff ) < depth.eps )
                if (length(jdepth) > 0 ) res = res[ jdepth, ]
              }

              if ( ntime>0 & ndist>0 ) {
                # if here, then both position and time data exist
                res$diff = abs( res$distance.km * res$time.hr ) # effective difference .. lower means more likely
                it = which.min( res$diff )
                matchlevel = res$diff[it]
              }

              if ( ntime==0 & ndist>0 ) {
                # no time data but some distance data .. retain what we can
                it = which.min( abs( res$distance.km) )
                matchlevel = 2 + abs( res$distance.km[it] )
              }

              if ( ntime>0 & ndist==0 ) {
                # no positional data but some time data .. retain what we can
                it = which.min( abs( res$time.hr ) )
                matchlevel = 2 +  abs( res$time.hr[it] )
              }

              if (length(it)==1 ) {
                # register the match only if a single solution is found
                gf$match.level[igg] = matchlevel
                gf$nm_id[igg] = res$nm_id[it]
                gf$nm_id0[igg] = res$nm_id0[it]
                gf$timestamp[igg] = gf$sdate[igg]
                gf$min.distance[igg] = res[it, "distance.km"]
                gf$time.difference[igg] = res[it, "time.hr"]
                gf$depth.difference[igg] = res[it, "depth.diff"]
                next()
              }

            } # if len reasonable > n
          } # end for loop  unmatched
        }  # end if

        # last stage ..  return hand matched id's where appropriate
        yy = which( is.na( gf$nm_id ) | gf$match.level > 3.25 )
        if (length(yy)>0) {
          gf$nm_id[yy] =  gf$nm_id0[yy]
          if (YR < datalog.year0 ) {
            # Perley years have many errors
            gf$match.level[yy] = gf$match.level[yy] / 2 # high priority .. encourages this solution to be kept
          }
          if (YR >= datalog.year0 ) {
            # more recent years .. carry the actual set/mission on the file log .. lower chance of erroroneous match
            gf$match.level[yy] =  gf$match.level[yy] / 4 # highest priority .. forces this solution to be kept
          }
        }

        # remove duplicated nm_id's
        uu = which( duplicated( gf$nm_id, incomparables=NA ) )
        if ( length(uu) > 0 ) {
          print( "Duplicated matches found. Rejecting poorer matches:")
          while ( length( uu) > 0 )  {
            did = unique( gf$nm_id[uu] )
            hh = did[1]
            ee = which( gf$nm_id == hh )
            print( gf[ee, ] )
            # browser()
            if (any( is.finite( gf$match.level[ee] ) ) ) {
              ff = which.min( gf$match.level[ee] )
              if ( is.finite(ff)) {
                gg = setdiff( ee, ee[ff] )
                print( paste("  Dropping match for:",  gf$id[gg] ) )
                gf$nm_id[gg] = NA
              }
            } else if ( any( is.finite( gf$min.distance[ee] ) ) ) {
              ff = which.min( gf$min.distance[ee] )
              if ( is.finite(ff)) {
                gg = setdiff( ee, ee[ff] )
                print( paste("  Dropping match for:",  gf$id[gg] ) )
                gf$nm_id[gg] = NA
              }
            } else if ( any( is.finite( gf$time.difference[ee] ) ) ) {
              ff = which.min( abs( gf$time.difference[ee] ))  # assuming time is correct ! which it is sometimes not ...
              if ( is.finite(ff)) {
                gg = setdiff( ee, ee[ff] )
                print( paste("  Dropping match for:",  gf$id[gg] ) )
                gf$nm_id[gg] = NA
              }
            }
            uu = which( duplicated( gf$nm_id, incomparables=NA ) )
          }
        }

      } # end if skipyear

      save(gf, file= fnYR, compress= TRUE)
      print( paste(fnYR))
      print( paste("Matched:", length(which(!is.na(gf$id))), " -- Total", nrow(gf)) )
    }  # end loop for YRS

    return( YRS )
  }


  # -------------------------------------


  if ( DS %in% c("sanity.checks", "sanity.checks.redo") ) {
   # Step to merge and filter data

   if (is.null (YRS) ) YRS = p$netmensuration.years
   dir.create( file.path( p$scanmar.dir, "sanity.checked"), recursive=TRUE, showWarnings=FALSE )

   if(DS=="sanity.checks") {
      out = NULL
      for ( YR in YRS ) {
        nm = NULL
        fn = file.path( p$scanmar.dir, "sanity.checked", paste("scanmar.sanity.checked", YR, "rdata", sep=".") )
        if ( file.exists(fn)) {
          load(fn)
          out = rbind( out, nm )
        }
      }
      return(out)
    }

    for ( YR in YRS ) {
      gf = NULL
      gf = netmensuration.scanmar( DS="basedata.lookuptable", p=p, YRS=YR  )
      ngf = nrow(gf)
      if (is.null(gf)) next()
      gf = gf[, c("id", "nm_id", "sdate", "geardesc", "bottom_depth", "timestamp", "min.distance", "time.difference", "match.level" ) ]
      gf = gf[ which( !is.na( gf$nm_id) ) , ]
      # gate and filter the wingspread and door spread data  ... multi-pass quantile trimming
      # this is a global analysis ... all data required

      nm = netmensuration.scanmar( DS="basedata", p=p, YRS=YR )
      if (is.null(nm)) next()

      nnm0 = length( unique(nm$nm_id))

      nm$id = NULL  # this was already processed in the basedata.loopkup

      nmj = which( is.na( nm$nm_id) )  #required
      if (length( nmj) > 0 ) nm = nm[ -nmj,]

      # drop a few vars that are not used and/or unreliable or unneeded
      nm$ltspeed = NULL
      nm$ctspeed = NULL
      nm$gyro = NULL
      nm$clearance = NULL
      nm$opening = NULL

      nmnrow0 = nrow( nm)
      nm = merge(nm, gf, by="nm_id", all.x=TRUE, all.y=FALSE, suffixes=c("", ".gf") )
      if( nrow(nm) != nmnrow0 ) stop("Merge error")

     # empty variable is not needed (crossed channel with doorspread), also values present look erroneous in NED2005001 1
      if (YR==2005) {
        i = which( nm$id=="NED2005001.1" )
        if (length(i) >0) {
          nm$doorspread[i] = NA
          nm$wingspread[i] = NA
        }
      }

     # coarse level gating

     # ID sets where American trawls were used for comparative surveys
     oo = which( nm$geardesc == "US 4 seam 3 bridle survey trawl" )
     if (length( oo) > 0 ) {
       ## additional gating goes here ... currently using the same rules ..
       nm$doorspread[oo] = filter.nets("doorspread.range", nm$doorspread[oo] )
       nm$wingspread[oo]  = filter.nets("wingspread.range", nm$wingspread[oo] )
 #      nm$clearance[oo]  = filter.nets("clearance.range", nm$clearance[oo] )
 #      nm$opening[oo]  = filter.nets("opening.range", nm$opening[oo] )
       nm$depth[oo]  = filter.nets("depth.range", nm$depth[oo] )
     }

     pp = which( nm$geardesc == "Western IIA trawl" )
     if (length(pp) >0 ) {
       nm$doorspread[pp] = filter.nets("doorspread.range", nm$doorspread[pp] )
       nm$wingspread[pp]  = filter.nets("wingspread.range", nm$wingspread[pp] )
 #      nm$clearance[pp]  = filter.nets("clearance.range", nm$clearance[pp] )
 #      nm$opening[pp]  = filter.nets("opening.range", nm$opening[pp] )
       nm$depth[pp]  = filter.nets("depth.range", nm$depth[pp] )
     }

      probs =c(0.005, 0.995)  # only to capture really large extreme, though much has already been removed by fixed range gating

      # simple range gates
      bad = which( nm$wingspread < 2 | nm$wingspread > 25   )
      if (length(bad) > 0 ) nm$wingspread[ bad] = NA
      bad = which( nm$doorspreadspread < 5 | nm$doorspread > 100 )
      if (length(bad) > 0 ) nm$doorspread[ bad] = NA

      # quantile-based gates
      # bad = which.quantile( nm$wingspread, probs=probs, inside=FALSE)
      # if (length(bad) > 0 ) nm$wingspread[ bad] = NA

      # bad = which.quantile( nm$doorspread, probs=probs, inside=FALSE)
      # if (length(bad) > 0 ) nm$doorspread[ bad] = NA

      ## NOTE:: droppping data without a set match ... this may be bit sever as there is data for
      ## forming doorspread/wingspread relationships but as they will not be computed and used in any manner ... dropping is OK

      bad = which( is.na( nm$nm_id ))
      if (length(bad) > 0 ) {
        nm$doorspread[ bad] = NA
        nm$wingspread[ bad] = NA
      }

      # good = which( is.finite( nm$doorspread + nm$wingspread + log(nm$depth) ) )
      # if (length(good) > 30 ) {
      #   dw = lm( doorspread ~ as.factor( nm_id ) + wingspread + log(depth), data=nm[good,], na.action="na.exclude" )
      #   # hist(dw$residuals, "fd")
      #   ddresid = residuals( dw )
      #   bad = which.quantile( ddresid, probs=probs, inside=FALSE)
      #   if ( length( bad) > 0 )  {
      #     nm$wingspread[ good[bad] ] = NA
      #     nm$doorspread[ good[bad] ] = NA
      #   }
      # } else {
      #   good = which( is.finite( nm$doorspread + nm$wingspread  ) )  # deoth missing for early series
      #   if (length(good) > 30 ) {
      #     dw = lm( doorspread ~ as.factor( nm_id ) + wingspread , data=nm[good,], na.action="na.exclude" )
      #     # hist(dw$residuals, "fd")
      #     ddresid = residuals( dw )
      #     bad = which.quantile( ddresid, probs=probs, inside=FALSE)
      #     if ( length( bad) > 0 )  {
      #       nm$wingspread[ good[bad] ] = NA
      #       nm$doorspread[ good[bad] ] = NA
      #     }
      #   }
      # }

      gooddata = which( !is.na( nm$id))
      if (length( gooddata) ==0 ) {
        print( paste("No id's found for", YR ))
        next()
      }
      nm = nm[gooddata, ]
      todrop = grep( ".gf$", names(nm))
      if (length(todrop)>0) nm = nm[ ,-todrop]

      # check for unbelievable matches based upon distance: these should really be right on top of each other ...
      distanceissues = which( abs( nm$min.distance) > 5 & nm$match.level > 2 )
      if (length( distanceissues ) >0 ) {
        uu = unique( nm$id[distanceissues] )
        for (u in uu ) {
          vv = which( nm$id==u )
          nm$nm_id[vv] = NA  # reject the match
        }
        oo = which( is.na( nm$nm_id) )
        if (length( oo) >0 ) nm = nm[ - oo, ]
      }

      # check for time zone type issues and assume gf is correct time
      timeissues = which( abs( nm$time.difference) > 1 & nm$match.level > 2 ) # time.difference is in hours
      if (length(timeissues) >0 ) {
        uu = unique( nm$id[ timeissues ])
        for (u in uu ) {
          vv = which( nm$id==u )
          nm$timestamp[vv] = nm$timestamp[vv] + round( nm$time.difference[vv], 0 )
        }
      }
      nm$date = substring(as.character(nm$timestamp), 1,10)

      ids = strsplit( nm$id, "[.]" )

      mission.trip = unlist( lapply( ids, function(x){x[[1]]} ) )
      setno = unlist( lapply( ids, function(x){x[[2]]} ) )

      nm$set = NA
      nm$set = as.numeric( setno )

      nm$trip = NA
      nm$trip = substring( mission.trip, 8,10 )
      nm$trip = as.numeric(nm$trip)
      nm$year= lubridate::year(nm$timestamp)

      fn = file.path( p$scanmar.dir, "sanity.checked", paste("scanmar.sanity.checked", YR, "rdata", sep=".") )
      save( nm, file=fn, compress=TRUE)
      print(fn)
      print( paste("Sets with usable data:", length(unique(nm$id)), " -- Logs found:", nnm0, " -- Total sets in gsinf", ngf ) )
    }
    return (YRS )
  }


  # -------------------


  if (DS %in% c("bottom.contact", "bottom.contact.redo", "bottom.contact.id" )) {

    if (is.null (YRS) ) YRS = p$netmensuration.years

    scanmar.bc.dir =  file.path(p$scanmar.dir, "bottom.contact" )
    dir.create( scanmar.bc.dir, recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.bc.dir, "results"), recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.bc.dir, "figures"), recursive=TRUE, showWarnings=FALSE )

    if ( DS=="bottom.contact"){
      if ( !is.null( setid ) ) {
        fn.bc = file.path( scanmar.bc.dir, "results", paste( "bc", setid, "rdata", sep=".") )
        bc = NULL
        if (file.exists(fn.bc)) load(fn.bc)
        return(bc)
      }

      out = NULL
      for ( YR in YRS ) {
        gsinf=NULL
        fn= file.path( scanmar.bc.dir, paste( "gsinf.bottom.contact", YR, "rdata", sep=".")  )
        if (file.exists(fn)) load(fn)
        out = rbind( out, gsinf )
      }
      gsinf0 = groundfish_survey_db( DS="gsinf" )
      if (!is.null(out)) gsinf0 = merge( gsinf0, out, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      gg = which( lubridate::year(gsinf0$sdate) %in% YRS )
      gs = NULL
      if (length(gg)>0) gs = gsinf0[ gg, ]
      return(gs)
    }

    gsinf0 = groundfish_survey_db( DS="gsinf" )
    gsinf0$year = lubridate::year( gsinf0$sdate )
    gsinf0.names = names( gsinf0 )
    gsinf0$bottom_duration = NA
    gsinf0$bc0.datetime = as.POSIXct(NA)
    gsinf0$bc1.datetime = as.POSIXct(NA)
    gsinf0$bc0.sd = NA
    gsinf0$bc1.sd = NA
    gsinf0$bc0.n = NA
    gsinf0$bc1.n = NA
    gsinf0$door.sa = NA
    gsinf0$wing.sa = NA
    gsinf0$door.mean = NA
    gsinf0$wing.mean = NA
    gsinf0$door.sd =  NA
    gsinf0$wing.sd =  NA
    gsinf0$bc.lon0 = NA
    gsinf0$bc.lon1 = NA
    gsinf0$bc.lat0 = NA
    gsinf0$bc.lat1 = NA
    gsinf0$bc.dist = NA
    gsinf0$bc.dist.v = NA
    gsinf0$bc.dist.h = NA
    gsinf0$bc.depth.mean = NA
    gsinf0$bc.depth.sd = NA
    gsinf0$bc.error.flag = NA


    if ( !is.null( debugid ) ) {
      YRS = unique( as.numeric( substring( debugid, 4,7) ))
      if (! YRS %in% c(1990:1992, 2004:max(p$yrs) ) ) {
        stop( paste( "Year did not parse correctly? ", YRS ) )
      }
    }


    for ( YR in YRS ) {

      skipyear = FALSE

      yy = which( gsinf0$year == YR )
      if (length(yy)==0 ) {
        skipyear = TRUE
      } else {
        gsinf = gsinf0[ yy, ]
      }

      nm = netmensuration.scanmar( DS="sanity.checks", p=p, YRS=YR )
      if (is.null( nm)) {
        skipyear = TRUE
      } else {
        nm = nm[which(is.finite(nm$depth)) ,  ]
        nm = nm[which(!is.na( nm$id ) ) , ]
        nm = nm[ order( nm$timestamp) ,]
      }

      if (nrow( nm) < 1 ) {
        skipyear = TRUE
      }

      uid = unique( nm$id)
      if ( !is.null( debugid ) ) uid = debugid

      ui = which (!is.na( uid) )
      if ( length( ui) == 0 ) {
        skipyear=TRUE
      } else {
        uid = sort( uid[ ui  ] )
      }

      if ( skipyear ) next()


      for ( id in uid) {
        print( id)
        ii = which( nm$id==id )  # rows of nm with scanmar/marport data
        uu = length( which( is.finite(nm[ii, "depth"])))
        if ( uu == 0 ) {
          print( "Net data not found ...")
          next()
        }
        if ( uu < 30 ) {
          print( "Note enough data found. Skipping ... ")
          next()
        }
        gii = which( gsinf$id==id )  # row of matching gsinf with tow info
        if (length(gii) != 1) {
          print( "No matching gsinf record found ...")
          next()  # no match in gsinf
        }

        nmii = nm[ which(nm$id==id) ,]

        # for the Perley years, there is an issue with tows that cross midnight
        # they were stored (in Oracle) with an incorrect day (1 day offset)
        tmed = median( nmii$timestamp )
        kk = which( nmii$timestamp < (tmed - dhours(1)) )
        if (length(kk) > 10) nmii$timestamp[kk] = nmii$timestamp[kk] + ddays(1)
        ll = which( nmii$timestamp > (tmed + dhours(1)) )
        if (length(ll) > 10) nmii$timestamp[ll] = nmii$timestamp[ll] - ddays(1)
        # final filter to retain a reasonable time range
        uu = which( (nmii$timestamp > (tmed - dhours(1))) &  (nmii$timestamp > (tmed + dhours(1)) ) )
        if (length(uu) > 30) nmii = nmii[uu,]

        # NOTE:: do not use time.gate for historical data .. inconsistent timestamps causes data loss
        # dropping time-gating as winch timestamps are too erratic and frequently wrong ...
        # define time gate -20 from t0 and 50 min from t0, assuming ~ 30 min tow
        # time.gate = list( t0=gsinf$sdate[gii] - dminutes(20), t1=gsinf$sdate[gii] + dminutes(50) )

        # another hack .. probably best to move these into another function to keep it contained...
        if (id=="TEL2004530.85") {
          # erratic recording with gaps and large noise signatures...
          baddata = which( nmii$timestamp < with_tz( ymd_hms("2004-07-27 00:37:00", tz="America/Halifax"), "UTC")
                         | nmii$timestamp > with_tz( ymd_hms("2004-07-27 01:09:00", tz="America/Halifax"), "UTC"))
          nmii$depth[ baddata ] = NA
        }

        # defaults appropriate for more modern scanmar data have > 3500 pings
        bcp = list( id=id, nr=nrow(nmii), tdif.min=9, tdif.max=45, depth.range=c(-65, 65) )  ### yes some are as short as 9 min
        bcp = bottom.contact.parameters( bcp ) # add other default parameters
        bcp = netmensuration.parameters.local.overrides( id, bcp ) # hacks to make strange profiles work go into this function

        # two depth sensors were used simultaneously but they are not calibrated!
        # remarkably hard to filter this out
        # send a trigger to bottom.contact to operate on this properly

        id.double.depth.sensors = paste( "NED2015002", c( 47:64 ), sep="." )
        if ( id %in% id.double.depth.sensors ) bcp$double.depth.sensors = TRUE
        if ( id %in% p$problem.sets ) next()

        bc = NULL #
        if ( !is.null( debugid ) ) {
          bc = bottom.contact(nmii, bcp, debugrun=TRUE )
        } else {
          bc = try( bottom.contact(nmii, bcp ), silent=TRUE )
        }


        if ( is.null(bc)) next()
        if ( "try-error" %in% class(bc) ) next()
        if ( exists("error.flag", bc) && is.finite(bc$error.flag)) {
          gsinf$bc.error.flag = bc$error.flag
          print( bc$error.flag )
          next()
        }
        if ( ! exists( "res", bc) ) {
          gsinf$bc.error.flag = "No solution found"
          print( bc$error.flag )
          next()
        }
        if ( exists( "dt", bc$res) && length(bc$res$dt) == 0 ) {
          gsinf$bc.error.flag = "No time solution found"
          print( bc$error.flag )
          next()
        }
        if ( exists( "dt", bc$res) && !is.finite(bc$res$dt) ) {
          gsinf$bc.error.flag = "No time solution found"
          print( bc$error.flag )
          next()
        }

        bottom.contact.plot( bc )
        plotfn = file.path( scanmar.bc.dir, "figures", paste(id, "pdf", sep="." ) )
        print (plotfn)
        dev.flush()
        dev.copy2pdf( file=plotfn )

        gsinf$bc0.datetime[gii] = bc$bottom0
        gsinf$bc1.datetime[gii] = bc$bottom1
        gsinf$bottom_duration[gii] = bc$bottom.diff
        gsinf$bc0.sd[gii] = bc$bottom0.sd
        gsinf$bc1.sd[gii] = bc$bottom1.sd
        gsinf$bc0.n[gii] =  bc$bottom0.n
        gsinf$bc1.n[gii] =  bc$bottom1.n

        bci = range(bc$bottom.contact.indices, na.rm=TRUE)
        gsinf$bc.lon0[gii] = bc$plotdata$longitude[ bci[1] ]
        gsinf$bc.lon1[gii] = bc$plotdata$longitude[ bci[2] ]
        gsinf$bc.lat0[gii] = bc$plotdata$latitude[ bci[1] ]
        gsinf$bc.lat1[gii] = bc$plotdata$latitude[ bci[2] ]

        gsinf$bc.depth.mean[gii] = bc$depth.mean
        gsinf$bc.depth.sd[gii] = bc$depth.sd

        if ( exists("surface.area", bc) ) {
          # print( bc$surface.area )
          if ( is.list( bc$surface.area )  & !is.na( bc$surface.area )  ) {
            if ( exists("door.sa", bc$surface.area ) ) gsinf$door.sa[gii] =  bc$surface.area$door.sa
            if ( exists("wing.sa", bc$surface.area ) ) gsinf$wing.sa[gii] =  bc$surface.area$wing.sa
            if ( exists("door.mean", bc$surface.area ) ) gsinf$door.mean[gii] =  bc$surface.area$door.mean
            if ( exists("wing.mean", bc$surface.area ) )  gsinf$wing.mean[gii] =  bc$surface.area$wing.mean
            if ( exists("door.sd", bc$surface.area ) ) gsinf$door.sd[gii] =  bc$surface.area$door.sd
            if ( exists("wing.sd", bc$surface.area ) ) gsinf$wing.sd[gii] =  bc$surface.area$wing.sd
            if ( exists("distances.total", bc$surface.area ) ) gsinf$bc.dist[gii] = max(bc$surface.area$distances.total, na.rm=TRUE)
            if ( exists("distances.vertical", bc$surface.area ) ) gsinf$bc.dist.v[gii]  =  max(bc$surface.area$distances.vertical, na.rm=TRUE)
            if ( exists("distances.horizontal", bc$surface.area ) ) gsinf$bc.dist.h[gii]  =  max(bc$surface.area$distances.horizontal, na.rm=TRUE)
          }
        }

        fn.bc = file.path( scanmar.bc.dir, "results", paste( "bc", id, "rdata", sep=".") )
        save ( bc, file=fn.bc, compress=TRUE )
      }
      if (is.null(debugid))  {
        # save only if not debug
        ## END of re-run area ...
        gsinf.tokeep = setdiff( names( gsinf), setdiff(gsinf0.names, "id") )
        gsinf = gsinf[ , gsinf.tokeep ]
        fn = file.path( scanmar.bc.dir, paste( "gsinf.bottom.contact", YR, "rdata", sep=".")  )
        save(gsinf, file=fn, compress= TRUE)
      }

    }  # end for years


    return( YRS )
  }




  # -------------
  if (DS %in% c("scanmar.filtered", "scanmar.filtered.redo", "scanmar.filtered.indices" )) {

    if (is.null (YRS) ) YRS = p$netmensuration.years

    scanmar.filtered.dir =  file.path(p$scanmar.dir, "scanmar.filtered" )
    dir.create( scanmar.filtered.dir, recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.filtered.dir, "results"), recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.filtered.dir, "figures"), recursive=TRUE, showWarnings=FALSE )

    if(DS=="scanmar.filtered"){
      nm = NULL
      for ( YR in YRS ) {
        sc = netmensuration.scanmar( DS="sanity.checks", p=p, YRS=YR )
        ii = netmensuration.scanmar( DS="scanmar.filtered.indices", p=p, YRS=YR )
        if ( !is.null(sc) & !is.null(ii) & length(ii) > 0)  nm = rbind( nm, sc [ii,] )
      }
      return(nm)
    }

    if(DS=="scanmar.filtered.indices"){
      res = NULL
      for ( YR in YRS ) {
        out = NULL
        fn = file.path( scanmar.filtered.dir, paste( "scanmar.filtered.indices", YR, "rdata", sep=".")  )
        if (file.exists(fn)) load(fn)
        res = c( res, out )
      }
      res = sort( unique( res))
      return(res)
    }

    nreq = 30
    sd.max = 30  # in seconds
    counts = 0

    for ( YR in YRS ) {
      out = NULL
      gs = netmensuration.scanmar( DS="bottom.contact", p=p, YRS=YR )
      if (is.null ( gs)) next()
      nm = netmensuration.scanmar( DS="sanity.checks", p=p, YRS=YR )
      if (is.null( nm)) next()
      nm$good = TRUE
      nm$good[which(!is.finite(nm$depth)) ] = FALSE
      nm$good[which(is.na( nm$id ) )  ]   = FALSE
      w = which( nm$good)
      if ( length(w) < 1 ) next()
      uid = unique( nm$id[w] )
      for ( id in uid) {
        # print( id)
        kk = jj = NULL
        kk = which( gs$id==id )
        jj = which( nm$id==id & nm$good )  # rows of nm with scanmar/marport data
        if (length( kk ) < 1) next()
        if (length( jj ) < nreq ) next()
        tk = which( nm$timestamp[jj] >= gs$bc0.datetime[kk] & nm$timestamp[jj] <= gs$bc1.datetime[kk] & nm$good[jj] )
        if (length(tk) < nreq ) next()
        ii = jj[tk]
        if ( length( which( is.finite(nm[ii, "depth"]))) < nreq ) next()
        if ( all (is.finite( c( gs$bc0.sd[kk], gs$bc1.sd[kk] ) ))) {
          if ( gs$bc0.sd[kk] <= sd.max & gs$bc1.sd[kk] <= sd.max )  {
            out = c(out, ii)
          }
        }
      }
      fn = file.path( scanmar.filtered.dir, paste( "scanmar.filtered.indices", YR, "rdata", sep=".")  )
      save( out, file=fn, compress= TRUE)
      nc = length( out)
      nu = length( uid )
      print( paste(fn, nc, nu) )
      counts = counts + nu
    } # end for years
    print( paste("Total count of unique id: ", counts ) )
    return( YRS)
  }



  # -----------------------------------

}



