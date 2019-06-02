

groundfish.db = function(  p=NULL, DS="refresh.all.data.tables", yrs=NULL, netmensuration.do=FALSE, ...  ) {

  # mostly for storage ... not too much processing
  if (is.null(p)) {
    if (is.null(yrs)){
      p = groundfish_parameters(...)
    } else {
      p = groundfish_parameters(yrs=yrs, ...)
    }
  }


  # ----------------

  if (DS =="refresh.bio.species.codes") {
    # the following is copied from taxonomy/src/taxonomy.r
    groundfish.db(p=p, DS="spcodes.rawdata.redo" )
    # bootstrap an initial set of tables .. these will be incomplete as a parsimonious tree needs to be created first but
    # it depends upon the last file created taxonomy.db("complete") .. so ...
    taxonomy.db( "groundfish.itis.redo" )  ## link itis with groundfish tables using taxa names, vernacular, etc
    taxonomy.db( "full.taxonomy.redo" )  # merge full taxonomic hierrachy (limit to animalia and resolved to species)
    ## taxonomy.db( "parsimonious.redo" )  # (re)create lookups from old codes to a parsimonious species list
    taxonomy.db( "life.history.redo" ) # add life history data (locally maintained in groundfish.lifehistory.manually.maintained.csv )
    taxonomy.db( "complete.redo" )
    taxonomy.db( "parsimonious.redo" )
  }


  # ----------------

  if (DS=="refresh.all.data.tables") {

    # rawdata data dump of aegis tables
    groundfish.db(p=p, DS="gscat.rawdata.redo" )
    groundfish.db(p=p, DS="gsdet.rawdata.redo" )
    groundfish.db(p=p, DS="gsinf.rawdata.redo" )

    #groundfish.db(p=p, DS="gshyd.profiles.rawdata.redo" )

    groundfish.db(p=p, DS="gsmissions.rawdata.redo" ) #  not working?

    update.infrequently = FALSE
    if (update.infrequently) {
      # the following do not need to be updated annually
      groundfish.db(p=p, DS="gscoords.rawdata.redo"  )
      groundfish.db(p=p, DS="spcodes.rawdata.redo"  )
      groundfish.db(p=p, DS="gslist.rawdata.redo"  )
      groundfish.db(p=p, DS="gsgear.rawdata.redo"  )
      groundfish.db(p=p, DS="gsstratum.rawdata.redo"  )
    }

    groundfish.db(p=p, DS="gscat.base.redo" )
    groundfish.db(p=p, DS="gsdet.redo" )
    groundfish.db(p=p, DS="gsinf.redo" )

    if ( netmensuration.do ) {
      # requires gsinf
      p$netmensuration.years = intersect( p$netmensuration.years, p$yrs ) #  1990:1992, 2004:max(p$yrs)

      # set id's that should be skipped:: corrupted/improper due to faulty sensor data, etc.
      p$problem.sets = c("NED2014018.27", "NED2014101.11", "NED2014101.12", "NED2014101.13",  "NED2014101.14",
              "NED2010027.143")

      # the following works upon many or annual time slices ( defined in p$netmensuration.years )
      netmensuration.scanmar( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
      netmensuration.scanmar( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
      netmensuration.scanmar( DS="sanity.checks.redo",  p=p )      # QA/QC of data
      netmensuration.scanmar( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

      # netmind base data filtered for fishing periods .. not really used except for some plots
      netmensuration.scanmar( DS="scanmar.filtered.redo",  p=p )

      netmensuration.figures( DS="all", p=p )
      # netmensuration.figures( DS="all", p=p, outdir=file.path(loc, "output")  )
      # see scripts/99.example.netmensuration.r for some additional figures, etc.

    }

    # swept areas are computed in bottom.contact.redo ..
    # this step estimates swept area for those where there was insufficient data to compute SA directly from logs,
    # estimate via approximation using speed etc.
    groundfish.db(p=p, DS="sweptarea.redo" )  ## this is actually gsinf with updated data, etc.

    #groundfish.db(p=p, DS="gshyd.profiles.redo"  )
    #groundfish.db(p=p, DS="gshyd.redo"  )
    #groundfish.db(p=p, DS="gshyd.georef.redo"  )  # not used here but used in temperature re-analysis

    if (0) {

      # lookupregion = lookup.strata()  # create strata vs region lookup table

      Vn = as.character(c(440:442))   # these are alphanumeric codes
      Vs = as.character(c(443:452))   # these are alphanumeric codes
      W  = as.character(c(453:466))   # these are alphanumeric codes
      X  = as.character(c(470:495))   # these are alphanumeric codes
      Ge = c("5Z1","5Z2","5Z3","5Z4","5Z5","5Z6","5Z7","5Z8")

      lookupregion = data.frame(strat=c(Vn, Vs, W, X, Ge), region=NA)
      lookupregion$strat = as.character(lookupregion$strat)

      lookupregion$region[lookupregion$strat %in% Vn] = "Vn"
      lookupregion$region[lookupregion$strat %in% Vs] = "Vs"
      lookupregion$region[lookupregion$strat %in% W ] = "W"
      lookupregion$region[lookupregion$strat %in% X ] = "X"
      lookupregion$region[lookupregion$strat %in% Ge] = "Ge"

    }

    # merged data sets
    groundfish.db(p=p, DS="set.base.redo"  ) # set info .. includes netmensuration.scanmar("sweptarea")
    groundfish.db(p=p, DS="gscat.redo"  ) # catches .. add correction factors

  }


# ----------------------

  if (DS %in% c("spcodes", "spcodes.rawdata", "spcodes.redo", "spcodes.rawdata.redo", "gstaxa", "gstaxa.redo"  ) ) {

    fnspc = file.path( p$datadir, "spcodes.rdata" )

    if ( DS %in% c( "spcodes", "spcodes.rawdata", "gstaxa" ) ) {
      load( fnspc )
      return( spcodes )
    }

    if ( DS %in% c( "spcodes.rawdata.redo", "spcodes.redo", "gstaxa.redo" ) ) {

      connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)
      spcodes = ROracle::dbGetQuery(connect, "select * from groundfish.gsspecies", as.is=T)
      ROracle::dbDisconnect(connect)
      names(spcodes) =  tolower( names(spcodes) )
      save(spcodes, file=fnspc, compress=T)
      print( fnspc )
      print("Should follow up with a refresh of the taxonomy.db " )
      return( fnspc )
    }
  }


  # --------------------



	if (DS %in% c( "gscat.rawdata", "gscat.rawdata.redo" ) ) {

    fn.root =  file.path( p$datadir, "trawl", "gscat" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( is.null(DS) | DS=="gscat.rawdata" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T )
				for ( fny in fl ) {
				load (fny)
				out = rbind( out, gscat )
			}
			return (out)
    }


    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

		for ( YR in p$yrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      gscat = ROracle::dbGetQuery( connect,  paste(
        "select i.*, substr(mission,4,4) year " ,
        " from groundfish.gscat i " ,
        " where substr(MISSION,4,4)=", YR)
      )

      names(gscat) =  tolower( names(gscat) )
      print(fny)
      save(gscat, file=fny, compress=T)
			gc()  # garbage collection
			print(YR)
		}

    ROracle::dbDisconnect(connect)
    return (fn.root)

	}


  # --------------------



  if (DS %in% c("gscat.base", "gscat.base.redo"  ) ) {

    fn = file.path( p$datadir, "gscat.base.rdata")

    if ( DS=="gscat.base" ) {
      load( fn )
      return (gscat)
    }

    gscat = groundfish.db(p=p, DS="gscat.rawdata"  )  # kg/set
    gscat$year = NULL

    gscat = gscat[ - which(gscat$spec %in% c(9000, 9630, 1200, 9400) ), ]  # 9000 = unident, digested remains; 9630 = organic debris; 1200 fish eggs
    # note: need to move to filtering step. 9400=garbage;

    gscat$totno[ which(gscat$spec ==1920) ] = 1  #  1920=bryozoans
    # still a few things with no weights .. blennies and sticklebacks

    # remove data where species codes are ambiguous, or missing or non-living items
    xx = which( !is.finite( gscat$spec) )
    if (length(xx)>0) gscat = gscat[ -xx, ]

    ii = taxonomy.filter.taxa( gscat$spec, taxafilter="living.only", outtype="rvsurveycodes" )
    gscat = gscat[ ii , ]

    min.number.observations.required = 5
    species.counts = as.data.frame( table( gscat$spec) )
    species.to.remove = as.numeric( as.character( species.counts[ which( species.counts$Freq < min.number.observations.required) , 1 ] ))

    ii = which( gscat$spec %in% species.to.remove )
    gscat = gscat[ -ii , ]

    gscat$id = paste(gscat$mission, gscat$setno, sep=".")

    # filter out strange data
		ii = which( gscat$totwgt >= 9999 )  # default code for NAs -- it seems
    if (length(ii)>0) gscat$totwgt[ii] = NA

		# ii = which( gscat$totwgt >= 5000 )  # upper limit of realistic kg/set
    # if (length(ii)>0) gscat$totwgt[ii] = 5000

		jj = which( gscat$totwgt == 0 )
		if (length(jj)>0) gscat$totwgt[jj] = NA

		kk = which( gscat$totno == 0 )
    if (length(kk)>0) gscat$totno[kk] = NA

    save(gscat, file=fn, compress=T)
    return( fn )
  }


  # -----------------


	if (DS %in% c( "gsdet.rawdata", "gsdet.rawdata.redo" ) ) {
    fn.root =  file.path( p$datadir, "trawl", "gsdet" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( DS=="gsdet.rawdata" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  )
				for ( fny in fl ) {
				load (fny)
				out = rbind( out, gsdet )
			}
			return (out)
    }


    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

		for ( YR in p$yrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      gsdet = ROracle::dbGetQuery( connect,  paste(
        "select i.*, substr(mission,4,4) year" ,
        " from groundfish.gsdet i " ,
        " where substr(mission,4,4)=", YR)
      )
      names(gsdet) =  tolower( names(gsdet) )
      gsdet$mission = as.character( gsdet$mission )
      save(gsdet, file=fny, compress=T)
      print(fny)
			gc()  # garbage collection
			print(YR)
		}
    ROracle::dbDisconnect(connect)

    return (fn.root)

	}

  # ----------------------

  if (DS %in% c("gsdet", "gsdet.redo") ) {


  # --------- codes ----------------
  # sex: 0=?, 1=male, 2=female,  3=?
  # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature),
  #      5=spawning(running), 6=spent, 7=recovering, 8=resting
  # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
  #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
  #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
  # --------- codes ----------------

    fn = file.path( p$datadir, "gsdet.rdata")

    if ( DS=="gsdet" ) {
      load( fn )
      return (gsdet)
    }

    gsdet = groundfish.db(p=p, DS="gsdet.rawdata" )
    gsdet$year = NULL

    oo = which(!is.finite(gsdet$spec) )
    if (length(oo)>0) gsdet = gsdet[-oo,]

    # remove data where species codes are ambiguous, or missing or non-living items
    gsdet = gsdet[ taxonomy.filter.taxa( gsdet$spec, taxafilter="living.only", outtype="rvsurveycodes" ) , ]

    gsdet$id = paste(gsdet$mission, gsdet$setno, sep=".")

    names(gsdet)[which(names(gsdet)=="fsex")] = "sex"
    names(gsdet)[which(names(gsdet)=="fmat")] = "mat"
    names(gsdet)[which(names(gsdet)=="flen")] = "len"  # cm
    names(gsdet)[which(names(gsdet)=="fwt")]  = "mass" # g

    gsdet$mass = gsdet$mass / 1000 # convert from grams to kg
    gsdet = gsdet[, c("id", "spec",  "sex", "mat", "len", "mass", "age") ]

    save(gsdet, file=fn, compress=T)

    return( fn )
  }


  # ----------------------


	if (DS %in% c( "gsinf.rawdata", "gsinf.rawdata.redo" ) ) {

    fn.root =  file.path(p$datadir, "trawl", "gsinf" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( is.null(DS) | DS=="gsinf.rawdata" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  )
				for ( fny in fl ) {
        load (fny)
        out = rbind( out, gsinf )
			}
			return (out)
    }


    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

		for ( YR in p$yrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      gsinf = ROracle::dbGetQuery( connect,  paste(
        "select * from groundfish.gsinf where EXTRACT(YEAR from SDATE) = ", YR )
      )
      names(gsinf) =  tolower( names(gsinf) )
      save(gsinf, file=fny, compress=T)
      print(fny)
			gc()  # garbage collection
			print(YR)
		}

    ROracle::dbDisconnect(connect)
    return (fn.root)

	}



# ----------------------


  if (DS %in% c("gsinf", "gsinf.redo" ) ) {
    fn = file.path( p$datadir, "gsinf.rdata")

    if ( DS=="gsinf" ) {
      load( fn )
      return (gsinf)
    }

    gsinf = groundfish.db(p=p, DS="gsinf.rawdata" )
    names(gsinf)[which(names(gsinf)=="type")] = "settype"

    gsgear = groundfish.db(p=p, DS="gsgear" )
    gsinf = merge (gsinf, gsgear, by="gear", all.x=TRUE, all.y=FALSE, sort= FALSE )

    # fix some time values that have lost the zeros due to numeric conversion
    gsinf$time = as.character(gsinf$time)

    tz.rawdata = "America/Halifax"  ## need to verify if this is correct
    tz.groundfish = "UTC"

    # by default it should be the correct timezone ("localtime") , but just in case
    tz( gsinf$sdate) = tz.rawdata
    gsinf$sdate = with_tz( gsinf$sdate, tz.groundfish )

    gsinf$edate = gsinf$etime
    tz( gsinf$edate) = tz.rawdata
    gsinf$edate = with_tz( gsinf$edate, tz.groundfish )


    # fix sdate - edate inconsistencies .. assuming sdate is correct
    gsinf$timediff.gsinf = gsinf$edate - gsinf$sdate
    oo = which( abs( gsinf$timediff.gsinf)  > dhours( 4 ) )
    if (length(oo)>0) {
      print( "Time stamps sdate and etime (renamed as edate) are severely off (more than 4 hrs):" )
      print( gsinf[oo,] )
      if (FALSE) {
        hist( as.numeric(  gsinf$timediff.gsinf[-oo]), breaks=200 )
        abline (v=30*60, col="red")  # expected value of 30 min
        abline (v=90*60, col="red")  # after 90 min
        abline (v=150*60, col="red")  # after 150 min
      }
    }
    uu = which( gsinf$timediff.gsinf < 0 ) # when tow end is before start
    gsinf$edate[uu]  = NA  # set these to NA until they can be corrected manually
    gsinf$timediff.gsinf[uu] =NA
    print( "Time stamps sdate and etime (renamed as edate) are severely off: edate is before sdate:" )
    print( gsinf[uu,] )

    if (FALSE)  hist( as.numeric(  gsinf$timediff.gsinf), breaks=200 )

    uu = which( gsinf$timediff.gsinf > dminutes(50) & gsinf$timediff.gsinf < dminutes(50+60) ) # assuming 50 min is a max tow length
    if (length(uu)>0) {
      gsinf$edate[uu] = gsinf$edate[uu] - dhours(1) ### this is assuming sdate is correct ... which might not be the case
      if (FALSE) {
        hist( as.numeric(  gsinf$timediff.gsinf[-oo]), breaks=200 )
      }
    }
    gsinf$timediff.gsinf = gsinf$edate - gsinf$sdate
    uu = which( gsinf$timediff.gsinf > dminutes(50) ) # assuming 50 min is a max tow length
    gsinf$edate[uu]  = NA  # set these to NA untile they can be corrected manually
    gsinf$timediff.gsinf[uu] =NA
      if (FALSE) {
        hist( as.numeric(  gsinf$timediff.gsinf), breaks=200 )
        abline (v=30*60, col="red")  # expected value of 30 min
        abline (v=90*60, col="red")  # after 90 min
        abline (v=150*60, col="red")  # after 150 min
      }

    gsinf$yr = lubridate::year( gsinf$sdate)

    gsinf$mission = as.character( gsinf$mission )
    gsinf$strat = as.character(gsinf$strat)
    gsinf$strat[ which(gsinf$strat=="") ] = "NA"
    gsinf$id = paste(gsinf$mission, gsinf$setno, sep=".")
    d = which(duplicated(gsinf$id))
    if (length(d) > 0) message("error: duplicates found in gsinf")

    gsinf$lat = gsinf$slat/100
    gsinf$lon = gsinf$slong/100
    gsinf$lat.end = gsinf$elat/100
    gsinf$lon.end = gsinf$elong/100

    if (mean(gsinf$lon,na.rm=T) >0 ) gsinf$lon = - gsinf$lon  # make sure form is correct
    if (mean(gsinf$lon.end,na.rm=T) >0 ) gsinf$lon.end = - gsinf$lon.end  # make sure form is correct

    gsinf = aegis::convert.degmin2degdec(gsinf, vnames=c("lon", "lat") )
    gsinf = aegis::convert.degmin2degdec(gsinf, vnames=c("lon.end", "lat.end") )

    gsinf$dist_km = gsinf$dist * 1.852  # nautical mile to km
    gsinf$dist_pos = geosphere::distGeo( gsinf[, c("lon","lat")], gsinf[, c("lon.end", "lat.end")])/1000

    ii = which( abs( gsinf$dist_km) > 10 ) # 10 km is safely too extreme
    if (length(ii)> 0) {
      gsinf$dist_km[ii] =  gsinf$dist_pos[ii]
    }

    ii = which( abs( gsinf$dist_pos) > 10 ) # 10 km is safely too extreme
    if (length(ii)> 0) {
      gsinf$dist_pos[ii] = gsinf$dist_km[ii]
      # assuming end positions are incorrect. This may not be a correct assumption!
      gsinf$lon.end[ii] = NA
      gsinf$lat.end[ii] = NA
    }


  ## !! GPS position-based distances do not always match the distance recorded
  ## plot( dist_pos ~ dist_km, gsinf, ylim=c(0,60))

    ft2m = 0.3048
    m2km = 1/1000
    nmi2mi = 1.1507794
    mi2ft = 5280
    gsinf$sakm2 = (41 * ft2m * m2km ) * ( gsinf$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
			oo = which( !is.finite(gsinf$sakm2 ))
				gsinf$sakm2[oo] = median (gsinf$sakm2, na.rm=T)
			pp = which( gsinf$sakm2 > 0.09 )
				gsinf$sakm2[pp] = median (gsinf$sakm2, na.rm=T)
    gsinf$bottom_depth = rowMeans( gsinf[, c("dmax", "depth" )], na.rm = TRUE )  * 1.8288  # convert from fathoms to meters
    ii = which( gsinf$bottom_depth < 10 | !is.finite(gsinf$bottom_depth)  )  # error
    gsinf$bottom_depth[ii] = NA
		gsinf = gsinf[, c("id", "yr", "sdate", "edate", "time", "strat", "area", "speed", "dist_km", "dist_pos",
                      "sakm2", "settype", "gear", "geardesc", "lon", "lat", "lon.end", "lat.end",
                      "surface_temperature","bottom_temperature","bottom_salinity", "bottom_depth")]

    save(gsinf, file=fn, compress=T)
    return(fn)
  }


# -------------


	if (DS %in% c( "gshyd.profiles.rawdata" , "gshyd.profiles.rawdata.redo" ) ) {

    fn.root =  file.path(p$datadir, "trawl", "gshyd" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( is.null(DS) | DS=="gshyd.profiles.rawdata" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  )
				for ( fny in fl ) {
				load (fny)
				out = rbind( out, gshyd )
			}
			return (out)
    }


    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

		for ( YR in p$yrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      gshyd = ROracle::dbGetQuery( connect,  paste(
        "select i.*, j.YEAR " ,
        " from groundfish.gshyd i, groundfish.gsmissions j " ,
        " where i.MISSION(+)=j.MISSION " ,
        " and YEAR=", YR )
      )
      names(gshyd) =  tolower( names(gshyd) )
      # if(all(is.na(gshyd$mission))) {
      # 	#if gshyd is not loaded and the odf files are obtained AMC
	     #    fy <- file.path(project.datadirectory("aegis", "temperature"), "data", "archive", "ctd",YR)
	     #    o <- compileODF(path=fy)
	     #    gshyd <- makeGSHYD(o)
      # }
      gshyd$mission = as.character( gshyd$mission )
      save(gshyd, file=fny, compress=T)
      print(fny)
			gc()  # garbage collection
			print(YR)
		}
		ROracle::dbDisconnect(connect)

    return ( fn.root )

	}

# ----------------------



  if (DS %in% c("gshyd.profiles", "gshyd.profiles.redo" ) ) {
    # full profiles
    fn = file.path( p$datadir,"gshyd.profiles.rdata")
    if ( DS=="gshyd.profiles" ) {
      load( fn )
      return (gshyd)
    }

    gshyd = groundfish.db(p=p, DS="gshyd.profiles.rawdata" )
    gshyd$id = paste(gshyd$mission, gshyd$setno, sep=".")
    gshyd = gshyd[, c("id", "sdepth", "temp", "sal", "oxyml" )]
    save(gshyd, file=fn, compress=T)
    return( fn )
  }


# ----------------------



  if (DS %in% c("gshyd", "gshyd.redo") ) {
    # hydrographic info at deepest point
    fn = file.path( p$datadir,"gshyd.rdata")
    if ( DS=="gshyd" ) {
      load( fn )
      return (gshyd)
    }
    gshyd = groundfish.db(p=p, DS="gshyd.profiles" )
    nr = nrow( gshyd)

    # candidate depth estimates from profiles
    deepest = NULL
    t = which( is.finite(gshyd$sdepth) )
    id = unique(gshyd$id)
    for (i in id) {
      q = intersect( which( gshyd$id==i), t )
      r = which.max( gshyd$sdepth[q] )
      deepest = c(deepest, q[r])
    }
    gshyd = gshyd[deepest,]
    oo = which( duplicated( gshyd$id ) )
    if (length(oo) > 0) stop( "Duplicated data in GSHYD" )

    gsinf = groundfish.db(p=p, DS="gsinf" )
    gsinf = gsinf[, c("id", "bottom_temperature", "bottom_salinity", "bottom_depth" ) ]
    gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )

    ## bottom_depth is a profile-independent estimate .. asuming it has higher data quality
    ii = which(!is.finite( gshyd$bottom_depth ))
    if (length(ii)>0) gshyd$bottom_depth[ii] = gshyd$sdepth[ii]
    gshyd$sdepth = gshyd$bottom_depth        #overwrite
    ii = which( gshyd$sdepth < 10 )
    if (length(ii)>0) gshyd$sdepth[ii] = NA

    ii = which( is.na( gshyd$temp) )
    if (length(ii)>0) gshyd$temp[ii] =  gshyd$bottom_temperature[ii]

    jj = which( is.na( gshyd$sal) )
    if (length(jj)>0) gshyd$sal[jj] =  gshyd$bottom_salinity[jj]
    gshyd$sal[gshyd$sal<5 ] = NA

    gshyd$bottom_depth = NULL
    gshyd$bottom_temperature = NULL
    gshyd$bottom_salinity = NULL


    save(gshyd, file=fn, compress=T)
    return( fn )
  }

# ----------------------


  if (DS %in% c("gshyd.georef", "gshyd.georef.redo") ) {
    # hydrographic info georeferenced
    fn = file.path( p$datadir,"gshyd.georef.rdata")
    if ( DS=="gshyd.georef" ) {
      load( fn )
      return (gshyd)
    }
    gsinf = groundfish.db(p=p, DS="gsinf" )
    gsinf$timestamp = gsinf$sdate
    gsinf$yr = lubridate::year( gsinf$timestamp)
    gsinf$longitude = gsinf$lon
    gsinf$latitude = gsinf$lat
    gsinf = gsinf[ , c( "id", "lon", "lat", "yr", "timestamp" ) ]
    gshyd = groundfish.db(p=p, DS="gshyd.profiles" )
    gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )
    gshyd$sal[gshyd$sal<5]=NA
    save(gshyd, file=fn, compress=T)
    return( fn )
  }


  # ----------------------


  if (DS %in% c("gsstratum", "gsstratum.obdc.redo") ) {
    fn = file.path( p$datadir,"gsstratum.rdata")
    if ( DS=="gsstratum" ) {
      load( fn )
      return (gsstratum)
    }

    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user,
        password=oracle.personal.password, believeNRows=F)
    gsstratum =  ROracle::dbGetQuery(connect, "select * from groundfish.gsstratum", as.is=T)
    ROracle::dbDisconnect(connect)
    names(gsstratum) =  tolower( names(gsstratum) )
    save(gsstratum, file=fn, compress=T)
    print(fn)
    return( fn )
  }


  # ----------------------


  if (DS %in% c("gsgear", "gsgear.rawdata.redo") ) {
    fn = file.path( p$datadir,"gsgear.rdata")
    if ( DS=="gsgear" ) {
      load( fn )
      return (gsgear)
    }

    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user,
        password=oracle.personal.password, believeNRows=F)
    gsgear =  ROracle::dbGetQuery(connect, "select * from groundfish.gsgear", as.is=T)
    ROracle::dbDisconnect(connect)
    names(gsgear) =  tolower( names(gsgear) )
    save(gsgear, file=fn, compress=T)
    print(fn)
    return( fn )
  }



  # ----------------------


  if (DS %in% c("gscoords", "gscoords.rawdata.redo") ) {
    # detailed list of places, etc
    fn = file.path( p$datadir,"gscoords.rdata")
    if ( DS=="gscoords" ) {
      load( fn )
      return (gscoords)
    }

    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user,
        password=oracle.personal.password, believeNRows=F)
    coords = ROracle::dbGetQuery(connect, "select * from mflib.mwacon_mapobjects", as.is=T)
    ROracle::dbDisconnect(connect)
    names(coords) =  tolower( names(coords) )
    save(coords, file=fn, compress=T)
    print(fn)
    return( fn )
  }

# ----------------------


 if (DS %in% c("gslist", "gslist.rawdata.redo") ) {
    fn = file.path( p$datadir,"gslist.rdata")
    if ( DS=="gslist" ) {
      load( fn )
      return (gslist)
    }

    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user,
        password=oracle.personal.password, believeNRows=F)
    gslist = ROracle::dbGetQuery(connect, "select * from groundfish.gs_survey_list")
    ROracle::dbDisconnect(connect)
    names(gslist) =  tolower( names(gslist) )
    save(gslist, file=fn, compress=T)
    print(fn)
    return( fn )
  }

# ----------------------

  if (DS %in% c("gsmissions", "gsmissions.rawdata.redo") ) {
    fnmiss = file.path( p$datadir,"gsmissions.rdata")

    if ( DS=="gsmissions" ) {
      load( fnmiss )
      return (gsmissions)
    }


    connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user,
        password=oracle.personal.password, believeNRows=F)
      gsmissions = ROracle::dbGetQuery(connect, "select MISSION, VESEL, CRUNO from groundfish.gsmissions")
      ROracle::dbDisconnect(connect)
      names(gsmissions) =  tolower( names(gsmissions) )
      save(gsmissions, file=fnmiss, compress=T)
    print(fnmiss)
    return( fnmiss )
  }

  # ----------------------


  if (DS %in% c("gscat", "gscat.redo") ) {
    # merge vessel info to compute trapable units / cf_cat

    fn = file.path( p$datadir, "gscat.rdata")
    if ( DS=="gscat" ) {
      load( fn )
      return (gscat)
    }

    gscat = groundfish.db(p=p, DS="gscat.base" ) #kg/set, no/set
    gscat = gscat[, c("id", "spec", "totwgt", "totno", "sampwgt" )] # kg, no/set

    set = groundfish.db(p=p, DS="set.base" )
    gscat = merge(x=gscat, y=set, by=c("id"), all.x=T, all.y=F, sort=F)
    rm (set)

    # combine correction factors or ignore trapability corrections ..
    # plaice correction ignored as they are size-dependent

    # correct for different survey vessels (after, L.P Fanning 1985):
    #   to obtain Alfred Needler comparable units:
    # Lady Hammond (1982) and Alfred Needler (1983 to present) used a Western IIA Otter Trawl
    # whereas The A.T. Cameron used a Yankee 36 ft trawl between 1970 to 1981

    # vessel change correction factors apply to these years:
    HAM=1   #  Lady Hammond (1979 - 1981)
    ATC=2   #  A.T. Cameron (1982 - 1983)

    # species codes used by the database
    cod=10
    haddock=11
    whitehake=12
    silverhake=19
    plaicelarge=40
    plaicesmall=40
    witch=41
    yellowtail=42
    winterflounder=43

    vc = NULL
    vc$cod[HAM]         = 0.8
    vc$haddock[HAM]     = 1.0
    vc$whitehake[HAM]   = 1.0
    vc$silverhake[HAM]  = 1.0
    vc$plaicesmall[HAM] = 1   # <=28cm
    vc$plaicelarge[HAM] = 1   # > 28cm
    vc$witch[HAM]       = 0.8
    vc$yellowtail[HAM]  = 0.8
    vc$winterflounder[HAM] = 1.0

    vc$cod[ATC]         = 0.8
    vc$haddock[ATC]     = 1.2
    vc$whitehake[ATC]   = 1.0
    vc$silverhake[ATC]  = 1.0
    vc$plaicesmall[ATC] = 0.7
    vc$plaicelarge[ATC] = 1.0
    vc$witch[ATC]       = 0.8
    vc$yellowtail[ATC]  = 0.8
    vc$winterflounder[ATC] = 1.0

    ids = substring(gscat$id,1,3)
    spec = gscat$spec

    gscat$cf_vessel = 1  # initialise .. default 1==no change
    gscat$cf_vessel[ which((ids=="HAM" & spec==cod)) ] = vc$cod[HAM]
    gscat$cf_vessel[ which((ids=="HAM" & spec==witch)) ] = vc$witch[HAM]
    gscat$cf_vessel[ which((ids=="HAM" & spec==yellowtail)) ] = vc$yellowtail[HAM]
    gscat$cf_vessel[ which((ids=="ATC" & spec==cod)) ] = vc$cod[ATC]
    gscat$cf_vessel[ which((ids=="ATC" & spec==haddock)) ] = vc$haddock[ATC]
    gscat$cf_vessel[ which((ids=="ATC" & spec==plaicesmall && len<=28)) ] = vc$plaicesmall[ATC]
    gscat$cf_vessel[ which((ids=="ATC" & spec==witch)) ] = vc$witch[ATC]
    gscat$cf_vessel[ which((ids=="ATC" & spec==yellowtail)) ] = vc$yellowtail[ATC]

    gscat$cf_cat = gscat$cf_tow * gscat$cf_vessel  # these are multipliers to get totwgt and totno as per unit surface area of "Alfred Needler comparable units"

    # ---- NOTE ::: sampwgt seems to be unreliable  -- recompute where necessary in "det"
    save(gscat, file=fn, compress=T )

    return (fn)
  }


# ----------------------


if (DS %in% c("sweptarea", "sweptarea.redo" )) {
  # merge bottom contact data into the main gsinf table and
  # then do some sanity checks on the SA estimates and
  # then compute best estimates where data are missing

  fn = file.path( p$datadir, "gsinf.sweptarea.rdata" )

  if (DS=="sweptarea") {
    gsinf = NULL
    if (file.exists(fn)) load(fn)
    return( gsinf )
  }

  gsinf = groundfish.db(p=p, DS="gsinf" )
  gsinf_bc = netmensuration.scanmar( DS="bottom.contact", p=p )

  toreject = which( !is.na( gsinf_bc$bc.error.flag ) )

  gsinf_bc$wing.sa [ toreject] = NA
  gsinf_bc$door.sa [ toreject] = NA

  newvars = setdiff( names( gsinf_bc ), names( gsinf)  )
  tokeep = c("id", newvars )

  ng = nrow( gsinf)
  gsinf = merge( gsinf, gsinf_bc[,tokeep], by="id", all.x=TRUE, all.y=FALSE )
  if ( ng != nrow(gsinf) ) error("merge error" )

  gsinf$dist_wing = gsinf$wing.sa / gsinf$wing.mean * 1000  # est of length of the tow (km)
  gsinf$dist_door = gsinf$door.sa / gsinf$door.mean * 1000 # est of length of the tow (km)
  gsinf$yr = lubridate::year(gsinf$sdate)

  # empirical distribution suggests (above)  hard limits of rn, ~ same as gating limits
  # .. too extreme means interpolation did not work well .. drop
  qnts = c( 0.005, 0.995 )
  w2a = which( gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5) )     # for distribution checks for western IIA trawl

      if (0) hist( gsinf$wing.mean[w2a], "fd", xlim=c( 8,22) )
      # rn = quantile( gsinf$wing.mean[w2a], probs=qnts, na.rm=TRUE )  # ranges from 11 to 20
      rn = c(6, 22)  # manual override
      i = which( (gsinf$wing.mean < rn[1] | gsinf$wing.mean > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5)  )
      if ( length(i) > 0) {
        gsinf$wing.mean[i] = NA
        gsinf$wing.sa[i] = NA
        gsinf$wing.sd[i] = NA
      }

      if (0) hist( gsinf$door.mean[w2a], "fd", xlim=c( 0, 85 ) )
      # rn = quantile( gsinf$door.mean[w2a], probs=qnts, na.rm=TRUE )  # ranges from 13 to 79
      rn = c(6, 85)
      i = which( (gsinf$door.mean < rn[1] | gsinf$door.mean > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5)  )
      if ( length(i) > 0) {
        gsinf$door.mean[i] = NA
        gsinf$door.sa[i] = NA
        gsinf$door.sd[i] = NA
      }

      # unreliable SD
      if (0) hist( gsinf$wing.sd[w2a], "fd", xlim=c( 0.1, 5 ) )
      # rn = quantile( gsinf$wing.sd[w2a], probs=qnts, na.rm=TRUE )  # ranges from 0.16 to 3.62
      rn = c(0.1, 5)
      i = which( (gsinf$wing.sd < rn[1] | gsinf$wing.sd > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5) )
      if ( length(i) > 0) {
        gsinf$wing.mean[i] = NA
        gsinf$wing.sa[i] = NA
        gsinf$wing.sd[i] = NA
      }

      if (0) hist( gsinf$door.sd[w2a], "fd", xlim=c( 0.1, 25 ) )
      # rn = quantile( gsinf$door.sd[w2a], probs=qnts, na.rm=TRUE )  # ranges from 0.42 to 16 .. using 0.1 to 20
      rn =c(0.1, 25)
      i = which( (gsinf$door.sd < rn[1] | gsinf$door.sd > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5) )
      if ( length(i) > 0) {
        gsinf$door.mean[i] = NA
        gsinf$door.sa[i] = NA
        gsinf$door.sd[i] = NA
      }

      # unreliable SA's
      if (0) hist( gsinf$wing.sa[w2a], "fd", xlim=c( 0.01, 0.08 ) )
      # rn = quantile( gsinf$wing.sa[w2a], probs=qnts, na.rm=TRUE )  # ranges from 0.02 to 0.064 .. using 0.01 to 0.08
      rn = c(0.02, 0.07)
      i = which( (gsinf$wing.sa < rn[1] | gsinf$wing.sa > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5) )
      if ( length(i) > 0) {
        gsinf$wing.mean[i] = NA
        gsinf$wing.sa[i] = NA
        gsinf$wing.sd[i] = NA
      }


      if (0) hist( gsinf$door.sa[w2a], "fd" , xlim=c( 0.02, 0.30 ))
      # rn = quantile( gsinf$door.sa[w2a], probs=qnts, na.rm=TRUE )  # ranges from 0.04 to 0.25 .. using 0.02 to 0.30
      rn = c(0.02, 0.26)
      i = which( (gsinf$door.sa < rn[1] | gsinf$door.sa > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5)  )
      if ( length(i) > 0) {
        gsinf$door.mean[i] = NA
        gsinf$door.sa[i] = NA
        gsinf$door.sd[i] = NA
      }


      # tow length est
      if (0) hist( gsinf$dist_wing[w2a], "fd", xlim=c( 1.75, 4.5 ) )
      # rn = quantile( gsinf$dist_wing[w2a], probs=qnts, na.rm=TRUE )  # ranges from 2.06 to 4.2 .. using 1.75 to 4.5
      rn = c(1.96, 4.0 )
      i = which( (gsinf$dist_wing < rn[1] | gsinf$dist_wing > rn[2] ) & gsinf$geardesc == "Western IIA trawl"  & gsinf$settype %in% c(1,2,5) )
      if ( length(i) > 0) {
        gsinf$dist_wing[i] = NA
        gsinf$wing.mean[i] = NA
        gsinf$wing.sa[i] = NA
        gsinf$wing.sd[i] = NA
      }

      if (0) hist( gsinf$dist_door[w2a], "fd", xlim=c( 1.75, 4.5 )  )
      # rn = quantile( gsinf$dist_door[w2a], probs=qnts, na.rm=TRUE )  # ranges from 2.03 to 4.2 .. using 1.75 to 4.5
      rn = c(1.75, 4.25)
      i = which( (gsinf$dist_door < rn[1] | gsinf$dist_door > rn[2] ) & gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5)  )
      if ( length(i) > 0) {
        gsinf$dist_door[i] = NA
        gsinf$door.mean[i] = NA
        gsinf$door.sa[i] = NA
        gsinf$door.sd[i] = NA
      }

      # basic (gating) sanity checks finished ..
      # now estimate swept area for data locations where estimates
      # do not exist or are problematic from bottom contact approach

      ## dist_km is logged distance in gsinf
      ## dist_pos is distance based upon logged start/end locations
      ## dist_bc is distance from bottom contact analysis
      ## dist_wing / dist_door .. back caluculated distance from SA

      # estimate distance of tow track starting with most reliable to least
      gsinf$distance = NA
      gsinf$distance[w2a] = gsinf$dist_wing[w2a]

      ii = intersect( which( !is.finite( gsinf$distance ) ) , w2a)
      if (length(ii) > 0) gsinf$distance[ii] = gsinf$dist_door[ii]

      ii = intersect( which( !is.finite( gsinf$distance ) ), w2a )
      if (length(ii) > 0) gsinf$distance[ii] = gsinf$dist_pos[ii]

      ii = intersect( which( !is.finite( gsinf$distance ) ), w2a )
      if (length(ii) > 0) gsinf$distance[ii] = gsinf$dist_km[ii]



      # wing and door spread models
      # there are differences due to nets config and/or sensors each year ..
      require(mgcv)
      gsinf$yr0 = gsinf$yr  # yr will be modified to permit prediction temporarilly

      ii = intersect( which( !is.finite( gsinf$wing.mean )) , w2a )
      if (length(ii)>0 & length(which( is.finite( gsinf$wing.mean))) > 100 ) {
        wm = gam( wing.mean ~ factor(yr) + s(lon,lat) + s(bottom_depth)+s(door.mean), data= gsinf[ w2a,] )
  #R-sq.(adj) =  0.633   Deviance explained = 64.2%
  #GCV = 3.3434  Scale est. = 3.2603    n = 1774
        jj = which( ! gsinf$yr %in% as.numeric(as.character(wm$xlevels[["factor(yr)"]])) )
        if (length(jj)>0) gsinf$yr[jj] = 2004  # to minimise discontinuity across year (and also visually close to median level)
        gsinf$wing.mean[ii] = predict( wm, newdata=gsinf[ii,], type="response" )
        gsinf$wing.sd[ii] = NA  # ensure sd is NA to mark it as having been estimated after the fact
      }


      ii = intersect( which( !is.finite( gsinf$wing.mean )) , w2a )
      if (length(ii)>0 & length(which( is.finite( gsinf$wing.mean))) > 100 ) {
        wm = gam( wing.mean ~ factor(yr) + s(lon,lat) + s(bottom_depth), data= gsinf[ intersect( w2a, which(! is.na(gsinf$wing.sd))),] )
  # summary(wm)
  #R-sq.(adj) =  0.591   Deviance explained = 60.1%
  #GCV = 3.7542  Scale est. = 3.6646    n = 1795
        jj = which( ! gsinf$yr %in% as.numeric(as.character(wm$xlevels[["factor(yr)"]])) )
        if (length(jj)>0) gsinf$yr[jj] = 2004  # to minimise discontinuity across year (and also visually close to median level)
        gsinf$wing.mean[ii] = predict( wm, newdata=gsinf[ii,], type="response" )
        gsinf$wing.sd[ii] = NA  # ensure sd is NA to mark it as having been estimated after the fact
      }


      ii = intersect( which( !is.finite( gsinf$wing.mean )) , w2a )
      if (length(ii)>0 & length(which( is.finite( gsinf$wing.mean))) > 100 ) {
         wm = gam( wing.mean ~ factor(yr) + s(lon,lat) , data= gsinf[ intersect( w2a, which(! is.na(gsinf$wing.sd))),] )
  # summary(wm)
  # R-sq.(adj) =  0.509   Deviance explained = 51.9%
  # GCV = 4.5011  Scale est. = 4.4031    n = 1795
        jj = which( ! gsinf$yr %in% as.numeric(as.character(wm$xlevels[["factor(yr)"]])) )
        if (length(jj)>0) gsinf$yr[jj] = 2004  # to minimise discontinuity across year (and also visually close to median level)
        gsinf$wing.mean[ii] = predict( wm, newdata=gsinf[ii,], type="response" )
        gsinf$wing.sd[ii] = NA  # ensure sd is NA to mark it as having been estimated after the fact
      }


      ii = intersect( which( !is.finite( gsinf$door.mean )), w2a )
      if (length(ii)>0 & length(which( is.finite( gsinf$door.mean))) > 100 ) {
        wd = gam( door.mean ~ factor(yr) + s(lon,lat) + s(bottom_depth)+s(wing.mean), data= gsinf[w2a,] )
  #R-sq.(adj) =  0.654   Deviance explained = 66.3%
  #GCV = 86.858  Scale est. = 84.594    n = 1454
        jj = which( ! as.character( gsinf$yr0) %in%  wd$xlevels[["factor(yr)"]] )
        if (length(jj)>0) gsinf$yr[jj] = 2004  # to minimise discontinuity across year (and also visually close to median level)
        gsinf$door.mean[ii] = predict( wd, newdata=gsinf[ii,], type="response" )
        gsinf$door.sd[ii] = NA  # ensure sd is NA to mark it as having been estimated after the fact
      }

      ii = intersect( which( !is.finite( gsinf$door.mean )), w2a )
      if (length(ii)>0 & length(which( is.finite( gsinf$door.mean))) > 100 ) {
        wd = gam( door.mean ~ factor(yr) + s(lon,lat) + s(bottom_depth), data= gsinf[ intersect( w2a, which(! is.na(gsinf$door.sd))),] )
        #      summary(wd)
  # R-sq.(adj) =   0.58   Deviance explained = 59.2%
  # GCV = 105.65  Scale est. = 102.61    n = 1454
        jj = which( ! as.character( gsinf$yr0) %in%  wd$xlevels[["factor(yr)"]] )
        if (length(jj)>0) gsinf$yr[jj] = 2004  # to minimise discontinuity across year (and also visually close to median level)
        gsinf$door.mean[ii] = predict( wd, newdata=gsinf[ii,], type="response" )
        gsinf$door.sd[ii] = NA  # ensure sd is NA to mark it as having been estimated after the fact
      }

      ii = intersect( which( !is.finite( gsinf$door.mean )), w2a )
      if (length(ii)>0 & length(which( is.finite( gsinf$door.mean))) > 100 ) {
        wd = gam( door.mean ~ factor(yr) + s(lon,lat) , data= gsinf[ intersect( w2a, which(! is.na(gsinf$door.sd))),] )
        #      summary(wd)
  #R-sq.(adj) =  0.486   Deviance explained = 50.2%
  #GCV = 2.2601  Scale est. = 2.1869    n = 1209
        jj = which( ! as.character( gsinf$yr0) %in%  wd$xlevels[["factor(yr)"]] )
        if (length(jj)>0) gsinf$yr[jj] = 2004  # to minimise discontinuity across year (and also visually close to median level)
        gsinf$door.mean[ii] = predict( wd, newdata=gsinf[ii,], type="response" )
        gsinf$door.sd[ii] = NA  # ensure sd is NA to mark it as having been estimated after the fact
      }


      # return correct years to data
      gsinf$yr =gsinf$yr0
      gsinf$yr0 = NULL

      # estimate SA:
      gsinf$wing.sa.crude = gsinf$distance * gsinf$wing.mean /1000
      gsinf$door.sa.crude = gsinf$distance * gsinf$door.mean /1000

      # gating
      bad = intersect( which( gsinf$wing.sa.crude > 0.09 ) , w2a)
      gsinf$wing.sa.crude[bad] = NA

      bad = intersect( which( gsinf$door.sa.crude > 0.03 ), w2a)
      gsinf$door.sa.crude[bad] = NA

      bad = intersect( which( gsinf$wing.sa > 0.09 ) , w2a)
      gsinf$wing.sa[bad] = NA

      bad = intersect( which( gsinf$door.sa > 0.03 ), w2a)
      gsinf$door.sa[bad] = NA

      ## create sweptarea :
      gl = range(gsinf$wing.sa, na.rm=TRUE) # best screening so far ..
      gsinf$sweptarea = gsinf$wing.sa

      ii = intersect( which( !is.finite( gsinf$sweptarea ) ), w2a)
      if (length(ii) > 0) gsinf$sweptarea[ii] = gsinf$wing.sa.crude[ii]

      jj = which(gsinf$sweptarea < gl[1] | gsinf$sweptarea > gl[2])
      if (length(jj) > 0) gsinf$sweptarea[jj] = NA

      ii = intersect( which( !is.finite( gsinf$door.sa ) ), w2a)
      if (length(ii) > 0) gsinf$door.sa[ii] = gsinf$door.sa.crude[ii]

      sayrw =  tapply( gsinf$wing.sa, gsinf$yr, mean, na.rm=TRUE)
      sayrp =  tapply( gsinf$sakm2, gsinf$yr, mean, na.rm=TRUE)  # based only on tow distance and "standard tow's" dimensions
      sayrwc =  tapply( gsinf$wing.sa.crude, gsinf$yr, mean, na.rm=TRUE)  # based on overall median width of all sets and towed distance

      ii = intersect( which( !is.finite( gsinf$sweptarea ) ), w2a)
      if (length(ii) > 0 ) gsinf$sweptarea[ii] = sayrw[as.character(gsinf$yr[ii])]
      jj = which(gsinf$sweptarea < gl[1] | gsinf$sweptarea > gl[2])
      if (length(jj) > 0) gsinf$sweptarea[jj] = NA

      ii = intersect( which( !is.finite( gsinf$sweptarea ) ), w2a)
      if (length(ii) > 0 ) gsinf$sweptarea[ii] = sayrwc[as.character(gsinf$yr[ii])]
      jj = which(gsinf$sweptarea < gl[1] | gsinf$sweptarea > gl[2])
      if (length(jj) > 0) gsinf$sweptarea[jj] = NA

      ii = intersect( which( !is.finite( gsinf$sweptarea ) ), w2a)
      if (length(ii) > 0 ) gsinf$sweptarea[ii] = sayrp[as.character(gsinf$yr[ii])]
      jj = which(gsinf$sweptarea < gl[1] | gsinf$sweptarea > gl[2])
      if (length(jj) > 0) gsinf$sweptarea[jj] = NA

      ii = intersect( which( !is.finite( gsinf$sweptarea ) ), w2a)
      if (length(ii) > 0 ) gsinf$sweptarea[ii] = median(gsinf$sweptarea, na.rm=TRUE)

#  gate other nets here ...
#    otter = which( gsinf$geardesc == "Yankee #36 otter trawl" )

    nores = which(!is.finite( gsinf$sweptarea ))
    gsinf$sweptarea[nores] = gsinf$sakm2[nores]
    # generic quantile-based truncation
    gears = unique( gsinf$geardesc )

    for (g in gears) {
      u = which( gsinf$geardesc==g )
      if (g=="Western IIA trawl") qr=gl  # created above
      qr = quantile( gsinf$sweptarea[u], probs=qnts, na.rm=TRUE )
      ii = intersect( which( gsinf$sweptarea < qr[1] | gsinf$sweptarea > qr[2] ) , u)
      if (length(ii) > 0) gsinf$sweptarea[ii] = median(gsinf$sweptarea[u], na.rm=TRUE)
    }

    # surface area / areal expansion correction factor: cf_tow
    gsinf$cf_tow = 1 / gsinf$sweptarea
    # nodata = which( !is.finite( gsinf$cf_tow ))
  save( gsinf, file=fn, compress=TRUE )
  return( fn )
}


# ----------------------


  if (DS %in% c("set.base", "set.base.redo") ) {
    fn = file.path(p$datadir, "set.base.rdata")
    if ( DS=="set.base" ) {
      load( fn )
      return ( set )
    }
    gsinf = groundfish.db(p=p, DS="sweptarea" )

    gshyd = groundfish.db(p=p, DS="gshyd" ) # already contains temp data from gsinf
    set = merge(x=gsinf, y=gshyd, by=c("id"), all.x=TRUE, all.y=FALSE, sort=FALSE)
    rm (gshyd, gsinf)
    oo = which( !is.finite( set$sdate)) # NED1999842 has no accompanying gsinf data ... drop it
    if (length(oo)>0) set = set[ -oo  ,]
    set$timestamp = set$sdate
    if (length(which(duplicated(set$id)))>0 ) stop("Duplicates found ...")
    set$oxysat = NA # do later in aegis

    save ( set, file=fn, compress=T)
    return( fn  )
  }


  # ----------------------


}
