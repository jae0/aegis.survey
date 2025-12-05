
survey_db = function( p=NULL, DS=NULL, year.filter=TRUE, add_groundfish_strata=FALSE, redo=FALSE, sppoly=NULL, quantile_upper_limit=0.99, fn=NULL ) {
  #\\ assimilation of all survey data into a coherent form

  dir.create( project.datadirectory( "aegis", "survey" ), showWarnings=FALSE, recursive=TRUE )

  if (DS %in% c("set.init") ) {
    # survet sets
    set = NULL # trip/set loc information
    fn = file.path( project.datadirectory( "aegis", "survey" ), "set.init.rdz"  )
    if ( !redo ) {
      if (file.exists( fn) ) set = read_write_fast( fn)
      return ( set )
    }

    set.names =  c("data.source", "id", "timestamp", "yr", "lon", "lat",
                  "z", "t", "sal", "oxyml", "sa", "sa_towdistance", "gear", "vessel", "setquality", "settype", "sweptarea" )

    if ( "groundfish" %in% p$data_sources ) {
      # settype:
      # 1=stratified random,
      # 2=regular survey,
      # 3=unrepresentative(net damage),
      # 4=representative sp recorded(but only part of total catch),
      # 5=comparative fishing experiment,
      # 6=tagging,
      # 7=mesh/gear studies,
      # 8=explorartory fishing,
      # 9=hydrography

      y = groundfish_survey_db(DS="set.base", yrs=p$yrs )
      setDT(y)

			y$data.source = "groundfish"
      y$sa = y$sweptarea  # sa is in km^2 .. best estimate given data
      # y$sa_towdistance_wing = y$wing.sa
      y$sa_towdistance = y$sakm2  # sakm2==(41 * ft2m * m2km ) * ( gsinf$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2 ; suvery_db:gsinf
      
      y$z = y$bottom_depth  # m
      i = which(!is.finite(y$z))
      if (length(i) > 0) y$z[i] = y$sdepth[i]

      i = which(!is.finite(y$sal))
      if (length(i) > 0) y$sal[i] = y$bottom_salinity[i]

      i = which( y$sal < 5 )
      if (length(i) > 0) y$sal[i] = NA

      i = which(!is.finite(y$temp))
      if (length(i) > 0) y$temp[i] = y$bottom_temperature[i]

      i = which(!is.finite(y$lon))
      if (length(i) > 0) y$lon[i] = y$bottom_temperature[i]

      y$gear = y$geardesc
      y$setquality = NA
      
			y$setquality[ which( y$settype %in% c(1,2,4,5,8) ) ] = "good"
      gsvn = c("data.source", "id", "timestamp", "yr", "lon", "lat",
                "z", "temp", "sal", "oxyml", "sa", "sa_towdistance", "gear", "vessel", "setquality", "settype", "sweptarea" )
      set = rbind( set, y[ , ..gsvn ] )
      names(set) = set.names
      rm (y); gc()
    }


    if ( "snowcrab" %in% p$data_sources ) {
      ps = bio.snowcrab::snowcrab_parameters( yrs=1999:max(p$yrs) )  # to obtain planar projection information and force snowcrab to use it
      y =  bio.snowcrab::snowcrab.db( p=ps, DS ="set.clean" )
      setDT(y)

			y$data.source = "snowcrab"
      y$gear = "Nephrops trawl"
      y$id = paste( y$trip, y$set, sep="." )
      y = planar2lonlat ( y, proj.type=ps$aegis_proj4string_planar_km )  # plon+plat required for lookups

      y$settype  = y$towquality  # copy  and overwrite  .. NOTE: in case of confusion, set_type is used in snow crab to indcate MAP or survey stations ...
      y$setquality = NA
      y$setquality[ which( y$towquality == 1 ) ] = "good"  # 1=good
      y$sal = NA  # dummy
      y$oxyml = NA # dummy var
      

      # hard-gating:
      qsa = c(0.001, 0.01)
      i = which( y$sa < qsa[1] )
      if (length(i) > 0) y$sa[i] = qsa[1]
      j = which( y$sa > qsa[2] )
      if (length(j) > 0) y$sa[j] = qsa[2]
      k = which( !is.finite(y$sa ) )
      if (length(k) > 0) y$sa[k] = median( y$sa[-k] )
      
      y$sweptarea = y$sa
      y$sa_towdistance = y$sa  #copy

      set = rbind( set, y[ , ..set.names ] )  # sa is in km^2

      rm (y); gc()
    }

    read_write_fast( set, fn=fn )
    return (set)
  }


  # --------------------


  if (DS %in% c("cat.init" ) ) {

    # all species caught
    cat = NULL # trip/cat loc information
    fn = file.path( project.datadirectory( "aegis", "survey" ), "cat.init.rdz"  )
    if ( !redo ) {
      if (file.exists( fn) ) cat = read_write_fast( fn)
      return ( cat )
    }

    ###  NOTE:: cf == correction factor is a reweighting required to make each totno and totwgt comparable for each set and species subsampling
    cat.names =  c("data.source", "id", "id2", "spec", "spec_bio", "totno_per_set", "totwgt_per_set", "vessel_correction", "sweptarea" )
    
    if ( "groundfish" %in% p$data_sources ) {

      x = groundfish_survey_db( DS="gscat"  )  #kg/set, no/set
      setDT(x)
      
			x$data.source = "groundfish"
      x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )
      x$id2 = paste(x$id, x$spec_bio, sep=".")
      x = x[spec_bio > 0, ]
      
      setnames(x, "totno",  "totno_per_set")
      setnames(x, "totwgt", "totwgt_per_set")


      # meansize.crude
      # fix missing numbers and mass estimates:
      # zeros for one while nonzeros for correpsonding records
      x$meanwgt_per_set = x$totwgt_per_set / x$totno_per_set  # kg / individual
 
      # weighted mean by species
      mw = x[, 
        .(meanweight_crude = sum(meanwgt_per_set * vessel_correction/sweptarea) / sum(vessel_correction/sweptarea)), 
        by=.(spec_bio)
      ]
      mw = mw[is.finite(meanweight_crude),]

      # meansize directly:
      k = groundfish_survey_db( DS="gsdet", yrs=p$yrs )
      setDT(k)
      
			k$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=k$spec )
      ml = k[ is.finite(len), .(meanlength_direct=mean(len)), by=.(spec_bio) ]
      mm = k[ is.finite(mass), .(meanweight_direct=mean(mass)), by=.(spec_bio) ]
      mw = merge( mw, ml, by="spec_bio", all=T, sort=T ) 
      mw = merge( mw, mm, by="spec_bio", all=T, sort=T )

      # directly determined mean size has greater reliability --- replace
      mw$meanweight = mw$meanweight_crude
      ii = which( is.finite(mw$meanweight_direct))
      mw$meanweight[ii] = mw$meanweight_direct[ii]
      mw = mw[which(is.finite(mw$meanweight)) ,]

      print( "Estimating catches from mean weight information... ")

      ii = which( x$totwgt_per_set > 0  & !is.finite(x$totno_per_set) )
      if (length(ii)>0) {
        # replace each number estimate with a best guess based upon average body weight in the historical record
        uu = unique( x$spec_bio[ii] )
        for (u in uu ) {
          os =  which( mw$spec_bio==u )
          if (length( os)==0 ) next()
          toreplace = intersect( ii, which( x$spec_bio==u) )
          x$totno_per_set[toreplace] = ceiling(x$totwgt_per_set[toreplace] / mw$meanweight[os])
        }
      }

      jj = which( x$totno_per_set >  0 & !is.finite(x$totwgt_per_set) )
      if (length(jj)>0) {
        # replace each number estimate with a best guess based upon average body weight in the historical record
        uu = unique( x$spec_bio[jj] )
        for (u in uu ) {
          os =  which( mw$spec_bio==u )
          if (length( os)==0 ) next()
          toreplace = intersect( jj, which( x$spec_bio==u) )
          x$totwgt_per_set[toreplace] = x$totno_per_set[toreplace] * mw$meanweight[os]
        }
      }


      # as spec codes have been altered, look for duplicates and update totals
      d = which(duplicated(x$id2))
      s = NULL
      for (i in d) {
        q = which(x$id2 == x$id2[i])
        x$totno_per_set[q[1]]  = sum( x$totno_per_set[q], na.rm=T )
        x$totwgt_per_set[q[1]] = sum( x$totwgt_per_set[q], na.rm=T )
        s = c(s, q[2:length(q)])
      }
			
			s = unique(s)

      if (length(s)>0) x = x[-s,]
      
			oo = which( duplicated( x$id2) )
      if ( length( oo )>0 ) {
        print( x[ oo , "id2"] )
        stop("Duplcated id2's in gscat"  )
      }

			ll = NULL
      ll = which( !is.finite(x$totno_per_set) & !is.finite(x$totwgt_per_set) )
      if (length(ll) > 0) x = x[-ll,]



      # if without sweptarea, then another gear, use SA based upon positional info: sakm2
      isana = which(! is.finite( x$sweptarea))
      if (length(isana) > 0) x$sweptarea[isana] = x$sakm2[isana]
 
      x = x[, ..cat.names]
      cat = rbind( cat, x )
      rm (x); gc()
    }

    if ( "snowcrab" %in% p$data_sources ) {
      x = bio.snowcrab::snowcrab.db( DS ="cat.georeferenced" ) # sa corrected ; kg/km2; no./km2
      setDT(x)

			x$data.source = "snowcrab"
      x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )
      x$id = paste( x$trip, x$set, sep="." )
      x$id2 = paste( x$trip, x$set, x$spec_bio, sep="." )

      x$totno_per_set = ceiling(x$totno * x$sa) # return to total per set rather than density
      x$totwgt_per_set = x$totmass * x$sa  # return to total per set rather than density
      x$vessel_correction = 1  #  (ie., no vessel "corrections")
      x$sweptarea = x$sa  

      x$totno_per_set[ x$totno_per_set > 500 ] = 500

      x = x[, ..cat.names]

      # snow crab are assumed to be real zeros .. find them and force 0 value
      iissp = taxonomy.recode( from="spec", to="parsimonious", tolookup=2526 ) # snow crab using groundfish codes
      oo = which( !is.finite(x$totno_per_set) & x$spec_bio==iissp  )
      if (length(oo) > 0 ) x$totno_per_set[oo] = 0
      oo = which( !is.finite(x$totwgt) & x$spec_bio== iissp )  # snow crab are assumed to be real zeros
      if (length(oo) > 0 ) x$totwgt[oo] = 0

			catnm = names(cat)
      cat = rbind( cat, x[,..catnm]  )
      rm (x); gc()
    }

    lh = taxonomy.db( "life.history" )
		setDT(lh)

    tnm = c("spec", "name.common", "name.scientific", "itis.tsn" )
		lh = lh[, ..tnm]
    cat = merge(x=cat, y=lh, by=c("spec"), all.x=T, all.y=F, sort=F)
    cat = cat[ which( cat$itis.tsn > 0 ), ]

    read_write_fast( cat, fn=fn )
    return (cat)
  }



  # --------------------


  if (DS %in% c( "det.init" ) ) {
    # all species caught
    det = NULL # biologicals
    fn = file.path( project.datadirectory( "aegis", "survey" ), "det.init.rdz"  )
    if ( !redo ) {
      if (file.exists( fn) ) det = read_write_fast( fn)
      return ( det )
    }


      # sex codes
      #  male = 0
      #  female = 1
      #  sex.unknown = 2

      # maturity codes
      #  immature = 0
      #  mature = 1
      #  mat.unknown = 2


    det.names =  c("data.source", "id", "id2", "individual", "spec", "spec_bio", "sex", "mass", "len", "mat")
    if ( "groundfish" %in% p$data_sources ) {

      x = groundfish_survey_db( DS="gsdet", yrs=p$yrs )
      setDT(x)

			x$data.source = "groundfish"

      x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )
      x$id2 = paste(x$id, x$spec_bio, sep=".")
      x = x[x$spec_bio > 0, ]
     
      # mass in kg, len in cm

      # convert sex codes to snow crab standard
      # --------- codes ----------------
      # sex: 0=undetermined, 1=male, 2=female,  3=hermaphrodite, 9= not examined
      # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature),
      #      5=spawning(running), 6=spent, 7=recovering, 8=resting
      # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
      #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
      #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
      # --------- codes ----------------

      u = which(x$spec==2526)
      if ( mean(x$len[u], na.rm=TRUE) > 20 ) {
        # 200 mm or 20 cm is really the upper limit of what is possible for snow crab (in 2016, it was 50)
        # if the mean is above this then there is an issue, assume it is recorded as mm
        # and convert to cm as that is the expectation in groundfish_survey_db p=p,and aegis_db
        message( "groundfish gsdet seems to have stored snowcrab lengths in mm .. fixing ? -- please check other species such as lobster, etc.")
        x$len[u] = x$len[u] / 10
        # mass looks ok .. in kg
      }

      sx = x$sex
      x$sex = NA
      oo = which( sx %in% c(0, 3, 9) ); if (length(oo)>0) x$sex[oo] = 2 # unknown
      oo = which( sx %in% c(1) ); if (length(oo)>0) x$sex[oo] = 0 # male
      oo = which( sx %in% c(2) ); if (length(oo)>0) x$sex[oo] = 1 # female

      # convert maturity to snow crab standard
      mt = x$mat
      x$mat = NA
      oo = which( mt %in% c(0) ); if (length(oo)>0) x$mat[oo] = 2 # unknown
      oo = which( mt %in% c(1) ); if (length(oo)>0) x$mat[oo] = 0  # immature
      oo = which( mt %in% c(2,3,4,5,6,7,8) ); if (length(oo)>0) x$mat[oo] = 1 # mature  -- investmvent into gonads has begun

      det = rbind( det, x[, ..det.names] )
      rm (x); gc()

    }

    if ( "snowcrab" %in% p$data_sources ) {
      # snow crab only ... add bycatch from survey :: TODO
      x = bio.snowcrab::snowcrab.db( DS ="det.georeferenced" )
      setDT(x)

			x$data.source = "snowcrab"
      x$spec = 2526
      x$spec_bio =  taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec ) # snow crab using groundfish codes
      x$id = paste( x$trip, x$set, sep="." )
      x$id2 = paste( x$id, x$spec_bio, sep=".")
      x$individual = x$crabno

      x$len = x$cw / 10  # convert mm to cm
      # x$cf_det = 1/x$sa  ########## <<<<<< ------ NOTE THIS accounts only for SA as there is no subsampling (so far)
      x$sex = as.numeric( as.character( x$sex) )
      x$mat = as.numeric( as.character( x$mat) )
      x$mass = x$mass /1000  # g to kg

      det = rbind( det, x[, ..det.names] )
      rm (x); gc()
    }


    read_write_fast( det, fn=fn )
    return (det)
  }


  # -------------

  if ( DS=="areal_units_input" ) {

    fn = file.path( p$datadir,  "areal_units_input.rdz" )
    if ( !file.exists(p$datadir)) dir.create( p$datadir, recursive=TRUE, showWarnings=FALSE )

    xydata = NULL
    if (!redo)  {
      if (file.exists(fn)) {
        xydata = read_write_fast( fn)
        return( xydata )
      }
    }
    xydata = survey_db( p=p, DS="filter"  )  #
    xydata = xydata[ , c("lon", "lat", "yr" )]
    xydata = st_as_sf ( xydata, coords= c('lon', 'lat') )
    st_crs(xydata) = st_crs(projection_proj4string("lonlat_wgs84"))
    xydata = st_transform( xydata, st_crs( p$areal_units_proj4string_planar_km ))
    xydata = xydata[ geo_subset( spatial_domain=p$spatial_domain, Z=xydata ) , ]

    read_write_fast(xydata, fn=fn )
    return( xydata )
  }


  # -------------


  if (DS =="set.base" ) {
    # lookup missing information

    set = NULL # trip/set loc information
    fn = file.path( project.datadirectory( "aegis", "survey" ), "set.base.rdz"  )
    if ( !redo ) {
      if (file.exists( fn) ) set = read_write_fast( fn)
      if( year.filter) if (exists("yrs", p) ) set = set[ set$yr %in% p$yrs, ]  # select to correct years
      return ( set )
    }

    set = survey_db( DS="set.init", p=p )
    set = set[ which(is.finite(set$lon + set$lat + set$yr ) ) , ]  #  fields are required
    oo =  which( !duplicated(set$id) )
    if (length(oo) > 0 ) set = set[ oo, ]
    set = lonlat2planar( set, proj.type=p$aegis_proj4string_planar_km )  # plon+plat required for lookups

    set$dyear = lubridate::decimal_date( set$timestamp ) - set$yr

    # merge depth
    iM = which( !is.finite(set$z) )
    if (length(iM > 0)) {
      pL = aegis.bathymetry::bathymetry_parameters( project_class="core"  )
      LUT= aegis_survey_lookuptable( aegis_project="bathymetry", 
        project_class="core", DS="aggregated_data", pL=pL )

      set$z[iM] = aegis_lookup( pL=pL, LOCS=set[ iM, c("lon", "lat")], LUT=LUT,
        project_class="core", output_format="points",  
        space_resolution=p$pres*2, variable_name="z.mean"  ) # core==unmodelled
    }

    # substrate lookup
    pS = substrate_parameters( p=parameters_reset(p), project_class="core"  )
    if (!(exists(pS$variabletomodel, set ))) set[,pS$variabletomodel] = NA
    iM = which(!is.finite( set[, pS$variabletomodel] ))
    if (length(iM > 0)) {
 
      LUT= aegis_survey_lookuptable( aegis_project="substrate", 
        project_class="core", DS="aggregated_data", pL=pS )

      set[iM, pS$variabletomodel] = aegis_lookup( pL=pS, LOCS=set[iM, c("lon", "lat")], LUT=LUT,
        project_class="core", output_format="points" , 
        space_resolution=p$pres*2, variable_name="substrate.grainsize.mean"  )
    }

    # merge temperature
    pT = temperature_parameters( p=parameters_reset(p), project_class="core", year.assessment=p$year.assessment  )
    if (!(exists(pT$variabletomodel, set ))) set[,pT$variabletomodel] = NA
    iM = which(!is.finite( set[, pT$variabletomodel] ))
    if (length(iM > 0)) {
      LUT= aegis_survey_lookuptable( aegis_project="temperature", 
        project_class="core", DS="aggregated_data", pL=pT )

      set[iM, pT$variabletomodel] = aegis_lookup( pL=pT, LOCS=set[ iM, c("lon", "lat", "timestamp")], 
        LUT=LUT, project_class="core", output_format="points",  
        variable_name="t.mean", space_resolution=p$pres*2, time_resolution=p$tres*2, 
        year.assessment=p$year.assessment, tz="America/Halifax" )
    }

    set$oxysat = oxygen_concentration_to_saturation( t.C=set$t, sal.ppt=set$sal, oxy.ml.l=set$oxyml)

    read_write_fast( set, fn=fn )
    return (set)
  }


  # ---------------------

  if (DS %in% c(  "lengthweight.parameters", "lengthweight.residuals") ) {

    ## TODO -- make parallel require(multicore)
    ## NOTE: location is outside of survey.db .. /home/jae/bio.data/aegis/data
    ddir = file.path( project.datadirectory("aegis"), "data" )
    dir.create( ddir, showWarnings=FALSE, recursive=TRUE )

    fn = file.path( ddir, "bio.length.weight.parameters.rdz" )
    fn2 = file.path( ddir, "bio.length.weight.residuals.rdz" )

    if ( !redo ) {
      if (DS=="lengthweight.parameters" ) {
        res = NULL
        if (file.exists( fn ) ) res = read_write_fast( fn )
        return( res )
      }

      if (DS=="lengthweight.residuals") {
        lwr = NULL
        if (file.exists( fn2 ) ) lwr = read_write_fast( fn2 )
        return( lwr )
      }
    }

    # this mirrors the relevent changes/recoding in aegis_db("det")
    x = survey_db( DS="det.init", p=p )

    x = x[ which( is.finite( x$spec_bio)), ]
    x$sex[ which( !is.finite(x$sex)) ] = 2 # set all uncertain sexes to one code sex code
    x$mat[ which( !is.finite(x$mat)) ] = 2 # set all uncertain sexes to one code sex code

    res = expand.grid(
      spec_bio = sort( unique( x$spec_bio )),
      sex = sort( unique( x$sex )),
      mat = sort( unique( x$mat ))
    )
    # sex codes (snowcrab standard)
    #  male = 0
    #  female = 1
    #  sex.unknown = 2

    # mat codes (snowcrab standard)
    #  imm = 0
    #  mat = 1
    #  mat.unknown = 2

    unknown = 2
    x$residual = NA

    # initialise new variables
    res$rsq = NA
    res$sigma = NA
    res$df = NA
    res$b0 = NA
    res$b1 = NA
    res$b0.se = NA
    res$b1.se = NA
    res$pvalue = NA
    for (i in 1:nrow(res)) {
      wsp = which( x$spec_bio == res$spec_bio[i] )
      if (length( wsp) < 3 ) next()
      # remove extremes for each species from the data to generate regressions
      ql = quantile( x$len[wsp], probs=c(0.005, 0.995), na.rm=T )
      qm = quantile( x$mass[wsp], probs=c(0.005, 0.995), na.rm=T )
      wqn =  which( x$len> ql[1] & x$len< ql[2] & x$mass> qm[1] & x$mass< qm[2]  )
      wsx = which( x$sex==res$sex[i] )
      if (res$sex[i]==unknown) wsx = wsp  # use all possible data (not just unknowns) for this class (hermaphrodite/unsexed/unknown)
      wmt = which( x$mat==res$mat[i] )
      if (res$sex[i]==unknown) wsx = wsp  # use all possible data (not just unknowns) for this class (mat unkown)
      w = intersect( intersect( intersect( wsp, wqn ), wsx ), wmt )
      nw = length(w)
      if ( nw > 5 ) {
        q = x[w ,]
        q.lm = try( lm( log10(mass) ~ log10(len), data=q ) )
        if (class( q.lm) %in% "error" ) next()
        s = summary( q.lm )
        res$rsq[i] = s$r.squared
        res$sigma[i] = s$sigma
        res$df[i] = s$df[2]
        res$b0[i] = s$coefficients[1]
        res$b1[i] = s$coefficients[2]
        res$b0.se[i] = s$coefficients[3]
        res$b1.se[i] = s$coefficients[4]
        res$pvalue[i] = pf(s$fstatistic[1],s$fstatistic[2],s$fstatistic[3],lower.tail=FALSE)
        x$residual[w] = rstandard(q.lm)
        print( res[i,] )
      }
    }
    ooo = which( abs( x$residual ) > 4 )
    if (length(ooo) > 0 ) x$residual [ooo] = NA
    lwr = x
    read_write_fast( lwr, fn=fn2 )
    read_write_fast( res, fn=fn )
    return( list(res=res, lwr=lwr) )

  }


  # --------------------


  if (DS %in% c("det" ) ) {

    # error checking, imputation, etc
    det = NULL
    fn = file.path( project.datadirectory( "aegis", "survey" ), "det.rdz"  )
    if (  !redo ) {
      if (file.exists( fn) ) det = read_write_fast( fn)
      return ( det )
    }

      # sex codes
      #  male = 0
      #  female = 1
      #  sex.unknown = 2

      # maturity codes
      #  immature = 0
      #  mature = 1
      #  mat.unknown = 2

    det = survey_db( DS="lengthweight.residuals", p=p )

    # fix mass, length estimates where possible using model parameters
    # try finest match first: by spec_bio:mat, spec_bio:sex, spec_bio

    lwp = survey_db( DS="lengthweight.parameters", p=p )
    # note: lwp$spec is derived from spec_bio, as above

    ims = which( !is.finite( det$mass) )
    sps = sort( unique( det$spec_bio[ ims ] ) )
    mats = sort( unique( det$mat))
    sexes = sort( unique( det$sex))

    for (sp in sps) {
      isp = which( det$spec_bio == sp )

      # first try exact matches based upon {spec_bio, mat, sex}
      for ( mat in mats ) {
      for ( sex in sexes ) {
        u = which( det$mat==mat & det$sex==sex & !is.finite(det$mass ) )
        w = intersect( isp, u )
        if (length(w) > 0) {
          v = which( lwp$spec_bio==sp & lwp$mat==mat & lwp$sex==sex )
          if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
        }
      }}

      # next try exact matches based upon {spec_bio, mat}
      for ( mat in mats ) {
        u = which( det$mat==mat & !is.finite(det$mass )  )
        w = intersect( isp, u )
        if (length(w) > 0) {
          v = which( lwp$spec_bio==sp & lwp$mat==mat & is.na(lwp$sex   ) )
          if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
        }
      }

      # next try exact matches based upon {spec_bio, sex}
      for ( sex in sexes ) {
        u = which( det$sex==sex & !is.finite(det$mass )  )
        w = intersect( isp, u )
        if (length(w) > 0) {
          v = which( lwp$spec_bio==sp & lwp$sex==sex & is.na(lwp$mat  ) )
          if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
        }
      }

      # next try exact matches based upon {spec_bio} only
        u = which( is.na(det$mass ))
        w = intersect( isp , u )
        if (length(w) > 0) {
          v = which( lwp$spec_bio==sp & is.na(lwp$sex) & is.na(lwp$mat  ) )
          if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
        }
    }

    # estimate metabolic_rates estimates (requires temperature estimate )
    set = survey_db( DS="set.base", p=p  ) # kg, no
    setDT( set )

		snm = c("id", "t")
		set = set[ , ..snm]  # temperature is required to estimate MR ..

    det = merge( det, set, by="id", all.x=T, all.y=F, sort=F )

    # copied from aegis.metabolism as it is not yet part of aegis

    metabolic_rates = function( mass.g, temperature.C=NULL ) {

      # require mass (g), temperature.C (C)
      # from Robinson et al. (1983)
      # specific standard MR
      #   = smr
      #   = 0.067 M^(-0.24) * exp(0.051 * Temp.Celsius)
      # (Temp.Celsius; M in grams, MR in ml O2/g/hr)
      #
      # Converting to more standard units:
      # 1 ml O2 = 4.8 cal (from paper)
      # and 1 W = 7537.2 kcal/yr
      # 1 ml O2 / g / hr = 4.8 * 24 * 365 kcal / yr / kg
      #                  = 4.8 * 24 * 365 / (7537.2 W) / kg
      #                  = 5.57873 W / kg


      # noting similarity to Arrehnius formulation:
      #   smr ~ A * exp( -Ea / (K * T) )
      #     where:  Ea = energy of activation {	kcal/mol or J/ mol }
      #             R  = 1.985877534 × 10−3	kcal/mol (Gas constant) or kB if on per molecule basis
      #             kB = 1.3806488(13)×10−23 J/K
      #             T  = temperature in Kelvin
      # then,
      #   A  ~ 0.067 * M^(-0.24) / 5.57873  { W / kg }
      #   Ea ~ -b2 * T.C * R (T.C +273.15)

      if (is.null(temperature.C)) temperature.C=10
      tmed = median( temperature.C, na.rm=T )
      oo = which(!is.finite( temperature.C) )
      if (length(oo)>0) temperature.C[oo] = tmed

      N.avogadro = 6.0221412927 * 10^23 ## mol−1
      kB = 1.380648813 * 10^-23 # Boltzman's constant  J/K
      # K =  kB * N.avogadro # R or kB , here directly R = kB {J/K} * N.avogadro {n/mol}  muiliplied to give units of J / K / mol
      K = 8.314462175 # as above to greater precision
      b0 = 0.067
      b1 = -0.24
      b2 = 0.051
      from.ml.O2.per.g.per.hr.to.W.per.kg = 1 / 5.57873

      # Arrhenius parameterization:
      # the pre-factor -> total number of collisions ... ie. similar to encounter rate
      A = b0 * (mass.g)^b1 * from.ml.O2.per.g.per.hr.to.W.per.kg
      oo = which( !is.finite(A) )

      if (length(oo)>0) A[oo] = NA

      # 'activation energy'
      Ea =  b2 * temperature.C * K * ( temperature.C + 273.15 )

      # fraction of collisions leading to a reaction (ie. a metabolic event -- e.g. dissipation) .. reacting or Pr of reaction -- incorporates (i.e., due to) temperature influence
      Pr.Reaction = exp( -Ea / (K * (temperature.C + 273.15) ) ) ## == exp( b2 * temperature.C )

      smr = A * Pr.Reaction  #  == b0 * (mass.g)^b1 * exp(  b2 * temperature.C ) * from.ml.O2.per.g.per.hr.to.W.per.kg
      mr = smr * mass.g

      x = data.table( smr=smr, mr=mr, Ea=Ea, A=A, Pr.Reaction=Pr.Reaction )

      ## units:
      ## smr,A ~ {W/kg};
      ## mr~{W};
      ## Ea ~ {J/mol} energy per per mol

      return(x)
    }
 
    detmr = metabolic_rates ( mass.g=det$mass * 1000, temperature.C=det$t )
    det = cbind( det, detmr )


    # determine weighting factor for individual-level measurements (len, weight, condition, etc)

    # at the set level, some species are not sampled even though sampwgt's are recorded
    # this makes the total biomass > than that estimated from "DET"
    # an additional correction factor is required to bring it back to the total biomass caught,
    # this must be aggregated across all species within each set :

    # correction factors for sampling etc after determination of mass and len
    # for missing data due to subsampling methodology
    # totals in the subsample that was taken should == sampwgt (in theory) but do not
    # ... this is a rescaling of the sum to make it a 'proper' subsample

    cat = survey_db( DS="cat.init", p=p )

    setDT(det)
    setDT(cat)

    det_summary = det[, .(
        det_totwgt_per_set=sum(mass, na.rm=TRUE), 
        det_totno_per_set=.N
      ), 
      by=.(id2)
    ]
    cat = det_summary[cat, on=.(id2) ] # merge

    # set-->kg/km^2, det-->km
    cat$det_totwgt_per_set[ which( !is.finite (cat$det_totwgt_per_set ))] = 0  ### when missing it means no determinations were made
    cat$cf_det_wgt =  ( cat$det_totwgt_per_set / cat$totwgt ) * (cat$vessel_correction / cat$sweptarea)   # cf_det is the multiplier required to make each det measurement scale properly to totwgt  including subsampling corrections

    cat$det_totno_per_set[ which( !is.finite (cat$det_totno_per_set ))] = 0  ### when missing it means no determinations were made
    cat$cf_det_no  = ( cat$det_totno_per_set / cat$totno ) * (cat$vessel_correction / cat$sweptarea)   # cf_det is the multiplier required to make each det measurement scale properly to totno  including subsampling corrections

    # assume no subsampling -- all weights determined from the subsample
    oo = which ( !is.finite( cat$cf_det_wgt ) |  cat$cf_det_wgt==0 )
    if (length(oo)>0) cat$cf_det_wgt[oo] = 1

    # assume no subsampling -- all weights determined from the subsample
    oo = which ( !is.finite( cat$cf_det_no ) |  cat$cf_det_no==0 )
    if (length(oo)>0) cat$cf_det_no[oo] = 1

    # oo = which ( cat$cf_det_wgt < 0.001 )
    # if (length(oo)>0) cat$cf_det_wgt[oo] = NA

    # oo = which ( cat$cf_det_wgt > 500 )
    # if (length(oo)>0) cat$cf_det_wgt[oo] = NA

    cat = cat[, c("id2", "cf_det_wgt", "cf_det_no")  ]
    det = det[ cat, on=.(id2) ]

    det$cf_det_wgt[!is.finite(det$cf_det_wgt)] = 1
    det$cf_det_no [!is.finite(det$cf_det_no)] = 1

    ## remaining NA's with cf_det are mostly due to bad hauls, broken nets etc.

    read_write_fast (det, fn=fn )
    return (det)
  }


  # --------------------


  if (DS %in% c("cat" ) ) {
    # all species caught
    cat = NULL # biologicals
    fn = file.path( project.datadirectory( "aegis", "survey" ), "cat.rdz"  )
    if ( !redo ) {
      if (file.exists( fn) ) cat = read_write_fast( fn)
      return ( cat )
    }

    set = survey_db( DS="set.init", p=p  ) # kg/km^2, no/km^2

    det = survey_db( DS="det", p=p  ) # size information, no, cm, kg
    det = det[ which( det$id %in% unique( set$id) ), ]

    cat = survey_db( DS="cat.init", p=p )
    cat = cat[ which( cat$id %in% unique( set$id) ), ]

    setDT(set)
    setDT(det)
    setDT(cat)

		snm =  c("id", "gear") 
    cat = set[, ..snm] [ cat, on=.(id) ] # merge
    oo = which( duplicated( cat$id2) )
    if (length( oo) > 0 ) cat = cat[ -oo, ]

    # note subsampling is on weight basis
    det_summary = det[, .(
        mass=mean(mass*cf_det_wgt, na.rm=TRUE), 
        len=mean(len*cf_det_wgt, na.rm=TRUE), 
        mr=sum(mr*cf_det_wgt, na.rm=TRUE),
        smr=mean(smr*cf_det_wgt, na.rm=TRUE), 
        residual=mean(residual*cf_det_wgt, na.rm=TRUE), 
        Ea=mean(Ea*cf_det_wgt, na.rm=TRUE), 
        A=mean(A*cf_det_wgt, na.rm=TRUE), 
        Pr.Reaction=mean(Pr.Reaction*cf_det_wgt, na.rm=TRUE) 
      ), 
      by=.(id2)
    ]
    cat = det_summary[cat, on=.(id2) ] # merge
    
    cat$mr[ which(!is.finite(cat$mr))] = 0
    cat$smr[ which(!is.finite(cat$smr))] = 0
    cat$Ea[ which(!is.finite(cat$Ea))] = 0
    cat$A[ which(!is.finite(cat$A))] = 0
    cat$Pr.Reaction[ which(!is.finite(cat$Pr.Reaction))] = 0

    # where det measurements not available, estimate mean mass from total weights and numbers
    oo = which( !is.finite( cat$mass ))
    if (length(oo) > 0 ) {
      cat$mass[oo] = cat$totwgt[oo] / cat$totno[oo]
    }
 
    read_write_fast (cat, fn=fn )
    return (cat)

  }



  # -------------



  if (DS %in% c("set" ) ) {

    # survet sets
    set = NULL # trip/set loc information
    fn = file.path( project.datadirectory( "aegis", "survey" ), "set.rdz"  )
    if (  !redo ) {
      if (file.exists( fn) ) set = read_write_fast( fn)
      return ( set )
    }

    set = survey_db( DS="set.base", p=p )
    if (exists("yrs", p)) set = set[ set$yr %in% p$yrs,  ]  # select to correct years

    det = survey_db( DS="det", p=p  ) # size information, no, cm, kg
    det = det[ which( det$id %in% unique( set$id) ), ]

    cat = survey_db( DS="cat", p=p )
    cat = cat[ which( cat$id %in% unique( set$id) ), ]

    setDT(set)
    setDT(det)
    setDT(cat)

    # NOTE: convention:: 
    #   cf is correction factor (multiplier to observations to get adjusted values)
    #   wt is weight (divisor to observations to get adjusted values)
    #   expected == value expected after correction/adjustment for sampling, subsampling etc

        #   cf = expected / observed
        #   wt = observed / expected
        #   cf = 1/wt

    # summaries from cat .. weighted by cf to make per standard unit
    # NOTE: cat$totno and cat$totwgt are not cf corrected
    cat_summary = cat[, .(
        totno_per_set = sum(totno_per_set, na.rm=TRUE),  # totno per set
        totwgt_per_set= sum(totwgt_per_set, na.rm=TRUE), # totno per set
        totno_adjusted= sum(totno_per_set*vessel_correction / sweptarea, na.rm=TRUE),
        totwgt_adjusted=sum(totwgt_per_set*vessel_correction / sweptarea, na.rm=TRUE)
      ), 
      by=.(id)
    ]
    set = cat_summary[set, on=.(id) ] # merge

    set$cf_set_mass = set$totwgt_adjusted / set$totwgt_per_set
    set$cf_set_no = set$totno_adjusted / set$totno_per_set

    # NOTE:: these should be == or ~= 1/set$sa ( done this way in case there has been other adjustmensts such as subampling, etc ..) .. these become offets required to express totwgt or totno as areal density per unit km^2 in Poisson models

    # summaries from det
    # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years
    det_summary = det[, .(
        mr=sum(mr*cf_det_wgt, na.rm=TRUE),
        smr=mean(smr*cf_det_wgt, na.rm=TRUE), 
        residual=mean(residual*cf_det_wgt, na.rm=TRUE), 
        mass=mean(mass*cf_det_wgt, na.rm=TRUE), 
        len=mean(len*cf_det_wgt, na.rm=TRUE), 
        Ea=mean(Ea*cf_det_wgt, na.rm=TRUE), 
        A=mean(A*cf_det_wgt, na.rm=TRUE), 
        Pr.Reaction=mean(Pr.Reaction*cf_det_wgt, na.rm=TRUE) 
      ), 
      by=.(id)
    ]
    set = det_summary[set, on=.(id) ] # merge
    
    set$mr[ which(!is.finite(set$mr))] = 0
    set$smr[ which(!is.finite(set$smr))] = 0
    set$Ea[ which(!is.finite(set$Ea))] = 0
    set$A[ which(!is.finite(set$A))] = 0
    set$Pr.Reaction[ which(!is.finite(set$Pr.Reaction))] = 0

    read_write_fast( set, fn=fn )
    return (set)
  }


  # ---------------------

  if (DS == "det.filter" ) {
    # selected for a given set of species  and size and sex and maturity
    # wrapper around survey_db and groundfish_survey_db to permit abundance data to be passed, including zero valued locations
    # selected for a given set of species  and size and sex and maturity
    # add zero valued locations too..

    # ft2m = 0.3048
    # m2km = 1/1000
    # nmi2mi = 1.1507794
    # mi2ft = 5280
    # k$sakm2 = (41 * ft2m * m2km ) * ( k$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
    # standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm

    # must not subset "set" until after all det/cat info has been resolved in order to reatin zerovalued sets
    set = det = NULL # trip/set loc information
    set = survey_db( DS="set.base", p=p )
    u = data.frame( matrix( unlist(strsplit( set$id, ".", fixed=TRUE)), ncol=2, byrow=TRUE), stringsAsFactors=FALSE )
    set$mission = as.character( u[,1] )
    set$setno = as.numeric( u[,2] )

    if (add_groundfish_strata) {
      areal_units_timeperiod = "pre2014"  # "pre2014" for older
      sppoly = maritimes_groundfish_strata( areal_units_timeperiod=areal_units_timeperiod )
      setDF(set)
      set = maritimes_groundfish_strata_identify( Y=set, sppoly=sppoly, xyvars=c("lon", "lat"), planar_crs_km=p$aegis_proj4string_planar_km )
      setDT(set)
    }

    # filter non-biologicals ... i.e, set characteristics
    if (exists("selection", p)) {
      if (exists("survey", p$selection)) {  # filter survey information
        if (exists("AUID", set) ) {
          if (exists("polygon_enforce", p$selection$survey) ) {
            set = set[ which(!is.na(set$AUID)), ] # remove unsetegorized sets
          }
          if (exists("strata_toremove", p$selection$survey) ) {
            todrop = which( set$AUID %in% strata_definitions( p$selection$survey[["strata_toremove"]] ) )
            if (length(todrop) > 0) set = set[- todrop, ]
          }
          if (exists("strata_tokeep", p$selection$survey) ){
            set = set[which( set$AUID %in% p$selection$survey[["strata_to_keep"]] ) , ]
          }
        }
        if (exists("months", p$selection$survey) ) set = set[ which(month(set$timestamp) %in% p$selection$survey[["months"]] ), ]
        isc = filter_data( set, p$selection$survey )
        if (length(isc) > 0) set = set[isc,]
        isc = NULL
      }
    }

    # indiviudal measurements filter
    det = survey_db( DS="det", p=p  ) # size information, no, cm, kg
    det = det[ which( det$id %in% unique( set$id) ), ]
      if (exists("selection", p)) {
        if (exists("biologicals", p$selection)) {  # filter biologicals
          isc = filter_data( det, p$selection$biologicals )
          if (length(isc) > 0) det = det[isc,]
          isc = NULL
        }
        if (exists("biologicals_using_snowcrab_filter_class", p$selection)) {  # filter biologicals using snow crab short-form ID
          warning( "Filtering using snow crab 'types' requires more data than is carried by survey_db. \n 
            .. Adding data directly from snowcrab.db .. this also means dropping other sources of data \n")
          det_sc = bio.snowcrab::snowcrab.db( DS ="det.georeferenced" )
          det_sc$spec = 2526
          det_sc$spec_bio =  taxonomy.recode( from="spec", to="parsimonious", tolookup=det_sc$spec ) # snow crab using groundfish codes
          det_sc$individual = paste( det_sc$trip, det_sc$set, det_sc$spec_bio, det_sc$crabno, sep=".")
          det_sc$filter.class = p$selection$biologicals_using_snowcrab_filter_class
          isc = bio.snowcrab::filter.class( x=det_sc, type=p$selection$biologicals_using_snowcrab_filter_class )
          if (length(isc) > 0) det_sc = det_sc[isc, c("individual", "filter.class") ]
          isc = NULL
          det = merge( det, det_sc, by="individual_id", all.x=TRUE, all.y=FALSE, suffixes=c("", "det_sc") )          
          det = det[ !is.na(det$filter.class), ]
        }

      }

    # summaries from det
    # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years
    # cf_det is the weight to make it sum up to the correct total catch (vs any subsamples) and tow length, etc

    setDT(set)
    setDT(det)
    det_summary = det[, .(
        totno=.N, 
        totwgt=sum(mass, na.rm=TRUE),
        mr=sum(mr*cf_det_wgt, na.rm=TRUE),
        smr=mean(smr*cf_det_wgt, na.rm=TRUE), 
        residual=mean(residual*cf_det_wgt, na.rm=TRUE), 
        mass=mean(mass*cf_det_wgt, na.rm=TRUE), 
        len=mean(len*cf_det_wgt, na.rm=TRUE), 
        Ea=mean(Ea*cf_det_wgt, na.rm=TRUE), 
        A=mean(A*cf_det_wgt, na.rm=TRUE), 
        Pr.Reaction=mean(Pr.Reaction*cf_det_wgt, na.rm=TRUE) 
      ), 
      by=.(id)
    ]
    set = det_summary[set, on=.(id) ] # merge

    cat = survey_db( DS="cat", p=p  ) # size information, no, cm, kg
    cat = cat[ which( cat$id %in% unique( set$id) ), ]
      if (exists("selection", p)) {
        if (exists("biologicals", p$selection)) {  # filter biologicals
          isc = filter_data( cat, p$selection$biologicals )
          if (length(isc) > 0) cat = cat[isc,]
          isc = NULL
        }
      }
    
    setDT(cat)
    cat_summary = cat[, .(
        totno_adjusted=sum(totno*vessel_correction / sweptarea, na.rm=TRUE),
        totwgt_adjusted=sum(totwgt*vessel_correction / sweptarea, na.rm=TRUE)
      ), 
      by=.(id)
    ]

    set = cat_summary[set, on=.(id) ] # merge

    set$totno_adjusted[ which(!is.finite(set$totno_adjusted))] = 0
    set$totwgt_adjusted[ which(!is.finite(set$totwgt_adjusted))] = 0
    set$totno[ which(!is.finite(set$totno))] = 0
    set$totwgt[ which(!is.finite(set$totwgt))] = 0

    set$cf_set_mass = set$totwgt_adjusted / set$totwgt
    set$cf_set_no = set$totno_adjusted / set$totno

    ii = which(!is.finite(set$cf_set_mass ))
    if (length(ii) > 0) set$cf_set_mass[ii] = 1/set$sweptarea[ii]

    ii = which(!is.finite(set$cf_set_no ))
    if (length(ii) > 0) set$cf_set_no[ii] = 1/set$sweptarea[ii]


    # NOTE:: these should be == or ~= 1/set$sa ( done this way in case there has been other adjustmensts such as subampling, etc ..) .. these become offets required to express totwgt or totno at a common areal density per unit km^2 in Poisson models

    # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years
    set$mr[ which(!is.finite(set$mr))] = 0
    set$smr[ which(!is.finite(set$smr))] = 0
    set$Ea[ which(!is.finite(set$Ea))] = 0
    set$A[ which(!is.finite(set$A))] = 0
    set$Pr.Reaction[ which(!is.finite(set$Pr.Reaction))] = 0

    # aggeragate measurement filter
    if (exists("selection", p)) {
      # last pass .. filter any set-level traits (mostly aggregate biological characgeristics, if any)
      if (exists("aggregate", p$selection)) {  # filter on survey characteristics
        if (exists("drop.unreliable.zeros.groundfish.data", p$selection$aggregate )) {
          if (p$selection$aggregate$drop.unreliable.zeros.groundfish.data) {
            # special flag .. unreliable zero's for snowcrab in the groundfish data
            todrop = which( set$data.source=="groundfish" & set$yr < 1999 & (set$totwgt ==0 | set$totno==0) )
            if (length(todrop)>0) set = set[ -todrop, ]
          }
        }
        isc = filter_data( set, p$selection$aggregate )
        if (length(isc) > 0) set = set[isc,]
        isc = NULL
      }
    }

    return (set)
  }


  # ---------------------

  if (DS == "cat.filter" ) {
    # selected for a given set of species  and size
    # wrapper around survey_db and groundfish_survey_db to permit abundance data to be passed, including zero valued locations
    # selected for a given set of species  and size and sex and maturity
    # add zero valued locations too..

    # ft2m = 0.3048
    # m2km = 1/1000
    # nmi2mi = 1.1507794
    # mi2ft = 5280
    # k$sakm2 = (41 * ft2m * m2km ) * ( k$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
    # standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm

    # must not subset "set" until after all det/cat info has been resolved in order to reatin zerovalued sets
    set = cat = NULL # trip/set loc information

    set = survey_db( DS="set.base", p=p )
    u = data.frame( matrix( unlist(strsplit( set$id, ".", fixed=TRUE)), ncol=2, byrow=TRUE), stringsAsFactors=FALSE )
    set$mission = as.character( u[,1] )
    set$setno = as.numeric( u[,2] )


    if (add_groundfish_strata) {
      areal_units_timeperiod = "pre2014"  # "pre2014" for older
      sppoly = maritimes_groundfish_strata( areal_units_timeperiod=areal_units_timeperiod )
      set = maritimes_groundfish_strata_identify( Y=set, sppoly=sppoly, xyvars=c("lon", "lat"), planar_crs_km=p$aegis_proj4string_planar_km  )
    }

    # filter non-biologicals ... i.e, set characteristics
    if (exists("selection", p)) {
      if (exists("survey", p$selection)) {  # filter survey information

        if (exists("AUID", set) ) {
          if (exists("polygon_enforce", p$selection$survey) ) {
            set = set[ which(!is.na(set$AUID)), ] # remove unsetegorized sets
          }
          if (exists("strata_toremove", p$selection$survey) ) {
            todrop = which( set$AUID %in% strata_definitions( p$selection$survey[["strata_toremove"]] ) )
            if (length(todrop) > 0) set = set[- todrop, ]
          }
          if (exists("strata_tokeep", p$selection$survey) ){
            set = set[which( set$AUID %in% p$selection$survey[["strata_to_keep"]] ) , ]
          }
        }

        if (exists("months", p$selection$survey) ) set = set[ which(month(set$timestamp) %in% p$selection$survey[["months"]] ), ]
        isc = filter_data( set, p$selection$survey )
        if (length(isc) > 0) set = set[isc,]
        isc = NULL
      }
    }

    # indiviudal measurements filter 
    cat = survey_db(DS="cat", p=p) #export from grounfish survey database .. weight (kg) and num per unit area (km^2)
    cat = cat[ which( cat$id %in% unique( set$id) ), ]
      if (exists("selection", p)) {
        if (exists("biologicals", p$selection)) {  # filter biologicals
          isc = filter_data( cat, p$selection$biologicals )
          if (length(isc) > 0) cat = cat[isc, ]
          isc = NULL
        }
      }

    cat$data.source = NULL
    set = merge( set, cat, by="id", all.x=TRUE, all.y=FALSE, suffixes=c("", ".cat") )
    set$totno_adjusted = set$totno * set$vessel_correction / set$sweptarea
    set$totwgt_adjusted = set$totwgt * set$cf_vessel / set$sweptarea

    set$totno_adjusted[ which(!is.finite(set$totno_adjusted))] = 0
    set$totwgt_adjusted[ which(!is.finite(set$totwgt_adjusted))] = 0
    set$totno[ which(!is.finite(set$totno))] = 0
    set$totwgt[ which(!is.finite(set$totwgt))] = 0

    # NOTE:: these should be == or ~= 1/set$sa ( done this way in case there has been other adjustmensts such as subampling, etc ..) .. these become offets required to express totwgt_adjusted ot totno_adjusted per unit km^2 in Poisson models
 
    # aggeragate measurement filter
    if (exists("selection", p)) {
      # last pass .. filter any set-level traits (mostly aggregate biological characgeristics, if any)
      if (exists("aggregate", p$selection)) {  # filter on survey characteristics
        if (exists("drop.unreliable.zeros.groundfish.data", p$selection$aggregate )) {
          if (p$selection$aggregate$drop.unreliable.zeros.groundfish.data) {
            # special flag .. unreliable zero's for snowcrab in the groundfish data
            todrop = which( set$data.source=="groundfish" & set$yr < 1999 & (set$totwgt ==0 | set$totno==0) )
            if (length(todrop)>0) set = set[ -todrop, ]
          }
        }
        isc = filter_data( set, p$selection$aggregate )
        if (length(isc) > 0) set = set[isc,]
        isc = NULL
      }
    }

    set$gear.cat = NULL

    # fix NA's 

    routine_replacement = c( "spec", "spec_bio", "name.common", "name.scientific", "itis.tsn" )
    for ( vn in routine_replacement ) {
      if (exists( vn, set)) {
        i = which( is.na(set[[vn]] ) )
        if (length(i) > 0) set[[vn]][i] = unique( set[[vn]][-i] )[1] 
      }
    }

    i = which( !is.finite(set$id2) )
    if (length(i) > 0) set$id2[i] = paste( set$id[i], set$spec[i], sep="."  ) 

    return (set)
  }

# ----------------------------------

  if (DS == "filter" ) {
    # really 'set.filter' .. i.e., summarize to set-level
    # selected for a given set of species  and size and sex and maturity
    # wrapper around survey_db and groundfish_survey_db to permit abundance data to be passed, including zero valued locations
    # selected for a given set of species  and size and sex and maturity
    # add zero valued locations too..

    # ft2m = 0.3048
    # m2km = 1/1000
    # nmi2mi = 1.1507794
    # mi2ft = 5280
    # k$sakm2 = (41 * ft2m * m2km ) * ( k$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
    # standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm

    # must not subset "set" until after all det/cat info has been resolved in order to reatin zerovalued sets

    set = cat = det = NULL # trip/set loc information

    set = survey_db( DS="set.base", p=p )
    u = data.frame( matrix( unlist(strsplit( set$id, ".", fixed=TRUE)), ncol=2, byrow=TRUE), stringsAsFactors=FALSE )
    set$mission = as.character( u[,1] )
    set$setno = as.numeric( u[,2] )

    if (add_groundfish_strata) {
      areal_units_timeperiod = "pre2014"  # "pre2014" for older
      sppoly = maritimes_groundfish_strata( areal_units_timeperiod=areal_units_timeperiod )
      set = maritimes_groundfish_strata_identify( Y=set, sppoly=sppoly, xyvars=c("lon", "lat"), planar_crs_km=p$aegis_proj4string_planar_km )
    }

    # filter non-biologicals ... i.e, set characteristics
    if (exists("selection", p)) {
      if (exists("survey", p$selection)) {  # filter survey information
        if (exists("AUID", set) ) {
          if (exists("polygon_enforce", p$selection$survey) ) {
            set = set[ which(!is.na(set$AUID)), ] # remove unsetegorized sets
          }
          if (exists("strata_toremove", p$selection$survey) ) {
            todrop = which( set$AUID %in% strata_definitions( p$selection$survey[["strata_toremove"]] ) )
            if (length(todrop) > 0) set = set[- todrop, ]
          }
          if (exists("strata_tokeep", p$selection$survey) ){
            set = set[which( set$AUID %in% p$selection$survey[["strata_to_keep"]] ) , ]
          }
        }
        if (exists("months", p$selection$survey) ) set = set[ which(month(set$timestamp) %in% p$selection$survey[["months"]] ), ]
        isc = filter_data( set, p$selection$survey )
        if (length(isc) > 0) set = set[isc,]
        isc = NULL
      }
    }

    data_source_base = "det"



    # indiviudal measurements filter
    if (exists("selection", p)) {
      if (exists("biologicals", p$selection)) {  # filter biologicals
        if ( length( p$selection$biologicals) == 1 ) {
          if ( exists( "spec_bio", p$selection$biologicals ) | exists( "spec", p$selection$biologicals ) ) {
            if ( !exists( "biologicals_using_snowcrab_filter_class", p$selection) ) { 
              data_source_base = "cat"  #only if a single filter based upon spec or spec_bio
            }
          }
        }
      }
    }

    if (data_source_base=="cat") {
      # merge from cat tables as there is no subselection by biologicals
      cat = survey_db( DS="cat", p=p  ) # size information, no, cm, weight (kg) and num per unit area (km^2)
      cat = cat[ which( cat$id %in% unique( set$id) ), ]
      isc = filter_data( cat, p$selection$biologicals )
      if (length(isc) > 0) cat = cat[isc,]
      isc = NULL
      
      cat$data.source = NULL
      set = merge( set, cat, by="id", all.x=TRUE, all.y=FALSE, suffixes=c("", ".cat") )
      set$totno_adjusted = set$totno * set$cf_vessel / set$sweptarea
      set$totwgt_adjusted = set$totwgt * set$cf_vessel / set$sweptarea
    }
    
    if (data_source_base=="det") {
      # weight and number summaries from det (overwrite those from cat as there is a subsampling)
      # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years
      # cf_det is the weight to make it sum up to the correct total catch (vs any subsamples) and tow length, etc
      det = survey_db( DS="det", p=p  ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]
      if (exists("selection", p)) {
        if (exists("biologicals", p$selection)) {  # filter biologicals
          isc = filter_data( det, p$selection$biologicals )
          if (length(isc) > 0) det = det[isc,]
          isc = NULL
        }
        if (exists("biologicals_using_snowcrab_filter_class", p$selection)) {  # filter biologicals using snow crab short-form ID
          warning( "Filtering using snow crab 'types' requires more data than is carried by survey_db. \n 
            .. Adding data directly from snowcrab.db .. this also means dropping other sources of data \n")
          det_sc = bio.snowcrab::snowcrab.db( DS ="det.georeferenced" )
          det_sc$spec = 2526
          det_sc$spec_bio =  taxonomy.recode( from="spec", to="parsimonious", tolookup=det_sc$spec ) # snow crab using groundfish codes
          det_sc$individual_id = paste( det_sc$trip, det_sc$set, det_sc$spec_bio, det_sc$crabno, sep=".")
          det_sc$filter.class = p$selection$biologicals_using_snowcrab_filter_class
          isc = bio.snowcrab::filter.class( x=det_sc, type=p$selection$biologicals_using_snowcrab_filter_class )
          if (length(isc) > 0) det_sc = det_sc[isc, c("individual_id", "filter.class") ]
          isc = NULL
          det$individual_id = paste( det$id2, det$individual, sep=".")
          det = merge( det, det_sc, by="individual_id", all.x=TRUE, all.y=FALSE, suffixes=c("", "det_sc") )
          det = det[ !is.na(det$filter.class), ]
        }
      }
      
      setDT(set)
      setDT(det)
      det_summary = det[, .(
          totno=.N, 
          totwgt=sum(mass, na.rm=TRUE),
          totno_adjusted=sum(cf_det_no, na.rm=TRUE),
          totwgt_adjusted=sum(mass*cf_det_wgt, na.rm=TRUE),
          mr=sum(mr*cf_det_wgt, na.rm=TRUE),
          smr=mean(smr*cf_det_wgt, na.rm=TRUE), 
          residual=mean(residual*cf_det_wgt, na.rm=TRUE), 
          mass=mean(mass*cf_det_wgt, na.rm=TRUE), 
          len=mean(len*cf_det_wgt, na.rm=TRUE), 
          Ea=mean(Ea*cf_det_wgt, na.rm=TRUE), 
          A=mean(A*cf_det_wgt, na.rm=TRUE), 
          Pr.Reaction=mean(Pr.Reaction*cf_det_wgt, na.rm=TRUE) 
        ), 
        by=.(id)
      ]

      set = det_summary[set, on=.(id) ] # merge

      set$mr[ which(!is.finite(set$mr))] = 0
      set$smr[ which(!is.finite(set$smr))] = 0
      set$Ea[ which(!is.finite(set$Ea))] = 0
      set$A[ which(!is.finite(set$A))] = 0
      set$Pr.Reaction[ which(!is.finite(set$Pr.Reaction))] = 0
    }

    set$totno_adjusted[ which(!is.finite(set$totno_adjusted))] = 0
    set$totwgt_adjusted[ which(!is.finite(set$totwgt_adjusted))] = 0
    set$totno[ which(!is.finite(set$totno))] = 0
    set$totwgt[ which(!is.finite(set$totwgt))] = 0

    # NOTE:: these should be == or ~= 1/set$sa ( done this way in case there has been other adjustmensts such as subampling, etc ..) .. these become offets required to express totwgt or totno at a common areal density per unit km^2 in Poisson models

    set$cf_set_mass = set$totwgt_adjusted / set$totwgt
    set$cf_set_no = set$totno_adjusted / set$totno

    ii = which(!is.finite(set$cf_set_mass ))
    if (length(ii) > 0) set$cf_set_mass[ii] = 1/set$sweptarea[ii]

    ii = which(!is.finite(set$cf_set_no ))
    if (length(ii) > 0) set$cf_set_no[ii] = 1/set$sweptarea[ii]

 
    # aggeragate measurement filter
    if (exists("selection", p)) {
      # last pass .. filter any set-level traits (mostly aggregate biological characgeristics, if any)
      if (exists("aggregate", p$selection)) {  # filter on survey characteristics
        if (exists("drop.unreliable.zeros.groundfish.data", p$selection$aggregate )) {
          if (p$selection$aggregate$drop.unreliable.zeros.groundfish.data) {
            # special flag .. unreliable zero's for snowcrab in the groundfish data
            todrop = which( set$data.source=="groundfish" & set$yr < 1999 & (set$totwgt ==0 | set$totno==0) )
            if (length(todrop)>0) set = set[ -todrop, ]
          }
        }
        isc = filter_data( set, p$selection$aggregate )
        if (length(isc) > 0) set = set[isc,]
        isc = NULL
      }
    }

    # fix NA's 
 
    routine_replacement = c( "spec", "spec_bio", "name.common", "name.scientific", "itis.tsn" )
    for ( vn in routine_replacement ) {
      if (exists( vn, set)) {
        i = which( is.na(set[[vn]] ) )
        if (length(i) > 0) set[[vn]][i] = unique( set[[vn]][-i] )[1] 
      }
    }

    i = which( !is.finite(set$id2) )
    if (length(i) > 0) set$id2[i] = paste( set$id[i], set$spec[i], sep="."  ) 

    return (set)
  }



# ----------------------------------


  if (DS == "carstm_inputs" ) {

    # format data, lookup variables where required and then create prediction surface
    require(carstm)
    
    if (is.null(sppoly)) sppoly = areal_units( p=p )

    crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
    sppoly = st_transform(sppoly, crs=crs_lonlat )
    sppoly$data_offset = sppoly$sa

    areal_units_fn = attributes(sppoly)[["areal_units_fn"]]

    if (is.null(fn))  fn = file.path( p$modeldir, p$carstm_model_label, paste("carstm_inputs", areal_units_fn, sep="_") )

    # inputs are shared across various secneario using the same polys
    #.. store at the modeldir level as default
    outputdir = dirname( fn )
    if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

    M = NULL
    if ( !redo ) {
      if (file.exists( fn) ) {
        message( "Loading previously saved carstm_inputs ... ", fn)
        M = read_write_fast( fn)
        return ( M )
      }
    }
    message( "Generating carstm_inputs ... ", fn)

    oo = p$selection$survey$strata_toremove 
    p$selection$survey$strata_toremove = NULL  # emphasize that all data enters analysis initially ..
    
    set = survey_db( p=p, DS="filter" ) # units:: kg / km^2 or  no / km^2


    crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
    inside = st_points_in_polygons(
      pts = st_as_sf( set[, c("lon", "lat")], coords=c("lon","lat"), crs=crs_lonlat ),
      polys = st_transform( st_union(sppoly), crs_lonlat ),
      method="sp::point.in.polygon"
    )

    set = set[which(inside), ]


    p$selection$survey$strata_toremove = oo  
     
    set$totno[which(!is.finite(set$totno))] = NA

    # ensure we have some estimate of sweptarea and choose the appropriate
    # one based upon which trawlable units we are using
    ft2m = 0.3048
    m2km = 1/1000
    nmi2mi = 1.1507794
    mi2ft = 5280
    standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm

    set$data_offset = switch( p$trawlable_units,
      standardtow =  rep(standardtow_sakm2, nrow(set)) , # "standard tow"
      towdistance = set$sa_towdistance,  # "sa"=computed from tow distance and standard width, 0.011801==),
      sweptarea = set$sa,  # swept area based upon stand tow width and variable lenths based upon start-end locations wherever possible
      direct_number = 1 / set[, "cf_set_no"],  # sa, subsampling
      direct_biomass = 1 / set[, "cf_set_mass"]  # sa, subsampling
    )

    set$data_offset = set$data_offset * set$vessel_corection  # the latter is ==1 but groundfish have misc species and vessel specific values which we ignore, can ignore but here in case you prefer use it

    set$data_offset[which(!is.finite(set$data_offset))] = median(set$data_offset, na.rm=TRUE )  # just in case missing data
    set = set[ which(  is.finite(set$data_offset)   ),  ]
 

    set$pa = presence.absence( X=set$totno / set$data_offset, px=p$habitat.threshold.quantile )$pa  # determine presence absence and weighting
    set$meansize  = set$totwgt / set$totno  # note, these are constrained by filters in size, sex, mat, etc. .. in the initial call

    set$tiyr = lubridate::decimal_date ( set$timestamp )  # required for inputdata
 

    # So fiddling is required as extreme events can cause optimizer to fail
    if (!is.null(quantile_upper_limit)) {
      # truncate upper bounds of density 

      ndensity = set$totno / set$data_offset
      qn = quantile( ndensity, quantile_upper_limit, na.rm=TRUE )
      ni = which( ndensity > qn )
      set$totno[ni] = floor( qn * set$data_offset[ni] )

      bdensity = set$totwgt / set$data_offset
      qm = quantile( bdensity, quantile_upper_limit, na.rm=TRUE )
      mi = which( bdensity > qm )
      set$totwgt[mi] = floor( qm * set$data_offset[mi] )
    }

    M = carstm_prepare_inputdata(
      p=p,
      M=set,
      sppoly=sppoly,
      APS_data_offset=1, #ie. SA
      vars_to_retain= p$vars_to_retain
    )

    # these vars being missing means zero-valued
    vars_to_zero = c( "mr", "Ea", "Pr.Reaction", "A", "smr" )
    for ( vn in vars_to_zero ) {
      if (exists( vn, M)) {
        i = which( is.na(M[, vn] ) )
        if (length(i) > 0) M[i, vn] = 0 
      }
    }

    if ( exists("substrate.grainsize", M)) M$log.substrate.grainsize = log( M$substrate.grainsize )

    if (!exists("yr", M)) M$yr = M$year  # req for meanweights

    # IMPERATIVE: 
    i =  which(!is.finite(M$z))
    j =  which(!is.finite(M$t)) 

    if (length(j)>0 | length(i)>0) {
      warning( "Some areal units that have no information on key covariates ... you will need to drop these and do a sppoly/nb reduction with areal_units_neighbourhood_reset() :")
          print( "Missing depths:")
      print(unique(M$AUID[i]) )
      print( "Missing temperatures:")
      print(unique(M$AUID[j] ) )
    }
    # M = M[ which( is.finite(M$t ) ), ]
    # M = M[ which( is.finite(M$z ) ), ]

    # predictions to: westeren 2a and NED
    gears_ref = "Western IIA trawl"
    i = which(is.na(M$gear)) 
    M$gear[ i ] = gears_ref
    gears = unique(M$gear[-i])
    gears = c( gears_ref, setdiff( gears, gears_ref ) ) # reorder
    M$gear = as.numeric( factor( M$gear, levels=gears ) )
    attr( M$gear, "levels" ) = gears


    M$vessel = substring(M$id,1,3)
    M$id = NULL 

    vessels_ref = "NED"
    i = which(is.na(M$vessel) )
    M$vessel[ i ] = vessels_ref
    vessels = unique(M$vessel[-i])
    vessels = c( vessels_ref, setdiff( vessels, vessels_ref ) ) # reorder
    M$vessel= as.numeric( factor( M$vessel, levels=vessels ) )
    attr( M$vessel, "levels" ) = vessels
 
    M$space = match( M$AUID, sppoly$AUID) # for bym/car .. must be numeric index matching neighbourhood graphs
    M$space_time = M$space  # copy for space_time component (INLA does not like to re-use the same variable in a model formula) 
    M$space_cyclic = M$space  # copy for space_time component (INLA does not like to re-use the same variable in a model formula) 

    M$time = match( M$year, p$yrs ) # copy for space_time component .. for groups, must be numeric index
    M$time_space = M$time    
    
    M$cyclic = match( M$dyri, discretize_data( span=c( 0, 1, p$nw) ) ) 
    M$cyclic_space = M$cyclic # copy cyclic for space - cyclic component .. for groups, must be numeric index
  
    read_write_fast( M, fn=fn )

    return(M)

  }

}
