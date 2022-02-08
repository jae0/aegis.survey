
carstm_prepare_inputdata = function( p, M, sppoly, 
  carstm_prediction_surface_parameters = NULL, 
  lookup_projects=c("bathymetry", "substrate", "temperature", "speciescomposition_pca1", "speciescomposition_pca2" ), 
  APS_data_offset=1, NA_remove=TRUE, vars_to_retain=NULL, vars_to_drop=NULL, lookup_exhaustive=TRUE, 
  retain_positions_outside_of_boundary= 0
) {

  # covariates only with stmv
  # covars = c("t", "tsd", "tmax", "tmin", "degreedays", "z",  "dZ", "ddZ", "substrate.grainsize" ) ;;

  # currently supported:
  # z = depth (m)
  # dZ = bottom slope (m/km)
  # ddZ = bottom curvature (m/km^2)
  # substrate.grainsize = mean grain size of bottom substrate (mm)
  # t = temperature (C) – subannual
  # tlb = temperature lower 95% bound (C) –subannual
  # tub = temperature upper 95% bound (C) –subannual
  # tmean = mean annual temperature
  # tsd = standard deviation of the mean annual temperature
  # tmin = minimum value of temperature in a given year – annual
  # tmax= maximum value of temperature in a given year – annual
  # tamplitude = amplitude of temperature swings in a year (tmax-tmin) – annual
  # degreedays = number of degree days in a given year – annual


  if ( is.null(carstm_prediction_surface_parameters)) {
    if (exists("carstm_prediction_surface_parameters", p)) carstm_prediction_surface_parameters = p$carstm_prediction_surface_parameters
  }  
  
  if ( is.null(carstm_prediction_surface_parameters)) {
    # these params are for observation data
    carstm_prediction_surface_parameters = list()

    if ("bathymetry"  %in% lookup_projects) carstm_prediction_surface_parameters[["bathymetry"]] = aegis.bathymetry::bathymetry_parameters( project_class="carstm" )  # full default
    if ("substrate"   %in% lookup_projects) carstm_prediction_surface_parameters[["substrate"]] = aegis.substrate::substrate_parameters(  project_class="carstm" )
    if ("temperature" %in% lookup_projects) carstm_prediction_surface_parameters[["temperature"]] =  aegis.temperature::temperature_parameters(  project_class="carstm" )
    if (any( grepl("speciescomposition", lookup_projects)) ) {
      carstm_prediction_surface_parameters[["speciescomposition_pca1"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="pca1"  )
      carstm_prediction_surface_parameters[["speciescomposition_pca2"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="pca2"  )
      carstm_prediction_surface_parameters[["speciescomposition_pca3"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="pca3"  )
      carstm_prediction_surface_parameters[["speciescomposition_ca1"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="ca1"  )
      carstm_prediction_surface_parameters[["speciescomposition_ca2"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="ca2"  )
      carstm_prediction_surface_parameters[["speciescomposition_ca3"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="ca3"  )
    }
 
  }

  lookup_parameters_names = names(carstm_prediction_surface_parameters)
   
  nS = nrow(M)

  setDT(M)

  if (!is.null(vars_to_retain)) {
    vv = setdiff(  vars_to_retain, names(M) ) 
    if (length(vv) > 0) {
      print( "Some variables to retain not found:")
      print( paste0(vv, sep=" ") )
    }
  }

  M$tag = "observations"
 
  crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
  sppoly = st_transform(sppoly, st_crs(crs_lonlat))
  
  Mpts = st_as_sf( M, coords=c("lon","lat"), crs=crs_lonlat )

  # observations
  M$AUID = st_points_in_polygons(
    pts = Mpts,
    polys = sppoly[, "AUID"],
    varname = "AUID"
  )
  
  ooo = which( is.na(M$AUID ) )
  if (length(ooo) > 0 )  {
    if ( retain_positions_outside_of_boundary ) {
      # associating closest polygon to a given data point
      au_centroid = st_transform( st_centroid(sppoly), st_crs( p$aegis_proj4string_planar_km))
      Mpts = st_transform( Mpts,  st_crs( p$aegis_proj4string_planar_km))
      for ( i in 1:length(ooo)) {
        j = ooo[i]
        dd = c(st_distance( Mpts[j,], au_centroid))
        k = which.min( dd  )
        if (dd[k] < retain_positions_outside_of_boundary ) M$AUID[j] = sppoly$AUID[k]
      }
    }
  } 

  ooo = which( is.na(M$AUID ) )
  if (length(ooo) > 0 )  {
      print("Dropping data with no associated areal units (probably need to alter sppoly):\n")
      print( M[ooo,] )
      plot(sppoly["AUID"], reset=FALSE)
      plot(st_as_sf( M, coords=c("lon","lat"), crs=crs_lonlat ), add=TRUE ) 
      plot(st_as_sf( M[ooo,], coords=c("lon","lat"), crs=crs_lonlat ), col="red", add=TRUE ) 
      # message( "If dropping these locations, in red, is OK, then press 'c' to continue.")
      # browser() 
      M = M[ - ooo, ]
  }

  ooo = dd = j = k = Mpts =au_centroid= NULL; gc()

  M$AUID = as.character( M$AUID )  # match each datum to an area

  nM = nrow(M)

  if ("bathymetry" %in% lookup_parameters_names) {
    require(aegis.bathymetry)
    message( "lookup: bathymetry observations")
    vn = "z"

    if ( !(exists( vn, M )))  M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters="bathymetry",    
        LOCS=M[ iM, c("lon", "lat")],  
        project_class="core", 
        output_format="points" , 
        DS="aggregated_data", 
        variable_name="z.mean", 
        space_resolution=p$pres,
        returntype="vector"
      ) 
    }

    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters="bathymetry",    
        LOCS=M[ iM, c("lon", "lat")],  
        project_class="stmv", 
        output_format="points" , 
        DS="complete", 
        variable_name="z",
        space_resolution=p$pres, 
        returntype="vector" 
      ) 
    }
 
    p_bathymetry_stmv = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="stmv" )

    LU = bathymetry_db( p=p_bathymetry_stmv, DS="baseline", varnames="all" )
    iML = match( 
      array_map( "xy->1", M[, c("plon","plat")], gridparams=p$gridparams ), 
      array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams ) 
    )
    vns = intersect(  c( "z", "dZ", "ddZ", "b.sdSpatial", "b.sdObs", "b.phi", "b.nu", "b.localrange" ), names(LU) )
    for (vn in setdiff( vns, "z") ) M[[ vn]] = LU[ iML, vn ]
    LU =  iML = vns = NULL

    if ( exists("spatial_domain", p)) {
      # need to be careful with extrapolation ...  filter depths
      ii = geo_subset( spatial_domain=p$spatial_domain, Z=M )
      if (length(ii)> 0 ) {
        nM = nrow(M)
        M = M[ ii , ] 
        message( "Dropping observations due to spatial domain and depth: ", (nM - length(ii)) )
      }
    }

    if (NA_remove) {
      ii = which( !is.finite( rowSums(M[, vns, with=FALSE] )  ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing depth lookup stats: ", length(ii))
      }
    }

  }


  # --------------------------


  if ("substrate" %in% lookup_parameters_names) {
    require(aegis.substrate)

    message( "lookup: substrate observations")

    vn = "substrate.grainsize"

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM]  = aegis_lookup( 
        parameters="substrate",  
        LOCS=M[iM, c("lon", "lat")], 
        project_class="core", 
        output_format="points", 
        DS="aggregated_data", 
        variable_name="substrate.grainsize.mean" 
      )  
    }   
    
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters="substrate",    
        LOCS=M[ iM, c("lon", "lat")],  
        project_class="stmv", 
        output_format="points" , 
        DS="complete", 
        variable_name="substrate.grainsize",
        space_resolution=p$pres, 
        returntype="vector" 
      ) 
    }
    # due to limited spatial range, resort to using some of the modelled results as well to fill in some gaps

    # yes substrate source coordinate system is same as for bathy .. to match substrate source for the data
    p_bathymetry_stmv = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="stmv" )

    LUB = bathymetry_db( p=p_bathymetry_stmv, DS="baseline", varnames="all" )
    iML = match( 
      array_map( "xy->1", M[, c("plon","plat")],  gridparams=p$gridparams ), 
      array_map( "xy->1", LUB[,c("plon","plat")], gridparams=p$gridparams ) 
    )

    p_substrate_stmv = substrate_parameters( spatial_domain=p$spatial_domain, project_class="stmv" )
    LU = substrate_db( p=p_substrate_stmv, DS="complete"  )

    vns = intersect(  c( 
        "substrate.grainsize", "substrate.grainsize.lb", "substrate.grainsize.ub", 
        "s.sdTotal", "s.sdSpatial", "s.sdObs", "s.phi", "s.nu", "s.localrange" 
      ), names(LU) )

    for (vn in vns  ) M[[ vn]] = LU[ iML, vn ]
    
    if (NA_remove) {
      ii = which( !is.finite( rowSums(M[, vns, with=FALSE] )  ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing substrate lookup stats: ", length(ii))
      }
    }
    LU =  iML = vns = NULL
  
  }


  # --------------------------


  if ("temperature" %in% lookup_parameters_names) {
    require(aegis.temperature)

    message( "lookup: temperature observations")

    vn = "t"

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters="temperature", # aggregated data takes text as param 
        LOCS=M[ iM, c("lon", "lat", "timestamp")],
        project_class="core", 
        DS="aggregated_data", 
        output_format="points", 
        variable_name="t.mean", 
        space_resolution = p$pres,
        time_resolution = 1/10 ,   
        tz="America/Halifax",
        year.assessment=p$year.assessment
      )
    }

    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["temperature"],  
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly,
        project_class = "carstm", # lookup from modelled predictions from carstm
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        year.assessment=p$year.assessment,
        space_resolution = p$pres,
        time_resolution = 1/10 ,  # fraction of year (2 months)
        tz="America/Halifax", 
        returntype = "vector"
      )
    }

    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing temperatures: ", length(ii))
      }

    }

    M = M[ which( M[[ vn]]  < 16 ) , ]  #

    # to to:  add stmv/hybrid 
  }


  # --------------------------

  if ("speciescomposition_pca1" %in% lookup_parameters_names) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca1 observations")

    vn = "pca1"

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters="speciescomposition_pca1",  
        LOCS=M[ iM, c("lon", "lat", "timestamp")],
        project_class="core", 
        DS="speciescomposition", 
        output_format="points", 
        variable_name=vn, 
        tz="America/Halifax",
        year.assessment=p$year.assessment
      )
    }

    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["speciescomposition_pca1"],  
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        year.assessment=p$year.assessment,
        space_resolution=p$pres,
        returntype = "vector"
      ) 
    }
    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing pca1: ", length(ii))
      }
    }
  }

  if ("speciescomposition_pca2" %in% lookup_parameters_names) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca2 observations")

    vn = "pca2"

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters="speciescomposition_pca2",  
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name=vn, 
          year.assessment=p$year.assessment,
          tz="America/Halifax" 
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["speciescomposition_pca2"],  
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        year.assessment=p$year.assessment,
        space_resolution=p$pres,
        returntype = "vector"
      ) 
    }
    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing pca2: ", length(ii))
      }
    }

  }

  if ("speciescomposition_pca3" %in% lookup_parameters_names) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca3 observations")

    vn = "pca3"

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters="speciescomposition_pca3", # core uses text 
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          year.assessment=p$year.assessment,
          variable_name=vn, 
          tz="America/Halifax" 
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["speciescomposition_pca3"], 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        year.assessment=p$year.assessment,
        space_resolution=p$pres,
        returntype = "vector"
      ) 
    }
    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing pca3: ", length(ii))
      }
    }

  }

  if ("speciescomposition_ca1" %in% lookup_parameters_names) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca1 observations")

    vn = "ca1"

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters="speciescomposition_ca1",  
        LOCS=M[ iM, c("lon", "lat", "timestamp")],
        project_class="core", 
        DS="speciescomposition", 
        output_format="points", 
        year.assessment=p$year.assessment,
        variable_name=vn, 
        tz="America/Halifax" 
      )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["speciescomposition_ca1"], 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        space_resolution=p$pres,
        year.assessment=p$year.assessment,
        returntype = "vector"
      )     }
      
    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing ca1: ", length(ii))
      }
    }
  }

  if ("speciescomposition_ca2" %in% lookup_parameters_names) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca2 observations")

    vn = "ca2"

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters="speciescomposition_ca2", 
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name=vn, 
          year.assessment=p$year.assessment,
          tz="America/Halifax" 
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["speciescomposition_ca2"], 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        space_resolution=p$pres,
        year.assessment=p$year.assessment,
        returntype = "vector"
      ) 
    }
      
    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing ca2: ", length(ii))
      }
    }

  }

  if ("speciescomposition_ca3" %in% lookup_parameters_names) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca3 observations")

    vn = "ca3"
    
    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters="speciescomposition_ca3",  
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name=vn, 
          year.assessment=p$year.assessment,
          tz="America/Halifax" 
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters=carstm_prediction_surface_parameters["speciescomposition_ca3"],  
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        space_resolution=p$pres,
        year.assessment=p$year.assessment,
        returntype = "vector"
      ) 
    }
      
    if (NA_remove) {
      ii = which( !is.finite( M[[ vn]] ))
      if (length(ii) > 0 ) {
        M = M[ -ii , ]
        message( "Dropping observations due to missing ca3: ", length(ii))
      }
    }

  }

  M$plon = M$plat = M$lon = M$lat = NULL

  if (any( grepl("offset", as.character(p$formula)))){
    if (!exists("data_offset", M)) {
      if (exists("data_offset", sppoly)) {
        message("data_offset not defined, using data_offset from sppoly")
        M$data_offset = sppoly$data_offset[ match(  M$AUID,  sppoly$AUID ) ]
      }
    }
  }
  

  if ( grepl( "year", p$aegis_dimensionality ) ) {
    if (!exists("year", M)) {
      if (exists("yr", M)) names(M)[which(names(M)=="yr") ] = "year"
    }
    # though dyear is required for seasonal models, it can also be used in annual as well 
    # that is, if not predicting the full seasonal array but only A GIVEN time slice 
    if (!exists("dyear", M)) {
      if (exists("tiyr", M )) M$dyear = M$tiyr - M$year 
    }
  }

  # to to:  add st,v/hybrid 


  # end observations
  # ----------


  # ----------
  # generate prediction surface locations (APS) 
  # .. use carstm predictions (project_class)
  
  message( "Creating prediction surface ... ")

  if (grepl("space", p$aegis_dimensionality)) {

    region.id = slot( slot(sppoly, "nb"), "region.id" )
    APS = st_drop_geometry(sppoly)
    setDT(APS)

    APS$AUID = as.character( APS$AUID )
    APS$tag ="predictions"

    if (exists("data_offset", M)) {
      APS$data_offset =  APS_data_offset   
      APS = APS[ , c( "AUID", "tag", "data_offset" ) ]
    }

    APS[, p$variabletomodel] = NA
   
  }

  if ( "bathymetry" %in% lookup_parameters_names ) {
    require(aegis.bathymetry)
    message( "lookup: bathymetry predictions")

    vn = "z"
    pc = carstm_prediction_surface_parameters[["bathymetry"]][["project_class"]]

    APS[[vn]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["bathymetry"], 
      LOCS=sppoly$AUID,
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution = switch( pc, carstm=p$pres, stmv=p$pres, p$pres ) ,
      returntype = "vector"
    ) 

    if (lookup_exhaustive) {

      if (pc != "stmv" ) {
        iM = which(!is.finite( APS[[vn]] )) 

        if (pc != "stmv") {
          if (length(iM) > 0 ) {
            # depth is very important
            APS[[vn]][iM] = aegis_lookup(  
              parameters="bathymetry",  
              LOCS=APS$AUID[iM],
              LOCS_AU=sppoly,
              project_class = "stmv", # lookup from modelled predictions from stmv
              output_format = "areal_units",
              variable_name="z", 
              space_resolution=p$pres ,
              returntype = "vector"
            ) 

          }
        }
      }

      iM = which(!is.finite( APS[[vn]] )) 
      if (length(iM) > 0 ) {
        # depth is very important so try again
        APS[[vn]][iM]  = aegis_lookup(  
          parameters="bathymetry", 
          LOCS=APS$AUID[iM],
          LOCS_AU=sppoly,
          project_class = "core", # lookup from aggregated data
          output_format = "areal_units",
          DS = "aggregated_data",  # needed for core 
          variable_name = "z.mean", 
          space_resolution=p$pres ,
          returntype = "vector"
        ) 
      }
    
    }

  }

  if ( "substrate" %in% lookup_parameters_names ) {
    require(aegis.substrate)

    message( "lookup: substrate predictions")

    vn = "substrate.grainsize"
    pc = carstm_prediction_surface_parameters[["substrate"]][["project_class"]]
    APS[[vn]]  = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["substrate"],  
      LOCS=sppoly$AUID,
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars = c("mean"),
      space_resolution = p$pres ,
      returntype = "vector"
    )  
 
  }

  # prediction surface in time
  # to this point APS is static, now add time dynamics (teperature),  expand APS to all time slices
  if ( grepl( "year", p$aegis_dimensionality ) | (grepl( "season", p$aegis_dimensionality )  ) ) {
    n_aps = nrow(APS)
    APS = cbind( APS[ rep.int(1:n_aps, p$nt), ], rep.int( p$prediction_ts, rep(n_aps, p$nt )) )
    names(APS)[ncol(APS)] = "tiyr"
    APS$timestamp = lubridate::date_decimal( APS$tiyr, tz=p$timezone )
    APS$year = trunc( APS$tiyr)
    APS$dyear = APS$tiyr - APS$year
  }



  # ---------------------
  if ( "temperature" %in% lookup_parameters_names ) {
    require(aegis.temperature)
    message( "lookup: temperature predictions")

    vn = "t"
    pc = carstm_prediction_surface_parameters[["temperature"]][["project_class"]]

    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["temperature"],   
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      year.assessment=p$year.assessment,
      tz="America/Halifax",
      returntype = "vector"
    )
  
  }


  if ( "speciescomposition_pca1" %in% lookup_parameters_names ) {


    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca1 predictions")

    vn = "pca1"
    pc = carstm_prediction_surface_parameters[["speciescomposition_pca1"]][["project_class"]]
 
    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["speciescomposition_pca1"],   
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly, 
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      year.assessment=p$year.assessment,
      returntype = "vector"
    ) 
  }


  if ( "speciescomposition_pca2" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca2 predictions")
    vn = "pca2"
    pc = carstm_prediction_surface_parameters[["speciescomposition_pca2"]][["project_class"]]

    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["speciescomposition_pca2"], 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      year.assessment=p$year.assessment,
      returntype = "vector"
    ) 
  }

  if ( "speciescomposition_pca3" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca3 predictions")
    vn = "pca3"
    pc = carstm_prediction_surface_parameters[["speciescomposition_pca3"]][["project_class"]]

    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["speciescomposition_pca3"], 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      year.assessment=p$year.assessment,
      returntype = "vector"
    ) 
  }


  if ( "speciescomposition_ca1" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca1 predictions")

    vn = "ca1"
    pc = carstm_prediction_surface_parameters[["speciescomposition_ca1"]][["project_class"]]

 
    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["speciescomposition_ca1"],  
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly, 
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      year.assessment=p$year.assessment,
      returntype = "vector"
    ) 
  }


  if ( "speciescomposition_ca2" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca2 predictions")
    vn = "ca2"
    pc = carstm_prediction_surface_parameters[["speciescomposition_ca2"]][["project_class"]]

    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["speciescomposition_ca2"],  
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      returntype = "vector"
    ) 
 }

  if ( "speciescomposition_ca3" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca3 predictions")
    vn = "ca3"
    pc = carstm_prediction_surface_parameters[["speciescomposition_ca3"]][["project_class"]]

    APS[[ vn ]] = aegis_lookup( 
      parameters=carstm_prediction_surface_parameters["speciescomposition_ca3"],  
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = pc, # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= switch( pc, carstm=list("predictions"), stmv=vn, vn ) ,
      statvars=c("mean"),
      space_resolution=p$pres ,
      returntype = "vector"
    ) 
 }

 
  if (!is.null(vars_to_retain)) {
    for (vn in vars_to_retain ) {
      if (!exists(vn, APS)) APS[[vn]] = NA
    }
  }

  if (!is.null(vars_to_drop)) {
    for (vn in vars_to_drop ) {
      if (exists(vn, APS)) APS[[vn]] = NULL
    }
  }

  # just in case missing in input data, generate and clean up
  if ( grepl( "year", p$aegis_dimensionality ) | (grepl( "season", p$aegis_dimensionality )  ) ) {
    if ( !exists("tiyr", M) ) M$tiyr = lubridate::decimal_date ( M$timestamp )
    if ( exists("timestamp", M) ) M$timestamp = NULL  # time-based matching finished (if any)
    if ( !exists("tiyr", APS) ) APS$tiyr = lubridate::decimal_date ( APS$timestamp )
    if ( exists("timestamp", APS) ) APS$timestamp = NULL  # time-based matching finished (if any)
  }
  


  # combined observations with prediction surface (for inla)
  vvv = intersect( names(APS), names(M) )
  M = rbind( M[, vvv, with=FALSE ], APS[, vvv, with=FALSE ] )

  APS = NULL; gc()

  # M$uid = 1:nrow(M)  # seems to require an iid model for each obs for stability .. use this for iid
  M$AUID  = as.character(M$AUID)  # revert to factors -- should always be a character
  M$space = as.character( M$AUID)
 
  if (exists("tiyr", M)) {
    M$tiyr  = trunc( M$tiyr / p$tres )*p$tres    # discretize for inla .. midpoints
    M$yr = trunc( M$tiyr)
    M$time = as.character( M$yr )  # copy for INLA

    # do not sepraate out as season can be used even if not predicted upon
    ii = which( M$dyear > 1) 
    if (length(ii) > 0) M$dyear[ii] = 0.99 # cap it .. some surveys go into the next year

    M$dyri = discretize_data( M[["dyear"]], discretizations()[["dyear"]] )
    M$cyclic = as.character( M$dyri )  # copy for carstm/INLA
  }
  
  M$tiyr = NULL

    message( "Number of initial observations:  ", nS  )
    message( "Number of observations in domain:  ", nM  )
    message( "Number of observations final:  ", length(which(M$tag=="observations" )) )
    message( "Number of predictions:  ", length(which(M$tag=="predictions" )) )

  return(M)
}
