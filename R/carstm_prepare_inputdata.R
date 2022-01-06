
carstm_prepare_inputdata = function( p, M, sppoly, 
  lookup_parameters = NULL, 
  lookup_projects=c("bathymetry", "substrate", "temperature", "speciescomposition_pca1", "speciescomposition_pca2" ), 
  APS_data_offset=1, NA_remove=TRUE, vars_to_retain=NULL, vars_to_drop=NULL, lookup_exhaustive=TRUE
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


  if ( is.null(lookup_parameters)) {
    if (exists("carstm_lookup_parameters", p)) lookup_parameters = p$carstm_lookup_parameters
  }  
  
  if ( is.null(lookup_parameters)) {
    if (exists("lookup_parameters", p)) lookup_parameters = p$lookup_parameters
  }  
  
  if ( is.null(lookup_parameters)) {
    # these params are for observation data
    lookup_parameters = list()

    if ("bathymetry"  %in% lookup_projects) lookup_parameters[["bathymetry"]] = aegis.bathymetry::bathymetry_parameters( project_class="carstm" )  # full default
    if ("substrate"   %in% lookup_projects) lookup_parameters[["substrate"]] = aegis.substrate::substrate_parameters(  project_class="carstm" )
    if ("temperature" %in% lookup_projects) lookup_parameters[["temperature"]] =  aegis.temperature::temperature_parameters(  project_class="carstm", yrs=p$yrs )
    if (any( grepl("speciescomposition", lookup_projects)) ) {
      lookup_parameters[["speciescomposition_pca1"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="pca1", yrs=p$yrs  )
      lookup_parameters[["speciescomposition_pca2"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="pca2", yrs=p$yrs  )
      lookup_parameters[["speciescomposition_pca3"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="pca3", yrs=p$yrs  )
      lookup_parameters[["speciescomposition_ca1"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="ca1", yrs=p$yrs  )
      lookup_parameters[["speciescomposition_ca2"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="ca2", yrs=p$yrs  )
      lookup_parameters[["speciescomposition_ca3"]] = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", variabletomodel="ca3", yrs=p$yrs  )
    }
 
  }

  lookup_parameters_names = names(lookup_parameters)
   
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

  # observations
  M$AUID = st_points_in_polygons(
    pts = st_as_sf( M, coords=c("lon","lat"), crs=crs_lonlat ),
    polys = sppoly[, "AUID"],
    varname = "AUID"
  )
  M = M[!is.na(M$AUID),]
  M$AUID = as.character( M$AUID )  # match each datum to an area

  nM = nrow(M)

  M_au = unique(M$AUID)
  sppoly_au = unique(sppoly$AUID)

  missing_au = setdiff( sppoly_au, M_au )
  if (length(missing_au) > 0 )  {
    warning("Areal units with no data found, this will likely cause problems:\n", paste0(missing_au, sep=" "))
    print("Areal units with no data found, this will likely cause problems:")
    print( paste0(missing_au, sep=", "))
  }
  
      
  if ("bathymetry" %in% lookup_parameters_names) {
    require(aegis.bathymetry)
    message( "lookup: bathymetry observations")
    vn = lookup_parameters[["bathymetry"]]$variabletomodel

    if ( !(exists( vn, M )))  M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters=lookup_parameters["bathymetry"], 
        LOCS=M[ iM, c("lon", "lat")],  
        project_class="core", 
        output_format="points" , 
        DS="aggregated_data", 
        variable_name="z.mean", 
        returntype="vector" 
      ) 
    }
 
    if ( exists("spatial_domain", p)) {
        # need to be careful with extrapolation ...  filter depths
        if (NA_remove)  {
          ii = which(! is.finite(M[[vn]] ) )
          if (length(ii) > 0 ) {
            M = M[ -ii , ]
            message( "Dropping observations due to spatial domain: ", length(ii))
          }
        }
        ii = geo_subset( spatial_domain=p$spatial_domain, Z=M )
        if (length(ii)> 0 ) {
          nM = nrow(M)
          M = M[ ii , ] 
          message( "Dropping observations due to spatial domain and depth: ", (nM - length(ii)) )
        }
    }


    lookup_parameters[["bathymetry_stmv"]] = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="stmv" )

    LU = bathymetry_db( p=lookup_parameters[["bathymetry_stmv"]], DS="baseline", varnames="all" )
    iML = match( 
      array_map( "xy->1", M[, c("plon","plat")], gridparams=p$gridparams ), 
      array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams ) 
    )
    vns = intersect(  c( "z", "dZ", "ddZ", "b.sdSpatial", "b.sdObs", "b.phi", "b.nu", "b.localrange" ), names(LU) )
    for (vn in setdiff( vns, "z") ) M[[ vn]] = LU[ iML, vn ]
    LU =  iML = vns = NULL
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

    vn = lookup_parameters[["substrate"]]$variabletomodel

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM]  = aegis_lookup( 
        parameters=lookup_parameters["substrate"], 
        LOCS=M[iM, c("lon", "lat")], 
        project_class="core", 
        output_format="points", 
        DS="aggregated_data", 
        variable_name="substrate.grainsize.mean" 
      )  
    }   
    
    # due to limited spatial range, resort to using some of the modelled results as well to fill in some gaps

    # yes substrate source coordinate system is same as for bathy .. to match substrate source for the data
    lookup_parameters[["bathymetry_stmv"]] = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="stmv" )

    LUB = bathymetry_db( p=lookup_parameters[["bathymetry_stmv"]], DS="baseline", varnames="all" )
    iML = match( 
      array_map( "xy->1", M[, c("plon","plat")],  gridparams=p$gridparams ), 
      array_map( "xy->1", LUB[,c("plon","plat")], gridparams=p$gridparams ) 
    )

    lookup_parameters[["substrate_stmv"]] = substrate_parameters( spatial_domain=p$spatial_domain, project_class="stmv" )
    LU = substrate_db( p=lookup_parameters[["substrate_stmv"]], DS="complete"  )

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

    vn = lookup_parameters[["temperature"]]$variabletomodel

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters=lookup_parameters["temperature"], 
        LOCS=M[ iM, c("lon", "lat", "timestamp")],
        project_class="core", 
        DS="aggregated_data", 
        output_format="points", 
        variable_name="t.mean", 
        tz="America/Halifax",
        yrs=p$yrs
      )
    }

    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
    
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="temperature", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly,
        project_class = "carstm", # lookup from modelled predictions from carstm
        output_format = "areal_units",
        variable_name=list("predictions"),
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        tz="America/Halifax",
        yrs=p$yrs,
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

    vn = lookup_parameters[["speciescomposition_pca1"]]$variabletomodel

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters=lookup_parameters["speciescomposition_pca1"], 
        LOCS=M[ iM, c("lon", "lat", "timestamp")],
        project_class="core", 
        DS="speciescomposition", 
        output_format="points", 
        variable_name="pca1", 
        tz="America/Halifax" ,
        yrs=p$yrs
      )
    }

    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="speciescomposition_pca1", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        variabletomodel=vn ,
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        yrs=p$yrs,
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

    vn = lookup_parameters[["speciescomposition_pca2"]]$variabletomodel

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters=lookup_parameters["speciescomposition_pca2"], 
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name="pca2", 
          tz="America/Halifax" ,
          yrs=p$yrs
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="speciescomposition_pca2", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        variabletomodel=vn ,
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        yrs=p$yrs,
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

    vn = lookup_parameters[["speciescomposition_pca3"]]$variabletomodel

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters=lookup_parameters["speciescomposition_pca3"], 
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name="pca2", 
          tz="America/Halifax" ,
          yrs=p$yrs
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="speciescomposition_pca3", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        variabletomodel=vn ,
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        yrs=p$yrs,
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

    vn = lookup_parameters[["speciescomposition_ca1"]]$variabletomodel

    if (!(exists(vn, M ))) M[[vn]] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[vn]][iM] = aegis_lookup(  
        parameters=lookup_parameters["speciescomposition_ca1"], 
        LOCS=M[ iM, c("lon", "lat", "timestamp")],
        project_class="core", 
        DS="speciescomposition", 
        output_format="points", 
        variable_name="pca1", 
        tz="America/Halifax" ,
        yrs=p$yrs
      )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="speciescomposition_ca1", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        variabletomodel=vn ,
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        yrs=p$yrs,
        returntype = "vector"
      ) 
    }
      
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

    vn = lookup_parameters[["speciescomposition_ca2"]]$variabletomodel

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters=lookup_parameters["speciescomposition_ca2"], 
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name="pca2", 
          tz="America/Halifax" ,
          yrs=p$yrs
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="speciescomposition_ca2", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        variabletomodel=vn ,
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        yrs=p$yrs,
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

    vn = lookup_parameters[["speciescomposition_ca3"]]$variabletomodel

    if (!(exists(vn, M ))) M[,vn] = NA
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
        M[[vn]][iM]  = aegis_lookup(  
          parameters=lookup_parameters["speciescomposition_ca3"], 
          LOCS=M[ iM, c("lon", "lat", "timestamp")], 
          project_class="core", 
          DS="speciescomposition", 
          output_format="points", 
          variable_name="pca2", 
          tz="America/Halifax" ,
          yrs=p$yrs
        )
    }
    iM = which(!is.finite( M[[vn]] ))
    if (length(iM > 0)) {
      M[[ vn ]][iM] = aegis_lookup( 
        parameters="speciescomposition_ca3", 
        LOCS=M[ iM , c("AUID", "timestamp")], 
        LOCS_AU=sppoly, 
        project_class = "carstm", # lookup from modelled predictions from carstm 
        output_format = "areal_units",
        variable_name=list("predictions"),
        variabletomodel=vn ,
        statvars=c("mean"),
        raster_resolution=min(p$gridparams$res) /2,
        yrs=p$yrs,
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

    vn = lookup_parameters[["bathymetry"]]$variabletomodel
 
    APS[[vn]] = aegis_lookup( 
      parameters="bathymetry", 
      LOCS=sppoly$AUID,
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name= list("predictions"),
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      returntype = "vector"
    ) 

    if (lookup_exhaustive) {

        iM = which(!is.finite( APS[[vn]] )) 
        if (length(iM) > 0 ) {
          # depth is very important
          APS[[vn]][iM] = aegis_lookup(  
            parameters="bathymetry", 
            LOCS=APS$AUID,
            LOCS_AU=sppoly,
            project_class = "stmv", # lookup from modelled predictions from stmv
            output_format = "areal_units",
            variable_name="z", 
            raster_resolution=min(p$gridparams$res) /2,
            returntype = "vector"
          ) 
        }


        iM = which(!is.finite( APS[[vn]] )) 
        if (length(iM) > 0 ) {
          # depth is very important
          APS[[vn]][iM] = aegis_lookup(  
            parameters="bathymetry", 
            LOCS=APS$AUID,
            LOCS_AU=sppoly,
            project_class = "core", # lookup from aggregated data
            output_format = "areal_units",
            DS = "aggregated_data",  # needed for core 
            variable_name = "z.mean", 
            raster_resolution=min(p$gridparams$res) /2,
            returntype = "vector"
          ) 
        }
    
    }

  }

  if ( "substrate" %in% lookup_parameters_names ) {
    require(aegis.substrate)

    message( "lookup: substrate predictions")

    vn = lookup_parameters[["substrate"]]$variabletomodel

    APS[[vn]]  = aegis_lookup( 
      parameters="substrate", 
      LOCS=sppoly$AUID,
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name = list("predictions"),
      statvars = c("mean"),
      raster_resolution = min(p$gridparams$res) /2,
      returntype = "vector"
    )  

    if (lookup_exhaustive) {

      iM = which(!is.finite( APS[[vn]] )) 
      if (length(iM) > 0 ) {

        APS[[vn]][iM] = aegis_lookup(  
          parameters="substrate", 
          LOCS=APS$AUID,
          LOCS_AU=sppoly,
          project_class = "stmv", # lookup from modelled predictions from stmv
          output_format = "areal_units",
          variable_name = "substrate.grainsize", 
          raster_resolution=min(p$gridparams$res) /2,
          returntype = "vector"
        ) 
      }

    }
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

    vn = lookup_parameters[["temperature"]]$variabletomodel
 
    APS[[ vn ]] = aegis_lookup( 
      parameters="temperature", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name=list("predictions"),
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      tz="America/Halifax",
      yrs=p$yrs,
      returntype = "vector"
    )
  
  }


  if ( "speciescomposition_pca1" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca1 predictions")

    vn = lookup_parameters[["speciescomposition_pca1"]]$variabletomodel
 
    APS[[ vn ]] = aegis_lookup( 
      parameters="speciescomposition_pca1", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly, 
      project_class = "carstm", # lookup from modelled predictions from carstm 
      output_format = "areal_units",
      variable_name=list("predictions"),
      variabletomodel=vn ,
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      yrs=p$yrs,
      returntype = "vector"
    ) 
  }


  if ( "speciescomposition_pca2" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca2 predictions")
    vn = lookup_parameters[["speciescomposition_pca2"]]$variabletomodel


    APS[[ vn ]] = aegis_lookup( 
      parameters="speciescomposition_pca2", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name=list("predictions"),
      variabletomodel=vn ,
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      yrs=p$yrs,
      returntype = "vector"
    ) 
  }

  if ( "speciescomposition_pca3" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition pca3 predictions")
    vn = lookup_parameters[["speciescomposition_pca3"]]$variabletomodel

    APS[[ vn ]] = aegis_lookup( 
      parameters="speciescomposition_pca3", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name=list("predictions"),
      variabletomodel=vn ,
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      yrs=p$yrs,
      returntype = "vector"
    ) 
  }


  if ( "speciescomposition_ca1" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca1 predictions")

    vn = lookup_parameters[["speciescomposition_ca1"]]$variabletomodel
 
    APS[[ vn ]] = aegis_lookup( 
      parameters="speciescomposition_ca1", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly, 
      project_class = "carstm", # lookup from modelled predictions from carstm 
      output_format = "areal_units",
      variable_name=list("predictions"),
      variabletomodel=vn ,
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      yrs=p$yrs,
      returntype = "vector"
    ) 
  }


  if ( "speciescomposition_ca2" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca2 predictions")
    vn = lookup_parameters[["speciescomposition_ca2"]]$variabletomodel

    APS[[ vn ]] = aegis_lookup( 
      parameters="speciescomposition_ca2", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name=list("predictions"),
      variabletomodel=vn ,
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      yrs=p$yrs,
      returntype = "vector"
    ) 
 }

  if ( "speciescomposition_ca3" %in% lookup_parameters_names ) {
    require(aegis.speciescomposition)
    message( "lookup: speciescomposition ca3 predictions")
    vn = lookup_parameters[["speciescomposition_ca3"]]$variabletomodel

    APS[[ vn ]] = aegis_lookup( 
      parameters="speciescomposition_ca3", 
      LOCS=APS[ , c("AUID", "timestamp")], 
      LOCS_AU=sppoly,
      project_class = "carstm", # lookup from modelled predictions from carstm
      output_format = "areal_units",
      variable_name=list("predictions"),
      variabletomodel=vn ,
      statvars=c("mean"),
      raster_resolution=min(p$gridparams$res) /2,
      yrs=p$yrs,
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
