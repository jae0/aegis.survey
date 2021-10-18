survey_parameters = function( p=NULL, project_name=NULL, project_class="core", ... ) {

  # ---------------------
  # deal with additional passed parameters
  p = parameters_add(p, list(...) ) # add passed args to parameter list, priority to args


  # ---------------------
  # create/update library list
  p$libs = c( p$libs, RLibrary ( "colorspace",  "fields", "geosphere", "lubridate",  "lattice",
    "parallel",  "rgdal", "rgeos",  "sf",  "GADMTools", "INLA", "data.table" ) )
  p$libs = c( p$libs, project.library ( "aegis", "aegis.polygons", "aegis.coastline", "aegis.bathymetry", "aegis.substrate", "aegis.temperature", "aegis.survey", "aegis.speciescomposition", "aegis.mpa", "netmensuration", "bio.taxonomy" ) )

  p$project_name = ifelse ( !is.null(project_name), project_name, "survey" )

  p$data_sources = c("groundfish", "snowcrab")

  if ( !exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", project_name )
  if ( !exists("datadir", p) )   p$datadir  = file.path( p$data_root, "data" )
  if ( !exists("modeldir", p) )  p$modeldir = file.path( p$data_root, "modelled" )

  if ( !file.exists(p$datadir) ) dir.create( p$datadir, showWarnings=F, recursive=T )
  if ( !file.exists(p$modeldir) ) dir.create( p$modeldir, showWarnings=F, recursive=T )

  if ( !exists("scanmar.dir", p) )  p$scanmar.dir = file.path( p$datadir, "nets", "Scanmar" )
  if ( !exists("marport.dir", p) )  p$marport.dir = file.path( p$datadir, "nets", "Marport" )

  p = parameters_add_without_overwriting( p,
    variabletomodel = "none",
    spatial_domain = "SSE",
    spatial_domain_subareas = c( "SSE.mpa" , "snowcrab"),  # this is for bathymetry_db, not stmv
    aegis_dimensionality="space-year"
  )

  p = spatial_parameters( p=p )  # default grid and resolution

  # define focal years for modelling and interpolation
  if (!exists("year.assessment", p )) {
    message("need probably want to assign current year.assessment, using current year for now")
    p$year.assessment = lubridate::year(lubridate::now())
  }

  yrs_default = 1970:p$year.assessment
  p = parameters_add_without_overwriting( p, yrs = yrs_default, timezone="America/Halifax" )  # default unless already provided
  p = temporal_parameters(p=p)

  if ( !exists("netmensuration.years", p) ) p$netmensuration.years = c(1990:1992, 2004:lubridate::year(lubridate::now())) # 2009 is the first year with set logs from scanmar available .. if more are found, alter this date

  p = parameters_add_without_overwriting( p,
    taxa =  "maxresolved",
    varstomodel = c( "pca1", "pca2", "ca1", "ca2" ),
    inputdata_spatial_discretization_planar_km = p$pres/2, # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    inputdata_temporal_discretization_yr = 1/12,  # ie., controls resolution of data prior to modelling to reduce data set and speed up modelling;; use 1/12 -- monthly or even 1/4.. if data density is low
    taxa.of.interest = aegis.survey::groundfish_variablelist("catch.summary"),
    season = "summer"
  )

  p$discretization = discretizations(p=p$discretization)  # key for discretization levels
  

  if (project_class=="core") {
    return(p)
  }


  if (project_class %in% c("carstm") ) {

    p$project_class = "carstm"

    if (!exists("variabletomodel", p)) stop( "The dependent variable, p$variabletomodel needs to be defined")


    # defaults in case not provided ...
    
    p = parameters_add_without_overwriting( p,
      areal_units_xydata = "survey_db(p=p, DS='areal_units_input')",
      areal_units_type = "lattice", # "stmv_fields" to use ageis fields instead of carstm fields ... note variables are not the same
      areal_units_resolution_km = 25, # default in case not provided ... 25 km dim of lattice ~ 1 hr; 5km = 79hrs; 2km = ?? hrs
      areal_units_proj4string_planar_km =  p$aegis_proj4string_planar_km,  # coord system to use for areal estimation and gridding for carstm
      # areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")  # coord system to use for areal estimation and gridding for carstm
      areal_units_overlay = "none",
      areal_units_timeperiod = "none",
      tus="yr",
      fraction_todrop = 1/5,
      fraction_cv = 1.0,
      fraction_good_bad = 0.8,
      nAU_min = 5,
      carstm_modelengine = "inla",  # {model engine}.{label to use to store}
      carstm_model_label = "default",
      carstm_inputs_prefilter = "sampled",
      carstm_inputs_prefilter_n = 100
    )


    if ( !exists("carstm_lookup_parameters", p))  {
        # generics using "default" carstm models and stmv solutions for spatial effects
        p$carstm_lookup_parameters = list()
        p$carstm_lookup_parameters = parameters_add_without_overwriting( p$carstm_lookup_parameters,
          bathymetry = bathymetry_parameters( project_class="stmv"  ),
          substrate = substrate_parameters(   project_class="stmv"  ),
          temperature = temperature_parameters( project_class="carstm", yrs=p$yrs ),
          speciescomposition_pca1 = speciescomposition_parameters(  project_class="carstm", variabletomodel="pca1", yrs=p$yrs  ),
          speciescomposition_pca2 = speciescomposition_parameters(  project_class="carstm", variabletomodel="pca2", yrs=p$yrs  )
        )
    }


    if ( grepl("inla", p$carstm_modelengine) ) {
      if ( !exists("carstm_model_label", p))  p$carstm_model_label = "production"
      if ( !exists("formula", p)  ) {
        p$formula = as.formula( paste(
         p$variabletomodel, ' ~ 1',
            ' + f( season, model="rw2", hyper=H$rw2, cyclic=TRUE ) ',
            ' + f( time, model="ar1",  hyper=H$ar1 ) ',
            ' + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) ',
            ' + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) ',
            ' + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) ',
            ' + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE ) ',
            ' + f( space_time, model="bym2", graph=slot(sppoly, "nb"), group=time_space, scale.model=TRUE, constr=TRUE, hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) '
        ))
      }

      if ( !exists("family", p)  )  p$family = "gaussian"
    }

    if ( p$inputdata_spatial_discretization_planar_km >= p$areal_units_resolution_km ) {
      warning( "p$inputdata_spatial_discretization_planar_km >= p$areal_units_resolution_km " )
    }
    message ("p$areal_units_resolution_km: ", p$areal_units_resolution_km)

    return(p)
  }


  if (project_class %in% c( "stmv") ) {
    p$libs = c( p$libs, project.library ( "stmv" ) )
    p$DATA = 'survey_db( p=p, DS="stmv_inputs" )'
    p$varstomodel = c()
    if (!exists("stmv_variables", p)) p$stmv_variables = list()
    if (!exists("LOCS", p$stmv_variables)) p$stmv_variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$stmv_variables)) p$stmv_variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv" )
    return(p)
  }


  if (project_class %in% c( "hybrid", "default") ) {
    p$libs = c( p$libs, project.library ( "stmv" ) )
    p$DATA = 'survey_db( p=p, DS="stmv_inputs" )'
    p$varstomodel = c()
    if (!exists("stmv_variables", p)) p$stmv_variables = list()
    if (!exists("LOCS", p$stmv_variables)) p$stmv_variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$stmv_variables)) p$stmv_variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv" )
    return(p)
  }


}
