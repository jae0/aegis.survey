survey_parameters = function( p=NULL, project_name=NULL, project_class="core", ... ) {

  # ---------------------
  # deal with additional passed parameters
  p = parameters_add(p, list(...) ) # add passed args to parameter list, priority to args


  # ---------------------
  # create/update library list
  p$libs = c( p$libs, RLibrary ( "colorspace",  "fields", "lubridate",  "lattice",
    "parallel",  "sf", "INLA", "data.table" ) )
  p$libs = c( p$libs, project.library ( "aegis", "aegis.polygons", "aegis.coastline", "aegis.bathymetry", "aegis.substrate", "aegis.temperature", "aegis.survey",  "aegis.mpa", "netmensuration", "bio.taxonomy" ) )

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
    spatial_domain = "SSE",
    spatial_domain_subareas = c( "SSE.mpa" , "snowcrab"),  # this is for bathymetry_db, not stmv
    dimensionality="space-time"
  )

  p = spatial_parameters( p=p )  # default grid and resolution

  # define focal years for modelling and interpolation

  if (!exists("year.assessment", p )) {
    if (exists("yrs", p)) {
      p$year.assessment = max(p$yrs) 
    } else {
      p$year.assessment = lubridate::year(lubridate::now())
    }
  }

  if (!exists("yrs", p ))  p$yrs = 1970:p$year.assessment
  
  p = parameters_add_without_overwriting( p, 
    timezone="America/Halifax",
    prediction_dyear = lubridate::decimal_date( lubridate::ymd("0000/Jul/01"))  # predict to a "summer" result (usually Jun-Aug) 
  )  # default unless already provided

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

#  p$discretization = discretizations(p=p$discretization)  # key for discretization levels
  

  if (project_class=="core") {
    return(p)
  }


  # default areal unit specification is for stratanl and basic groundfish survey design for all subsequenct projects below

    p = parameters_add_without_overwriting( p, 
      areal_units_xydata = "survey_db(p=p, DS='areal_units_input')",
      areal_units_type = "stratanal_polygons_pre2014", # "stmv_fields" to use ageis fields instead of carstm fields ... note variables are not the same
      areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
      areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
      areal_units_overlay = "none",
      areal_units_timeperiod = "pre2014"    # "pre2014" for older
    )

    p = parameters_add_without_overwriting( p, habitat.threshold.quantile = 0.05 ) # quantile beyond which a location is considered improper habitat


  if (project_class=="stratanal") {
    return(p)
  }

  

  if (project_class %in% c("carstm") ) {

    p$project_class = "carstm"

    if (!exists("variabletomodel", p)) stop( "The dependent variable, p$variabletomodel needs to be defined")

    # NOTE: default polygons are not the same as those of stratanal  ...
    
    p = parameters_add_without_overwriting( p,
      tus="yr",
      fraction_todrop = 1/5,
      fraction_cv = 1.0,
      fraction_good_bad = 0.8,
      nAU_min = 5,
      carstm_modelengine = "inla",  # {model engine}.{label to use to store}
      carstm_model_label = "default",  # in case it is not set, default to all data .. 
      carstm_model_label_lookup = "default",  # careful .. this is used for lookup processes with space time component
      carstm_inputs_prefilter = "sampled",
      carstm_inputs_prefilter_n = 100,
      vars_to_retain = c("totno", "totwgt", "pa", "meansize", "data_offset", "gear", "data.source", "id")
    )


    if ( !exists("carstm_prediction_surface_parameters", p))  {
        # generics :  carstm models ("default") and stmv ("default") solutions for spatial effects
        # generally better to specify exactly rather than relying upon generics
        message("\n survey_parameters :: data lookups are using generic settings, you might want to specify labels directly for more control. \n")
        p$carstm_prediction_surface_parameters = list()
        p$carstm_prediction_surface_parameters = parameters_add_without_overwriting(   p$carstm_prediction_surface_parameters,
          bathymetry = aegis.bathymetry::bathymetry_parameters( project_class="stmv", spatial_domain=p$spatial_domain, stmv_model_label="default" ),
          substrate = aegis.substrate::substrate_parameters(   project_class="stmv", spatial_domain=p$spatial_domain, stmv_model_label="default" ),
          temperature = aegis.temperature::temperature_parameters( project_class="carstm", carstm_model_label=p$carstm_model_label_lookup, yrs=p$yrs ),
          speciescomposition_pca1 = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", carstm_model_label=p$carstm_model_label_lookup, variabletomodel="pca1", yrs=p$yrs  ),
          speciescomposition_pca2 = aegis.speciescomposition::speciescomposition_parameters(  project_class="carstm", carstm_model_label=p$carstm_model_label_lookup, variabletomodel="pca2", yrs=p$yrs  )
        )
    }


    if ( grepl("inla", p$carstm_modelengine) ) {

      if ( !exists("formula", p)  ) {
        p$formula = as.formula( paste(
         p$variabletomodel, ' ~ 1',
            ' + f( cyclic, model="seasonal", scale.model=TRUE, season.length=10, hyper=H$iid  ) ',
            ' + as.factor(time) ',
            ' + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) ',
            ' + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) ',
            ' + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) ',
            ' + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE ) ',
            ' + f( space_time, model="bym2", graph=slot(sppoly, "nb"), group=time_space, scale.model=TRUE, constr=TRUE, hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) '
        ))
      }

      if ( !exists("family", p)  )  p$family = "gaussian"
    }


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
