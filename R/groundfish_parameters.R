groundfish_parameters = function( p=NULL, project_name=NULL, project_class="core", ... ) {

  # ---------------------
  # deal with additional passed parameters
  p = parameters_add(p, list(...) ) # add passed args to parameter list, priority to args


  # ---------------------
  # create/update library list
  p$libs = c( p$libs, RLibrary ( "colorspace",  "fields", "geosphere", "lubridate",  "lattice",
    "maps", "mapdata", "maptools", "parallel",  "rgdal", "rgeos",  "sp", "splancs", "GADMTools" ) )
  p$libs = c( p$libs, project.library ( "aegis", "aegis.bathymetry", "aegis.survey", "netmensuration" ) )

  p$project_name = ifelse ( !is.null(project_name), project_name, "groundfish" )

  if ( !exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project_name )
  if ( !exists("datadir", p) )   p$datadir  = file.path( p$data_root, "data" )
  if ( !exists("modeldir", p) )  p$modeldir = file.path( p$data_root, "modelled" )

  if ( !file.exists(p$datadir) ) dir.create( p$datadir, showWarnings=F, recursive=T )
  if ( !file.exists(p$modeldir) ) dir.create( p$modeldir, showWarnings=F, recursive=T )

  if (!exists("spatial_domain", p) ) p$spatial_domain = "SSE"
  if (!exists("spatial_domain_subareas", p)) p$spatial_domain_subareas = c( "snowcrab", "SSE.mpa" )
  p = spatial_parameters( p=p)  # default (= only supported resolution of 0.2 km discretization)  .. do NOT change

  if ( !exists("scanmar.dir", p) )  p$scanmar.dir = file.path( p$datadir, "nets", "Scanmar" )
  if ( !exists("marport.dir", p) )  p$marport.dir = file.path( p$datadir, "nets", "Marport" )
  if ( !exists("yrs", p) ) p$yrs=1970:lubridate::year(lubridate::now())
  if ( !exists("netmensuration.years", p) ) p$netmensuration.years = c(1990:1992, 2004:lubridate::year(lubridate::now())) # 2009 is the first year with set logs from scanmar available .. if more are found, alter this date

  p$taxa.of.interest = aegis.survey::groundfish_variablelist("catch.summary")
  p$season = "summer"
  p$taxa =  "maxresolved"
  p$nw = 10  # from temperature.r, number of intervals in a year
  p$clusters = rep("localhost", detectCores() )


  if (project_class=="core") {
    return(p)
  }

  if (project_class %in% c("stmv") ) {
    p$libs = c( p$libs, project.library ( "stmv" ) )
    return(p)
  }


  if (project_class %in% c("default", "hybrid") ) {
    p$libs = c( p$libs, project.library ( "stmv" ) )
    return(p)
  }


  if (project_class %in% c("carstm") ) {
    p$libs = c( p$libs, project.library ( "carstm" ) )

    return(p)
  }

}
