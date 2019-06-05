groundfish_parameters = function( p=NULL, project.name=NULL, project.mode="default", ... ) {

  # ---------------------
  # deal with additional passed parameters
  if ( is.null(p) ) p=list()
  p_add = list(...)
  if (length(p_add) > 0 ) p = c(p, p_add)
  i = which(duplicated(names(p), fromLast = TRUE ))
  if ( length(i) > 0 ) p = p[-i] # give any passed parameters a higher priority, overwriting pre-existing variable


  # ---------------------
  # create/update library list
  p$libs = c( p$libs, RLibrary ( "colorspace",  "fields", "geosphere", "lubridate",  "lattice",
    "maps", "mapdata", "maptools", "parallel",  "rgdal", "rgeos",  "sp", "splancs", "GADMTools" ) )
  p$libs = c( p$libs, project.library ( "aegis", "aegis.bathymetry", "aegis.survey", "netmensuration" ) )

  p$project.name = ifelse ( !is.null(project.name), project.name, "groundfish" )

  if ( !exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
  if ( !exists("datadir", p) )   p$datadir  = file.path( p$data_root, "data" )
  if ( !exists("modeldir", p) )  p$modeldir = file.path( p$data_root, "modelled" )

  if ( !file.exists(p$datadir) ) dir.create( p$datadir, showWarnings=F, recursive=T )
  if ( !file.exists(p$modeldir) ) dir.create( p$modeldir, showWarnings=F, recursive=T )

  if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
  if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )
  p = spatial_parameters( p=p)  # default (= only supported resolution of 0.2 km discretization)  .. do NOT change

  if ( !exists("scanmar.dir", p) )  p$scanmar.dir = file.path( p$datadir, "nets", "Scanmar" )
  if ( !exists("marport.dir", p) )  p$marport.dir = file.path( p$datadir, "nets", "Marport" )
  if ( !exists("yrs", p) ) p$yrs=1970:lubridate::year(lubridate::now())
  if ( !exists("netmensuration.years", p) ) p$netmensuration.years = c(1990:1992, 2004:lubridate::year(lubridate::now())) # 2009 is the first year with set logs from scanmar available .. if more are found, alter this date

  p$taxa.of.interest = aegis.survey::groundfish.variablelist("catch.summary")
  p$season = "summer"
  p$taxa =  "maxresolved"
  p$nw = 10  # from temperature.r, number of intervals in a year
  p$clusters = rep("localhost", detectCores() )


  if (project.mode=="default") {
    return(p)
  }

  if (project.mode=="stmv") {
    p$libs = c( p$libs, project.library ( "stmv" ) )
    return(p)
  }


  if (project.mode=="carstm") {
    p$libs = c( p$libs, project.library ( "carstm" ) )

    return(p)
  }

}
