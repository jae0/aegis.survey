
aegis_survey_lookuptable = function( aegis_project, project_class, DS=NULL, 
    variable_name=NULL, pL=NULL, space_resolution=NULL, time_resolution=NULL, tz=NULL ) {

    # select appropriate lookup table given project info
    
    if ( "bathymetry" %in% aegis_project ) {
    
        if ( is.null(pL) )  pL = bathymetry_parameters(  project_class=project_class  )
        if ( project_class %in% c("core" ) ) LUT = bathymetry_db ( p=pL, DS=DS ) 
        if ( project_class %in% c("stmv", "hybrid") ) LUT = bathymetry_db ( p=pL, DS="complete" )   
        if ( project_class %in% c("carstm" )) {
          LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
          LUT$space = LUT$space_id  
        }

        if ( project_class %in% c("core", "stmv", "hybrid") )  {
            if ( is.null(space_resolution) ) if (exists( "pres", pL)) space_resolution = pL$pres
            setDT(LUT)
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
              LUT = LUT[, setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=list(plon, plat) ]
            }
            setDF(LUT)
            attr(LUT, "space_resolution") = space_resolution
            attr(LUT, "pL") = pL
          }

      }

      if ( "substrate" %in% aegis_project ) {
        if ( is.null(pL) )  pL = substrate_parameters(  project_class=project_class  )
        if ( project_class %in% c("core" ) ) LUT = substrate_db ( p=pL, DS=DS ) 
        if ( project_class %in% c("stmv", "hybrid") ) {
          LUT = substrate_db ( p=pL, DS="complete" )   
          pB = bathymetry_parameters( spatial_domain=pL$spatial_domain, project_class=project_class  )
          BA = bathymetry_db ( p=pB, DS="baseline", varnames=c("lon", "lat")  )
          LUT = cbind( LUT, BA )
        }
        if ( project_class %in% c("carstm" )) {
          LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
          LUT$space = LUT$space_id  
        }
      
        if ( project_class %in% c("core", "stmv", "hybrid") )  {
            if ( is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
            setDT(LUT)
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
              LUT = LUT[, setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=list(plon, plat) ]
            }
            setDF(LUT)
            attr(LUT, "space_resolution") = space_resolution
            attr(LUT, "pL") = pL
          }

      }

      if ( "temperature" %in% aegis_project ) {

        if ( is.null(pL) )  pL = temperature_parameters(  project_class=project_class, year.assessment=year.assessment  )
        if ( project_class %in% c("core" ))  LUT = temperature_db ( p=pL, DS=DS )  # "aggregated_data", "bottom.all"
        if ( project_class %in% c("stmv", "hybrid") )  LUT = temperature_db ( p=pL, DS="complete" ) 
        if ( project_class %in% c("carstm" )) {
          LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
          LUT$space = LUT$space_id  
          LUT$time  = LUT$time_id  
          LUT$cyclic = LUT$cyclic_id  
        }

        if ( project_class %in% c("core", "stmv", "hybrid") )  {
            if ( is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
            if ( is.null(time_resolution))  if (exists( "tres", pL)) time_resolution =  pL$tres
            setDT(LUT)
            if ( time_resolution != pL$tres ) {
              LUT$dyear = trunc(LUT$dyear / time_resolution + 1 ) * time_resolution
            }
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
            }
            if ( time_resolution != pL$tres |  space_resolution != pL$pres ) {
              LUT = LUT[, setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=list(plon, plat, yr, dyear) ]
              LUT$timestamp = lubridate::date_decimal( LUT$yr+LUT$dyear, tz=tz )
            }
            setDF(LUT)
            attr(LUT, "space_resolution") = space_resolution
            attr(LUT, "time_resolution") = time_resolution
            attr(LUT, "pL") = pL
          }
    
      }


      if ( grepl("speciescomposition", aegis_project) ) {
        if (aegis_project == "speciescomposition_pca1") sc_vn = "pca1" 
        if (aegis_project == "speciescomposition_pca2") sc_vn = "pca2" 
        if (aegis_project == "speciescomposition_pca3") sc_vn = "pca3" 
        if (aegis_project == "speciescomposition_ca1")  sc_vn = "ca1" 
        if (aegis_project == "speciescomposition_ca2")  sc_vn = "ca2" 
        if (aegis_project == "speciescomposition_ca3")  sc_vn = "ca3" 
        if (is.null(pL) )  pL = speciescomposition_parameters(  project_class=project_class, variabletomodel=sc_vn , year.assessment=year.assessment )
        if ( project_class %in% c("core" ) ) LUT = speciescomposition_db ( p=pL, DS=DS )  
        if ( project_class %in% c( "stmv", "hybrid") )  LUT = aegis_db( p=pL, DS="complete" )   
        if ( project_class %in% c("carstm" )) {
          LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
          LUT$space = LUT$space_id  
          LUT$time  = LUT$time_id  
          LUT$cyclic = LUT$cyclic_id  
        }
          if ( project_class %in% c("core", "stmv", "hybrid") )  {
            if (is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
            if (is.null(time_resolution))  if (exists( "tres", pL)) time_resolution =  pL$tres
            setDT(LUT)
            if ( time_resolution != pL$tres ) {
              LUT$dyear = trunc(LUT$dyear / time_resolution + 1 ) * time_resolution
            }
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
            }
            if ( time_resolution != pL$tres |  space_resolution != pL$pres ) {
              LUT = LUT[, setNames(.(mean( get(sc_vn), na.rm=TRUE) ), sc_vn), by=list(plon, plat, yr, dyear) ]
              LUT$timestamp = lubridate::date_decimal( LUT$yr+LUT$dyear, tz=tz )
            }
            setDF(LUT)
            attr(LUT, "space_resolution") = space_resolution
            attr(LUT, "time_resolution") = time_resolution
            attr(LUT, "pL") = pL
          }

      }


      if ( grepl("snowcrab", aegis_project) ) {
        if (aegis_project == "snowcrab_number") sc_vn = "number" 
        if (aegis_project == "snowcrab_biomass") sc_vn = "biomass" 
        if (aegis_project == "snowcrab_meansize") sc_vn = "meansize" 
        if (aegis_project == "snowcrab_pa")  sc_vn = "pa" 

        if (is.null(pL) ) {
          if ( sc_vn == "number" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              family="poisson",
              carstm_model_label = "default",  # default is the default anything else and you are on your own
              selection = list(type = "number")
            )

          } else if ( sc_vn == "biomass" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              carstm_model_label = "default",  # default is the default anything else and you are on your own
            #   carstm_model_label = paste(   carstm_model_label,   variabletomodel, sep="_")  
              family =  "gaussian" ,  
              selection = list(type = "biomass")
            )

          } else if ( sc_vn == "meansize" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              carstm_model_label = "default",  # default is the default anything else and you are on your own
            #   carstm_model_label = paste(   carstm_model_label,   variabletomodel, sep="_")  
              family =  "gaussian" ,  
              selection = list(type = "meansize")
            )

          } else if ( sc_vn == "pa" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              carstm_model_label = "default",  # default is the default anything else and you are on your own
            #   carstm_model_label = paste(   carstm_model_label,   variabletomodel, sep="_")  
              family =  "gaussian" ,  
              selection = list(type = "presence_absence")
            )

          } else { 
            pL = p = bio.snowcrab::load.environment( year.assessment=year.assessment )
          }
        } 

        
        if ( project_class %in% c("core" ) ) LUT = snowcrab.db ( p=pL, DS=DS )  
        if ( project_class %in% c( "stmv", "hybrid") )  LUT = aegis_db( p=pL, DS="complete" )   
        if ( project_class %in% c("carstm" )) {
          LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
          LUT$space = LUT$space_id  
          LUT$time  = LUT$time_id  
          LUT$cyclic = LUT$cyclic_id  
        }

        if ( project_class %in% c("core", "stmv", "hybrid") )  {
            if ( is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
            if ( is.null(time_resolution))  if (exists( "tres", pL)) time_resolution =  pL$tres
            setDT(LUT)
            if ( time_resolution != pL$tres ) {
              LUT$dyear = trunc(LUT$dyear / time_resolution + 1 ) * time_resolution
            }
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
            }
            if ( time_resolution != pL$tres |  space_resolution != pL$pres ) {
              LUT = LUT[, setNames(.(mean( get(sc_vn), na.rm=TRUE) ), sc_vn), by=list(plon, plat, yr, dyear) ]
              LUT$timestamp = lubridate::date_decimal( LUT$yr+LUT$dyear, tz=tz )
            }
            setDF(LUT)
            attr(LUT, "space_resolution") = space_resolution
            attr(LUT, "time_resolution") = time_resolution
            attr(LUT, "pL") = pL

          }

      }

    return(LUT)
}