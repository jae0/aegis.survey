
survey_parameter_list = function( runtype, yrs, selection, project_name="atlantic_cod" ) {

  if (project_name=="atlantic_cod") {
    
    p = list( label = runtype )

    if (runtype == "abundance.space_iid.year_iid" ) {
      
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        vars_to_retain = c("totwgt"),  # to compute mean size
        areal_units_type="stratanal_polygons_pre2014",
        trawlable_units="sweptarea",
        carstm_model_label = runtype,
        family = "poisson",
        formula = formula( totno ~ 1 + offset( log( data_offset) )
              + f(strata, model="iid", group=year, hyper=H$iid)
              + f(year, model="iid", hyper=H$iid )
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        vars_to_retain = c("totwgt"),  # to compute mean size
        areal_units_type="stratanal_polygons_pre2014",
        trawlable_units="sweptarea",
        carstm_model_label = runtype,
        family = "gaussian",
        formula = formula( meansize ~ 1  
              + f(strata, model="iid", group=year, hyper=H$iid)
              + f(year, model="iid", hyper=H$iid )
        )
      )

    }
  

    if (runtype == "abundance.space_iid.year_iid.envir" ) {

      p$type="abundance"

      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        vars_to_retain = c("totwgt"),  # to compute mean size
        areal_units_type="stratanal_polygons_pre2014",
        trawlable_units="sweptarea",
        carstm_model_label = runtype,
        family = "poisson",
        formula = formula( totno ~ 1 + offset( log( data_offset) )
              + f(strata, model="iid", group=year, hyper=H$iid)
              + f(year, model="iid", hyper=H$iid )
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        vars_to_retain = c("totwgt"),  # to compute mean size
        areal_units_type="stratanal_polygons_pre2014",
        trawlable_units="sweptarea",
        carstm_model_label = runtype,
        family = "gaussian",
        formula = formula( meansize ~ 1  
              + f(strata, model="iid", group=year, hyper=H$iid)
              + f(year, model="iid", hyper=H$iid )
        )
      )

    }



    return(p)

  }



}