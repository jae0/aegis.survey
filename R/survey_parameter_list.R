
survey_parameter_list = function( runtype, yrs, selection, project_name="survey", 
  trawlable_units = "towdistance", areal_units_type = "stratanal_polygons_pre2014",
  carstm_model_label = "default"
) {

  if (project_name=="atlantic_cod") {
    
    p = list( label = runtype )

    if (runtype == "abundance.space_factor.time_factor" ) {
      
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space)
            + as.factor(time)
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula =  formula( 
          meansize ~ 1  
            + as.factor(space)
            + as.factor(time)
        )
      )

      return(p)

    }
  

    if (runtype == "abundance.space_factor.time_factor_full" ) {
      
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space) * as.factor(time)
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula =  formula( 
          meansize ~ 1  
            + as.factor(space) * as.factor(time)
        )
      )

      return(p)

    }
  
    if (runtype == "abundance.space_time_factor_ar1" ) {
      
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space) : as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid )  # required to stabilize missing combos
#            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="ar1", hyper=H$ar1_group))  # required to stabilize missing combos
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula = formula( 
          meansize ~ 1 
            + as.factor(space) : as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid )  # required to stabilize missing combos
#            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="ar1", hyper=H$ar1_group))  # required to stabilize missing combos

        )
      )
      return(p)

    }
  

    if (runtype == "abundance.space_time_factor" ) {
      
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space) * as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="iid", hyper=H$iid))  # required to stabilize missing combos
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula = formula( 
          meansize ~ 1 
            + as.factor(space) * as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="iid", hyper=H$iid))  # required         )
        )
      )
      return(p)

    }
  
    if (runtype == "abundance.space_iid.time_iid" ) {
      
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula = formula( 
          meansize ~ 1  
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
        )
      )
      return(p)

    }
  


    if (runtype == "abundance.space_iid.time_iid.space_time_iid" ) {
 
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
          + f(space, model="iid", hyper=H$iid)
          + f(time,  model="iid", hyper=H$iid )
          + f(space_time, model="iid", group=time_space, hyper=H$iid)
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula = formula( 
          meansize ~ 1  
          + f(space, model="iid", hyper=H$iid)
          + f(time,  model="iid", hyper=H$iid )
          + f(space_time, model="iid", group=time_space, hyper=H$iid)
        )
      )
      return(p)
  
    }


    if (runtype == "abundance.space_bym2.time_factor.space_time_bym2.envir.eco" ) {
 
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(time)  
#               + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
#            + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula = formula( 
          meansize ~ 1  
            + as.factor(time)   
#               + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
#            + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      )
      return(p)
  
    }



    if (runtype == "abundance.space_bym2.time_ar1.space_time_bym2.envir.eco" ) {
 
      p$type="abundance"
      
      p$pN = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "totno",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "poisson", 
        formula = formula( 
          totno ~ 1 + offset( data_offset )
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
#            + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      )

      p$pW = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow weight",
        yrs = yrs,
        selection = selection,
        variabletomodel = "meansize", 
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "gaussian",
        formula = formula( 
          meansize ~ 1  
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
#            + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      )
      return(p)
  
    }

    ## habitat


    if (runtype == "habitat.space_factor.time_factor" ) {
      
      p$type="habitat"
      
      p$pH = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer habitat",
        yrs = yrs,
        selection = selection,
        variabletomodel = "pa",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "binomial", 
        formula = formula( 
          pa ~ 1  
            + as.factor(space)
            + as.factor(time)
        )
      )
      return(p)

    }
  
    if (runtype == "habitat.space_iid.time_iid" ) {
      
      p$type="habitat"
      
      p$pH = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer habitat",
        yrs = yrs,
        selection = selection,
        variabletomodel = "pa",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "binomial", 
        formula = formula( 
          pa ~ 1  
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
        )
      )
      return(p)

 
    }
  


    if (runtype == "habitat.space_iid.time_iid.space_time_iid" ) {
 
      p$type="habitat"
      
      p$pH = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer habitat",
        yrs = yrs,
        selection = selection,
        variabletomodel = "pa",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "binomial", 
        formula = formula( 
          pa ~ 1 
          + f(space, model="iid", hyper=H$iid)
          + f(time,  model="iid", hyper=H$iid )
          + f(space_time, model="iid", group=time_space, hyper=H$iid)
        )
      )
      return(p)
 

    if (runtype == "habitat.space_bym2.time_factor.space_time_bym2.envir.eco" ) {
 
      p$type="habitat"
      
      p$pH = aegis.survey::survey_parameters(
        project_class = "carstm",
        project_name="atlantic_cod",  
        label ="Atlantic cod summer standardtow totno",
        yrs = yrs,
        selection = selection,
        variabletomodel = "pa",  
        areal_units_type=areal_units_type,
        trawlable_units=trawlable_units,
        carstm_model_label = carstm_model_label,
        family = "binomial", 
        formula = formula( 
          pa ~ 1 
            + as.factor(time)  
#               + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( substrate.grainsize, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      )
      return(p)
    }


    }


 }  # end atlantic cod


}