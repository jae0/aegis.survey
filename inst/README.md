
# Using aegis.survey
 
Once the parameters are understood, the internal filters can be used to quickly interface with survey data which is useful for routine modelling.

For example, to conduct a simple Conditional Autogressive Model (BYM) on Atlantic cod, one needs but specify the run parameters for ["carstm"](https://github.com/jae0/carstm/) to the selection criteria. 

Then one can create the model input data and output prediction locations, both concatentated into data frame M:

```r 
  p = survey_parameters(
    project_class = "carstm",
    project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
    label ="Atlantic cod summer",
    speciesname = "Atlantic_cod",
    trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],
    carstm_model_label="default",   # default = 1970:present, alt: 1999_present
    yrs = yrs,
    selection = selection,
    variabletomodel = "totno",
    vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
    areal_units_type = "stratanal_polygons_pre2014",
    areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
    areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")
    areal_units_overlay = "none",
    areal_units_timeperiod = "pre2014",    # "pre2014" for older
  )


  M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, qupper=0.99 )


```

The specific form of the model can be specified directly or stored and read in from file:

```r

model_label = "A.S_bym2.T_ar1.ST_bym2.env.eco"

survey_params = survey_parameter_list( model_label=model_label, p=p )

# or specified directly:

survey_params$label = model_label

      survey_params$type="abundance"

      survey_params$pN = p
      survey_params$pN$label ="Atlantic cod summer standardtow totno"
      survey_params$pN$carstm_model_label = model_label
      survey_params$pN$variabletomodel = "totno"
      survey_params$pN$family = "poisson"
      survey_params$pN$formula = formula(
        totno ~ 1 + offset( data_offset )  # CARSTM does log-transformation internally 
            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid))
#            + f( gear, model="iid",  hyper=H$iid )
            + f( time, model="ar1",  hyper=H$ar1 )
#          ' + f( S, model="seasonal", scale.model=TRUE, season.length=10, hyper=H$iid  )',
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
#            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 )
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group))
      )


      survey_params$pW = p
      survey_params$pW$label ="Atlantic cod summer standardtow meansize"
      survey_params$pW$carstm_model_label = model_label
      survey_params$pW$variabletomodel = "meansize"
      survey_params$pW$family = "gaussian"
      survey_params$pW$formula = formula (
          meansize ~ 1
            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid))
            + f( time, model="ar1",  hyper=H$ar1 )
#           + f( cyclic, model="seasonal", scale.model=TRUE, season.length=10, hyper=H$iid  )
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
#            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hsyper=H$rw2)
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 )
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group))
      )

require (INLA)
car_results = survey_index( params=survey_params, M=M, sppoly=sppoly, redo_model=TRUE )


```
 