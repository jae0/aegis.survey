
# aegis.survey

This package integrates data from various surveys. Currently they include:

- ![Groundfish RV surveys](https://github.com/jae0/aegis.survey/blob/master/R/groundfish_survey_db.R); and,
- ![Snow crab surveys](https://github.com/jae0/bio.snowcrab/blob/master/R/snowcrab_db.R).

<!--
- ![Nova Scotia Zone 1 Western](./inst/doc/Nova%20Scotia%20__%20Zone%201%20-%20Western/README.md) -->

The main categories of information streams are named:
- "set" for sample events at a set-level (location, time, etc.)
- "cat" for information on captured organisms (biomass, number) by species,
- "det" for information measured from individual orgamisms (length, width, mass).

For data extraction, a list of selection criteria are required. For example, with Atlantic cod in the Martimes Region of Canada, one can specify:


```
require(aegis.survey)

spatial_domain = "SSE"  # short for Scotian Shelf Ecosystem or NAFO Div. 4VWX)
yrs = 1970:2021
groundfish_survey_species_code = 10 # cod

# basic selection criteria
selection = list(
  biologicals=list(
    spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
  ),
  survey=list(
    data.source="groundfish",
    yr = yrs,      # time frame for comparison specified above
    months=6:8,
    # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
    # ranged_data="dyear"
    settype = 1,
    gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
    strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
    polygon_enforce=TRUE
  )
)

```

The criteria are then passed along with other parameters related to spatial domain and other parameters related to the polygons of the areal units in the domain, normalizing factors (e.g., towdistance) and other treatment of the data:

```

p = survey_parameters(
  project_class = "stratanal",
  project_name="survey",
  label ="Atlantic cod summer",
  speciesname = "Atlantic_cod",
  trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
  carstm_model_label="stratnal",   # default = 1970:present, alt: 1999_present
  selection = selection,
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
  areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
  areal_units_overlay = "none",
  areal_units_timeperiod = "pre2014"    # "pre2014" for older
)

sppoly = areal_units( p=p  )  # polygons with areal designations as variable "AUID"

set = stratanal_data(
  toget="stratanal",
  selection=selection,
  trawlable_units="towdistance",
  sppoly=sppoly
)


```

To replicate the "stranal" method used by many groundfish assessments,

```
results = strata_timeseries(
  set=set,
  variable="totwgt",
  speciesname=p[["speciesname"]],
  yrs=p$yrs,
  alpha.t = 0.05 # confidence interval for t-dist assumption eg. 0.05 = 95%, 0.1 = 90%
)

plot( results$Y ~ p[["yrs"]] )

```


The folowing is the code to be run to generate some plots and can be hidden in a comment:

```

  outdir = file.path( "~", "bio", "aegis.survey", "inst", "doc")

  grDevices::png(filename = file.path(outdir, "stratanl_timeseries.png"))
    # using cubic folded root to visualize
    frp = 1/2  # folded root power
    xvals = seq(0, nx, by=20)
    yrange = folded_root(c(0.96, 1), frp)
    yticks = seq( yrange[1], yrange[2], length.out=8)
    yvals = round( folded_root( yticks, frp, inverse=TRUE) *100 , 2)  # convert to %
    graphics::plot( 0,0, type="n", xlab="", ylab="", ylim=yrange, xlim=c(0, nx), axes=FALSE)
    i = 1
    graphics::lines( folded_root(results$Y, frp) ~ p[["yrs"]], col=scales::alpha(colours[i], 0.9), lty=ltypes[i]   )
    graphics::axis( 1, at=xvals )
    graphics::axis( 2, at=yticks, labels=yvals )
    graphics::legend( "bottomleft", legend=aus, col=colours, lty=ltypes, bty="n" )
    graphics::title( xlab="Time (days)", ylab=" XXX (Folded root=1/2)" )
  grDevices::dev.off()


```

The utility of the above is that it is quite simple once the parameters are understood. Further, individual level information can e filtered resulting in a fast interface for data extraction and routine modelling.

For example, to conduct a simple Conditional Autogressive Model (BYM), one needs but specify the run paramters for "carstm" and create the model input data and bound with prediction data (M):

```

  p = survey_parameters(
    project_class = "carstm",
    project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
    label ="Atlantic cod summer",
    speciesname = "Atlantic_cod",
    trawlable_units = c( "standardtow", "towdistance", "sweptarea")[3],
    carstm_model_label=runtype,   # default = 1970:present, alt: 1999_present
    runtype=runtype,
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

```
mf = "A.S_bym2.T_ar1.ST_bym2.env.eco"

survey_params = survey_parameter_list( mf=mf, p=p )

# or specified directly:

survey_params$label = mf

      survey_params$type="abundance"

      survey_params$pN = p
      survey_params$pN$label ="Atlantic cod summer standardtow totno"
      survey_params$pN$carstm_model_label = mf
      survey_params$pN$variabletomodel = "totno"
      survey_params$pN$family = "poisson"
      survey_params$pN$formula = formula(
        totno ~ 1 + offset( data_offset )
            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid))
#            + f( gear, model="iid",  hyper=H$iid )
            + f( time, model="ar1",  hyper=H$ar1 )
            # + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   )
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
      survey_params$pW$carstm_model_label = mf
      survey_params$pW$variabletomodel = "meansize"
      survey_params$pW$family = "gaussian"
      survey_params$pW$formula = formula (
          meansize ~ 1
            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid))
            + f( time, model="ar1",  hyper=H$ar1 )
           # + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   )
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
#            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 )
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group))
      )

require (INLA)
car_results = survey_index( params=survey_params, M=M, sppoly=sppoly, redo_model=TRUE )


```

# aegis.survey

This package integrates data from various surveys. Currently they include:

- ![Groundfish RV surveys](https://github.com/jae0/aegis.survey/blob/master/R/groundfish_survey_db.R); and,
- ![Snow crab surveys](https://github.com/jae0/bio.snowcrab/blob/master/R/snowcrab_db.R).

<!--
- ![Nova Scotia Zone 1 Western](./inst/doc/Nova%20Scotia%20__%20Zone%201%20-%20Western/README.md) -->

The main categories of information streams are named:
- "set" for sample events at a set-level (location, time, etc.)
- "cat" for information on captured organisms (biomass, number) by species,
- "det" for information measured from individual orgamisms (length, width, mass).

For data extraction, a list of selection criteria are required. For example, with Atlantic cod in the Martimes Region of Canada, one can specify:


```
require(aegis.survey)

spatial_domain = "SSE"  # short for Scotian Shelf Ecosystem or NAFO Div. 4VWX)
yrs = 1970:2021
groundfish_survey_species_code = 10 # cod

# basic selection criteria
selection = list(
  biologicals=list(
    spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
  ),
  survey=list(
    data.source="groundfish",
    yr = yrs,      # time frame for comparison specified above
    months=6:8,
    # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
    # ranged_data="dyear"
    settype = 1,
    gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
    strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
    polygon_enforce=TRUE
  )
)

```

The criteria are then passed along with other parameters related to spatial domain and other parameters related to the polygons of the areal units in the domain, normalizing factors (e.g., towdistance) and other treatment of the data:

```

p = survey_parameters(
  project_class = "stratanal",
  project_name="survey",
  label ="Atlantic cod summer",
  speciesname = "Atlantic_cod",
  trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
  carstm_model_label="stratnal",   # default = 1970:present, alt: 1999_present
  selection = selection,
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
  areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
  areal_units_overlay = "none",
  areal_units_timeperiod = "pre2014"    # "pre2014" for older
)

sppoly = areal_units( p=p  )  # polygons with areal designations as variable "AUID"

set = stratanal_data(
  toget="stratanal",
  selection=selection,
  trawlable_units="towdistance",
  sppoly=sppoly
)


```

To replicate the "stranal" method used by many groundfish assessments,

```
results = strata_timeseries(
  set=set,
  variable="totwgt",
  speciesname=p[["speciesname"]],
  yrs=p$yrs,
  alpha.t = 0.05 # confidence interval for t-dist assumption eg. 0.05 = 95%, 0.1 = 90%
)

plot( results$Y ~ p[["yrs"]] )

```


The folowing is the code to be run to generate some plots and can be hidden in a comment:

```

  outdir = file.path( "~", "bio", "aegis.survey", "inst", "doc")

  grDevices::png(filename = file.path(outdir, "stratanl_timeseries.png"))
    # using cubic folded root to visualize
    frp = 1/2  # folded root power
    xvals = seq(0, nx, by=20)
    yrange = folded_root(c(0.96, 1), frp)
    yticks = seq( yrange[1], yrange[2], length.out=8)
    yvals = round( folded_root( yticks, frp, inverse=TRUE) *100 , 2)  # convert to %
    graphics::plot( 0,0, type="n", xlab="", ylab="", ylim=yrange, xlim=c(0, nx), axes=FALSE)
    i = 1
    graphics::lines( folded_root(results$Y, frp) ~ p[["yrs"]], col=scales::alpha(colours[i], 0.9), lty=ltypes[i]   )
    graphics::axis( 1, at=xvals )
    graphics::axis( 2, at=yticks, labels=yvals )
    graphics::legend( "bottomleft", legend=aus, col=colours, lty=ltypes, bty="n" )
    graphics::title( xlab="Time (days)", ylab=" XXX (Folded root=1/2)" )
  grDevices::dev.off()


```

The utility of the above is that it is quite simple once the parameters are understood. Further, individual level information can e filtered resulting in a fast interface for data extraction and routine modelling.

For example, to conduct a simple Conditional Autogressive Model (BYM), one needs but specify the run paramters for "carstm" and create the model input data and bound with prediction data (M):

```

  p = survey_parameters(
    project_class = "carstm",
    project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
    label ="Atlantic cod summer",
    speciesname = "Atlantic_cod",
    trawlable_units = c( "standardtow", "towdistance", "sweptarea")[3],
    carstm_model_label=runtype,   # default = 1970:present, alt: 1999_present
    runtype=runtype,
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

```
mf = "A.S_bym2.T_ar1.ST_bym2.env.eco"

survey_params = survey_parameter_list( mf=mf, p=p )

# or specified directly:

survey_params$label = mf

      survey_params$type="abundance"

      survey_params$pN = p
      survey_params$pN$label ="Atlantic cod summer standardtow totno"
      survey_params$pN$carstm_model_label = mf
      survey_params$pN$variabletomodel = "totno"
      survey_params$pN$family = "poisson"
      survey_params$pN$formula = formula(
        totno ~ 1 + offset( data_offset )
            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid))
#            + f( gear, model="iid",  hyper=H$iid )
            + f( time, model="ar1",  hyper=H$ar1 )
            # + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   )
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
      survey_params$pW$carstm_model_label = mf
      survey_params$pW$variabletomodel = "meansize"
      survey_params$pW$family = "gaussian"
      survey_params$pW$formula = formula (
          meansize ~ 1
            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid))
            + f( time, model="ar1",  hyper=H$ar1 )
           # + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   )
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
#            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 )
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group))
      )

require (INLA)
car_results = survey_index( params=survey_params, M=M, sppoly=sppoly, redo_model=TRUE )


```