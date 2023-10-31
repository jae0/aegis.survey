
# NOTE ::  this is just for the cod habitat paper. 
# it is now redundant as habitat is also done in 10b_cod_carstm_tessilation.R  via a Hurdle type model
 
 
# ------------------------------------------------
# Atlantic cod comparison of CAR (ICAR/BYM) Poisson process models
# using sweptarea only on a lattice system with environmental covariates.
# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.
# This adds some differences relative to "statanal" (which uses sa in sq nautical miles, btw)

# NOTE:: unlike stratanl, we do not need to remove strata until the last /aggregation step

# the variations examined here:
  
  
  parameter_set = "habitat_paper"  # used by 10_cod_workspace to defined parameter subsets
  year.assessment = 2022


  source( file.path( code_root, "aegis.survey", "inst", "scripts", "10_cod_workspace.R" ) )

  # NOTE:  must use "1970_present" for cod
  # NOTE: requires bathymetry, substrate, groundfish, snowcrab, temperature, survey, speciescomposition
  
 
  # --------------------------------  
  # choose design

  mf = c( "stratanal_design", "tesselation" ) [2]


  if (mf == "stratanal_design") {
    ## -- stratanal design-based polygons 
    p$label ="Atlantic cod summer standardtow habitat stratanal"
 
    # limit to generic summer survey 
    p$selection$survey$months = 6:8 # "summer"
    p$selection$survey$settype = c(1,2)
      # settype:
      # 1=stratified random,
      # 2=regular survey,
      # 3=unrepresentative(net damage),
      # 4=representative sp recorded(but only part of total catch),
      # 5=comparative fishing experiment,
      # 6=tagging,
      # 7=mesh/gear studies,
      # 8=explorartory fishing,
      # 9=hydrography
    
    p = parameters_add( p, list(
      areal_units_type = "stratanal_polygons_pre2014",
      areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
      areal_units_resolution_km = 25, # km  
      areal_units_overlay = "none",
      areal_units_timeperiod = "pre2014"    # "pre2014" for older
    ) )
  
    p$formula = formula( 
        pa ~ 1 
          + f( time, model="ar1",  hyper=H$ar1 ) 
          + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
          + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
    )
    p$family = "binomial" 


    # create polygons
    redo_sppoly=FALSE
    # redo_sppoly=TRUE 
    ff = survey_db( p=p, DS="areal_units_input", redo=TRUE )
    ff = NULL
    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=redo_sppoly, areal_units_constraint="survey"  )
    plot(sppoly["AUID"], reset=FALSE)
 
    redo_survey_data = FALSE
    # redo_survey_data = TRUE
    M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, quantile_upper_limit=0.99, fn=file.path( p$modeldir, p$speciesname,  "carstm_inputs_stratanal.rdata" )  )


  }



  if (mf == "tesselation") {
   
    # tesselation-based solution
    p$label ="Atlantic cod summer standardtow habitat tesselation"
    p = parameters_add( p, list(
      areal_units_type = "tesselation",
      areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
      areal_units_resolution_km = 1, # km  
      areal_units_constraint_ntarget = 30,
      areal_units_constraint_nmin = 10,  
      areal_units_overlay = "none",
      sa_thresold_km2 = 5,
      fraction_cv = 0.65,   # ie. stop if sd/mean is less than 
      fraction_todrop = 0.1  # control frction dropped on each iteration: speed vs roughness 
    ) )

    p$formula = formula( 
        pa ~ 1 
          + f( time, model="ar1",  hyper=H$ar1 ) 
          + f( cyclic, model="seasonal", scale.model=TRUE, season.length=10, hyper=H$iid  )
          + f( gear, model="iid",  hyper=H$iid ) 
          + f( inla.group( t, method="quantile", n=13 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( inla.group( z, method="quantile", n=13 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
          + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
    )
    p$family = "binomial" 
   
    # create polygons
    redo_sppoly=FALSE
    # redo_sppoly=TRUE 
    
    invisible( survey_db( p=p, DS="areal_units_input", redo=redo_sppoly)  ) # update data file used for sppoly creation

    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=redo_sppoly, areal_units_constraint="survey", verbose=TRUE )
    plot(sppoly["AUID"], reset=FALSE)

    # 1025 areal units.

    redo_survey_data = FALSE
    # redo_survey_data = TRUE
    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=FALSE, areal_units_constraint="survey", verbose=TRUE )
    M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, quantile_upper_limit=0.99, fn=file.path( p$modeldir, p$speciesname,  "carstm_inputs_tesselation.rdata" )  )


  }

  # basic param set definitions END
  # --------------------------------  



  # --------------------------------  
  # analysis and output
  

  # Figure 1. average bottom temperature of prediction surface (1 July)
  Mp  = data.table( M[which(M$tag=="predictions" & is.finite(M$t) ),] )
  o = Mp[, list(mean=mean(t), lb025=quantile(t, probs=0.025), ub975=quantile(t, probs=0.975)), by=yr]
  trange = range( o[,c("mean","lb025", "ub975")]) * c(0.9, 1.1)
  plot( mean ~ yr, o, type="b", ylim = trange, ylab="Bottom temperature, bottom deg C", xlab="year", lwd=1.5)
  lines (lb025~yr, o, col="darkgray", lty="dashed")
  lines (ub975~yr, o, col="darkgray", lty="dashed")
  
 

  p$space_name = sppoly$AUID 
  p$space_id = 1:nrow(sppoly)

  p$time_name = as.character(p$yrs)
  p$time_id =  1:p$ny

  p$cyclic_name = as.character(p$cyclic_levels)
  p$cyclic_id = 1:p$nw


  res = carstm_model( p=p, data=M, sppoly=sppoly, redo_fit=TRUE, 
    posterior_simulations_to_retain="predictions", 
    control.family=list( control.link=list(model="logit") ), ## this is the default for binomial, just here to show wher to use it
    theta = c(0.773, 3.539, 1.854, 0.849, 1.791, 0.699, -0.676, 4.617, -0.314, 3.963, 2.988), # 2021 solution
    # toget = c("summary", "fixed_effects", "predictions" ), 
    # toget = c("summary", "fixed_effects", "random_other", "predictions"), 
    num.threads="6:2"  # adjust for your machine
  ) 
 
 

  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )
  names(res[["random"]])


  fit = carstm_model( p=p, DS="carstm_modelled_fit",  sppoly = sppoly )  # extract currently saved model fit
  names(  fit$summary.random)
  i = 1
  plot(inverse.logit(fit$summary.random[[i]]$mean) ~ fit$summary.random[[i]]$ID, ylab="probability", xlab=names(fit$summary.random)[i])

  i = "time"  
  i = "cyclic" 
  i = "gear" 
  i = "inla.group(t, method = \"quantile\", n = 13)"
  i = "inla.group(z, method = \"quantile\", n = 13)" "space"

  dta = res$random[[i]]

  plot( mean ~ ID , dta, type="b", ylim=c(0,1))
  lines( quant0.025 ~ ID, dta, lty="dashed")
  lines( quant0.975 ~ ID, dta, lty="dashed")


  dta = fit$summary.random[[i]] 
  plot( inverse.logit(dta$mean) ~ dta$ID,  type="b", ylim=c(0,1))
  lines( inverse.logit(dta[,4]) ~ dta$ID,  lty="dashed")
  lines( inverse.logit(dta[,6]) ~ dta$ID,  lty="dashed")


  # Figure 2. Habitat vs time - marginal
  b0 = res$summary$fixed_effects["(Intercept)", "mean"]
  ts = res$random$time
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] 
  plot( mean ~ ID, ts, type="b", ylim=c(0,1) , lwd=1.5, xlab="year")
  lines( quant0.025 ~ ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ ID, ts, col="gray", lty="dashed")


  # Figure 2. Habitat vs season
  b0 = res$summary$fixed_effects["(Intercept)", "mean"]
  ts = res$random$cyclic
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] 
  plot( mean ~ID, ts, type="b", ylim=c(0,1), lwd=1.5, xlab="fractional year")
  lines( quant0.025 ~ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ID, ts, col="gray", lty="dashed")







  params = list()
  vars_to_copy = c(  "space", "time", "dyears" )  # needed for plotting 
  for ( vn in vars_to_copy ) params[[vn]] = res[[vn]]

  pa = res[["sims"]][["predictions"]]
  # pa = inverse.logit(pa)
  pa[!is.finite(pa)] = NA
 
  # create for mapping .. in probability
  oo = simplify2array(pa)
  params[["predictions"]] = res[[ "predictions" ]] * NA
  params[["predictions"]][,,1]  = apply( oo, c(1,2), mean, na.rm=TRUE ) 
  params[["predictions"]][,,2]  = apply( oo, c(1,2), sd, na.rm=TRUE ) 
  params[["predictions"]][,,3]  = apply( oo, c(1,2), quantile, probs=0.025, na.rm=TRUE ) 
  params[["predictions"]][,,4]  = apply( oo, c(1,2), median, na.rm=TRUE )
  params[["predictions"]][,,5]  = apply( oo, c(1,2), quantile, probs=0.975, na.rm=TRUE ) 
  attr( params[["predictions"]], "units") = "probability"
  oo = NULL

  # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
  au_sa="au_sa_km2"
  sa = sppoly[[au_sa]] 
  attributes( sa ) = NULL

  sims = colSums( pa * sa / sum(  sa ), na.rm=TRUE ) 

  oo = simplify2array(sims)
  params[["habitat"]] = data.frame( cbind(
    mean = apply( oo, 1, mean, na.rm=TRUE ), 
    sd   = apply( oo, 1, sd , na.rm=TRUE), 
    median = apply( oo, 1, median, na.rm=TRUE ), 
    q025 = apply( oo, 1, quantile, probs=0.025, na.rm=TRUE ),
    q975 = apply( oo, 1, quantile, probs=0.975, na.rm=TRUE ) 
  ))
  attr( params[["habitat"]], "units") = "probability"
  oo = NULL

  RES[[mf]] = params
    
  save(RES, file=results_file, compress=TRUE)   # load(results_file)     # store some of the aggregate timeseries in this list

  # to reload:
  fit = carstm_model( p=RES[[mf]]$pH, DS="carstm_modelled_fit", sppoly=sppoly )
  res = carstm_model( p=RES[[mf]]$pH, DS="carstm_modelled_summary", sppoly=sppoly )


  # analysis and output END
  # --------------------------------  



  # --------------------------------  
  # maps and plots


  # Figure timeseries of habitat -- predicted average timeseries aggregated from posterior samples
  vn = "habitat"
  ppa = RES[[mf]][[vn]] # aggregate summaries 

  units = attr( ppa, "units")
  plot( ppa[["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="slategray", type="b", main=mf, ylab="Probability", xlab="Year", ylim=c(0.2,0.8), pch=19  )
  lines( ppa[["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="slategray" )
  lines( ppa[["q025"]] ~ RES$yr, lty="dotted", lwd=1, col="slategray"  )
  lines( ppa[["q975"]] ~ RES$yr, lty="dotted", lwd=1, col="slategray"  )
  abline( h=0.5, lty="dashed",  col="slategray" )
 

  units = attr( ppa, "units")
  plot( ppa[["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="slategray", type="b", main=mf, ylab="Probability", xlab="Year", ylim=c(0.2,0.8), pch=19, axes=FALSE  )
  lines( ppa[["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="slategray" )
  lines( ppa[["q025"]] ~ RES$yr, lty="dotted", lwd=1, col="slategray"  )
  lines( ppa[["q975"]] ~ RES$yr, lty="dotted", lwd=1, col="slategray"  )
  abline( h=0.5, lty="dashed",  col="slategray" )
  axis(1)
  axis(2)

  # Figure 2. Habitat vs time - marginal
  b0 = res$summary$fixed_effects["(Intercept)", "mean"]
  ts = res$random$time
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] 
  plot( mean ~ ID, ts, type="b", ylim=c(0,1) , lwd=1.5, xlab="year", col="red")
  lines( quant0.025 ~ ID, ts,  lty="dashed", col="orange")
  lines( quant0.975 ~ ID, ts,  lty="dashed", col="orange")
  abline( h=0.5, lty="dashed",  col="slategray" )
  abline( v=1992, lty="dashed",  col="slategray")
 

  i = "inla.group(t, method = \"quantile\", n = 13)"
  dta = fit$summary.random[[i]] 
  plot( inverse.logit(dta$mean) ~ dta$ID,  type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0.2, 0.8), 
    xlab="Bottom temperature (degrees Celsius)", ylab="Probability" )
  lines( inverse.logit(dta[,4]) ~ dta$ID,  lty="dotted", col="slategray")
  lines( inverse.logit(dta[,6]) ~ dta$ID,  lty="dotted", col="slategray")
  abline( h=0.5, lty="dashed",  col="slategray" )


  i = "inla.group(z, method = \"quantile\", n = 13)"
  dta = fit$summary.random[[i]] 
  plot( inverse.logit(dta$mean) ~ dta$ID,  type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0, 0.8),
    xlab="Depth (m)", ylab="Probability" )
  lines( inverse.logit(dta[,4]) ~ dta$ID,  lty="dotted", col="slategray")
  lines( inverse.logit(dta[,6]) ~ dta$ID,  lty="dotted", col="slategray")
  abline( h=0.5, lty="dashed",  col="slategray" )


 

  i = "time"
  dta = fit$summary.random[[i]] 
  plot( inverse.logit(dta$mean) ~ dta$ID,  type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0, 1),
    xlab="Time", ylab="Probability" )
  lines( inverse.logit(dta[,4]) ~ dta$ID,  lty="dotted", col="slategray")
  lines( inverse.logit(dta[,6]) ~ dta$ID,  lty="dotted", col="slategray")
  abline( h=0.5, lty="dashed",  col="slategray" )


 


  i = "cyclic"
  dta = fit$summary.random[[i]] 
  dta$ID = dta$ID /10 
  plot( inverse.logit(dta$mean) ~ dta$ID,  type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0, 1), xlim=c(0.1,1),
    xlab="Season", ylab="Probability" )
  lines( inverse.logit(dta[,4]) ~ dta$ID,  lty="dotted", col="slategray")
  lines( inverse.logit(dta[,6]) ~ dta$ID,  lty="dotted", col="slategray")
  abline( h=0.5, lty="dashed",  col="slategray" )



  i = "gear"
  dta = fit$summary.random[[i]] 
 
  x = dta$ID
  y = inverse.logit(dta$mean)
  y0 = inverse.logit(dta[,4])
  y1 = inverse.logit(dta[,6])

  gears = c("Western IIA", "Yankee #36", "US 4seam 3beam",  "Engle", "Campelen 1800", "Nephrops" )       

  plot( y ~ x,  type="p", col="slategray", pch=19,   ylim=c(0, 1), axes=FALSE,
    ylab="Probability", xlab="" )
  arrows(x0=x, y0=y0, x1=x, y1=y1, code=3, angle=90, length=0.1)
  axis(2 )
  par(las=2, mgp=c(3,0,-4), mai=c(2,1,1,1) )
  axis(1, at=x, labels=gears, lty="blank", lwd=0.2 )
  abline( h=0.5, lty="dashed",  col="slategray" )



 


  bb = apply( pa , c(2,3), function(u) u*sa )
  params[["habitat_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
  attr( params[["habitat_simulations"]], "units") = "probability"   
  hist(  RES[[mf]][["habitat_simulations"]][1,] )  # posterior distributions






  # maps

  # map it
  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7
  background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 

  plot_crs = st_crs(sppoly)
  
  require(tmap)

    additional_features =  
      tm_shape( st_transform( maritimes_groundfish_strata(areal_units_timeperiod = p$areal_units_timeperiod), plot_crs ) ) + 
        tm_borders( col="darkgrey", alpha=0.9, lwd=1.5)   + 
      tm_shape( aegis.bathymetry::isobath_db(  depths=c( seq(0, 500, by=100), 1000), project_to=plot_crs  ), projection=plot_crs ) +
        tm_lines( col="slategray", alpha=0.5, lwd=0.5) 
      #   +
      # tm_shape( aegis.coastline::coastline_db( DS="eastcoast_gadm", project_to=plot_crs ), projection=plot_crs ) +
      #   tm_polygons( col="lightgray", alpha=0.5 , border.alpha =0.5)

    (additional_features)

    ---> convert above to ggplot2:: TODO



  fn_root = "Predicted_habitat_probability"
  title = "Predicted habitat probability"

  plot_crs = pci$aegis_proj4string_planar_km
  # managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )
  # time_match = "2020"
      

  vn = "predictions"
  brks = pretty(  quantile( RES[[mf]][[vn]], probs=c(0.025,0.975), na.rm=TRUE )  )

  outputdir = file.path( p$modeldir, p$carstm_model_label, "predicted.habitat" )
  
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )


  # spatial only

  vn=c( "random", "space", "iid" )
  vn=c( "random", "space", "bym2" )
  vn=c( "random", "space", "combined" )

  tmatch = ""

  fn = file.path( outputdir, paste(fn_root, paste0(vn, collapse=" "),  "png", sep=".") )

  carstm_map(  res=res, vn=vn, tmatch=tmatch, 
      sppoly = sppoly, 
      colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
      additional_features=additional_features,
      outfilename=fn,
      title ="Probability"
  )


  # space_time

  # attr( RES[[mf]][["predictions"]], "units")
  vn=c( "random", "space_time", "iid" )
  vn=c( "random", "space_time", "bym2" )
  vn=c( "random", "space_time", "combined" )
  vn="predictions" 
  
  tmatch="2000"

  for (y in res$time_name ){
    time_match = as.character(y) 
    
    fn = file.path( outputdir, paste(fn_root, paste0(vn, collapse="_"), time_match, "png", sep=".") )
    carstm_map(  res=res, vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
      additional_features=additional_features,
      title= paste("habitat", time_match) , #paste(fn_root, time_match, sep="_"),  
      outfilename=fn
    )
  }



  #preds from simulations
  vn="predictions" 
  
  for (y in res$time_name ){
    time_match = as.character(y) 
    
    fn = file.path( outputdir, paste(fn_root, paste0(vn, collapse="_"), time_match, "png", sep=".") )
    carstm_map(  res=RES[[mf]], vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
      additional_features=additional_features,
      title= paste("habitat", time_match) , #paste(fn_root, time_match, sep="_"),  
      outfilename=fn
    )
  }


  # temperature timeseries
  require(aegis.temperature)


  yrs=1970:year.assessment

  polys = st_sf( st_union( st_geometry( sppoly ) ) )
  polys$AUID = 0

  
  
  pt = temperature_parameters( 
      project_class="carstm", 
      yrs=yrs, 
      carstm_model_label="1970_present"
  ) 

  tss = aegis_lookup(  
    parameters=list( temperature = pt ), 
    LOCS=expand.grid( AUID=polys$AUID, timestamp=pt$yrs + 0.75 ), LOCS_AU=polys, 
    project_class="carstm", output_format="areal_units", 
    variable_name=list( "predictions" ), statvars=c("mean", "sd"), space_resolution=pt$pres, year.assessment=pt$year.assessment,
    tz="America/Halifax",
    returntype = "data.table"
  ) 

  yran = c( 2, 11 )
  nrep = 10000
  nd = length(yrs)
  y = matrix( NA, nrow=nrep, ncol=nd )
  ncolors = 600
  prs = seq( from=0.025, to=0.975, length.out=ncolors*2 )  # 95% CI
  alpha = seq( from=0.85, to=0.95, length.out=ncolors )

  cols_plot = c("Blues", "Purples",  "Reds", "Dark Mint", "Greens", "Light Grays")
  ncc = length(cols_plot)
  
  cols = matrix( NA, ncol=ncc, nrow=ncolors*2 )
  for (i in 1:ncc ) {
    cols[,i] = c( rev(hcl.colors(ncolors, cols_plot[i], alpha=rev(alpha))), hcl.colors(ncolors, cols_plot[i], alpha=(alpha) ) ) 
  }

  plot( 0, 0, type="n", ylim=yran, xlim=range(yrs), axes=FALSE, xlab="Year", ylab="Bottom temperature (Celsius)" ) #change xlim to yrs0 to remove 3 yr projection

    for (i in 1:nd) y[,i] = rnorm( nrep, mean=tss$predictions_mean[i], sd=tss$predictions_sd[i])
    Bq =  apply( y, 2, quantile, probs=prs, na.rm=T  )
    for ( k in 1:length(prs) ) lines ( yrs, Bq[k,], lwd=3, col=cols[k,6] )
    lines ( yrs, Bq[ncolors,], lwd=3, col=cols[ncolors, 6]  ) # median
    axis(2)
  axis(1 )
  
 
  # --------------------------------  
  # maps and plots END
     



  # --------------------------------  
  # other variations


# Figure XX 3D plot of habitat vs temperature vs depth  
 

b0 = res$summary$fixed_effects["(Intercept)", "mean"]

pt = res$random$'inla.group(t, method = "quantile", n = 13)'
pz = res$random$'inla.group(z, method = "quantile", n = 13)'
 


ptz = expand.grid( temp=pt$ID, depth =pz$ID ) 

ptz = merge( ptz, pt, by.x="temp", by.y="ID", all.x=TRUE, all.y=FALSE )
ptz = merge( ptz, pz, by.x="depth", by.y="ID", all.x=TRUE, all.y=FALSE, suffixes=c(".temp", ".depth") )

ptz$prob = ptz$mean.temp * ptz$mean.depth 
ptz$depth = - ptz$depth

require(MBA)

nx = 100
ny = 100

layout( matrix(1:4, ncol=2, byrow=TRUE ))
Z = mba.surf(ptz[, c("temp", "depth", "prob")], no.X=nx, no.Y=ny, extend=TRUE) $xyz.est

image(Z, col = rev(gray.colors(30, gamma=1.75)))
contour(Z, add = TRUE, drawlabels = TRUE, lty="dotted")
  
plot( -ID ~ mean, pz, type="b")

plot( mean ~ ID, pt, type="b")


library(plot3D)

xx = t(t(rep(1, ny))) %*% Z$x
yy = t( t(t(rep(1, nx))) %*% Z$y )

surf3D( x=xx, y=yy, z=Z$z, colkey = TRUE,  
       box = TRUE, bty = "b", phi = 20, theta = 145, contour=TRUE, ticktype = "detailed") 


     
     
-- zero-inflated binomial
-- negative binomial ?
-- split by size (as covariate) and sex


### end


Fixed effects:
             mean    sd 0.025quant 0.5quant 0.975quant  mode kld
(Intercept) 0.729 0.835     -0.978    0.747      2.371 0.795   0

Random effects:
  Name	  Model
    time AR1 model
   cyclic RW2 model
   gear IID model
   inla.group(t, method = "quantile", n = 13) RW2 model
   inla.group(z, method = "quantile", n = 13) RW2 model
   space BYM2 model
   space_time BYM2 model

Model hyperparameters:
                                                           mean    sd 0.025quant 0.5quant 0.975quant  mode
Precision for time                                        2.199 0.880      0.906    2.062      4.304 1.794
Rho for time                                              0.945 0.024      0.887    0.949      0.981 0.957
Precision for cyclic                                     10.103 8.524      2.401    7.605     32.763 4.878
Precision for gear                                        2.644 2.004      0.483    2.127      7.909 1.264
Precision for inla.group(t, method = "quantile", n = 13)  7.771 5.083      2.226    6.440     21.173 4.608
Precision for inla.group(z, method = "quantile", n = 13)  2.326 1.807      0.419    1.852      7.075 1.089
Precision for space                                       0.527 0.057      0.432    0.521      0.656 0.505
Phi for space                                             0.989 0.007      0.970    0.991      0.998 0.994
Precision for space_time                                  0.715 0.079      0.563    0.714      0.872 0.719
Phi for space_time                                        0.982 0.012      0.952    0.985      0.997 0.991
GroupRho for space_time                                   0.909 0.016      0.877    0.909      0.940 0.909

Deviance Information Criterion (DIC) ...............: 25510.73
Deviance Information Criterion (DIC, saturated) ....: 106901.67
Effective number of parameters .....................: 1658.90

Watanabe-Akaike information criterion (WAIC) ...: 25434.74
Effective number of parameters .................: 1472.26

Marginal log-Likelihood:  -1141.71 
Posterior summaries for the linear predictor and the fitted values are computed
(Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')


$fixed_effects
                   mean          sd  quant0.025    quant0.5  quant0.975   parameter
(Intercept) 0.654464715 0.167125766 0.274041987 0.678336145 0.914067629 (Intercept)

$random_effects
                                                     mean            sd  quant0.025    quant0.5  quant0.975
SD time                                       0.714835151 0.14452594515 0.483034961 0.696046615 1.047932563
SD cyclic                                     0.373905248 0.12061142173 0.175642553 0.362479958 0.641369826
SD gear                                       0.740360979 0.27783853101 0.357064849 0.685181249 1.431460089
SD inla.group(t, method = "quantile", n = 13) 0.406325879 0.11538559108 0.218217561 0.393915186 0.667154488
SD inla.group(z, method = "quantile", n = 13) 0.793512013 0.30132877508 0.377062255 0.734272803 1.540128758
SD space                                      1.382964318 0.07311068886 1.235213264 1.385268872 1.520898744
SD space_time                                 1.188400262 0.06640993281 1.071841015 1.182958558 1.331733592
Rho for time                                  0.944820325 0.02437687658 0.886948406 0.948377039 0.980350916
GroupRho for space_time                       0.909349255 0.01638869404 0.876758925 0.909392291 0.940228027
Phi for space                                 0.989074889 0.00748414658 0.969654713 0.990833583 0.997561253
Phi for space_time                            0.982315562 0.01197440909 0.952050126 0.984901314 0.996778392
                                                                                  parameter
SD time                                                                             SD time
SD cyclic                                                                         SD cyclic
SD gear                                                                             SD gear
SD inla.group(t, method = "quantile", n = 13) SD inla.group(t, method = "quantile", n = 13)
SD inla.group(z, method = "quantile", n = 13) SD inla.group(z, method = "quantile", n = 13)
SD space                                                                           SD space
SD space_time                                                                 SD space_time
Rho for time                                                                   Rho for time
GroupRho for space_time                                             GroupRho for space_time
Phi for space                                                                 Phi for space
Phi for space_time                                                       Phi for space_time
