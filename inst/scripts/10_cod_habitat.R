
 
# ------------------------------------------------
# Atlantic cod comparison of CAR (ICAR/BYM) Poisson process models
# using sweptarea only on a lattice system with environmental covariates.
# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.
# This adds some differences relative to "statanal" (which uses sa in sq nautical miles, btw)

# NOTE:: unlike stratanl, we do not need to remove strata until the last /aggregation step

# the variations examined here:

# ----------------------------------------------
# define model_forms: params are stored in  survey_parameter_list()

# adding settype 2 and 5 (comparative tows, and generic surveys) 


# set up the run parameters
  require(aegis.survey)

  spatial_domain = "SSE"
  
  yrs = 1970:2021
  runtype = "1970_present"
 
  groundfish_survey_species_code = 10 # cod


  # basic selection criteria
  ## note difference from stratanal: all gears kept, and all areas kept
  selection = list(
    biologicals=list(
      spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
    ),
    survey=list(
      data.source = c("groundfish", "snowcrab"),
      yr = yrs,      # time frame for comparison specified above
      # months=6:11,   #"summer"
      # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
      # ranged_data="dyear"
      settype = c(1,2,5,8 ),
      # gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
      # strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
      polygon_enforce=TRUE
    )
  )

  p = survey_parameters(
    project_class = "carstm",
    project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
    label ="Atlantic cod summer",
    speciesname = "Atlantic_cod",
    trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  
    # carstm_model_label = "full_model"   ## <<----- 
    carstm_model_label=runtype,   # default = 1970:present, alt: 1999_present 
    runtype=runtype,
    yrs = yrs,
    selection = selection,
    type="habitat",
    variabletomodel = "pa",  
    vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id")  # to compute mean size, etc
  )

  results_file = file.path( p$modeldir, p$speciesname , "RES_habitat_comparisons.rdata" )

  RES= list( yr = yrs )


  # basic param set definitions END
  # --------------------------------  
 




  # --------------------------------  
  # choose design

  mf = c( "stranal_design", "tesselation" ) [2]


  if (mf == "stranal_design") {
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
    ff = survey_db( p=p, DS="areal_units_input", redo=TRUE, fn=file.path( p$modeldir, p$speciesname , mf, "carstm_inputs.rdata" )  )
    ff = NULL
    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=redo_sppoly, areal_units_constraint="survey"   )  # generic
    plot(sppoly["AUID"], reset=FALSE)
 

    redo_survey_data = FALSE
    # redo_survey_data = TRUE
    M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, quantile_upper_limit=0.99, 
      fn=file.path( p$modeldir, p$speciesname , mf, "carstm_inputs_stratanal.rdata" )  )

    M$log.substrate.grainsize = log( M$substrate.grainsize )

  }



  if (mf == "tesselation") {
   
    # tesselation-based solution
    p$label ="Atlantic cod summer standardtow habitat tesselation"
    p = parameters_add( p, list(
      areal_units_type = "tesselation",
      areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
      areal_units_resolution_km = 1, # km  
      areal_units_constraint_ntarget = 50,
      areal_units_constraint_nmin = 10,  
      areal_units_overlay = "none",
      sa_thresold_km2 = 5,
      fraction_cv = 0.65,   # ie. stop if sd/mean is less than 
      fraction_todrop = 0.1  # control frction dropped on each iteration: speed vs roughness 
    ) )

    p$formula = formula( 
        pa ~ 1 
          + f( time, model="ar1",  hyper=H$ar1 ) 
          + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic=TRUE, values=cyclic_values ) 
          + f( gear, model="iid",  hyper=H$iid ) 
          + f( inla.group( t, method="quantile", n=17 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( inla.group( z, method="quantile", n=17 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( inla.group( log.substrate.grainsize, method="quantile", n=17 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
          + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
          + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
    )
    p$family = "binomial" 
   
    # create polygons
    redo_sppoly=FALSE
    # redo_sppoly=TRUE 
    
    invisible( survey_db( p=p, DS="areal_units_input", redo=redo_sppoly, fn=file.path( p$modeldir, p$speciesname , mf, "carstm_inputs.rdata" ) ) ) # update data file used for sppoly creation

    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=redo_sppoly, areal_units_constraint="survey", verbose=TRUE, areal_units_fn_full=file.path( p$modeldir, p$speciesname , mf, "sppoly.rdata" )  )
    plot(sppoly["AUID"], reset=FALSE)

    # 1025 areal units.


    redo_survey_data = FALSE
    # redo_survey_data = TRUE
    M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, quantile_upper_limit=0.99, 
      fn=file.path( p$modeldir, p$speciesname , mf, "carstm_inputs_tesselation.rdata" )  )

    # dropping the gears with less than 300 stations
    table( M$gear)
    xtabs( ~ yr+gear, M[M$tag=="observations"])
    #     1     2     3     4     5     6 
    # 68086  1684   346    79   153  8369 
    levels( M$gear)
    # [1] "Western IIA trawl"               "Yankee #36 otter trawl"          "US 4 seam 3 bridle survey trawl"
    # [4] "Engle (bottom) trawl"            "Campelen 1800 survey trawl"      "Nephrops trawl"                 

    M = M[ M$gear %in% c(1, 2, 6 ),  ]
    M$log.substrate.grainsize = log(M$substrate.grainsize )


    if (0) {
        # testing estimates if one uses each year as the distributional basis .. minimal change in results/interpretation
        M$log.z = log(M$z )

        M$pa0 = M$pa

        M$pa = NA
        p$habitat.threshold.quantile = 0.05

        for (y in unique(M$yr)) {
          yy = which( M$tag=="observations" & M$yr==y)
          if (length(yy) > 0 ) M$pa[yy] = presence.absence( X=M$totno[yy] / M$data_offset[yy], px=p$habitat.threshold.quantile )$pa  # determine presence absence and weighting
        }
      
        # "miscalassification" ~ 321 / 24941 = 0.0129 ... low due to time bias in abundance
        xtabs( ~pa+pa0, M)

        #           pa0
        # pa      0     1
        #   0 10574   134
        #   1   187 14046

        M$density = M$totwgt / M$data_offset
        M$abundance = NA

        for (i in unique(M$gear)) {
          ii = which( M$gear==i & M$density > 0 & M$tag=="observations" )
          if (length(ii) > 0 ) M$abundance[ii] = quantile_estimate( M$density[ii] ) 
        }
        
        ab = M[, mean(abundance, na.rm=TRUE), by=yr ]
        names(ab) = c("yr", "abundance_quantile")

        M = merge(M, ab, by="yr")

    }

    hist(M$z)


  }

  # basic param set definitions END
  # --------------------------------  



  # --------------------------------  
  # analysis and output
  

  # Figure 1. average bottom temperature of prediction surface (1 July)
  Mp  = data.table( M[which(M$tag=="predictions"),] )
  o = Mp[, list(mean=mean(t), lb025=quantile(t, probs=0.025), ub975=quantile(t, probs=0.975)), by=yr]
  trange = range( o[,c("mean","lb025", "ub975")]) * c(0.9, 1.1)
  plot( mean ~ yr, o, type="b", ylim = trange, ylab="Bottom temperature, bottom deg C", xlab="year", lwd=1.5)
  lines (lb025~yr, o, col="darkgray", lty="dashed")
  lines (ub975~yr, o, col="darkgray", lty="dashed")
  
 
  fit = carstm_model( p=p, data=M, sppoly=sppoly, redo_fit=TRUE, 
    posterior_simulations_to_retain="predictions", 
    control.family=list( control.link=list(model="logit") ), ## this is the default for binomial, just here to show wher to use it 
    theta = c( 0.158, 4.251, 1.954, 2.745, 1.831, 1.622, 5.499, -0.393, 4.635, -0.436, 3.954, 3.201 ), # 2021 solution
    num.threads="6:2"  # adjust for your machine
  )  

  if (0) {

    # to reload:
    # NOTE: fit contains estimates on link scale
    fit = carstm_model( p=p, DS="carstm_modelled_fit", sppoly=sppoly )


    carstm_plotxy( fit, vn=c( "fit", "summary.random", "time" ), 
      transf=inverse.logit, 
      type="b", ylim=c(0,1), xlab="Year", ylab="Probability", h=0.5, v=1992   )

    carstm_plotxy( fit, vn=c( "fit", "summary.random", "cyclic" ), 
      transf=inverse.logit, 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0.35, 0.65),
      xlab="Season", ylab="Probability", h=0.5  )

    carstm_plotxy( fit, vn=c( "fit", "summary.random", "inla.group(t, method = \"quantile\", n = 17)" ), 
      transf=inverse.logit,   
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0.2, 0.8) ,
      xlab="Bottom temperature (degrees Celcius)", ylab="Probability", h=0.5  )

    carstm_plotxy( fit, vn=c( "fit", "summary.random", "inla.group(z, method = \"quantile\", n = 17)" ), 
      transf=inverse.logit,  
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0, 0.8) ,
      xlab="Depth (m)", ylab="Probability", h=0.5  )

    carstm_plotxy( fit, vn=c( "fit", "summary.random", "inla.group(log.substrate.grainsize, method = \"quantile\", n = 17)" ), 
      transf=inverse.logit, ylim=c(0.35, 0.65), 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5,
      xlab="Ln(grain size; mm)", ylab="Probability", h=0.5  )

    gears = c("Western IIA", "Yankee #36", "US 4seam 3beam",  "Engle", "Campelen 1800", "Nephrops" )
    carstm_plotxy( fit, vn=c( "fit", "summary.random", "gear" ), subtype="errorbar", errorbar_labels=gears[c(1,2,6)],
      type="p",
      transf=inverse.logit, ylim=c(0.25, 0.75), 
      col="slategray", pch=19, lty=1, lwd=2.5,
      xlab="Gear type", ylab="Probability", h=0.5  )

  }

  # NOTE: res contains estimates on user scale
  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )
  names(res[["random"]])
  names(  fit$summary.random)
  # "time"  
  # "cyclic" 
  # "gear" 
  # "inla.group(t, method = \"quantile\", n = 13)"
  # "inla.group(z, method = \"quantile\", n = 13)"  
 

  # Figure: Habitat vs time - marginal
  carstm_plotxy( res, vn=c("res", "random", "time"),   type="b", ylim=c(0, 1) , lwd=1.5, xlab="year" )

  # Figure: Habitat vs season
  carstm_plotxy( res, vn=c("res", "random", "cyclic"),  type="b", ylim=c(0.35, 0.65) , lwd=1.5, xlab="fractional year" )
    
  sims = survey_estimates ( pH=params$pH, sppoly=sppoly )  # aggregate simulations and summarize 
    
  save( sims, file=results_file, compress=TRUE)   # load(results_file)     # store some of the aggregate timeseries in this list
  # load( results_file )
  


  # analysis and output END
  # --------------------------------  



  # --------------------------------  
  # maps and plots

  # Figure timeseries of habitat -- predicted average timeseries aggregated from posterior samples
  carstm_plotxy( sims, vn=c("sims", "habitat"),  type="b", ylim=c(0.2, 0.8) , 
    lwd=1.5, col="slategray", 
    ylab="Probability", xlab="Year", pch=19, h=0.5,  )



  bb = apply( pa , c(2,3), function(u) u*sa )
  sims[["habitat_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
  attr( sims[["habitat_simulations"]], "units") = "probability"   

  hist(  sims[["habitat_simulations"]][1,] )  # posterior distributions



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



  fn_root = "Predicted_habitat_probability"
  title = "Predicted habitat probability"

  plot_crs = pci$aegis_proj4string_planar_km
  # managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )
  # time_match = "2020"
      

  vn = "predictions"
  brks = pretty(  quantile( sims[[vn]], probs=c(0.025,0.975), na.rm=TRUE )  )

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
      # palette="-RdYlBu",
      plot_elements=c(  "compass", "scale_bar", "legend" ),
      additional_features=additional_features,
      outfilename=fn,
      background = background,
      tmap_zoom= c(map_centre, map_zoom),
      title ="Probability"
  )


  # space_time

  # attr( res[["predictions"]], "units")
  vn=c( "random", "space_time", "iid" )
  vn=c( "random", "space_time", "bym2" )
  vn=c( "random", "space_time", "combined" )
  vn="predictions" 
  
  tmatch="2000"

  for (y in res$time ){
    time_match = as.character(y) 
    
    fn = file.path( outputdir, paste(fn_root, paste0(vn, collapse="_"), time_match, "png", sep=".") )
    carstm_map(  res=res, vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      # breaks=brks,
      # palette="RdYlBu",
      plot_elements=c(  "compass", "scale_bar", "legend" ),
      additional_features=additional_features,
      title= paste("habitat", time_match) , #paste(fn_root, time_match, sep="_"),  
      outfilename=fn,
      background = background,
      # vwidth = 1600,
      # vheight=1000,
      map_mode="view",
      tmap_zoom= c(map_centre, map_zoom)
    )
  }



  #preds from simulations
  vn="predictions" 
  
  for (y in res$time ){
    time_match = as.character(y) 
    
    fn = file.path( outputdir, paste(fn_root, paste0(vn, collapse="_"), time_match, "png", sep=".") )
    carstm_map(  res=res, vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      # breaks=brks,
      # palette="RdYlBu",
      plot_elements=c(  "compass", "scale_bar", "legend" ),
      additional_features=additional_features,
      title= paste("habitat", time_match) , #paste(fn_root, time_match, sep="_"),  
      outfilename=fn,
      background = background,
      # vwidth = 1600,
      # vheight=1000,
      map_mode="view",
      tmap_zoom= c(map_centre, map_zoom)
    )
  }

 
  # --------------------------------  
  # maps and plots END
     



  # --------------------------------  
  # Figure XX 3D plot of habitat vs temperature vs depth  via splines
  
  tvar = "inla.group(t, method = \"quantile\", n = 17)"
  temp_fn = carstm_spline( res, vn=c("random", tvar), statvar="mean" ) 
  temp_fn_lb = carstm_spline( res, vn=c("random", tvar), statvar="quant0.025" ) 
  temp_fn_ub = carstm_spline( res, vn=c("random", tvar), statvar="quant0.975" ) 

  zvar = "inla.group(z, method = \"quantile\", n = 17)"
  depth_fn = carstm_spline( res, vn=c("random", zvar), statvar="mean" ) 
  depth_fn_lb = carstm_spline( res, vn=c("random", zvar), statvar="quant0.025" ) 
  depth_fn_ub = carstm_spline( res, vn=c("random", zvar), statvar="quant0.975" ) 

  svar = "inla.group(log.substrate.grainsize, method = \"quantile\", n = 17)"
  substrate_fn = carstm_spline( res, vn=c("random", svar), statvar="mean" ) 
  substrate_fn_lb = carstm_spline( res, vn=c("random", svar), statvar="quant0.025" ) 
  substrate_fn_ub = carstm_spline( res, vn=c("random", svar), statvar="quant0.975" ) 

  psz = expand.grid( substrate=seq( log(0.001), log(90.5 ), by=0.1 ), depth=seq(0,500, by=50))
  psz$prob_s = substrate_fn( psz$substrate )
  psz$prob_s_lb = substrate_fn_lb( psz$substrate )
  psz$prob_s_ub = substrate_fn_ub( psz$substrate )

  psz$depth = - psz$depth
  plot( prob_s ~ substrate, psz[which(psz$depth==-100),], type="b", pch=19, xlab="substrate", ylab="probability", ylim=c(0.35, 0.7) )
  lines( prob_s_lb ~ substrate, psz[which(psz$depth==-100),], lty="dashed" )
  lines( prob_s_ub ~ substrate, psz[which(psz$depth==-100),], lty="dashed" )



  ptz = expand.grid( temp=seq(-1, 12, by=0.5), depth=seq(0,500, by=50))
  ptz$prob_t = temp_fn(ptz$temp) 
  ptz$prob_t_lb = temp_fn_lb(ptz$temp) 
  ptz$prob_t_ub = temp_fn_ub(ptz$temp) 

  ptz$prob_z = depth_fn(ptz$depth)
  ptz$prob_z_lb = depth_fn_lb(ptz$depth)
  ptz$prob_z_ub = depth_fn_ub(ptz$depth)

  ptz$depth = - ptz$depth


  for (i in c("prob_t", "prob_z", "prob_t_lb", "prob_z_lb", "prob_t_ub", "prob_z_ub") ) {
    o = which( ptz[,i] < 0) 
    if (length(o) > 0) ptz[o,i] = 0
    o = which( ptz[,i] > 1) 
    if (length(o) > 0) ptz[o,i] = 1
  }

  ptz$prob = ptz$prob_t * ptz$prob_z
  ptz$prob_lb = ptz$prob_t_lb * ptz$prob_z_lb
  ptz$prob_ub = ptz$prob_t_ub * ptz$prob_z_ub


  par(mai=c(0.7, 0.7, 0.4, 0.4)) 
  layout( matrix(1:4, ncol=2, byrow=TRUE ))
  nx = 100
  ny = 100
  require(MBA)
  Z = mba.surf(ptz[, c("temp", "depth", "prob")], no.X=nx, no.Y=ny, extend=TRUE) $xyz.est

  image(Z, col = rev(gray.colors(30, gamma=1.75)))
  contour(Z, add = TRUE, drawlabels = TRUE, lty="dotted")
  plot( depth ~ prob_z, ptz[which(ptz$temp==4),], type="b", pch=19, xlab="probability", ylab="depth", xlim=c(0,0.8))
  lines( depth ~ prob_z_lb, ptz[which(ptz$temp==4),], lty="dashed")
  lines( depth ~ prob_z_ub, ptz[which(ptz$temp==4),], lty="dashed")

  plot( prob_t ~ temp, ptz[which(ptz$depth==-100),], type="b", pch=19, xlab="temperature", ylab="probability")
  lines( prob_t_lb ~ temp, ptz[which(ptz$depth==-100),], lty="dashed" )
  lines( prob_t_ub ~ temp, ptz[which(ptz$depth==-100),], lty="dashed" )

  library(plot3D)
  # reduce res to see texture in 3D
  nx = 30
  ny = 30
  Z = mba.surf(ptz[, c("temp", "depth", "prob")], no.X=nx, no.Y=ny, extend=TRUE) $xyz.est
  xx = t(t(rep(1, ny))) %*% Z$x
  yy = t( t(t(rep(1, nx))) %*% Z$y )
  surf3D( x=xx, y=yy, z=Z$z, colkey = TRUE,  
        box = TRUE, bty = "b", phi = 15, theta = 235, contour=TRUE, ticktype = "detailed") 





 
     
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

 

# bring in temperature surface


  # use highest resolution depths and aggregate temps

  # depths aggregated (averaged) to 0.5 km X 0.5 km basis
  Z = aegis.bathymetry::bathymetry_db( 
    p = aegis.bathymetry::bathymetry_parameters( spatial_domain="canada.east.superhighres" ), 
    DS ="aggregated_data" 
  )  
  crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
  inside = st_points_in_polygons(
    pts = st_as_sf( Z[, c("lon", "lat")], coords=c("lon","lat"), crs=crs_lonlat ),
    polys = st_transform( st_union(sppoly), crs_lonlat )
  )

  Z = Z[which(is.finite(inside)), ]

  Z = lonlat2planar(Z, proj.type=p$aegis_proj4string_planar_km ) 

  Z$prob_depth = depth_fn( Z$z.mean )
  Z$prob_depth[ which(Z$prob_depth < 0) ] = 0
  Z$prob_depth[ which(Z$prob_depth > 1) ] = 1

  dr = seq(0,1, by=0.05)
  aegis_map( xyz=Z[, c("plon", "plat", "prob_depth")], depthcontours=TRUE, pts=NULL,
    annot="Cod habitat depth", annot.cex=1,
    at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain )

  sppoly$corearea = res$random$space$combined[, "mean"] - 1.96 * res$random$space$combined[, "sd"] 

  plot(sppoly["corearea"])
  

  # add pure spatial effect 
  AU = st_cast(sppoly, "MULTIPOLYGON" )
  AU = sf::st_transform( AU, crs=st_crs(p$aegis_proj4string_planar_km) )
  bm = match( AU$AUID, res$space )  

  LOCS = sf::st_as_sf( Z, coords=c("lon", "lat") )
  st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
  LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
  LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" )   

  raster_resolution=1  #km
  raster_template = raster::raster( AU, res=raster_resolution, crs=st_crs( p$aegis_proj4string_planar_km ) )
  gridparams = p$gridparams 
  gridparams$res = c(raster_resolution, raster_resolution)
  # prep-pass with a au_index variable to get index
  AU$au_index = 1:nrow(AU)
  LL = fasterize::fasterize( AU, raster_template, field="au_index" )
  o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
  st_crs(o) = st_crs( AU )
  ii = match(
    array_map( "xy->1", st_coordinates(LOCS), gridparams=gridparams ),
    array_map( "xy->1", st_coordinates(o), gridparams=gridparams )
  )

  LL = o = NULL 
  gc()
  statvars =   c("mean", "sd", "quant0.025", "quant0.5", "quant0.975")
  for (g in 1:length(statvars)) {
    stat_var = statvars[g]
    vnm = paste( "spatial_effect",  stat_var, sep="_" )
    AU[[vnm]] = res$random$space$combined[ bm, stat_var ]
    LL = fasterize::fasterize( AU, raster_template, field=vnm )
    o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
    st_crs(o) = st_crs( AU )
    LOCS[[vnm]] = o[["layer"]][ ii ]
  }

  # this is the pure spatial effect discretized to Z
  Z$prob_core = LOCS[["spatial_effect_mean"]] 
  aegis_map( xyz=Z[, c("plon", "plat", "prob_core")], depthcontours=TRUE, pts=NULL,
    annot="Cod habitat pure space", annot.cex=1.25,
    at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain 
  )


  # add substrate
  require(aegis.substrate)
  # pS = substrate_parameters( project_class="carstm"   ) 

  Z$substrate = aegis_lookup( 
      parameters="substrate", # defaults for lookup
      LOCS=Z[, c("lon", "lat" )], 
      project_class="stmv", 
      output_format="points", 
      variable_name="substrate.grainsize",
      # variable_name=list("predictions"),
      statvars=c("mean"),
      space_resolution=p$pres,
      returntype = "vector"
    )

  Z$log.substrate = log(Z$substrate)
  Z$log.substrate[ !is.finite(Z$log.substrate) ] = mean( Z$log.substrate, na.rm=TRUE )
  Z$prob_substrate = substrate_fn( Z$log.substrate )
  Z$prob_substrate[ which(Z$prob_substrate < 0) ] = 0
  Z$prob_substrate[ which(Z$prob_substrate > 1) ] = 1

 
  dr = seq(0.35, 0.75, by=0.05)
  aegis_map( xyz=Z[, c("plon", "plat", "prob_substrate")], depthcontours=TRUE, pts=NULL,
    annot="Cod habitat substrate", annot.cex=1,
    at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain )







  # ------------
  # add temperature
  require(aegis.temperature)

  year.assessment = 2021

  pT = temperature_parameters( project_class="carstm", yrs=1970:year.assessment, carstm_model_label="1970_present"  ) 
  
  # Compute temperature-related compenent
  times = 0:(p$ny-1) + decimal_date( lubridate::ymd("1970-07-01")  )  
  nt = length(times )
  temp = matrix(NA, nrow=nrow(Z), ncol=nt )
  for (i in 1:nt ) {
    Z$timestamp = times[i]
    temp[,i] = aegis_lookup( 
      parameters="temperature", # defaults for lookup
      LOCS=Z[, c("lon", "lat", "timestamp")], 
      project_class="carstm", 
      output_format="points", 
      variable_name=list("predictions"),
      statvars=c("mean"),
      space_resolution= p$pres,
      time_resolution= p$tres,
      cyclic_resolution=0.25,
      tz="America/Halifax", 
      year.assessment=year.assessment,
      returntype = "vector"
    )
  }

  i=which(is.na(temp[,1]))
  temp=temp[-i,]
  Z=Z[-i,]

  temp_prob = temp[]* NA
  for (i in 1:nt ) {
    temp_prob[,i] = temp_fn( temp[,i] )
  }
  temp_prob[ which(temp_prob < 0) ] = 0
  temp_prob[ which(temp_prob > 1) ] = 1

  dev.new(); 
  for (i in 1:ncol(temp_prob)) {
    # i = 1
    Z$prob_temp = temp_prob[,i] 
    print(
      aegis_map( xyz=Z[, c("plon", "plat", "prob_temp")], depthcontours=TRUE, pts=NULL,
        annot=paste("Cod habitat temperature", p$yrs[i]), annot.cex=1.25,
        at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain 
      )
    )
  }



  if (0) {
    # using aggegrate depths and tmps
      resT = carstm_model( p=pT, DS="carstm_modelled_summary", sppoly=areal_units( p=pT )  ) # to load currently saved results

      # choose only predictions in "summer"
      i_summ = which( resT$data$tag=="predictions" & resT$data$dyri==0.65 )
      head( resT$data ) 
    
      space = resT$space
      time = resT$time
      cyclic = resT$cyclic  

      ipred = which( 
        resT$data$tag=="predictions" & 
        resT$data[,"space0"] %in% space  &  
        resT$data[,"time0"] %in% time &  
        resT$data[,"cyclic0"] %in% cyclic 
      )  # ignoring U == predict at all seassonal components ..

      V = resT$predictions[,,,1]  # means
      H = Z = T[]*NA

      matchfrom = list( space=resT$data[ipred, "space"], time=resT$data[ipred, "time"], cyclic=resT$data[ipred, "cyclic0"] )
      matchto = list( space=space, time=time, cyclic=cyclic  )

      # expand Z to match time to make computations simpler
      Z  = reformat_to_array( input=resT$data[ipred,"z"], matchfrom=matchfrom, matchto=matchto )

      for (i in 1:dim(Z)[2] ) {
        for (j in 1:dim(Z)[3]) {
          V[,i,j] = temp_fn(  V[,i,j] ) 
          Z[,i,j] = depth_fn( Z[,i,j] )
        }
      }
      
      H[,i,j] = temp_fn( T[,i,j] ) * depth_fn( Z[,i,j] )

      H[H<0] = 0
      H[H>1] = 1
      
      m = colMeans(H[,,5], na.rm=TRUE)  # "summer = 0.65" 
      m = colMeans(V[,,5], na.rm=TRUE)  # "summer = 0.65" 
      m = colMeans(Z[,,5], na.rm=TRUE)  # "summer = 0.65" 

      apply( H, 2, function(x) length(which(x>0.5)) )

  }




  ############
  # this is the static component = pure space + depth + substrate

  dr = seq(0, 0.35, by=0.05)

  Z$prob_static = Z$prob_core * Z$prob_depth * Z$prob_substrate
  aegis_map( xyz=Z[, c("plon", "plat", "prob_static")], depthcontours=TRUE, pts=NULL,
    annot="Cod habitat static", annot.cex=1.25,
    at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain 
  )


  # static (core area + depth) + temmperaure

  cod_hab = temp_prob * Z$prob_static 
  cod_hab[ which(cod_hab < 0) ] = 0
  cod_hab[ which(cod_hab > 1) ] = 1
  cod_hab[ which(!is.finite(cod_hab)) ] = 0

  dev.new(); hist(cod_hab, "fd")

  dr = seq(0,0.18, by=0.01)
  
  dev.new(); 
  for (i in 1:ncol(temp_prob)) {
    Z$prob_cod = cod_hab[,i]
    print(
      aegis_map( xyz=Z[, c("plon", "plat", "prob_cod")], depthcontours=TRUE, pts=NULL,
        annot=paste( "Cod habitat temp+depth", p$yrs[i] ), annot.cex=1.25,
        at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain 
      )
    )
  }

  ic = which(Z$prob_static > 0.25 )

  g = colMeans(cod_hab[ic,], na.rm=TRUE)
  plot(g, type="b")

  count_hab = function(x) length(which(x > 0.25))

  o= apply( cod_hab[ic,], 2, count_hab )
  plot(o ~ p$yrs, type="b", ylim=c(0, max(o)))

  o= apply( cod_hab[ic,], 2, median )
  plot(o, type="b")

  Z$prob_cod = Z$prob_space * Z$prob_temp

  aegis_map( xyz=Z[ic, c("plon", "plat", "prob_cod")], depthcontours=TRUE, pts=NULL,
    annot="Cod habitat ", annot.cex=1.25,
    at=dr, col.regions=(color.code( "seis", dr)), corners=p$corners, spatial_domain=p$spatial_domain 
  )
  

# ---------------------------
np = 5000
psims = inla.posterior.sample( np, fit  ) 


# order of effects gets messed up .. must use names
psims_rn = gsub( "[:].*$", "", rownames(psims[[1]]$latent) )
i_b0 = grep("\\(Intercept\\)", psims_rn )
i_temp = grep( "inla.group\\(t", psims_rn )
i_depth = grep( "inla.group\\(z", psims_rn )
i_cyclic = grep( "^cyclic$", psims_rn )
i_gear = grep( "^gear$", psims_rn )
i_time = grep( "^time$", psims_rn )
 

ns = length( res$space )

iid = 1:ns
bym = (ns+1) : (2*ns)
i_space = grep( "^space$", psims_rn )
i_space_iid = i_space[iid]
i_space_bym = i_space[bym]


sti = expand.grid( space=res$space, type = c("iid", "bym"), time=res$time, stringsAsFactors =FALSE ) # bym2 effect: bym and iid with annual results
iid = which(sti$type=="iid") #  spatiotemporal interaction effects  iid
bym = which(sti$type=="bym") #  spatiotemporal interaction effects bym

i_space_time = grep( "^space_time$", psims_rn )
i_space_time_iid = i_space_time[iid]
i_space_time_bym = i_space_time[bym]

tempsQ  = res$random[[4]]$ID
depthsQ = res$random[[5]]$ID

matchto   = list( space=res$space, time=res$time  )
matchfrom = list( space=sti[["space"]][iid], time=sti[["time"]][iid]  )



pred_func = function(x, threshold =NULL) {
  Y = x$latent
  yrr = matrix(  Y[i_time], ncol=length(res$time), nrow=length(res$space), byrow=TRUE )
  depth_fn = splinefun( depthsQ,  Y[i_depth], method="monoH.FC" )
  temp_fn  = splinefun( tempsQ,   Y[i_temp], method="monoH.FC"  )
  depths = depth_fn( res$data$z[which(res$data$tag=="predictions")] )
  temps = temp_fn(  res$data$t[which(res$data$tag=="predictions")] )
  depths = reformat_to_array(  input =depths , matchfrom = matchfrom, matchto = matchto )
  temps = reformat_to_array(  input =temps, matchfrom = matchfrom, matchto = matchto )
  Wbym = Wiid = array( NA, dim=c( length( res$space), length(res$time) ), dimnames=list( space=res$space, time=res$time ) )
  Wiid = reformat_to_array(  input = unlist(Y[i_space_time_iid]), matchfrom = matchfrom, matchto = matchto )
  Wbym = reformat_to_array(  input = unlist(Y[i_space_time_bym]), matchfrom = matchfrom, matchto = matchto )
  # pred = Y[i_b0]  + yrr+ Y[i_cyclic[7]]  + Y[i_space_iid] + Y[i_space_bym]  + temps + depths 
  # pred = Y[i_b0] + yrr + Y[i_cyclic[7]]  + Y[i_space_iid] + Y[i_space_bym]  + Wbym + Wiid + temps + depths 
  pred =   Y[i_b0] + Y[i_cyclic[7]]  + Y[i_space_iid] + Y[i_space_bym] + Wbym + Wiid + temps + depths 
  
  if (!is.null(threshold)) {
    il = which( pred <= threshold )
    iu = which( pred > threshold )
    pred[ il ] = 0
    pred[ iu ] = 1
  }
  pred
} 

oo =  ( simplify2array( lapply( psims, pred_func ) ) )
gg = apply( inverse.logit(oo)*sppoly$au_sa_km2, 2, mean, na.rm=TRUE ) 
plot( gg ~ res$time, type="b")
abline(v=1990)


oo = simplify2array( lapply( psims, pred_func, threshold = 0 ) )
gg = apply( oo*sppoly$au_sa_km2, 2, sum, na.rm=TRUE ) /  sum(sppoly$au_sa_km2)/ np
plot(gg ~ res$time, type="b")
abline(v=1990)

gg = apply( oo, 2, median, na.rm=TRUE )
lines(gg ~ res$time, col="green")
gg = apply( oo, 2, quantile, probs=0.025, na.rm=TRUE )
lines(gg ~ res$time, col="red")


gg = inverse.logit( apply( oo, 2, mean, na.rm=TRUE )) 


g = invlink( g ) 

lnk_function = inla.link.logit
lnk_function_predictions = inla.link.identity  # binomial seems to be treated differently by INLA



