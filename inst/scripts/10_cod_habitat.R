
 
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
      # months=6:8,  #"summer"
      # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
      # ranged_data="dyear"
      settype = c(1,2,4,5,8),
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
    ff = survey_db( p=p, DS="areal_units_input", redo=TRUE)
    ff = NULL
    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=redo_sppoly, areal_units_constraint="survey"  )
    plot(sppoly["AUID"], reset=FALSE)
 
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
      sa_threshold_km2 = 5,
      fraction_cv = 0.5,   # ie. stop if sd/mean is less than 
      fraction_todrop = 0.1  # control frction dropped on each iteration: speed vs roughness 
    ) )

    p$formula = formula( 
        pa ~ 1 
          + f( time, model="ar1",  hyper=H$ar1 ) 
          + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic=TRUE, values=cyclic_values ) 
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
    
    invisible( survey_db( p=p, DS="areal_units_input", redo=redo_sppoly) ) # update data file used for sppoly creation

    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=redo_sppoly, areal_units_constraint="survey", verbose=TRUE )
    plot(sppoly["AUID"], reset=FALSE)


  }

  # basic param set definitions END
  # --------------------------------  



  # --------------------------------  
  # analysis and output

  redo_survey_data = FALSE
  # redo_survey_data = TRUE
  M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, quantile_upper_limit=0.99 )
  

  # Figure 1. average bottom temperature of prediction surface (1 July)
  Mp  = data.table( M[which(M$tag=="predictions"),] )
  o = Mp[, list(mean=mean(t), lb025=quantile(t, probs=0.025), ub975=quantile(t, probs=0.975)), by=yr]
  trange = range( o[,c("mean","lb025", "ub975")]) * c(0.9, 1.1)
  plot( mean ~ yr, o, type="b", ylim = trange, ylab="Bottom temperature, bottom deg C", xlab="year", lwd=1.5)
  lines (lb025~yr, o, col="darkgray", lty="dashed")
  lines (ub975~yr, o, col="darkgray", lty="dashed")
  


  fith = carstm_model( p=p, data=M, sppoly=sppoly, redo_fit=TRUE, 
    posterior_simulations_to_retain="predictions", scale_offsets=TRUE,
    control.family=list(control.link=list(model="logit"))
  ) 


  names(  fith$summary.random)
  i = 1
  plot(inverse.logit(fith$summary.random[[i]]$mean) ~ fith$summary.random[[i]]$ID)



  # Figure 2. Habitat vs time
  b0 = resh$summary$fixed_effects["(Intercept)", "mean"]
  ts = resh$random$time
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] + b0 
  plot( mean ~ ID, ts, type="b", ylim=c(-2,2)+b0, lwd=1.5, xlab="year")
  lines( quant0.025 ~ ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ ID, ts, col="gray", lty="dashed")


  # Figure 2. Habitat vs season
  b0 = resh$summary$fixed_effects["(Intercept)", "mean"]
  ts = resh$random$cyclic
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] + b0
  plot( mean ~ID, ts, type="b", ylim=c(-1.5, 1.5)+b0, lwd=1.5, xlab="fractional year")
  lines( quant0.025 ~ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ID, ts, col="gray", lty="dashed")





# to get priors ::
# taken from INLA:::plot.inla

  plot( fith,  plot.prior=TRUE,   plot.fixed.effects=TRUE,  plot.lincomb = TRUE,
                   plot.random.effects = TRUE,
                   plot.hyperparameters = TRUE,
                   plot.predictor = TRUE,
                   plot.q = TRUE,
                   plot.cpo = TRUE
               )

# fixed
fix <- fith$marginals.fixed
labels.fix <- names(fith$marginals.fixed)

i=1 # loop
m <- inla.smarginal(fix[[i]])
my.plot(m, type = "l", main = paste("PostDens [", 
inla.nameunfix(labels.fix[i]), "]", sep = ""),  sub = sub, xlab = "", ylab = "")

all.hyper <- inla.all.hyper.postprocess( fith$all.hyper)
xy <- (inla.get.prior.xy(section = "fixed", hyperid = labels.fix[i], all.hyper = all.hyper, range = range(m$x), debug = debug))
my.lines(xy, col = "blue")


# hypers
hyper <- x$internal.marginals.hyperpar
if (!is.null(hyper)) {

# loop
hh <- hyper[[i]]
if (!is.null(hh)) {
  label <- inla.nameunfix(names(hyper)[i])
  m <- inla.smarginal(hh)
  my.plot(m, type = "l", ylab = "", xlab = "")
  my.title(main = paste("PostDens [", label, 
    "]", sep = ""))


  id <- unlist(strsplit(attr(hyper[[i]], "hyperid"),  "\\|"))
  if (length(id) > 0) {
    xy <- (inla.get.prior.xy(section = tolower(id[2]),  hyperid = id[1], all.hyper = all.hyper, 
    range = range(m$x), intern = FALSE, debug = debug))
    my.lines(xy, col = "blue")
  }
}




  resh = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )

  names(resh[["random"]])
  resh[["random"]][["gear"]]

  params = list()
  vars_to_copy = c(  "space", "time", "dyears" )  # needed for plotting 
  for ( vn in vars_to_copy ) params[[vn]] = resh[[vn]]

  pa = resh[["predictions_posterior_simulations"]]
  # pa = inverse.logit(pa)
  pa[!is.finite(pa)] = NA
 
  # create for mapping .. in probability
  oo = simplify2array(pa)
  params[["predictions"]] = resh[[ "predictions" ]] * NA
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

  bb = apply( pa , c(2,3), function(u) u*sa )
  params[["habitat_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
  attr( params[["habitat_simulations"]], "units") = "probability"

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
  fith = carstm_model( p=RES[[mf]]$pH, DS="carstm_modelled_fit", sppoly=sppoly )
  resh = carstm_model( p=RES[[mf]]$pH, DS="carstm_modelled_summary", sppoly=sppoly )


  # analysis and output END
  # --------------------------------  



  # --------------------------------  
  # maps and plots


  # Figure timeseries of habitat
  vn = "habitat"
  RES[[mf]][[vn]] # aggregate summaries 

  units = attr( RES[[mf]][[vn]], "units")
  plot( RES[[mf]][[vn]][["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="blue", type="b", main=mf, ylab=units, xlab="year" )
  lines( RES[[mf]][[vn]][["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="blue", type="b" )

  hist(  RES[[mf]][["habitat_simulations"]][1,] )  # posterior distributions




  # maps

  plot_crs = st_crs(sppoly)

    additional_features =  
      tm_shape( st_transform( maritimes_groundfish_strata(areal_units_timeperiod = p$areal_units_timeperiod), plot_crs ) ) + 
        tm_borders( col="darkgrey", alpha=0.9, lwd=1.5)   + 
      tm_shape( aegis.bathymetry::isobath_db(  depths=c( seq(0, 500, by=100), 1000), project_to=plot_crs  ), projection=plot_crs ) +
        tm_lines( col="slategray", alpha=0.5, lwd=0.5) 
      #   +
      # tm_shape( aegis.coastline::coastline_db( DS="eastcoast_gadm", project_to=plot_crs ), projection=plot_crs ) +
      #   tm_polygons( col="lightgray", alpha=0.5 , border.alpha =0.5)

    (additional_features)

    vn=c( "random", "space", "combined" )
    vn=c( "random", "spacetime", "combined" )
    vn="predictions"  # numerical density (km^-2)

    tmatch="2015"
 
    carstm_map(  res=resh, vn=vn, tmatch=tmatch, 
        palette="-RdYlBu",
        plot_elements=c(  "compass", "scale_bar", "legend" ),
        additional_features=additional_features,
        tmap_zoom= c(map_centre, map_zoom),
        title =paste( paste0(vn, collapse=" "), paste0(tmatch, collapse="-"), "probability"  )
    )


  # map it
  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7
  background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 


  fn_root = "Predicted_habitat_probability"
  title = "Predicted habitat probability"

  plot_crs = pci$aegis_proj4string_planar_km
  # managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )
  # time_match = "2020"
      

  vn = "predictions"
  brks = pretty(  quantile( RES[[mf]][[vn]], probs=c(0.025,0.975), na.rm=TRUE )  )

  outputdir = file.path( p$modeldir, p$carstm_model_label, "predicted.habitat" )
  
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  # attr( RES[[mf]][["predictions"]], "units")

  for (y in res$year ){
    time_match = as.character(y) 
    
    fn = file.path( outputdir, paste(fn_root, "png", sep=".") )
    carstm_map(  res=RES[[mf]], vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      # breaks=brks,
      # palette="RdYlBu",
      title= paste("habitat", time_match) , #paste(fn_root, time_match, sep="_"),  
      # outfilename=fn,
      background = background,
      # vwidth = 1600,
      # vheight=1000,
      map_mode="view",
      tmap_zoom= c(map_centre, map_zoom)
      #plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" )
    )
  }

 
  # --------------------------------  
  # maps and plots END
     



  # --------------------------------  
  # other variations


# Figure XX 3D plot of habitat vs temperature vs depth  
 

b0 = resh$summary$fixed_effects["(Intercept)", "mean"]

pt = resh$random$t
pz = resh$random$z

ptz = expand.grid( temp=pt$ID, depth =pz$ID ) 

ptz = merge( ptz, pt, by.x=pt, by.y=ID, all.x=TRUE, all.y=FALSE )
ptz = merge( ptz, pz, by.x=pz, by.y=ID, all.x=TRUE, all.y=FALSE, suffixes=c(".temp", ".depth") )

ptz$prob = ptz$mean.temp * ptz$mean.depth * b0

Z = mba.surf(ptz[,"mean.temp", "mean.depth", "prob"], no.X=100, no.Y=100, extend=TRUE) $xyz.est

persp(Z, theta = 45, phi = 20, col = "green3", scale = FALSE,
           ltheta = -120, shade = 0.75, expand = 10, border = NA, box = FALSE)
 

library(plot3D)

surf3D( X, Y, Z, colkey = TRUE,  
       box = TRUE, bty = "b", phi = 20, theta = 45, contour=TRUE, ticktype = "detailed") 


     
     
-- zero-inflated binomial
-- negative binomial ?
-- split by size (as covariate) and sex


### end

 