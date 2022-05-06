
 
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

# parameter_set = "stratanal_iid"  # used by 10_cod_workspace to load a defined parameter subset
parameter_set = "tesselation"  # used by 10_cod_workspace to defined parameter subsets

source( file.path( code_root, "aegis.survey", "inst", "scripts", "10_cod_workspace.R" ) )

outputdir = file.path( dirname(results_file), p$carstm_model_label  )
if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )


  
# ----------------------------------------------
# Atlantic cod with a CAR (ICAR/BYM) Poisson process models with tesselation


# instead of dropping right away, carry the data as it represents neighbourhood information and additional data
# reset sppoly to full domain

# auid to drop to match Michelle's extraction for "stratanal"

if (redo_data) {
  # tesselation
  sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=TRUE  )  # required to reset save location
  

  M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE, quantile_upper_limit=0.99, 
    fn=file.path( p$modeldir, p$speciesname, "carstm_inputs_tesselation.rdata" ) )
}


sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84") )

# auid to drop to match Michelle's extraction for "stratanal"
auid_to_drop = strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ) 
sppoly = set_surface_area_to_NA( sppoly, auid_to_drop )  # do not drop data .. only set areas beyond domain to NA
sppoly$filter = ifelse(is.finite( sppoly$au_sa_km2 ), 1, NA)



# plot figure for ms following just creates the background map 
# .. must send boundaries of areal units again as a separate feature to plot
aus =  
  tm_shape( sppoly[ which(sppoly$filter==1), ] ) + 
    tm_borders( col="plum", alpha=0.75, lwd=1)  +
  tm_shape( sppoly[ which(is.na(sppoly$filter) ), ] ) + 
    tm_borders( col="lightgray", alpha=0.75, lwd=1)  

sppoly$dummy_var = NA
outfilename = file.path( outputdir , "areal_units_tesselation.png" )
carstm_map(  sppoly=sppoly, vn="dummy_var",
    additional_features=additional_features+aus,
    # palette="-RdYlBu",
    plot_elements=c( "compass", "scale_bar", "legend"  ), 
    scale=1.5,
    map_mode="plot",
    tmap_zoom= c(map_centre, map_zoom),
    outfilename=outfilename
) 


M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, quantile_upper_limit=0.99, 
    fn=file.path( p$modeldir, p$speciesname, "carstm_inputs_tesselation.rdata" ) )

ip = which(M$tag == "predictions")
io = which(M$tag == "observations")
iq = unique( c( which( M$totno > 0), ip ) ) # subset to positive definite data (for number and size)
iw = unique( c( which( M$totno > 30), ip ) ) # subset to positive definite data (for number and size)

pN = survey_parameter_list( p=p, model_label=p$carstm_model_type, type="abundance" )
pW = survey_parameter_list( p=p, model_label=p$carstm_model_type, type="meansize" )
pH = survey_parameter_list( p=p, model_label=p$carstm_model_type, type="habitat" )

 
# size model
fit = NULL; gc()
fit = carstm_model( p=pW, data=M[iw,], sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
  #theta= c( 0.088, 2.950, 0.943, 3.230, 3.676, 4.382, 3.781, 3.952, 3.313, 2.603, -0.044, 2.566, 3.194),
  # control.inla = list( strategy='adaptive' ), 
  num.threads="4:2", mc.cores=2 
)  

# numerical model
fit = NULL; gc()
fit = carstm_model( p=pN, data=M[iq,], sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
  #theta=c(1.131, 0.767, 2.593, -0.659, -1.411, -1.689, -0.254, -2.234, 3.394, -2.381, -1.399, 0.371) ,
  # control.inla = list( strategy='adaptive', int.strategy="eb" ), 
  num.threads="4:2", mc.cores=2 
)  

# plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

# habitat model
fit = NULL; gc()
fit = carstm_model( p=pH, data=M, sppoly=sppoly, posterior_simulations_to_retain="predictions", 
  # control.inla = list( strategy='adaptive' ), 
  num.threads="4:2", mc.cores=2   
) 
# plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )


# reload collator
RES = readRDS( results_file )

# NOTE: below we divide by 10^6 to convert  kg -> kt;; kt/km^2
# with "habitat" at habitat definition of prob=0.05 (hurdle process)  
sims = carstm_posterior_simulations( pN=pN, pW=pW, pH=pH, sppoly=sppoly, pa_threshold=0.05 ) * sppoly$au_sa_km2 / 10^6  
RES[[p$carstm_model_type]] = carstm_posterior_simulations_summary( sims ) 


saveRDS( RES, results_file, compress=TRUE )
# RES = readRDS( results_file )
  


( fn = file.path( outputdir, "biomass_timeseries.png") )
png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
  plot( mean ~ year, data=RES[[p$carstm_model_type]], col="slategray", pch=19, lty=1, lwd=2.5, 
  type="b", ylab="Biomass index (kt)", xlab="", ylim=c(0,190))
  lines( lb025 ~ year, data=RES[[p$carstm_model_type]], lty="dashed", col="gray" )
  lines( ub975 ~ year, data=RES[[p$carstm_model_type]], lty="dashed", col="gray" )
dev.off()
 

# map it ..mean density (choose appropriate p$carstm_model_type/"sims", above)
 
# equivalent of the persistent spatial effect (global spatial mean)
Bg = apply( sims, c(1), mean, na.rm=TRUE )  # global spatial means  
brks = pretty( log10( quantile( Bg, probs=c(0.05, 0.95), na.rm=TRUE ))  )
vn =  "biomass_mean_global" 
sppoly[,vn] = log10( Bg )

outfilename = file.path( outputdir , "predictions", paste( "biomass", "spatial_effect", "png", sep=".") )
carstm_map(  sppoly=sppoly, vn=vn,
    breaks=brks,
    additional_features=additional_features,
#    title= y, #paste( "log_10( Predicted biomass density; kg/km^2 )", y ),
    palette="-RdYlBu",
    plot_elements=c( "compass", "scale_bar", "legend" ), 
    scale=1.5,
    map_mode="view",
    tmap_zoom= c(map_centre, map_zoom),
    outfilename=outfilename
) 
 
B = apply( sims, c(1,2), mean, na.rm=TRUE  ) # means by year
brks = pretty( log10( quantile( B[], probs=c(0.05, 0.95), na.rm=TRUE  ))  )
vn = paste("biomass", "predicted", sep=".")
for (i in 1:length(pN$yrs) ){
  y = as.character( pN$yrs[i] )
  sppoly[,vn] = log10( B[,y] )
  outfilename = file.path( outputdir, "predictions",  paste( "biomass", y, "png", sep=".") )
  carstm_map(  sppoly=sppoly, vn=vn,
      breaks=brks,
      additional_features=additional_features,
      title= y, #paste( "log_10( Predicted biomass density; kg/km^2 )", y ),
      palette="-RdYlBu",
      plot_elements=c( "compass", "scale_bar", "legend" ), 
      scale=1.5,
      map_mode="view",
      tmap_zoom= c(map_centre, map_zoom), 
      outfilename=outfilename
  )
}


# comparative plots of timeseries:
# dev.new(width=11, height=7)
# nvn = setdiff( names(RES), "yr" )
# vc = paste( "full_model" )

# nv  = which( nvn %in% c( "full_model",  "stratanal.towdistance")  )
# col = c("slategray", "turquoise", "darkorange", "lightgreen", "navyblue", "darkred",  "turquoise", "cyan", "darkgreen", "purple", "darkgray", "pink" )
# pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20, 19, 23)
# lty = c(2, 1, 4, 5, 6, 7, 1, 3, 4, 5, 6 )
# lwd = c(3, 6, 6, 2, 4, 6, 2, 4, 6, 5, 4 )
# type =c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l")

# plot( 0, 0, type="n", xlim=range(RES[["yr"]]), ylim=c(0, 320), xlab="Year", ylab="kt", main="Comparing input data treatment and sweptareas")
# for (j in 1:length(nv) ) {
#   i = nv[j]
#   lines( mean ~ year, data=RES[[nvn[i]]], lty=lty[j], lwd=lwd[j], col=col[j], pch=pch[j], type=type[j])
#   lines( lb025 ~ year, data=RES[[nvn[i]]], lty="dotted", lwd=1, col=col[j] )
#   lines( ub975 ~ year, data=RES[[nvn[i]]], lty="dotted", lwd=1, col=col[j] )
# }

# legend("topright", legend=nvn[nv], lty=lty, col=col, lwd=lwd )

# alternative plot (Figure 2)
dev.new(width=14, height=8, pointsize=20)

library(ggplot2)

r1 = RES[["stratanal.standardtow"]]
r1$Method = "Stratanal" 

r2 = RES[["full_model"]]
r2$Method = "CAR/Hurdle"

keep = intersect( names(r1), names(r2) )
dta = rbind( r1[, keep ], r2[, keep] ) 
dta[dta<0] = 0
 
ggplot( dta, aes(year, mean, fill=Method, colour=Method) ) +
  geom_ribbon(aes(ymin=lb025, max=ub975), alpha=0.2, colour=NA) +
  geom_line() +
  labs(x="Year", y="Biomass (kt)", size = rel(1.5)) +
  # scale_y_continuous( limits=c(0, 300) )  
  theme_light( base_size = 22 ) 




# --- simple ribbon plot

# start from a vanilla R session (plotly does not like startup)
 

# end
# ------------------------------------------------
 
# Figure 1alt. average bottom temperature of prediction surface (whole year spatial and temporal variability)
  pt = temperature_parameters( 
      project_class="carstm", 
      yrs=1970:2021, 
      carstm_model_label="1970_present" 
    ) 
  tspol = areal_units( p=pt )
  tspol = set_surface_area_to_NA( tspol, auid_to_drop )  # do not drop data .. only set areas beyond domain to NA

  res = carstm_model( p=pt, DS="carstm_modelled_summary", sppoly=tspol  ) # to load currently saved results

  aufilter = ifelse( is.finite(tspol$au_sa_km2), 1, NA )
  res = res$predictions[,,,"mean"] * aufilter  # subannual means
  res_mean = apply(res, 2, mean, na.rm=TRUE )
  res_q025 = apply(res, 2, quantile, probs=0.025, na.rm=TRUE )
  res_q975 = apply(res, 2, quantile, probs=0.975, na.rm=TRUE )
  
  trange = range(  c(res_q975, res_q025) ) * c(0.9, 1.1)
  plot( res_mean ~ RES$yr, type="b", pch=19, col="slategray", ylim = trange, ylab="Bottom temperature, Celsius", xlab="Year", lwd=1.5)
  lines( res_q025 ~RES$yr, col="darkgray", lty="dashed")
  lines( res_q975 ~RES$yr, col="darkgray", lty="dashed")




# --------------------------------  
# maps and plots
 
  p = pH
  fn_root = "Predicted_habitat_probability"
  title = "Predicted habitat probability"
  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale
  
  carstm_plots( res, outputdir, fn_root, sppoly, additional_features, background, map_centre, map_zoom)

  # posterior predictions: timeseries 
  preds = res[["predictions"]] * sppoly$filter # space x year (in 1 JULY)

  # spatial CI
    preds = data.table(
      mean = apply( preds, 2, mean, na.rm=TRUE) ,
      q025 = apply( preds, 2, quantile, probs=0.025, na.rm=TRUE) ,
      q975 = apply( preds, 2, quantile, probs=0.975, na.rm=TRUE) 
    )

    plot( 0 , 0, type="n", ylab="Probability", xlab="Year", ylim=c(0, 1), xlim=range( RES$yr)   )
    lines( preds$mean ~ RES$yr, lty=1, lwd=2.5, col="slategray" )
    lines( preds$q025 ~ RES$yr, lty="dotted", lwd=1, col="slategray"  )
    lines( preds$q975 ~ RES$yr, lty="dotted", lwd=1, col="slategray"  )

    abline( h=0.5, lty="dashed",  col="slategray" )

  
  # from sims:
  
    # with "habitat" at habitat definition of prob=0.05 (hurdle process)  
    sims = carstm_posterior_simulations( pH=pH, sppoly=sppoly, pa_threshold=0.05 ) 
    sims = sims * sppoly$au_sa_km2 / sum(  sppoly$au_sa_km2, na.rm=TRUE )  # area weighted average

    lab = paste(p$carstm_model_type, "habitat", sep="_")
    RES[[lab]] = carstm_posterior_simulations_summary( sims )  # sum area weighted probs
  
    ( fn = file.path( outputdir, "habitat_timeseries.png") )
    png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
      plot( mean ~ year, data=RES[[lab]], col="slategray", pch=19, lty=1, lwd=2.5, 
      type="b", ylab="Probability", xlab="", ylim=c(0,1) )
      lines( lb025 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
      lines( ub975 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
    dev.off()
    


  p = pN
  fn_root = "Predicted_numerical_density"
  title = "Predicted numerical density"
  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale
  carstm_plots( res, outputdir, fn_root, sppoly, additional_features, background, map_centre, map_zoom )
 # from sims:
  
    # with "habitat" at habitat definition of prob=0.05 (hurdle process)  
    sims = carstm_posterior_simulations( pN=pN, sppoly=sppoly, pa_threshold=0.05 ) 
    sims = sims * sppoly$au_sa_km2  / 10^6 # n -> G n  # expand densities to number and then sum below (carstm_posterior_simulations_summary)

    lab = paste(p$carstm_model_type, "number", sep="_")
    RES[[lab]] = carstm_posterior_simulations_summary( sims )  # sum area weighted probs
  
    ( fn = file.path( outputdir, "number_timeseries.png") )
    png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
      plot( mean ~ year, data=RES[[lab]], col="slategray", pch=19, lty=1, lwd=2.5, 
      type="b", ylab=bquote("Number" ~ 10^6), xlab="", ylim=c(0,160) )
      lines( lb025 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
      lines( ub975 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
    dev.off()
 
    

  p = pW
  fn_root = "Predicted_mean_weight"
  title = "Predicted mean weight"
  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale
  carstm_plots( res, outputdir, fn_root, sppoly, additional_features, background, map_centre, map_zoom)
  
    sims = carstm_posterior_simulations( pW=pW, sppoly=sppoly, pa_threshold=0.05 ) 
    sims = sims * sppoly$au_sa_km2 / sum(  sppoly$au_sa_km2, na.rm=TRUE )  # area weighted average

    lab = paste(p$carstm_model_type, "weight", sep="_")
    RES[[lab]] = carstm_posterior_simulations_summary( sims )  # sum area weighted probs
  
    ( fn = file.path( outputdir, "weight_timeseries.png") )
    png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
      plot( mean ~ year, data=RES[[lab]], col="slategray", pch=19, lty=1, lwd=2.5, 
      type="b", ylab="Weight (kg)", xlab="", ylim=c(0, 2.75) )
      lines( lb025 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
      lines( ub975 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
    dev.off()
 

  saveRDS( RES, results_file, compress=TRUE )
  # RES = readRDS( results_file )
    

  if (0) {
    fit = carstm_model( p=p, DS="carstm_modelled_fit", sppoly=sppoly )
    names( fit$summary.random)
    res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale
    names( res[["random"]])
    # "time"  
    # "cyclic" 
    # "gear" sppoly$filter 
    # "inla.group(t, method = \"quantile\", n = 11)"
    # "inla.group(z, method = \"quantile\", n = 11)"
    # etc 
  }



# --------------------------------  
# Figure  3D plot of habitat vs temperature vs depth  via splines

  p = pH
  fn_root = "Predicted_habitat_probability"
  title = "Predicted habitat probability"
  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale

  o = carstm_2D_effects_probability( 
    res=res,
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)" 
  )
 
  # use a larger domain than sppoly for the following estimate:
  # sppoly is constrained to sampled locations, and so missing a lot of the inshore areas

  crs_plot = st_crs( p$aegis_proj4string_planar_km )
  domain = polygon_managementareas( species="maritimes" )
  domain = st_transform( domain, crs_plot )
  x11(); plot(domain[1])  
  
  o = carstm_optimal_habitat( 
    res = res,
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)",
    domain=domain, 
    depths=c(0, 80),   # from visual analysis (carstm_2D_effects_probability)
    temperatures=c(4,5) # from visual analysis (carstm_2D_effects_probability)
  ) 

  # estimate of surface area of optimal habitat (depth only):
  # SA = 63819 km^2  (max) 





# ---------------------------
# operating directly upon posterior samples:

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



