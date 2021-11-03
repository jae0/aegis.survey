
survey_index = function( params, M, extrapolation_limit=NULL, extrapolation_replacement="extrapolation_limit", au_sa="au_sa_km2", redo_model=TRUE, redo_sppoly=FALSE, redo_surveydata=FALSE ) {

    # see snowcrab methods for more variations/details
    if (0) {
      params$type="abundance"
      extrapolation_limit=NULL
      extrapolation_replacement="extrapolation_limit"
      au_sa="au_sa_km2"
      redo_model=TRUE
      redo_sppoly=FALSE 
      redo_surveydata=FALSE
    }

  # parameter list can be varied: either pW (meansize) and pN (numbes) or pB (biomass) or pH (habitat) 
  pci = NULL
  if (params$type=="abundance") pci = params$pN
  if (params$type=="meansize")  pci = params$pW
  if (params$type=="biomass")   pci = params$pB
  if (params$type=="habitat")   pci = params$pH
  if (is.null(pci)) stop("parameter list is not correct ...")

 
  sppoly = areal_units( p=pci, duplications_action="separate", redo=redo_sppoly )  # separate ids for each new sub area
  sppoly$strata_to_keep = TRUE
  # sppoly$strata_to_keep = ifelse( as.character(sppoly$AUID) %in% strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ), FALSE,  TRUE )
      # plot(  sppoly["AUID"])
  params$sppoly = sppoly 
 
  M = survey_db( p=params, DS="carstm_inputs", sppoly=sppoly, redo=redo_surveydata )
  params$M = M

  if (is.null(extrapolation_limit)) {
    if (exists("quantile_bounds", params$pN )) {
      extrapolation_limit = quantile( M$totno/M$data_offset, probs=params$pN$quantile_bounds[2], na.rm=T) # 10014.881
    }
  }

  # So fiddling is required as extreme events can cause optimizer to fail
  ql = c(0, 0.999)  # truncate 99.9% bound

  nn = M$totno/M$data_offset
  qn = quantile( nn, ql, na.rm=TRUE )
  ni = which( nn > qn[2] )

  ww = M$totwgt/M$data_offset
  qw = quantile( ww, ql, na.rm=TRUE )
  wi = which( ww > qw[2] )

  mm = M$totno 
  qm = quantile( mm, ql, na.rm=TRUE )
  mi = which( mm > qm[2] )

  vv = M$totwgt 
  qv = quantile( vv, ql, na.rm=TRUE )
  vi = which( vv > qv[2] )

  # repeatedly extreme events
  ii = intersect( ni, wi )
  ii = intersect( ii, mi )
  ii = intersect( ii, vi )

  M$totno[ii] = floor( qm[2] )
  M$totwgt[ii] =   qv[2] 

  # assuming data_offset is not too extreme: 
  # M[i,]
  #      AUID          tag  data_offset totno       z substrate.grainsize year       dyear    t          pca1         pca2
  # 298   444 observations 0.0462881453  2889 35.6616        1.4415774964 1973 0.561596271 5.97 -0.1539538038 0.0580442368
  # 1017  450 observations 0.0323656969 12850 43.8912        0.3866592625 1982 0.560549848 2.54 -0.1978402951 0.1038139127
  # 1780  456 observations 0.0367693481  1848 56.6928        1.4131698521 1989 0.565877093 3.13 -0.0519987927 0.0716135259
  # 2200  444 observations 0.0397392372  3930 40.2336        0.9403246868 1993 0.565053272 4.13 -0.1828536016 0.1527385957
  # 3595  455 observations 0.0378602602  2069 45.7200        0.0122716563 2006 0.579807839 7.15 -0.1283472829 0.0953725754
 
  if (params$type=="biomass") {
  # operating directly upon biomass (as a lognormal)
  
  }

  if (params$type=="meansize") {
  # operating on measnize as a gaussian
  
  }


  if (params$type=="abundance") {
  # operate upon numbers as a poisson and meansize as a gaussian

    if (redo_model) {
      # size model
      fit = carstm_model( p=params$pW, data=M, redo_fit=TRUE, posterior_simulations_to_retain="predictions", 
        control.inla = list( strategy='adaptive'  ), num.threads="4:2", mc.cores=2 )  
      fit = NULL; gc()
 

 browser()
 
      # numerical model
      fit = carstm_model( p=params$pN, data=M, redo_fit=TRUE, posterior_simulations_to_retain="predictions", scale_offsets=TRUE, 
        control.inla = list( strategy='adaptive' ), 
        num.threads="4:2", mc.cores=2 )  
        #  scale_offsets when using offsets for more stable results
        # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
      fit = NULL; gc()
    }
 

    resw = carstm_model( p=params$pW, DS="carstm_modelled_summary" )
    resn = carstm_model( p=params$pN, DS="carstm_modelled_summary" )
 

    vars_to_copy = c(  "space", "time", "dyears" )
    for ( vn in vars_to_copy ) params[[vn]] = resn[[vn]]


    wgts = resw[["predictions_posterior_simulations"]]
    wgts[!is.finite(wgts)] = NA
    wgts[wgts<0] = NA

    nums = resn[[ "predictions_posterior_simulations" ]]   # numerical density (per km^2)
    nums[!is.finite(nums)] = NA


    if (!is.null(extrapolation_limit)) {

      uu = which( nums > extrapolation_limit )
      if (length(uu) > 0 ) {
        # about 2.9% have values greateer than reasonable
        if (is.character(extrapolation_replacement)) if (extrapolation_replacement=="extrapolation_limit" ) extrapolation_replacement = extrapolation_limit
        nums[ uu] = extrapolation_replacement
        warning("\n Extreme-valued predictions were found, capping them to max observed rates .. \n you might want to have more informed priors, or otherwise set extrapolation=NA to replacement value \n")
      }
    }
    
    if (!is.null(extrapolation_replacement)) {
      uu = which( nums > extrapolation_limit )
      if (length(uu)>0) nums[ uu] = extrapolation_replacement
    }

    biom = nums * wgts / 10^6  # kg / km^2 -> kt / km^2
    biom[!is.finite(biom)] = NA
    nums = wgts = NULL

    # create for mapping ..
    params[["predictions"]] = resn[[ "predictions" ]] * NA
    params[["predictions"]][,,1]  = apply( simplify2array(biom*1000), c(1,2), mean, na.rm=TRUE ) 
    params[["predictions"]][,,2]  = apply( simplify2array(biom*1000), c(1,2), sd, na.rm=TRUE ) 
    params[["predictions"]][,,3]  = apply( simplify2array(biom*1000), c(1,2), quantile, probs=0.025, na.rm=TRUE ) 
    params[["predictions"]][,,4]  = apply( simplify2array(biom*1000), c(1,2), median, na.rm=TRUE )
    params[["predictions"]][,,5]  = apply( simplify2array(biom*1000), c(1,2), quantile, probs=0.975, na.rm=TRUE ) 
    attr( params[["predictions"]], "units") = "t / km^2"
 
    # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
    sims = colSums( biom * sppoly[[au_sa]], na.rm=TRUE )

    params[["biomass"]] = data.frame( cbind(
      mean = apply( simplify2array(sims), 1, mean, na.rm=TRUE ), 
      sd   = apply( simplify2array(sims), 1, sd , na.rm=TRUE), 
      median = apply( simplify2array(sims), 1, median, na.rm=TRUE ), 
      q025 = apply( simplify2array(sims), 1, quantile, probs=0.025, na.rm=TRUE ),
      q975 = apply( simplify2array(sims), 1, quantile, probs=0.975, na.rm=TRUE ) 
    ))
    
    attr( params[["biomass"]], "units") = "kt"

    return(params)

  }

  if (params$type=="habitat") {

    # numerical model
    if (redo_model) {
      fit = carstm_model( p=params$pH, data=M, redo_fit=TRUE, posterior_simulations_to_retain="predictions", scale_offsets=TRUE,
        control.family=list(control.link=list(model="logit"))
      )  #  scale_offsets when using offsets for more stable results
      # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
      fit = NULL; gc()
    }

    resh = carstm_model( p=params$pH, DS="carstm_modelled_summary" )
    pa =  resh[["predictions_posterior_simulations"]]
    pa[!is.finite(pa)] = NA

    #       pa = inverse.logit(pa)
    #       pa[!is.finite(pa)] = NA

    # might as well just keep it in resh 
    # params[["spatial_combined"]] = resn[[ "spatial_combined" ]]   
    # attr( params[["spatial_combined"]], "units") = "probability"
 
    # params[["predictions"]] = resn[[ "predictions" ]]   
    # attr( params[["predictions"]], "units") = "probability"

    sims = colSums( pa * sppoly[[au_sa]]/ sum(  sppoly[[au_sa]] ), na.rm=TRUE )

    params[["habitat"]] = data.frame( cbind(
      mean = apply( simplify2array(sims), 1, mean ), 
      sd   = apply( simplify2array(sims), 1, sd ), 
      median = apply( simplify2array(sims), 1, median ), 
      q025 = apply( simplify2array(sims), 1, quantile, probs=0.025 ),
      q975 = apply( simplify2array(sims), 1, quantile, probs=0.975 ) 
    ))
     
    attr( params[["habitat"]], "units") = "probability"

    return(params)
  }

}
