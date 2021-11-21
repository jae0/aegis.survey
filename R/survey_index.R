
survey_index = function( params, M, extrapolation_limit=NULL, sppoly=NULL, au_sa="au_sa_km2", subset_data=NULL, redo_model=TRUE ) {

    # see snowcrab methods for more variations/details
    if (0) {
      params$type="abundance"
      extrapolation_limit=NULL
      au_sa="au_sa_km2"
      redo_model=TRUE
    }
 
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
      fit = carstm_model( p=params$pW, data=M, sppoly=sppoly, redo_fit=TRUE, posterior_simulations_to_retain="predictions", 
        control.inla = list( strategy='adaptive'   ), num.threads="4:2", mc.cores=2 )  
      fit = NULL; gc()
 
      # numerical model
      fit = carstm_model( p=params$pN, data=M, sppoly=sppoly, redo_fit=TRUE, posterior_simulations_to_retain="predictions", scale_offsets=TRUE, 
        control.inla = list( strategy='adaptive'  ), 
        num.threads="4:2", mc.cores=2 )  
        #  scale_offsets when using offsets for more stable results
        # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
      fit = NULL; gc()
    }

    resw = carstm_model( p=params$pW, DS="carstm_modelled_summary", sppoly=sppoly )
    resn = carstm_model( p=params$pN, DS="carstm_modelled_summary", sppoly=sppoly )

    vars_to_copy = c(  "space", "time", "dyears" )
    for ( vn in vars_to_copy ) params[[vn]] = resn[[vn]]

    wgts = resw[["predictions_posterior_simulations"]]
    wgts[!is.finite(wgts)] = NA
    wgts[wgts<0] = NA

    if (!is.null(extrapolation_limit) ) {
      wgts_limits = quantile( M$meansize, probs=extrapolation_limit, na.rm=TRUE )
    } else {
      wgts_limits = range(M$meansize, na.rm=TRUE)
    }

    wgts[wgts < wgts_limits[1] ] = wgts_limits[1]
    wgts[wgts > wgts_limits[2] ] = wgts_limits[2]
    
    nums = resn[[ "predictions_posterior_simulations" ]]   # numerical density (per km^2)
    nums[!is.finite(nums)] = NA

    if (!is.null(extrapolation_limit) ) {
      nums_limits = quantile( M$totno/M$data_offset, probs=extrapolation_limit, na.rm=TRUE )
    } else {
      nums_limits = range( M$totno/M$data_offset, na.rm=TRUE)
    }

    nums[nums < nums_limits[1] ] = nums_limits[1]
    nums[nums > nums_limits[2] ] = nums_limits[2]
   
    biom = nums * wgts / 10^6  # kg / km^2 -> kt / km^2
    biom[!is.finite(biom)] = NA

    if (!is.null(extrapolation_limit) ) {
      biom_limits = quantile( M$totwgt/M$data_offset, probs=extrapolation_limit, na.rm=TRUE )
    } else {
      biom_limits = range( M$totwgt/M$data_offset, na.rm=TRUE)
    }
    biom[biom < biom_limits[1] ] = biom_limits[1]
    biom[biom > biom_limits[2] ] = biom_limits[2]

    nums = wgts = NULL

    # create for mapping .. in t/km^2
    params[["predictions"]] = resn[[ "predictions" ]] * NA
    params[["predictions"]][,,1]  = apply( simplify2array(biom*1000), c(1,2), mean, na.rm=TRUE ) 
    params[["predictions"]][,,2]  = apply( simplify2array(biom*1000), c(1,2), sd, na.rm=TRUE ) 
    params[["predictions"]][,,3]  = apply( simplify2array(biom*1000), c(1,2), quantile, probs=0.025, na.rm=TRUE ) 
    params[["predictions"]][,,4]  = apply( simplify2array(biom*1000), c(1,2), median, na.rm=TRUE )
    params[["predictions"]][,,5]  = apply( simplify2array(biom*1000), c(1,2), quantile, probs=0.975, na.rm=TRUE ) 
    attr( params[["predictions"]], "units") = "t / km^2"
 
    # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
    sims = colSums( biom * sppoly[[au_sa]], na.rm=TRUE ) 

    sa =   sppoly[[au_sa]] 
    attributes( sa ) = NULL
    bb = apply( biom , c(2,3), function(u) u*sa )
    params[["biomass_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
    attr( params[["biomass_simulations"]], "units") = "kt"

    params[["biomass"]] = data.frame( cbind(
      mean = apply( simplify2array(sims), 1, mean, na.rm=TRUE ), 
      sd   = apply( simplify2array(sims), 1, sd , na.rm=TRUE), 
      median = apply( simplify2array(sims), 1, median, na.rm=TRUE ), 
      q025 = apply( simplify2array(sims), 1, quantile, probs=0.025, na.rm=TRUE ),
      q975 = apply( simplify2array(sims), 1, quantile, probs=0.975, na.rm=TRUE ) 
    ))
    attr( params[["biomass"]], "units") = "kt"
    
    
    if (!is.null(subset_data)) {
      ss = which( sppoly$AUID %in% subset_data )
      if (length(ss) > 0) {
        sa =   sppoly[[au_sa]][ss] 
        attributes( sa ) = NULL
        bb = apply( biom[ ss,, ] , c(2,3), function(u) u*sa )

        params[["biomass_subset_simulations"]] = apply( bb, c(2,3), sum, na.rm=TRUE )
        attr( params[["biomass_subset_simulations"]], "units") = "kt"

        params[["biomass_subset"]] = data.frame( cbind(
          mean = apply( params[["biomass_subset_simulations"]] , 1, mean, na.rm=TRUE ), 
          sd   = apply( params[["biomass_subset_simulations"]] , 1, sd , na.rm=TRUE), 
          median = apply( params[["biomass_subset_simulations"]] , 1, median, na.rm=TRUE ), 
          q025 = apply( params[["biomass_subset_simulations"]] , 1, quantile, probs=0.025, na.rm=TRUE ),
          q975 = apply( params[["biomass_subset_simulations"]] , 1, quantile, probs=0.975, na.rm=TRUE ) 
        ))
        attr( params[["biomass_subset"]], "units") = "kt"
        attr( params[["biomass_subset"]], "areal_units") = subset_data
      }
    }

    return(params)

  }

  # -------------------

  if (params$type=="habitat") {

    # numerical model
    if (redo_model) {
      fit = carstm_model( p=params$pH, data=M, sppoly=sppoly, redo_fit=TRUE, posterior_simulations_to_retain="predictions", scale_offsets=TRUE,
        control.family=list(control.link=list(model="logit"))
      ) 
      # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
      fit = NULL; gc()
    }

    resh = carstm_model( p=params$pH, DS="carstm_modelled_summary" )

    vars_to_copy = c(  "space", "time", "dyears" )
    for ( vn in vars_to_copy ) params[[vn]] = resh[[vn]]

    pa = resh[["predictions_posterior_simulations"]]
    # pa = inverse.logit(pa)
    pa[!is.finite(pa)] = NA

    # might as well just keep it in resh 
    # params[["spatial_combined"]] = resh[[ "spatial_combined" ]]   
    # attr( params[["spatial_combined"]], "units") = "probability"
 
    # create for mapping .. in probability
    params[["predictions"]] = resh[[ "predictions" ]] * NA
    params[["predictions"]][,,1]  = apply( simplify2array(pa), c(1,2), mean, na.rm=TRUE ) 
    params[["predictions"]][,,2]  = apply( simplify2array(pa), c(1,2), sd, na.rm=TRUE ) 
    params[["predictions"]][,,3]  = apply( simplify2array(pa), c(1,2), quantile, probs=0.025, na.rm=TRUE ) 
    params[["predictions"]][,,4]  = apply( simplify2array(pa), c(1,2), median, na.rm=TRUE )
    params[["predictions"]][,,5]  = apply( simplify2array(pa), c(1,2), quantile, probs=0.975, na.rm=TRUE ) 
    attr( params[["predictions"]], "units") = "probability"
 
    # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
    sims = colSums( pa * sppoly[[au_sa]] / sum(  sppoly[[au_sa]] ), na.rm=TRUE ) 

    sa = sppoly[[au_sa]] 
    attributes( sa ) = NULL
    bb = apply( pa , c(2,3), function(u) u*sa )
    params[["habitat_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
    attr( params[["habitat_simulations"]], "units") = "probability"

    params[["habitat"]] = data.frame( cbind(
      mean = apply( simplify2array(sims), 1, mean, na.rm=TRUE ), 
      sd   = apply( simplify2array(sims), 1, sd , na.rm=TRUE), 
      median = apply( simplify2array(sims), 1, median, na.rm=TRUE ), 
      q025 = apply( simplify2array(sims), 1, quantile, probs=0.025, na.rm=TRUE ),
      q975 = apply( simplify2array(sims), 1, quantile, probs=0.975, na.rm=TRUE ) 
    ))
    attr( params[["habitat"]], "units") = "probability"
    
    if (!is.null(subset_data)) {
      ss = which( sppoly$AUID %in% subset_data )
      if (length(ss) > 0) {
        sa =   sppoly[[au_sa]][ss] 
        attributes( sa ) = NULL
        bb = apply( pa[ ss,, ] , c(2,3), function(u) u*sa )

        params[["habitat_subset_simulations"]] = apply( bb, c(2,3), sum, na.rm=TRUE )
        attr( params[["habitat_subset_simulations"]], "units") = "probability"

        params[["habitat_subset"]] = data.frame( cbind(
          mean = apply( params[["habitat_subset_simulations"]] , 1, mean, na.rm=TRUE ), 
          sd   = apply( params[["habitat_subset_simulations"]] , 1, sd , na.rm=TRUE), 
          median = apply( params[["habitat_subset_simulations"]] , 1, median, na.rm=TRUE ), 
          q025 = apply( params[["habitat_subset_simulations"]] , 1, quantile, probs=0.025, na.rm=TRUE ),
          q975 = apply( params[["habitat_subset_simulations"]] , 1, quantile, probs=0.975, na.rm=TRUE ) 
        ))
        attr( params[["habitat_subset"]], "units") = "probability"
        attr( params[["habitat_subset"]], "areal_units") = subset_data
      }
    }

    return(params)
  }

}
