
survey_index = function( type="abundance", params, extrapolation_limit=NULL, extrapolation_replacement="extrapolation_limit", au_sa="au_sa_km2", redo=FALSE ) {

    # see snowcrab methods for more variations/details
   
     if (type=="abundance") {


      pN = params$pN
      pW = params$pW
      sppoly = params$sppoly

      M = survey_db( p=pN, DS="carstm_inputs", sppoly=sppoly, redo=redo )

      #size model
      if (redo) {
        fit = carstm_model( p=pW, data=M, redo_fit=TRUE, posterior_simulations_to_retain="predictions" ) # alt: control.inla = list( strategy='adaptive', int.strategy="eb" ) )
        fit = NULL; gc()
      }
      resw = carstm_model( p=pN, DS="carstm_modelled_summary" )
      wgts = resw[["predictions_posterior_simulations"]]
      wgts[!is.finite(wgts)] = NA

      # numerical model
      if (redo) {
        fit = carstm_model( p=pN, data=M, redo_fit=TRUE, posterior_simulations_to_retain="predictions", scale_offsets=TRUE )  #  scale_offsets when using offsets for more stable results
        # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
        fit = NULL; gc()
      }

      resn = carstm_model( p=pN, DS="carstm_modelled_summary" )
      nums = resn[[ "predictions_posterior_simulations" ]]   # numerical density (per km^2)
      nums[!is.finite(nums)] = NA

      if (is.na(extrapolation_limit)) extrapolation_limit = quantile( M$totno/M$data_offset, probs=pN$quantile_bounds[2], na.rm=T) # 10014.881

      uu = which( nums > extrapolation_limit )
      if (length(uu) > 0 ) {
        # about 2.9% have values greateer than reasonable
        if (is.character(extrapolation_replacement)) if (extrapolation_replacement=="extrapolation_limit" ) extrapolation_replacement = extrapolation_limit
        nums[ uu] = extrapolation_replacement
        warning("\n Extreme-valued predictions were found, capping them to max observed rates .. \n you might want to have more informed priors, or otherwise set extrapolation=NA to replacement value \n")
      }

      biom = nums * wgts / 10^6  # kg / km^2 -> kt / km^2
      nums = wgts = NULL

      # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
      sims = colSums( biom * sppoly[, au_sa], na.rm=TRUE )

      params["biomass_mean"] = apply( simplify2array(sims), 1, mean )
      params["biomass_sd"] = apply( simplify2array(sims), 1, sd )
      params["biomass_median"] = apply( simplify2array(sims), 1, median )
      params["biomass_lb"] = apply( simplify2array(sims), 1, quantile, probs=0.025 )
      params["biomass_ub"] = apply( simplify2array(sims), 1, quantile, probs=0.975 )
      attr( params, "units") = "kt / km^2"

      return(params)

    }

    if (type=="habitat") {

      pH = params$pH
      sppoly = params$sppoly

      M = survey_db( p=pH, DS="carstm_inputs", sppoly=sppoly, redo=redo )

      # numerical model
      if (redo) {
        fit = carstm_model( p=pH, data=M, redo_fit=TRUE, posterior_simulations_to_retain="predictions", scale_offsets=TRUE,
          control.family=list(control.link=list(model="logit"))
        )  #  scale_offsets when using offsets for more stable results
        # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
        fit = NULL; gc()
      }

      resh = carstm_model( p=pH, DS="carstm_modelled_summary" )
      pa =  resh[["predictions_posterior_simulations"]]
      pa[!is.finite(pa)] = NA
      #       pa = inverse.logit(pa)
      #       pa[!is.finite(pa)] = NA

      sims = colSums( pa *  sppoly[, au_sa]/ sum( sppoly[, au_sa]), na.rm=TRUE )

      params["habitat_mean"] = apply( simplify2array(sims), 1, mean )
      params["habitat_sd"] = apply( simplify2array(sims), 1, sd )
      params["habitat_median"] = apply( simplify2array(sims), 1, median )
      params["habitat_lb"] = apply( simplify2array(sims), 1, quantile, probs=0.025 )
      params["habitat_ub"] = apply( simplify2array(sims), 1, quantile, probs=0.975 )
      attr( params, "units") = "probability"

      return(params)
    }

}
