survey_estimates = function( pW=NULL, pN=NULL, pH=NULL, sppoly=NULL, AUID_sa="au_sa_km2", extrapolation_limits=NULL, AUID_subset=NULL, AUID_sa_subset=NULL ) {

    out = list( pW=pW, pN=pN, pH=pH, sppoly=sppoly, extrapolation_limits=extrapolation_limits, AUID_sa="au_sa_km2", AUID_subset=AUID_subset, AUID_sa_subset=NULL )

    if (!is.null(pW)) {
      resw = carstm_model( p=pW, DS="carstm_modelled_summary", sppoly=sppoly )
      vars_to_copy = c(  "space", "time", "dyears" )
      for ( vn in vars_to_copy ) out[[vn]] = resw[[vn]]
      wgts = resw[["sims"]][["predictions"]]
      wgts[!is.finite(wgts)] = NA
      wgts[wgts<0] = NA
      if (!is.null(extrapolation_limits) ) {
        wgts[wgts < extrapolation_limits$wgts[1] ] = extrapolation_limits$wgts[1]
        wgts[wgts > extrapolation_limits$wgts[2] ] = extrapolation_limits$wgts[2]
      }
    }    

    if (!is.null(pN)) {
      resn = carstm_model( p=pN, DS="carstm_modelled_summary", sppoly=sppoly )
      vars_to_copy = c(  "space", "time", "dyears" )
      for ( vn in vars_to_copy ) out[[vn]] = resn[[vn]]
      nums = resn[["sims"]][["predictions"]]   # numerical density (per km^2)
      nums[!is.finite(nums)] = NA
      if (!is.null(extrapolation_limits) ) {
        nums[nums < extrapolation_limits$nums[1] ] = extrapolation_limits$nums[1]
        nums[nums > extrapolation_limits$nums[2] ] = extrapolation_limits$nums[2]
      }
    }

    if (!is.null(pH)) {
      resh = carstm_model( p=params$pH, DS="carstm_modelled_summary" )
      vars_to_copy = c(  "space", "time", "dyears" )
      for ( vn in vars_to_copy ) params[[vn]] = resh[[vn]]
      pa = resh[["sims"]][["predictions"]]
      # pa = inverse.logit(pa)
      pa[!is.finite(pa)] = NA
    }

    sa =  sppoly[[AUID_sa]] 
    attributes( sa ) = NULL

    if (!is.null(pW) & is.null(pN)) {
      # weights only
      out[["W"]] = resw  # already computed and redundant but here for completeness
      out[["W"]][["sims"]][["predictions"]] = wgts  # ower-write with filtered results
      if (!is.null(AUID_subset)) {
        ss = which( sppoly$AUID %in% AUID_subset )
        if (length(ss) > 0) {
          # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
          sa =   sppoly[[AUID_sa_subset]][ss] 
          attributes( sa ) = NULL
          bb = apply( wgts[ ss,, ] , c(2,3), function(u) u*sa )
          out[["W"]][["weights_subset_simulations"]] = apply( bb, c(2,3), sum, na.rm=TRUE )
          attr( out[["W"]][["weights_subset_simulations"]], "units") = "kg"
          out[["W"]][["weights_subset"]] = data.frame( cbind(
            mean = apply( out[["W"]][["weights_subset_simulations"]] , 1, mean, na.rm=TRUE ), 
            sd   = apply( out[["W"]][["weights_subset_simulations"]] , 1, sd , na.rm=TRUE), 
            median = apply( out[["W"]][["weights_subset_simulations"]] , 1, median, na.rm=TRUE ), 
            q025 = apply( out[["W"]][["weights_subset_simulations"]] , 1, quantile, probs=0.025, na.rm=TRUE ),
            q975 = apply( out[["W"]][["weights_subset_simulations"]] , 1, quantile, probs=0.975, na.rm=TRUE ) 
          ))
          attr( out[["W"]][["weights_subset"]], "units") = "kg"
          attr( out[["W"]][["weights_subset"]], "areal_units") = AUID_subset
        }
      }
    }

    if (is.null(pW) & !is.null(pN)) {
      # numbers only
      out[["N"]] = resn  # already computed and redundant but here for completeness
      out[["N"]][["sims"]][["predictions"]] = nums  # ower-write with filtered results
      if (!is.null(AUID_subset)) {
        ss = which( sppoly$AUID %in% AUID_subset )
        if (length(ss) > 0) {
          # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
          sa =   sppoly[[AUID_sa_subset]][ss] 
          attributes( sa ) = NULL
          bb = apply( nums[ ss,, ] , c(2,3), function(u) u*sa )
          out[["N"]][["number_subset_simulations"]] = apply( bb, c(2,3), sum, na.rm=TRUE )
          attr( out[["N"]][["number_subset_simulations"]], "units") = "number"  # total number per AUID
          out[["N"]][["number_subset"]] = data.frame( cbind(
            mean = apply( out[["N"]][["number_subset_simulations"]] , 1, mean, na.rm=TRUE ), 
            sd   = apply( out[["N"]][["number_subset_simulations"]] , 1, sd , na.rm=TRUE), 
            median = apply( out[["N"]][["number_subset_simulations"]] , 1, median, na.rm=TRUE ), 
            q025 = apply( out[["N"]][["number_subset_simulations"]] , 1, quantile, probs=0.025, na.rm=TRUE ),
            q975 = apply( out[["N"]][["number_subset_simulations"]] , 1, quantile, probs=0.975, na.rm=TRUE ) 
          ))
          attr( out[["N"]][["number_subset"]], "units") = "number"
          attr( out[["N"]][["number_subset"]], "areal_units") = AUID_subset
        }
      }

    }

    if (!is.null(pW) & !is.null(pN)) {
      biom = nums * wgts / 10^6  # kg / km^2 -> kt / km^2
      biom[!is.finite(biom)] = NA
      nums = wgts = NULL
      # create for mapping .. in t/km^2
      out[["B"]][["predictions"]] = resn[[ "predictions" ]] * NA
      out[["B"]][["predictions"]][,,1]  = apply( simplify2array(biom*1000), c(1,2), mean, na.rm=TRUE ) 
      out[["B"]][["predictions"]][,,2]  = apply( simplify2array(biom*1000), c(1,2), sd, na.rm=TRUE ) 
      out[["B"]][["predictions"]][,,3]  = apply( simplify2array(biom*1000), c(1,2), quantile, probs=0.025, na.rm=TRUE ) 
      out[["B"]][["predictions"]][,,4]  = apply( simplify2array(biom*1000), c(1,2), median, na.rm=TRUE )
      out[["B"]][["predictions"]][,,5]  = apply( simplify2array(biom*1000), c(1,2), quantile, probs=0.975, na.rm=TRUE ) 
      attr( out[["B"]][["predictions"]], "units") = "t / km^2"
      sims = colSums( biom * sppoly[[AUID_sa]], na.rm=TRUE ) 
      bb = apply( biom , c(2,3), function(u) u*sa )
      out[["B"]][["biomass_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
      attr( out[["B"]][["biomass_simulations"]], "units") = "kt"
      out[["B"]][["biomass"]] = data.frame( cbind(
        mean = apply( simplify2array(sims), 1, mean, na.rm=TRUE ), 
        sd   = apply( simplify2array(sims), 1, sd , na.rm=TRUE), 
        median = apply( simplify2array(sims), 1, median, na.rm=TRUE ), 
        q025 = apply( simplify2array(sims), 1, quantile, probs=0.025, na.rm=TRUE ),
        q975 = apply( simplify2array(sims), 1, quantile, probs=0.975, na.rm=TRUE ) 
      ))
      attr( out[["B"]][["biomass"]], "units") = "kt"
      if (!is.null(AUID_subset)) {
        ss = which( sppoly$AUID %in% AUID_subset )
        if (length(ss) > 0) {
          # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
          sa =   sppoly[[AUID_sa_subset]][ss] 
          attributes( sa ) = NULL
          bb = apply( biom[ ss,, ] , c(2,3), function(u) u*sa )
          out[["B"]][["biomass_subset_simulations"]] = apply( bb, c(2,3), sum, na.rm=TRUE )
          attr( out[["B"]][["biomass_subset_simulations"]], "units") = "kt"
          out[["B"]][["biomass_subset"]] = data.frame( cbind(
            mean = apply( out[["B"]][["biomass_subset_simulations"]] , 1, mean, na.rm=TRUE ), 
            sd   = apply( out[["B"]][["biomass_subset_simulations"]] , 1, sd , na.rm=TRUE), 
            median = apply( out[["B"]][["biomass_subset_simulations"]] , 1, median, na.rm=TRUE ), 
            q025 = apply( out[["B"]][["biomass_subset_simulations"]] , 1, quantile, probs=0.025, na.rm=TRUE ),
            q975 = apply( out[["B"]][["biomass_subset_simulations"]] , 1, quantile, probs=0.975, na.rm=TRUE ) 
          ))
          attr( out[["B"]][["biomass_subset"]], "units") = "kt"
          attr( out[["B"]][["biomass_subset"]], "areal_units") = AUID_subset
        }
      }
    }
  

    if (!is.null(pH)) {
      # create for mapping .. in probability
      out[["H"]][["predictions"]] = resh[[ "predictions" ]] * NA
      out[["H"]][["predictions"]][,,1]  = apply( simplify2array(pa), c(1,2), mean, na.rm=TRUE ) 
      out[["H"]][["predictions"]][,,2]  = apply( simplify2array(pa), c(1,2), sd, na.rm=TRUE ) 
      out[["H"]][["predictions"]][,,3]  = apply( simplify2array(pa), c(1,2), quantile, probs=0.025, na.rm=TRUE ) 
      out[["H"]][["predictions"]][,,4]  = apply( simplify2array(pa), c(1,2), median, na.rm=TRUE )
      out[["H"]][["predictions"]][,,5]  = apply( simplify2array(pa), c(1,2), quantile, probs=0.975, na.rm=TRUE ) 
      attr( out[["H"]][["predictions"]], "units") = "probability"
  
      # if subsetting then use appropriate SA other than total sa (is. sa associated with a given management unit)
      sims = colSums( pa * sppoly[[AUID_sa]] / sum(  sppoly[[AUID_sa]] ), na.rm=TRUE ) 


      sa = sppoly[[AUID_sa]] 
      attributes( sa ) = NULL
      bb = apply( pa , c(2,3), function(u) u*sa )
      out[["H"]][["habitat_simulations"]]  = apply( bb, c(2,3), sum, na.rm=TRUE )
      attr( out[["H"]][["habitat_simulations"]], "units") = "probability"

      out[["H"]][["habitat"]] = data.frame( cbind(
        mean = apply( simplify2array(sims), 1, mean, na.rm=TRUE ), 
        sd   = apply( simplify2array(sims), 1, sd , na.rm=TRUE), 
        median = apply( simplify2array(sims), 1, median, na.rm=TRUE ), 
        q025 = apply( simplify2array(sims), 1, quantile, probs=0.025, na.rm=TRUE ),
        q975 = apply( simplify2array(sims), 1, quantile, probs=0.975, na.rm=TRUE ) 
      ))
      attr( out[["H"]][["habitat"]], "units") = "probability"
      
      if (!is.null(AUID_subset)) {
        ss = which( sppoly$AUID %in% AUID_subset )
        if (length(ss) > 0) {
          sa =   sppoly[[AUID_sa]][ss] 
          attributes( sa ) = NULL
          bb = apply( pa[ ss,, ] , c(2,3), function(u) u*sa )

          out[["H"]][["habitat_subset_simulations"]] = apply( bb, c(2,3), sum, na.rm=TRUE )
          attr( out[["H"]][["habitat_subset_simulations"]], "units") = "probability"

          out[["H"]][["habitat_subset"]] = data.frame( cbind(
            mean = apply( out[["H"]][["habitat_subset_simulations"]] , 1, mean, na.rm=TRUE ), 
            sd   = apply( out[["H"]][["habitat_subset_simulations"]] , 1, sd , na.rm=TRUE), 
            median = apply( out[["H"]][["habitat_subset_simulations"]] , 1, median, na.rm=TRUE ), 
            q025 = apply( out[["H"]][["habitat_subset_simulations"]] , 1, quantile, probs=0.025, na.rm=TRUE ),
            q975 = apply( out[["H"]][["habitat_subset_simulations"]] , 1, quantile, probs=0.975, na.rm=TRUE ) 
          ))
          attr( out[["H"]][["habitat_subset"]], "units") = "probability"
          attr( out[["H"]][["habitat_subset"]], "areal_units") = AUID_subset
        }
      }
    }

    return(out)

}