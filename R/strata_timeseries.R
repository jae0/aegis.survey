strata_timeseries = function( set, gini_compute=FALSE, bootstrap_compute=FALSE, ... ) {
  
  # simple area-weighted sum with bootstrapped CI/means ... callled "stratanal" at BIO/St-Andrews
  # no autocorrelation in time/space, no covariates
  # gini estimates are not used and can be removed

  params = list(...)

  res = data.frame()
  
  vn = params[["variable"]]

  # convert kg ->  kt
  set[ , vn ] = set[ , vn ] / 10^6

  for (yr in params[["yrs"]]){
   
    i = which(set$yr == yr)

    if (length(i) < 2) next()

    yhi = split(set[ i, vn ], set[i,"strat"] ) #split the variable by strata

    nh = as.vector(sapply(yhi, length)) #numer of tows per strata
    nhws = sapply(yhi, function(x) length(x [x > 0])) #calculate the number of samples > 1 in each strata
    Nh = sapply( split(set[ i, "nh" ], set[i, "strat"] ), mean, na.rm=TRUE) #split the variable by strata to get trawalable units in each strata
    Nhsum = sum(Nh, na.rm=TRUE)

    Wh = Nh / Nhsum #strata percent of the total area in trawlable units

    #Calculate Stratified Estimates and Confidence intervals
    #-------------------------------------------------------------------------------------
    yh = as.vector(sapply(yhi, mean, na.rm=TRUE)) #mean of variable for each strata
    yst = sum(Wh * yh, na.rm = TRUE) #sum of the mean of the variable for each strata, multiplied by percent area of each strata

    sh = as.vector(sapply(yhi, var, na.rm=TRUE)) #calculate variance of each variable, per strata


    se.yst = sqrt(sum((((Nh * (Nh - nh))/Nhsum^2) * sh)/nh, na.rm = TRUE)) #calculate standard error
    ah = (Nh * (Nh - nh))/nh #
    df.yst = (sum(ah * sh, na.rm = TRUE)^2)/(sum(((ah * sh)^2)/(nh - 1), na.rm = TRUE)) #degrees of freedom

    #Calculate the confidence interval, based on the t distribution, rather than the normal distribution
    #qt fuction calculates the value for the 95% confidence interval by looking up the t distribution,
    #based on the degrees of freedom calculated above. This is multiplied by the standard error calculated above
    #formulas are: Lower limit = M - (tCL)(sM), Upper limit = M + (tCL)(sM)
    #------------------------------------------------------------------------------------
    ci.yst = yst + c(qt(params[["alpha.t"]]/2, df.yst), -qt(params[["alpha.t"]]/2, df.yst)) * se.yst #confidence interval

    dwao = sum(Wh*(nhws / nh), na.rm = TRUE) * sum(Nh, na.rm = TRUE) * 0.011801   #not sure what this is supposed to be

    #Calculate Design Weighted Area Occupied
    if (gini_compute) {
      # not required
      #Calculate Gini Index  .. Needed? JC?
      gi = NA
      gi = gini(x=yh, y=Nh)
    }

 
    # Use mirror match method (BWR) to calculate confidence intervals in Bias Corrected (BC) method to calculate confidence interval
    #-------------------------------------------------------------------------
    # call = match.call(expand = FALSE)

    if ( bootstrap_compute ) {
      nrs = 1:params[["nresamp"]]
      kh = (nh - 1)/(1 - nh/Nh)
      ph = ((1/kh) - (1/ceiling(kh)))/((1/floor(kh)) - (1/ceiling(kh)))
 
      if (gini_compute) {
        # not required
        out = matrix(0, nrow=params[["nresamp"]]+1, ncol=3, dimnames = list(c("Actual", nrs), c("Mean", "Variance",'gini')))
        out[1, ] = c(yst, (se.yst)^2, gi)
      } else {
        out = matrix(0, nrow=params[["nresamp"]]+1, ncol=2, dimnames = list(c("Actual", nrs), c("Mean", "Variance" )))
        out[1, ] = c(yst, (se.yst)^2 )
      }

      for (i in nrs) {
        yhib = bwr.boot(yhi, kh, ph, sample, replace = TRUE, simplify = FALSE)
        yhib[nh == 1] = yhi[nh == 1]
        nhws = sapply(yhib, FUN = function(x) sum(x > 0))
        n_yhib = as.vector(sapply(yhib, length))
        var_yhib = as.vector(sapply(yhib, var))
        mean_yhib = as.vector(sapply(yhib, mean))

        out[i + 1, 1] = sum(Wh * mean_yhib, na.rm = TRUE)  # mean`
        out[i + 1, 2] = sum( (((Nh * (Nh - n_yhib))/sum(Nh)^2) * var_yhib ) / n_yhib, na.rm = TRUE)      # variance
        if (gini_compute) {
          # not required
          out[i + 1, 3] = gini(x = mean_yhib, y = Nh)  # gini
        } 
      }

      orig.mean  = out[1, 1]
      orig.var   = out[1, 2]
      boot.means = out[nrs+1, 1]
      boot.vars  = out[nrs+1, 2]
    
      if (gini_compute) {
        # not required
        gi = out[nrs+1, 3]
        gini.mean = mean(gi)
        ci.boot.gini = quantile(gi, probs = c(params[["alpha.b"]]/2, (1 - params[["alpha.b"]]/2), 0.5), na.rm=T)
      }
    }


    #Summary of Bootstrapped means
    #-------------------------------------------------------------------------------------------------------
    options(digits = 4)

    if ( bootstrap_compute ) {

      boot.est = mean(boot.means) #bootstrap mean
      ci.boot=list()

      loc.bc = sum(boot.means < boot.est)
      lim.bc = sort(boot.means)[c(loc.bc, loc.bc + 1)]
      z0 = (loc.bc + ((boot.est - lim.bc[1])/(lim.bc[2] - lim.bc[1])))
      z0 = qnorm(z0/length(boot.means))
      probs.z0 = pnorm(qnorm(c(params[["alpha.b"]]/2, (1 - params[["alpha.b"]]/2), 0.5)) + 2 * z0)

      ci.boot.meanBCA = BCa(boot.means,0.01,alpha=c(0.025,0.975),mean)
      ci.boot[[1]] = ci.boot.mean = c(ci.boot.meanBCA[4:5], ci.boot.meanBCA[3])  #ci.boot.mean = quantile(boot.means, probs = probs.z0)
      
      if (gini_compute) {
        ci.boot[[2]] = ci.boot.gini
      }
    }

    #Print out the yearly estimates and write them to a data frame
    #--------------------------------------------------------------------------------------------------------
    if (0) {
      options(digits = max(options()$digits - 5, 5))

      if (params[["prints"]]) {cat(
        "\n",
        "Pop Total =", format(yst * Nhsum), "\n",
      #  "Original Mean =", format(orig.mean), "\n",
        "Year =", yr, "\n"
      #  "Original Variance =", format(orig.var), "\n",
      #  "Number of bootstraps = ", length(boot.means), "\n",
      #  "Bootstrap Mean=", format(boot.est), "\n",
      #  "Variance of Bootstrap Mean=", format(var(boot.means)), "\n",
      #  "CI Method=", c(params[["CI.method"]]), "\n",
      #  "CI's for alpha=", params[["alpha.b"]], "are ", format(ci.boot.mean[1:2]), "\n",
      #  "Length =", format(ci.boot.mean[2] - ci.boot.mean[1]), "\n",
      #  "Shape=", format(log((ci.boot.mean[2] - ci.boot.mean[3])/(ci.boot.mean[3] - ci.boot.mean[1]))), "\n",
      #  "Resample Method = ", params[["method"]], "\n")
      ) }
    }

  
    
    res_summary = data.frame(
      year = as.numeric(yr),
      Y = yst * Nhsum,
      Ylb = ci.yst[1]*Nhsum,
      Yub = ci.yst[2]*Nhsum,
      dwao = dwao
    )

    if ( bootstrap_compute ) {
      res_summary$orig.mean = as.numeric(format(orig.mean))
      res_summary$boot.mean = as.numeric(format(boot.est))
      res_summary$var.boot.mean = as.numeric(format(var(boot.means)))
      res_summary$lower.ci = as.numeric(format(ci.boot.mean[1]))
      res_summary$upper.ci = as.numeric(format(ci.boot.mean[2]))
      res_summary$length = as.numeric(format(ci.boot.mean[2] - ci.boot.mean[1]))

      if (gini_compute) {
        # not required
        res_summary$gini = gini.mean
        res_summary$lower.ci.gini = format(ci.boot.gini[1])
        res_summary$upper.ci.gini = format(ci.boot.gini[2])
      }
    }

    res = rbind(res_summary, res )

  }

  # library(zoo)
  #Calculate 3 yr Mean
  # res$mean.3.yr = zoo::rollapply(res$boot.mean, 3, mean, fill=NA, align="right")

  #Calculate Mean Lines
  res$median = median(res$boot.mean)
  # res$median.50 = median(res$boot.mean) * 0.5
  # res$gm.40 = geometric.mean (res$boot.mean) * 0.4
  
  res$speciesname=params[["speciesname"]]
  res$variable = vn

  print(res)

  attr( res, "units") = "kt"

  return(res)
}
