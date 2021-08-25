
# Example analysis to model and create probablity distributions of species in some geometry
# borrows from CARstan .. see 00_example.R for standard examples.

# this is a spatial-temporal example with multiple stations in AU's and unique TU's temporal components



  require(sf)
  require(cmdstanr)

  require(aegis.polygons)
  require(carstm)

  year.assessment=2017
  p = aegis.survey::survey_parameters( yrs=1970:year.assessment )

  # set up default map projection
  # p$areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")   # oblique mercator, centred on Scotian Shelf rotated by 325 degrees
  p$areal_units_type="inla_mesh"
  p$areal_units_overlay="none"
  p$areal_units_proj4string_planar_km=projection_proj4string("utm20")
  p$areal_units_resolution_km = 50  # length scale to base mesh sizes
  p$inputdata_spatial_discretization_planar_km = 1
  p$areal_units_timeperiod = "pre2014"

  p$boundingbox = list( xlim = c(-70.5, -56.5), ylim=c(39.5, 47.5)) # bounding box for plots using spplot
  p = c(p, aegis.coastline::coastline_layout( p=p ) )
  p$mypalette=RColorBrewer::brewer.pal(9, "YlOrRd")


  sppoly = areal_units( p=p, xydata=survey_db(p=p, DS="areal_units_input"), areal_units_type="inla_mesh", sa_threshold_km2=10, redo=TRUE )
  sppoly = areal_units( p=p, xydata=survey_db(p=p, DS="areal_units_input"), areal_units_type="tesselation", sa_threshold_km2=10, redo=TRUE )

  if (0) {
    plot(sppoly[,"AUID"], col="orange")
    plot( slot(sppoly, "nb"), coords=st_centroid(st_geometry( as(sppoly, "sf")) ), add=T, col="green" )
  }


groundfish_species_code = 10  #  10= cod

p$selection=list(
  label = "Cod summer standard tow",  # a label
  trawlable_units = "standardtow", # choices: "standardtow", "towdistance", "sweptarea"  # basis for estimating densities
  biologicals=list(
    spec = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_species_code )
  ),
  survey=list(
    data.source="groundfish",
    yr = 1999:year.assessment,      # time frame for comparison
    dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
    settype = 1,
    gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
    strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # strata to remove from standard strata-based analysis
    polygon_enforce=TRUE,
    ranged_data="dyear"
  )
)


set = survey_db( p=p, DS="filter", add_groundfish_strata=TRUE )
set$tag = "observations"

crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
sppoly = st_transform(sppoly, crs=crs_lonlat )

set$AUID = st_points_in_polygons(
  pts = st_as_sf( set, coords=c("lon","lat"), crs=crs_lonlat ),
  polys = sppoly[, "AUID"],
  varname="AUID"
)

set$uid = paste(set$AUID, set$year, set$dyear_discret, sep=".")


    APS = as.data.frame(sppoly)
    APS$AUID = as.character( APS$AUID )
    APS$tag ="predictions"
    APS[,p$variabletomodel] = NA


    BI = carstm_model ( p=pB, DS="carstm_modelled_summary" )
    jj = match( as.character( APS$AUID), as.character( BI$AUID) )
    APS[, pB$variabletomodel] = BI[[ paste(pB$variabletomodel,"predicted",sep="." ) ]] [jj]
    jj =NULL
    BI = NULL

    SI = carstm_model ( p=pS, DS="carstm_modelled_summary" )
    jj = match( as.character( APS$AUID), as.character( SI$AUID) )
    APS[, pS$variabletomodel] = SI[[ paste(pS$variabletomodel,"predicted",sep="." )]] [jj]
    jj =NULL
    SI = NULL

    # to this point APS is static, now add time dynamics (teperature)
    # ---------------------

    vn = c( p$variabletomodel, pB$variabletomodel,  pS$variabletomodel, "tag", "AUID" )
    APS = APS[, vn]

    # expand APS to all time slices
    n_aps = nrow(APS)
    APS = cbind( APS[ rep.int(1:n_aps, p$nt), ], rep.int( p$prediction_ts, rep(n_aps, p$nt )) )
    names(APS) = c(vn, "tiyr")
    APS$year = floor( APS$tiyr)
    APS$dyear = APS$tiyr - APS$year


    TI = carstm_model ( p=pT, DS="carstm_modelled_summary" )
    TI = TI[[ paste(pT$variabletomodel,"predicted",sep="." )]]
    au_map = match( APS$AUID, dimnames(TI)$AUID )
    year_map = match( as.character(APS$year), dimnames(TI)$year )
    dyear_breaks = c(p$dyears, p$dyears[length(p$dyears)]+ diff(p$dyears)[1] )
    dyear_map = as.numeric( cut( APS$dyear, breaks=dyear_breaks, include.lowest=TRUE, ordered_result=TRUE, right=FALSE ) )
    dindex = cbind(au_map, year_map, dyear_map )
    APS[, pT$variabletomodel] = TI[ dindex]
    TI = NULL


    PI = carstm_model ( p=pPC1, DS="carstm_modelled_summary" )
    PI = PI[[ paste(pPC1$variabletomodel,"predicted",sep="." )]]
    au_map = match( APS$AUID, dimnames(PI)$AUID )
    year_map = match( as.character(APS$year), dimnames(PI)$year )
    dindex = cbind(au_map, year_map )
    APS[, pPC1$variabletomodel] = PI [dindex]
    PI = NULL

    PI = carstm_model ( p=pPC2, DS="carstm_modelled_summary" )
    PI = PI[[ paste(pPC2$variabletomodel,"predicted",sep="." )]]
    au_map = match( APS$AUID, dimnames(PI)$AUID )
    year_map = match( as.character(APS$year), dimnames(PI)$year )
    dindex = cbind(au_map, year_map  )
    APS[, pPC2$variabletomodel] = PI [dindex]
    PI = NULL

    # useful vars to have for analyses outside of carstm_model
    varstoadd = c( "totwgt", "totno", "sa", "data_offset",  "zn", "qn" )

    for (vn in varstoadd) if (!exists( vn, APS)) APS[,vn] = NA
    APS$data_offset = 1  # force to solve for unit area

    M = rbind( set[, names(APS)], APS )
    APS = NULL


fit = carstm_model( p=pB, data=M, DS="redo", carstm_model_label="test"  ) # run model and obtain predictions








  ## method 2: # using the "traditional" polygons
  ## see https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf

  areal_units_timeperiod = "pre2014"  # "pre2014" for older
  sppoly = maritimes_groundfish_strata( areal_units_timeperiod=areal_units_timeperiod  )
  sppoly = st_transform(sppoly, st_crs(p$areal_units_proj4string_planar_km) )
  sppoly$au_sa_km2 = st_area(sppoly)  # (km^2 -> km^2)
  sppoly = st_transform(sppoly, st_crs(projection_proj4string("lonlat_wgs84")) )
  plot(sppoly)

  W.nb = maritimes_groundfish_strata( areal_units_timeperiod=areal_units_timeperiod, returntype="neighbourhoods" )
  W = spdep::nb2mat(W.nb, style="B", zero.policy=TRUE) # adjacency matrix ; B = binary ; W=row standardized etc

  library(rgeos)
  # sset = maritimes_groundfish_strata_identify( Y=sset, sppoly=sppoly, xyvars=c("lon", "lat"), planar_crs_km=p$areal_units_proj4string_planar_km, plotdata=TRUE )
  sset = sset[ which(!is.na(sset$AUID)), ]

  sdat = sset[ sset$yr %in% c(2010, 2011, 2012) ,]
  sdat$dummy = 1
  sdat$sa[which(!is.finite(sdat$sa))] = median(sdat$sa, na.rm=TRUE )
  sdat$totno = as.integer( sdat$totno * sdat$sa) # storeed as density .. convert back to counts
  nd = which(!is.finite(sdat$totno))
  sdat$totno[nd] = 0


  # sparse form of the CAR
  # X = model.matrix( ~ c(scale(sdat$dummy)) )
  X = model.matrix( ~ sdat$dummy -1 )

  DATAINPUT = c( list(
    nAU = nrow(sppoly),  # number of Areal Units
    nTU = length(unique(sdat$yr)),   # number of Temporal Units
    K = nrow(X),         # number of observations
    L = ncol(X),         # number of coefficients
    Wn = sum(W) / 2,     # number of neighbor pairs
    X = X,               # design matrix
    Y = as.integer( sdat$totno),               # observed number of cases
    log_offset = log(sdat$sa * 1000*1000) ,         # log(expected) num. cases / km^2
    AU = as.integer(as.factor(sdat$AUID)), # numeric codes of strata
    TU = as.integer(as.factor(sdat$yr)), # numerical value of time untis
    W = W,               # adjacency matrix
    scaling_factor = bym_scaling_factor(W.nb) ), # additional parameters required by the bym_scaled model (topology and variance scale factor)
    nb2edge(W.nb)
  )



stcode = stan_model_code( "bym_scaled_heirarchical_ar1_nonseparable" )  # derived from basic scaled form with multiple samples per AU permitted, # ~ 800 sec = 6 min
# stcode = stan_model_code( "bym_scaled_heirarchical_ar1_separable" )  # derived from basic scaled form with multiple samples per AU permitted, # ~ 800 sec = 6 min

# -----------------------------

  MS = stan_initialize( stan_code=stan_model_code( "bym_scaled_heirarchical_ar1_nonseparable" ) )
  MS$compile()

  fit = MS$sample(
      data=DATAINPUT,
      iter_warmup = 1000,
      iter_sampling = 1000,
      chains=4,               # number of Markov chains
      cores = 4,               # number of cores (using 2 just for the vignette)
      adapt_delta = 0.975, 
      max_treedepth=14,
      verbose=TRUE ); # ~ 1 min per chain

  pars = c('B[1]',  'rho[1,1]', 'sigma[1,1]',  'mu[1,1]', 'ar1[1]', 'lp__')

  print(fit, pars =pars)
  traceplot(fit, pars = pars)   # visualize results

  M = stan_extract( as_draws_df( fit$draws() ) )

  yhat = apply( M[["yhat"]], 2, mean )

  plot( yhat ~ log(DATAINPUT$Y+0.1) )
  cor(yhat , log(DATAINPUT$Y+0.1)  ) #  0.5772 ; R^2=0.333


  log_lik1 <- extract_log_lik(fit, merge_chains = FALSE)
  rel_n_eff <- relative_eff(exp(log_lik1))
  fit_loo_1 = loo(log_lik1, r_eff = rel_n_eff, cores = 2)

fit_loo_1

# Computed from 2000 by 144 log-likelihood matrix
#
#          Estimate     SE
# elpd_loo  -2579.8  488.9
# p_loo      5358.5 1122.7
# looic      5159.5  977.9
# ------
# Monte Carlo SE of elpd_loo is NA.
#
# Pareto k diagnostic values:
#                          Count Pct.    Min. n_eff
# (-Inf, 0.5]   (good)     77    53.5%   106
#  (0.5, 0.7]   (ok)        9     6.2%   99
#    (0.7, 1]   (bad)       8     5.6%   2
#    (1, Inf)   (very bad) 50    34.7%   0
#
#   compare( fit_loo_1 , fit_loo_2 )




  tu  = 1 # time unit
  sppoly$mu = apply( M[["mu"]][,,tu], 2, mean )

  vn = "mu"
  brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
  spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent", xlim=p$boundingbox$xlim, ylim=p$boundingbox$ylim )

  dev.new()

  tu  = 2 # time unit
  sppoly$mu = apply( M[["mu"]][,,tu], 2, mean )
  vn = "mu"
  brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
  spplot( as(sppoly, "Spatial"), vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent", xlim=p$boundingbox$xlim, ylim=p$boundingbox$ylim )
