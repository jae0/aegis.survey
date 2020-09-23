
# ------------------------------------------------
# Atlantic cod comparison of CAR (ICAR/BYM) Poisson process models
# using sweptarea only
#
# Now we move to the use of ICAR-BYM type approach and environmental covariates.


# ---- using alternate strata -----------



# ------------------------------------------------
# load data common environment and parameter setting
# source( system.file( "scripts", "00_cod_comparisons_data_environment.R", package = "carstm") )

# --------------------------------
# construct basic parameter list defining the main characteristics of the study
# and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
# NOTE: the data selection is the same as in (01_cod_comparisons_basic_stranal.R)
p = carstm::carstm_parameters(
  id ="Atlantic cod summer standardtow",
  speciesname = "Atlantic_cod",
  groundfish_species_code = 10,   #  10= cod
  yrs = 1970:2017,
  areal_units_resolution_km = 25,
  trawlable_units = "towdistance"  # <<<<<<<<<<<<<<<<<<
  # trawlable_units = "standardtow"
  # trawlable_units = "sweptarea"
)



# --------------------------------
# parameter setting used to filter data via 'survey_db( DS="filter")'
# unlike stratanl, we do not need to remove strata until the last /aggregation step
p = aegis.survey::survey_parameters(
  p=p,
  selection=list(
    biologicals=list(
      spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=p$groundfish_species_code )
    ),
    survey=list(
      data.source="groundfish",
      yr = p$yrs,  # time frame for comparison specified above
      months=6:8,  # "summer"
      # dyear = c(150,250)/365, # alternate way of specifying season: summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
      settype = 1, # same as geartype in groundfish_survey_db
      gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
      polygon_enforce=TRUE,  # make sure mis-classified stations or incorrectly entered positions get filtered out
      ranged_data = c("dyear")  # not used .. just to show how to use range_data
    )
  )
)


# ------------------------------------------------
## using the "standard" polygon definitions  .. see https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.
# This adds some variabilty relative to "statanal" (which uses sa in sq nautical miles, btw)


sppoly = areal_units(
  areal_units_source="lattice",
  areal_units_resolution_km=p$areal_units_resolution_km,
  areal_units_overlay="groundfish_strata",
  aegis_internal_resolution_km=p$pres,
  sa_threshold_km2 = 1,
  spatial_domain=p$spatial_domain,
  areal_units_proj4string_planar_km=projection_proj4string("utm20")  # projection to compute areas
)
# further filtering can be done here .. .strata to use for aggregations
# sppoly$strata_to_keep = ifelse( as.character(sppoly$AUID) %in% strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ), FALSE,  TRUE )
sppoly$strata_to_keep = TRUE

sppoly = neighbourhood_structure( sppoly=sppoly )


# --------------------------------
# Get the data
p$selection$survey$strata_toremove = NULL  # emphasize that all data enters analysis initially ..

set = survey_db( p=p, DS="filter" )

# categorize Strata
o = over( SpatialPoints( set[,c("lon", "lat")], sp::CRS(projection_proj4string("lonlat_wgs84")) ), spTransform(sppoly, sp::CRS(projection_proj4string("lonlat_wgs84")) ) ) # match each datum to an area
set$AUID = o$AUID
o = NULL
set = set[ which(!is.na(set$AUID)),]

set$totno[which(!is.finite(set$totno))] = NA


# --------------------------------
# ensure we have some estimate of sweptarea and choose the appropriate
# one based upon which trawlable units we are using
ft2m = 0.3048
m2km = 1/1000
nmi2mi = 1.1507794
mi2ft = 5280
standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm
set$data_offset = switch( p$trawlable_units,
  standardtow =  rep(standardtow_sakm2, nrow(set)) , # "standard tow"
  towdistance = set$sa_towdistance,  # "sa"=computed from tow distance and standard width, 0.011801==),
  sweptarea = set$sa  # swept area based upon stand tow width and variable lenths based upon start-end locations wherever possible
)
set$data_offset[which(!is.finite(set$data_offset))] = median(set$data_offset, na.rm=TRUE )  # just in case missing data


# ------------------------------------------------
# update set with AUID factor variables and a few other repeatedly used variables
set$AUID = factor(set$AUID, levels=levels(sppoly$AUID))
set$yr_factor = factor(set$yr)
set$iid_error = 1:nrow(set) # for inla indexing
set$tag = "observations"


## --------------------------------
# construct meanweights matrix
weight_year = meanweights_by_arealunit( set=set, AUID=as.character( sppoly$AUID ), yrs=p$yrs, fillall=TRUE, annual_breakdown=TRUE )
# weight_year = weight_year[, match(as.character(p$yrs), colnames(weight_year) )]
# weight_year = weight_year[ match(as.character(sppoly$AUID), rownames(weight_year) )]





# adjust based upon RAM requirements and ncores
ncores = floor( ram_local( "ncores", ram_main=4, ram_process=6 ) / 2 )
inla.setOption(num.threads=ncores)
inla.setOption(blas.num.threads=ncores)



# RES = data.frame(yr=p$selection$survey[["yr"]]) # collect model comparisons
if (0) {
  fn = file.path( project.datadirectory( "carstm" ), "RES.rdata" )
  # save(RES, file=fn)
  # load(fn)
}


## ----------------------------------
# covariates of interest
covars = c("t", "tsd", "tmax", "tmin", "degreedays", "z",  "dZ", "ddZ", "substrate.grainsize" )

  # currently supported:
  # z = depth (m)
  # dZ = bottom slope (m/km)
  # ddZ = bottom curvature (m/km^2)
  # substrate.grainsize = mean grain size of bottom substrate (mm)
  # t = temperature (C) – subannual
  # tlb = temperature lower 95% bound (C) –subannual
  # tub = temperature upper 95% bound (C) –subannual
  # tmean = mean annual temperature
  # tsd = standard deviation of the mean annual temperature
  # tmin = minimum value of temperature in a given year – annual
  # tmax= maximum value of temperature in a given year – annual
  # tamplitude = amplitude of temperature swings in a year (tmax-tmin) – annual
  # degreedays = number of degree days in a given year – annual



# extract covariates and supplent survey data via lookups
set = aegis_db_lookup(
  X=set,
  lookupvars=covars,
  xy_vars=c("lon", "lat"),
  time_var="timestamp"
)



  extraction_method = "grid"

  if (extraction_method=="grid") {
    # collapse PS vars with time into APS (and regrid via raster)
    APS = aegis_db_extract(
      vars=covars,
      yrs=p$yrs,
      spatial_domain=p$spatial_domain,
      dyear=p$prediction_dyear,
      returntype="data.frame",
      areal_units_resolution_km=p$areal_units_resolution_km,
      aegis_proj4string_planar_km=sp::CRS(p$aegis_proj4string_planar_km)
    )
  }

  if (extraction_method=="polygon") {
    res = aegis_db_extract_by_polygon(
      sppoly=sppoly,
      vars=covars,
      spatial_domain=p$spatial_domain,
      yrs=p$yrs,
      dyear=0.6 # 0.6*12 months = 7.2 = early July
    )
    APS = aegis_prediction_surface( aegis_data=res$means  ) # merge data into prediction surface and add tags
  }


  APS$totno = NA
  APS$yr = as.numeric( APS$year)
  APS$data_offset = 1  # force to be density n/km^2
  APS$tag = "predictions"

  # APS = planar2lonlat(APS, p$aegis_proj4string_planar_km )

  # AUID reset to be consistent in both data and prediction areal units
  o = over( SpatialPoints( APS[,c("lon", "lat")], sp::CRS(projection_proj4string("lonlat_wgs84")) ), spTransform(sppoly, sp::CRS(projection_proj4string("lonlat_wgs84")) ) ) # match each datum to an area
  APS$AUID = o$AUID
  APS = APS[ which(!is.na(APS$AUID)),]

  o = NULL

  #  good data
  ok = which(
    is.finite(set[,"totno"]) &   # INLA can impute Y-data
    is.finite(set$data_offset) &
    !is.na(set$AUID)
  )





varstokeep = c( "totno", "AUID", "yr", "t", "tsd", "tmin", "tmax", "degreedays", "z", "dZ", "ddZ", "substrate.grainsize", "data_offset", "tag" )

M = rbind( set[ok, varstokeep], APS[,varstokeep] )

M = M[ which(
      is.finite(M$data_offset) &
      !is.na(M$AUID)
    ) , ]




M$t[!is.finite(M$t)] = median(M$t, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$tsd[!is.finite(M$tsd)] = median(M$tsd, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$tmin[!is.finite(M$tmin)] = median(M$tmin, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$tmax[!is.finite(M$tmax)] = median(M$tmax, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$degreedays[!is.finite(M$degreedays)] = median(M$degreedays, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$z[!is.finite(M$z)] = median(M$z, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$dZ[!is.finite(M$dZ)] = median(M$dZ, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$ddZ[!is.finite(M$ddZ)] = median(M$ddZ, na.rm=TRUE )  # missing data .. quick fix .. do something better
M$substrate.grainsize[!is.finite(M$substrate.grainsize)] = median(M$substrate.grainsize, na.rm=TRUE )  # missing data .. quick fix .. do something better




M$ti = discretize_data( M$t, p$discretization$t )
M$tisd = discretize_data( M$tsd, p$discretization$tsd )
M$timin = discretize_data( M$tmin, p$discretization$tmin )
M$timax = discretize_data( M$tmax, p$discretization$tmax )
M$di = discretize_data( M$t, p$discretization$degreedays )
M$zi = discretize_data( M$t, p$discretization$z )
M$zid = discretize_data( M$t, p$discretization$dZ )
M$zidd = discretize_data( M$t, p$discretization$ddZ )
M$si = discretize_data( M$t, p$discretization$substrate.grainsize )



M = M[ which(
      is.finite(M$ti) &
      is.finite(M$zi)
    ) , ]
M$yr_factor = factor( as.character(M$yr) )
M$AUID  = factor( as.character(M$AUID), levels=levels( sppoly$AUID ) )
M$strata  = as.numeric( M$AUID)
M$year  = as.numeric( M$yr_factor)
M$iid_error = 1:nrow(M) # for inla indexing for set level variation


M$pa = presence.absence( X={M$totno / M$data_offset}, px=0.05 )$pa  # determine presence absence and weighting


# ---------------------
# generic PC priors
m = log( {set$totno / set$data_offset}[ok] )
m[!is.finite(m)] = min(m[is.finite(m)])

H = carstm_hyperparameters( sd(m, na.rm=TRUE), alpha=0.5, median(m, na.rm=TRUE) )
# H$prec$prec.intercept = 1e-9




# ------------------------------------------------
# Model lattice 0:
# "INLA Envir iid|year	iid|strata"
# simple factorial with totno and poisson; 79 configs; 6 hrs

fit = inla(
  formula =
    totno ~ 1 + offset( log( data_offset) )
      + f(strata, model="iid", group=year, hyper=H$iid)
      + f(year, model="iid", hyper=H$iid )
      + f(iid_error, model="iid", hyper=H$iid)
      + f(ti, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
      + f(zi, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2),
  family = "poisson", # "zeroinflatedpoisson0",
  data= M,
  control.compute=list(cpo=TRUE, waic=TRUE, dic=TRUE, config=TRUE),
  control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  control.predictor=list(compute=FALSE, link=1 ),
  # control.fixed=H$fixed,  # priors for fixed effects, generic is ok
  # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
  num.threads=4,
  blas.num.threads=4,
  verbose=TRUE
)

if (0) {
  # /home/jae/bio/carstm/fit0_lattice.rdata
  fn0 = file.path( project.datadirectory( "carstm" ), "fit0_lattice.rdata" )
  # save( fit, file=fn0 )
  # load(fn0)
}

s = summary(fit)
s$dic$dic  # 33662
s$dic$p.eff # 6011

# Fixed effects:
#              mean   sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) 1.399 0.38      0.648      1.4      2.146 1.401   0

# Random effects:
#   Name	  Model
#     strata IID model
#    year IID model
#    iid_error IID model
#    ti RW2 model
#    zi RW2 model

# Model hyperparameters:
#                           mean      sd 0.025quant 0.5quant 0.975quant   mode
# Precision for strata     0.511   0.047      0.428    0.508      0.614  0.500
# GroupRho for strata      0.831   0.033      0.750    0.837      0.878  0.854
# Precision for year       1.519   0.284      1.021    1.501      2.129  1.468
# Precision for iid_error  0.384   0.011      0.367    0.383      0.410  0.377
# Precision for ti        73.936 177.837      7.600   31.052    403.217 12.648
# Precision for zi         3.862   2.582      0.586    3.319     10.214  1.764

# Expected number of effective parameters(stdev): 6229.08(17.18)
# Number of equivalent replicates : 1.38

# Deviance Information Criterion (DIC) ...............: 33662.06
# Deviance Information Criterion (DIC, saturated) ....: 14056.61
# Effective number of parameters .....................: 6011.19

# Marginal log-Likelihood:  -22766.99
# Posterior marginals for the linear predictor and
#  the fitted values are computed


plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )


# reformat predictions into matrix form
ii = which(M$tag=="predictions")
out = reformat_to_array(
  input = fit$summary.fitted.values[ ii, "mean" ],
  matchfrom = list( AUID=M$AUID[ii], yr_factor=M$yr_factor[ii]),
  matchto   = list( AUID=sppoly$AUID, yr_factor=factor(p$yrs) )
)
# out[ out>1e10] = NA
RES$space_iid.year_iid = colSums( {out * weight_year * sppoly$au_sa_km2}[sppoly$strata_to_keep, ], na.rm=TRUE )

lines( space_iid.year_iid ~ yr, data=RES, lty=1, lwd=2.5, col="blue", type="b")

# map it
vn = "pred"
yr = "2017"
slot(sppoly, "data")[,vn] = out[,yr] * weight_year[,yr]  # biomass density
brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )



# ------------------------------------------------
# Model lattice 1:
# "INLA Envir AR1|year iid|Strata"	ar1	rw2: temp+depth, no car just iid in space
# simple factorial with totno and poisson; 79 configs; 6 hrs

fit = inla(
  formula =
    totno ~ 1 + offset( log( data_offset) )
      + f(strata, model="iid", group=year, hyper=H$iid)
      + f(year, model="ar1", hyper=H$ar1 )
      + f(iid_error, model="iid", hyper=H$iid)
      + f(ti, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
      + f(zi, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2),
  family = "poisson", # "zeroinflatedpoisson0",
  data= M,
  control.compute=list(cpo=TRUE, waic=TRUE, dic=TRUE, config=TRUE),
  control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  control.predictor=list(compute=FALSE, link=1 ),
  # control.fixed=H$fixed,  # priors for fixed effects, generic is ok
  # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
  num.threads=4,
  blas.num.threads=4,
  verbose=TRUE
)


if (0) {
  fn1 = file.path( project.datadirectory( "carstm" ), "fit1_lattice.rdata" )
  # save( fit, file=fn1 )
  # load(fn1)
}

s = summary(fit)
s$dic$dic  # 33671
s$dic$p.eff # 5963

# Fixed effects:
#              mean    sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) 1.587 0.512       0.53    1.601      2.563 1.627   0

# Random effects:
#   Name	  Model
#     strata IID model
#    year AR1 model
#    iid_error IID model
#    ti RW2 model
#    zi RW2 model

# Model hyperparameters:
#                           mean     sd 0.025quant 0.5quant 0.975quant   mode
# Precision for strata     0.482  0.037      0.410    0.482      0.553  0.486
# GroupRho for strata      0.699  0.037      0.618    0.702      0.763  0.711
# Precision for year       2.619  0.775      1.410    2.516      4.429  2.322
# Rho for year             0.930  0.030      0.856    0.935      0.973  0.945
# Precision for iid_error  0.486  0.018      0.453    0.485      0.524  0.483
# Precision for ti        16.711 10.509      4.592   14.128     44.203 10.158
# Precision for zi         4.299  2.803      1.183    3.581     11.659  2.549

# Expected number of effective parameters(stdev): 6218.74(15.87)
# Number of equivalent replicates : 1.41

# Deviance Information Criterion (DIC) ...............: 33832.75
# Deviance Information Criterion (DIC, saturated) ....: 14151.68
# Effective number of parameters .....................: 5996.79

# Marginal log-Likelihood:  -22808.94
# Posterior marginals for the linear predictor and
#  the fitted values are computed

plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )


# reformat predictions into matrix form
ii = which(M$tag=="predictions")
out = reformat_to_array(
  input = fit$summary.fitted.values[ ii, "mean" ],
  matchfrom = list( AUID=M$AUID[ii], yr_factor=M$yr_factor[ii]),
  matchto   = list( AUID=sppoly$AUID, yr_factor=factor(p$yrs) )
)
# out[ out>1e10] = NA
RES$space_iid.year_ar1 = colSums( {out * weight_year * sppoly$au_sa_km2}[sppoly$strata_to_keep, ], na.rm=TRUE )

lines( space_iid.year_ar1 ~ yr, data=RES, lty=1, lwd=2.5, col="blue", type="b")

# map it
vn = "pred"
yr = "2017"
slot(sppoly, "data")[,vn] = out[,yr] * weight_year[,yr]  # biomass density
brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )




# ------------------------------------------------
#  Model lattice 2: CAR simple and year iid
# 46hr; 45 configs

fit = inla(
  formula = totno ~ 1
    + offset( log(data_offset) )
    + f(iid_error, model="iid", hyper=H$iid)
    + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(year, model="iid", hyper=H$iid)
    + f(strata, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family = "poisson",
  data=M,
  control.compute=list(cpo=TRUE, waic=TRUE, dic=TRUE, config=TRUE),
  control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  control.predictor=list(compute=TRUE, link =1 ), # compute=TRUE on each data location
  control.fixed=H$fixed,  # priors for fixed effects
  control.inla=list(  correct=TRUE, correct.verbose=FALSE ), # strategy="laplace", cutoff=1e-6,
  #num.threads=4,
  #blas.num.threads=4,
  verbose=TRUE
)
s = summary(fit)
s$dic$dic  #  33684
s$dic$p.eff # 5967

plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

if (0) {
  fn2 = file.path( project.datadirectory( "carstm" ), "fit2_lattice.rdata" )
  # save( fit, file=fn2 )
  # load(fn2)
}
# Fixed effects:
#              mean    sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) 1.747 0.554      0.547    1.792      2.694 1.895   0

# Random effects:
#   Name	  Model
#     iid_error IID model
#    ti RW2 model
#    zi RW2 model
#    year IID model
#    strata BYM2 model

# Model hyperparameters:
#                          mean    sd 0.025quant 0.5quant 0.975quant  mode
# Precision for iid_error 0.377 0.009      0.360    0.377      0.394 0.377
# Precision for ti        8.477 5.883      2.082    6.950     23.877 4.727
# Precision for zi        0.957 1.054      0.116    0.643      3.721 0.300
# Precision for year      1.280 0.275      0.780    1.271      1.851 1.261
# Precision for strata    0.411 0.044      0.334    0.409      0.505 0.402
# Phi for strata          0.991 0.010      0.964    0.994      1.000 0.999

# Expected number of effective parameters(stdev): 6183.49(15.01)
# Number of equivalent replicates : 1.39

# Deviance Information Criterion (DIC) ...............: 33683.79
# Deviance Information Criterion (DIC, saturated) ....: 14078.33
# Effective number of parameters .....................: 5967.35

# Marginal log-Likelihood:  -22512.83

# reformat predictions into matrix form
ii = which(M$tag=="predictions")
out = reformat_to_array(
  input = fit$summary.fitted.values[ ii, "mean" ],
  matchfrom = list( AUID=M$AUID[ii], yr_factor=M$yr_factor[ii] ),
  matchto   = list( AUID=sppoly$AUID, yr_factor=factor(p$yrs) )
)
# out[ out>1e10] = NA
RES$space_car.year_iid = colSums( {out * weight_year * sppoly$au_sa_km2}[sppoly$strata_to_keep, ], na.rm=TRUE )

lines( space_car.year_iid ~ yr, data=RES, lty=1, lwd=2.5, col="red", type="b" )


dev.new();
plot( fit$marginals.hyperpar$"Phi for strata", type="l")  # posterior distribution of phi nonspatial dominates


# map it ..mean density
vn = "pred"
slot(sppoly, "data")[,vn] = out[,"2017"]
brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )



# ------------------------------
# Model 3 Lattice  CAR in space grouped by year and ar1 in time (year)
# 81 configs and about 97 hrs!

fit = inla(
  formula = totno ~ 1
    + offset( log(data_offset) )
    + f(iid_error, model="iid", hyper=H$iid)
    + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(year, model="ar1", hyper=H$ar1)
    + f(strata, model="bym2", graph=slot(sppoly, "nb"), group = year, scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family = "poisson",
  data=M,
  control.compute=list(cpo=TRUE, waic=TRUE, dic=TRUE, config=TRUE),
  control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  control.predictor=list(compute=TRUE, link =1 ), # compute=TRUE on each data location
  control.fixed=H$fixed,  # priors for fixed effects
  control.inla=list(  correct=TRUE, correct.verbose=FALSE ), # strategy="laplace", cutoff=1e-6,
  verbose=TRUE
)

if (0) {
  fn3 = file.path( project.datadirectory( "carstm" ), "fit3_lattice.rdata" )
  # save( fit, file=fn3 )
  # load(fn3)
}

s = summary(fit)
s$dic$dic  #  33524
s$dic$p.eff #  5883


# Fixed effects:
#              mean    sd 0.025quant 0.5quant 0.975quant  mode   kld
# (Intercept) 1.346 0.846     -0.441    1.416      2.747 1.551 0.001

# Random effects:
#   Name	  Model
#     iid_error IID model
#    ti RW2 model
#    zi RW2 model
#    year AR1 model
#    strata BYM2 model

# Model hyperparameters:
#                           mean     sd 0.025quant 0.5quant 0.975quant  mode
# Precision for iid_error  0.457  0.016      0.427    0.457      0.491 0.455
# Precision for ti        20.494 27.129      3.755   12.438     86.279 6.599
# Precision for zi         1.109  2.131      0.068    0.525      5.776 0.168
# Precision for year       1.195  1.065      0.078    0.894      3.930 0.209
# Rho for year             0.970  0.028      0.895    0.979      0.998 0.996
# Precision for strata     0.330  0.038      0.264    0.327      0.413 0.320
# Phi for strata           0.978  0.025      0.909    0.986      0.999 0.998
# GroupRho for strata      0.761  0.042      0.671    0.763      0.836 0.768

# Expected number of effective parameters(stdev): 6103.29(10.74)
# Number of equivalent replicates : 1.41

# Deviance Information Criterion (DIC) ...............: 33523.51
# Deviance Information Criterion (DIC, saturated) ....: 13918.06
# Effective number of parameters .....................: 5882.96

# Marginal log-Likelihood:  -16469.56
# Posterior marginals for the linear predictor and
#  the fitted values are computed



# reformat predictions into matrix form
ii = which(M$tag=="predictions")
out = reformat_to_array(
  input = fit$summary.fitted.values[ ii, "mean" ],
  matchfrom = list( AUID=M$AUID[ii], yr_factor=M$yr_factor[ii]),
  matchto   = list( AUID=sppoly$AUID, yr_factor=factor(p$yrs) )
)
# out[ out>1e10] = NA
RES$space_car.year_ar1 = colSums( {out * weight_year * sppoly$au_sa_km2}[sppoly$strata_to_keep, ], na.rm=TRUE )

lines( space_car.year_ar1 ~ yr, data=RES, lty=1, lwd=2.5, col="red", type="b")


# map it ..mean density
vn = "pred"
slot(sppoly, "data")[,vn] = out[,"2017"]
brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )


plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )


plot( fit$marginals.hyperpar$"Phi for strata", type="l")  # posterior distribution of phi nonspatial dominates
plot( fit$marginals.hyperpar$"Precision for strata", type="l")
plot( fit$marginals.hyperpar$"Precision for setno", type="l")




#########################################
### PRESENCE-ABSENCE
#########################################




H = carstm_hyperparameters( sd(M$pa, na.rm=TRUE) )

# single car
# 3.25hrs; 45 configs
fit = inla(
  formula = pa ~ 1
  + f(iid_error, model="iid", hyper=H$iid)
  + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(year, model="iid", hyper=H$iid)
  + f(strata, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family="binomial",  # alternates family="zeroinflatedbinomial0", family="zeroinflatedbinomial1",
  data=M,
  control.family=list(control.link=list(model="logit")),
  control.compute=list(cpo=TRUE, waic=TRUE, dic=TRUE, config=TRUE),
  control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  control.predictor=list(compute=TRUE, link=1 ), # compute=TRUE on each data location
  control.fixed=H$fixed,  # priors for fixed effects
  control.inla=list(  correct=TRUE, correct.verbose=FALSE ), # strategy="laplace", cutoff=1e-6,
  verbose=TRUE
)

if (0) {
  fn4 = file.path( project.datadirectory( "carstm" ), "fit4_lattice.rdata" )
  save( fit, file=fn4 )
  # load(fn4)
}


plot(fit )
s = summary(fit)
s$dic$dic  # 8742
s$dic$p.eff # 301.5

# Fixed effects:
#               mean   sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) -0.263 0.35         -1   -0.245      0.373 -0.21   0

# Random effects:
#   Name	  Model
#     iid_error IID model
#    ti RW2 model
#    zi RW2 model
#    year IID model
#    strata BYM2 model

# Model hyperparameters:
#                            mean       sd 0.025quant 0.5quant 0.975quant   mode
# Precision for iid_error 321.732 1171.499      7.291   92.214   2026.074 16.637
# Precision for ti          8.700    8.321      1.387    6.275     30.555  3.409
# Precision for zi          2.215    2.537      0.281    1.456      8.738  0.697
# Precision for year        1.818    0.405      1.141    1.779      2.725  1.705
# Precision for strata      0.587    0.071      0.461    0.582      0.739  0.572
# Phi for strata            0.979    0.024      0.915    0.987      0.998  0.995

# Expected number of effective parameters(stdev): 308.74(30.77)
# Number of equivalent replicates : 28.41

# Deviance Information Criterion (DIC) ...............: 8741.62
# Deviance Information Criterion (DIC, saturated) ....: 8741.62
# Effective number of parameters .....................: 301.45

# Marginal log-Likelihood:  -4228.65
# Posterior marginals for the linear predictor and
#  the fitted values are computed


# reformat predictions into matrix form
ii = which(M$tag=="predictions")
out = reformat_to_array(
  input = fit$summary.fitted.values[ ii, "mean" ],
  matchfrom = list( AUID=M$AUID[ii], yr_factor=M$yr_factor[ii]),
  matchto   = list( AUID=sppoly$AUID, yr_factor=factor(p$yrs) )
)
# out[ out>1e10] = NA

RES$habitat_strata_CAR.yr_iid = colSums( {out * sppoly$au_sa_km2 }[sppoly$strata_to_keep,], na.rm=TRUE ) /sum(sppoly$au_sa_km2[sppoly$strata_to_keep]) # sa weighted average prob habitat

# map it
vn = "pred"
slot(sppoly, "data")[,vn] = out[,"2017"]
brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )


# -------------

# car by year
# 100 hrs; 79 configs
fit = inla(
  formula = pa ~ 1
  + f(iid_error, model="iid", hyper=H$iid)
  + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(year, model="iid", hyper=H$iid)
  + f(strata, model="bym2", graph=slot(sppoly, "nb"), group = year, scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family="binomial",  # alternates family="zeroinflatedbinomial0", family="zeroinflatedbinomial1",
  data=M,
  control.family=list(control.link=list(model="logit")),
  control.compute=list(dic=TRUE, config=TRUE),
  control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  control.predictor=list(compute=TRUE, link=1 ), # compute=TRUE on each data location
  control.fixed=H$fixed,  # priors for fixed effects
  control.inla=list(  correct=TRUE, correct.verbose=FALSE ), # strategy="laplace", cutoff=1e-6,
  verbose=TRUE
)

if (0) {
  fn5 = file.path( project.datadirectory( "carstm" ), "fit5_lattice.rdata" )
  save( fit, file=fn5 )
  # load(fn5)
}


plot(fit )
s = summary(fit)
s$dic$dic  # Inf
s$dic$p.eff # 301.5



# reformat predictions into matrix form
ii = which(M$tag=="predictions")
out = reformat_to_array(
  input = fit$summary.fitted.values[ ii, "mean" ],
  matchfrom = list( AUID=M$AUID[ii], yr_factor=M$yr_factor[ii]),
  matchto   = list( AUID=sppoly$AUID, yr_factor=factor(p$yrs) )
)
# out[ out>1e10] = NA

RES$habitat_strata_CAR_yr.yr_iid = colSums( {out * sppoly$au_sa_km2 }[sppoly$strata_to_keep,], na.rm=TRUE ) /sum(sppoly$au_sa_km2[sppoly$strata_to_keep]) # sa weighted average prob habitat

# map it
vn = "pred"
slot(sppoly, "data")[,vn] = out[,"2017"]
brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )


####################



dev.new(width=11, height=7)
col = c("slategray", "turquoise", "darkorange", "green", "blue", "darkred", "cyan", "darkgreen", "purple" )
pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20)
lty = c(1, 3, 4, 5, 6, 7, 1, 3, 4 )
lwd = c(4, 4, 4, 4, 4, 4, 4, 4, 4 )
type =c("l", "l", "l", "l", "l", "l", "l", "l", "l")
legend=c("Standard tow stratanal", "INLA Envir", "INLA Envir AR1", "INLA Envir CAR", "INLA Envir AR1 CAR", "INLA Envir AR1 CAR|year", "INLA Envir AR1|strata CAR", "INLA Envir AR1|strata CAR|year", "INLA Envir CAR|year")

plot( stratanal_towdistance  ~ yr, data=RES, lty=lty[1], lwd=lwd[1], col=col[1], pch=pch[1], type=type[1], ylim=c(0,0.46e9), xlab="Year", ylab="kg")
lines( INLA.Envir.1 ~ yr, data=RES, lty=lty[2], lwd=lwd[2], col=col[2], pch=pch[2], type=type[2])
lines( space_iid.year_iid ~ yr, data=RES, lty=lty[3], lwd=lwd[3], col=col[3], pch=pch[3], type=type[3])
lines( space_car.year_iid ~ yr, data=RES, lty=lty[4], lwd=lwd[4], col=col[4], pch=pch[4], type=type[4])  # yr_iid
lines( space_car.year_ar1 ~ yr, data=RES, lty=lty[5], lwd=lwd[5], col=col[5], pch=pch[5], type=type[5])
lines( space_car.year_ar1_year ~ yr, data=RES, lty=lty[6], lwd=lwd[6], col=col[6], pch=pch[6], type=type[6])
lines( INLA.Envir.AR1_strata.CAR ~ yr, data=RES, lty=lty[7], lwd=lwd[7], col=col[7], pch=pch[7], type=type[7])
lines( INLA.Envir.AR1_strata.CAR_year ~ yr, data=RES, lty=lty[8], lwd=lwd[8], col=col[8], pch=pch[8], type=type[8])
lines( INLA.Envir.yr_iid.CAR_year ~ yr, data=RES, lty=lty[9], lwd=lwd[9], col=col[9], pch=pch[9], type=type[9])



legend("topright", legend=legend, lty=lty, col=col, lwd=lwd )





### end
