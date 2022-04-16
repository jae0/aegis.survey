
# ------------------------------------------------
# Atlantic cod 

# comparison of naive strata-based averages and carstm-based solutions using the same strata
# summer RV surveys only

# NOTE: This replicates standard groundfish strata-based estimation of means and totals
# "standard" random-stratified estimation functions (based on stratanal and bootstrap estimation techniques )


# set up the run parameters
require(aegis)
require(aegis.survey)



# ------------------------------------------------
# PART 1 -- simple "stratanal" 

# construct basic parameter list defining the main characteristics of the study
# parameter setting used to filter data via 'survey_db( DS="filter")'
# specific selection params required for survey_db(DS="filter") data selection mechanism

p = survey_parameters(
  project_class = "stratanal",
  project_name="survey",  
  speciesname = "Atlantic_cod",
  trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
  carstm_model_label="Atlantic_cod_summer_RV_1970_present_stratanal",   # default = 1970:present, alt: 1999_present 
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
  areal_units_overlay = "none",
  areal_units_timeperiod = "pre2014",    # "pre2014" for older
  selection = selection
)

# sppoly is used for "stratanal_designated_au" method .. which is the survey.db standard
sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84")  )

if (0) {
  # no data in these areal units: remove .. they seem to be US locations
  plot(sppoly["AUID"], reset=FALSE)
  plot(sppoly[which(sppoly$AUID %in% auid_to_drop), "AUID"], col="green", add=TRUE )
}

# this is to match Michelle's extraction for "Summer RV" 
sppoly = sppoly[ -which( sppoly$AUID %in% auid_to_drop ), ]
# x11(); plot(sppoly["AUID"]

RES= list( yr = p$yrs )
for ( data_approach in c( "stratanal_direct", "stratanal_designated_au", "stratanal" ) ) {
for ( tu in c( "standardtow", "towdistance", "sweptarea") ) {  
  # c("Standard tow", "Length adjusted", "Length & width adjusted")
  bi = strata_timeseries(
    set=stratanal_data( toget=data_approach, selection=selection, trawlable_units=tu, sppoly=sppoly ),
    variable="totwgt", speciesname=p[["speciesname"]], yrs=p$yrs,
    alpha.t = 0.05 # confidence interval for t-tdist assumption eg. 0.05 = 95%, 0.1 = 90%
  )
  mf = paste(data_approach, tu, sep=".")
  RES[[mf]] = bi[ match(RES[["yr"]], bi$year), ]
  RES[[mf]]$label = mf
}}

outputdir = file.path( p$modeldir,  p$carstm_model_label )
if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )
results_file = file.path( outputdir, "RES_basic_stratanal.RDS" )

saveRDS( RES, results_file, compress=TRUE )
# RES = readRDS( results_file )
 

# --------- 
# comparative plots:

dev.new(width=11, height=7)
nvn = setdiff( names(RES), "yr" )
nv = 1:length(nvn)

col = c("slategray", "turquoise", "darkorange", "green", "blue", "darkred", "cyan", "darkgreen", "purple" )[nv]
pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20)[nv] 
lty = c(1, 3, 4, 5, 6, 7, 1, 3, 4 )[nv]
lwd = c(2, 4, 6, 2, 4, 6, 2, 4, 6 )[nv]
type =c("l", "l", "l", "l", "l", "l", "l", "l", "l") [nv]

plot( 0, 0, type="n", xlim=range(RES[["yr"]]), ylim=c(0, 3.2e2), xlab="Year", ylab="kt", main="Comparing input data treatment and sweptareas")
for (i in nv) {
  lines( Y ~ RES[["yr"]], data=RES[[nvn[i]]], lty=lty[i], lwd=lwd[i], col=col[i], pch=pch[i], type=type[i])
}
legend("topright", legend=nvn, lty=lty, col=col, lwd=lwd )
# note: sweptarea methods have lower peak abundance

dev.new(width=6, height=4)
hist( RES[["stratanal.towdistance"]]$Y / RES[["stratanal.standardtow"]]$Y, breaks=20 )

dev.new(width=6, height=4)
hist( RES[["stratanal.sweptarea"]]$Y / RES[["stratanal.standardtow"]]$Y, breaks=20 )

o = data.table(
  RES[["stratanal_direct.towdistance"]]$Y,   # testing stananal transcription 
  RES[["stratanal_direct.sweptarea"]]$Y,     # testing stananal transcription
  RES[["stratanal_direct.standardtow"]]$Y,   # testing stananal transcription
  RES[["stratanal.towdistance"]]$Y, # testing survey.db extraction sanity
  RES[["stratanal.sweptarea"]]$Y,  # testing survey.db extraction sanity 
  RES[["stratanal.standardtow"]]$Y, # testing survey.db extraction sanity
  RES[["stratanal_designated_au.towdistance"]]$Y, # testing survey.db and use of sppoly sanity
  RES[["stratanal_designated_au.sweptarea"]]$Y, # testing survey.db and use of sppoly sanity
  RES[["stratanal_designated_au.standardtow"]]$Y # testing survey.db and use of sppoly sanity
)

cor( o, use="pairwise.complete.obs" )  # all about the same

plot( o )


# as a check: these were Michelle's results: 
# derived from a base access of gcat without correction factors for boat, species, etc)  
# NOTE here they are in kg .. but they are now recorded as kt

#         year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length
# 2.5%    2017  14593959 totwgt_sd    3.4420    3.4258       3.61840   3.3099   3.5451 0.235210 21313 0.81003
# 2.5%34  2016  27531380 totwgt_sd    6.4932    6.3838      23.36500   6.0890   6.6869 0.597900 20779 0.89443
# 2.5%33  2015   8915342 totwgt_sd    2.1027    2.0970       0.17429   2.0716   2.1232 0.051683 24031 0.71914
# 2.5%32  2014  28570078 totwgt_sd    6.7382    6.8005      13.48700   6.5766   7.0328 0.456180 20416 0.88363
# 2.5%31  2013  12550459 totwgt_sd    2.9600    2.9837       1.13150   2.9189   3.0504 0.131470 24549 0.76574
# 2.5%30  2012   9538831 totwgt_sd    2.2497    2.2245       0.37251   2.1873   2.2630 0.075729 22789 0.76290
# 2.5%29  2011  35724538 totwgt_sd    8.4256    8.4033      20.51000   8.1265   8.6906 0.564150 24609 0.79815
# 2.5%28  2010  44532221 totwgt_sd   10.5030   10.3040      43.71900   9.9038  10.7220 0.817780 28273 0.83744

# these are with "standard tow" assumptions: 
# differences are likely due to "species-specific correction factors"
as.data.table(RES[["stratanal.standardtow"]])[ year %in% 2017:2010, ][ order(year, decreasing=T), 1:4]
   year      Y     Ylb    Yub
1: 2017 14.819  -5.712  35.35
2: 2016 21.425 -13.804  56.65
3: 2015  8.701   5.219  12.18
4: 2014 27.143  -8.456  62.74
5: 2013 12.283   2.258  22.31
6: 2012  9.101   2.816  15.39
7: 2011 34.485 -10.460  79.43
8: 2010 42.893 -28.528 114.31
 
# using towed distance
as.data.table(RES[["stratanal.towdistance"]])[ year %in% 2017:2010, ][ order(year, decreasing=T), 1:4]
    year      Y     Ylb    Yub
1: 2017 14.981  -5.396  35.36
2: 2016 23.727 -16.849  64.30
3: 2015  8.776   5.295  12.26
4: 2014 28.490  -7.601  64.58
5: 2013 12.428   2.440  22.42
6: 2012  9.335   3.007  15.66
7: 2011 35.664 -11.300  82.63
8: 2010 43.780 -28.081 115.64

# using sweptarea
as.data.table(RES[["stratanal.sweptarea"]])[ year %in% 2017:2010, ][ order(year, decreasing=T), 1:4]
   year      Y     Ylb    Yub
1: 2017 14.574  -5.888  35.04
2: 2016 20.672 -13.953  55.30
3: 2015  7.377   4.434  10.32
4: 2014 25.253  -9.145  59.65
5: 2013 10.286   1.342  19.23
6: 2012  8.833   2.857  14.81
7: 2011 34.814 -13.503  83.13
8: 2010 45.638 -34.375 125.65


set = stratanal_data(  p=p, toget="stratanal", selection=selection, trawlable_units="sweptarea", sppoly=sppoly )

set$strata_year = paste( set$AUID, set$yr, sep=".")
nn = applySummary( set[, c("strata_year", "totno")]  )

V = expand.grid( AUID=unique(set$AUID), yr=sort( unique(set$yr) ) )
V$strata_year = paste( V$AUID, V$yr, sep=".")
V = merge( V, nn, by="strata_year", all.x=TRUE, all.y=FALSE, suffixes=c("", ".totno") )

dev.new(); plot( log(totno.mean) ~ log(totno.sd), V ); abline(0,1) ## looks like a Poisson ..

# overall they look good, some minor variability due to species more sanity checking and imputation of strange data in survey.db 

### end basic stranal comparisons ### 



# ------------------------------------------------
## Part 2: mimic stranal with aegis.survey::survey_db with glm

# -- glm methods here


### end basic stranal glm comparisons ###





# ------------------------------------------------
# Part 3: Atlantic cod with a CAR (ICAR/BYM) Poisson process models

# using sweptarea only on groundfish polygons  with environmental covariates.
# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.
# This adds some differences relative to "statanal" (which uses sa in sq nautical miles, btw)
# NOTE:: unlike stratanl, we do not need to remove strata until the last /aggregation step
 

# set up the run parameters
  
p = survey_parameters(
  project_class = "carstm",
  project_name="survey",  # "survey" == keyword used to bring in domain of maritimes boundaries groundfish surveys; otherwise use xydata
  speciesname = "Atlantic_cod",
  label ="Atlantic cod stratanal polygons",
  trawlable_units = "towdistance",  
  carstm_model_label="Atlantic_cod_summer_RV_1970_present",   # default = 1970:present, alt: 1999_present 
  yrs = yrs,
  variabletomodel = "totno",
  vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
  areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_timeperiod = "pre2014",    # "pre2014" for older
  selection = selection
)

# reset sppoly to full domain
# instead of dropping right away, carry the data as it represents neighbourhood information and additional data
sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84")  )
sppoly$strata_to_keep = ifelse( sppoly$AUID %in% auid_to_drop, FALSE,  TRUE )
 
 
M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE, quantile_upper_limit=0.99, 
  fn=file.path( p$modeldir, p$speciesname, "carstm_inputs_tesselation.rdata" ) )


# drop data withough covariates 
i = which(!is.finite( rowSums(M[, .(z, t, pca1, pca2 ) ] )) )
au = unique( M$AUID[i] )

#M = M[ which( !(M$AUID %in% au )) , ]

#sppoly = sppoly[ which(! sppoly$AUID %in% au ), ] 
sppoly = areal_units_neighbourhood_reset( sppoly, snap=2 )


ip = which(M$tag == "predictions")
io = which(M$tag == "observations")

M$data_offset[ io ] = M$data_offset[ io ] * 1000

data_offset_prediction = median( M$data_offset[ io ]  )
M$data_offset[ ip ] = data_offset_prediction

mf =  "S_bym2.T_ar1.ST_bym2.env.eco"
mf =  "full_model"

pN = survey_parameter_list( mf=mf, p=p, type="abundance" )
pW = survey_parameter_list( mf=mf, p=p, type="meansize" )
pH = survey_parameter_list( mf=mf, p=p, type="habitat" )


# redo_model = TRUE
redo_model = FALSE

# size model
fit = carstm_model( p=pW, data=M, sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
  #theta= c( 0.088, 2.950, 0.943, 3.230, 3.676, 4.382, 3.781, 3.952, 3.313, 2.603, -0.044, 2.566, 3.194),
  control.inla = list( strategy='adaptive' ), 
  num.threads="4:2", mc.cores=2 
)  
fit = NULL; gc()

# numerical model
fit = carstm_model( p=pN, data=M, sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
  #theta=c(1.131, 0.767, 2.593, -0.659, -1.411, -1.689, -0.254, -2.234, 3.394, -2.381, -1.399, 0.371) ,
  control.inla = list( strategy='adaptive' ), 
  num.threads="4:2", mc.cores=2 
)  

# plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
fit = NULL; gc()

# habitat model
fit = carstm_model( p=pH, data=M, sppoly=sppoly, posterior_simulations_to_retain="predictions", 
  control.inla = list( strategy='adaptive' ), 
  num.threads="4:2", mc.cores=2   
) 
# plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
fit = NULL; gc()


# aggregate and extimate from posterior simulations
extrapolation_limits = list()
extrapolation_limits$wgts = quantile( M$meansize, probs=c(0.025, 0.975), na.rm=TRUE )
extrapolation_limits$nums = quantile( M$totno/M$data_offset, probs=c(0.025, 0.975), na.rm=TRUE )

sims = survey_estimates ( pW=pW, pN=pN, pH=pH, 
    sppoly=sppoly, extrapolation_limits=extrapolation_limits ) 

sims[["B"]]$space = sims$space
sims[["B"]]$time = sims$time
sims[["B"]]$dyears = sims$dyears



save( sims, file=results_file, compress=TRUE)   # load(results_file)     # store some of the aggregate timeseries in this list



if (0) {
 
  vn = "biomass"
  units = attr( sims[["B"]][[vn]], "units")
  plot( sims[["B"]][[vn]][["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="blue", type="b", main=mf, ylab=units, xlab="year" )
  lines( sims[["B"]][[vn]][["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="blue", type="b" )

  hist(  sims[["B"]][["biomass_simulations"]][1,] )  # posterior distributions
  # hist(  sims[["B"]][["biomass_subset_simulations"]][1,] ) 

  sims[["B"]][["biomass"]] # aggregate summaries 

  # map it
  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7
  background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 

  if ( "biomass" ) {
    fn_root = "Predicted_biomass"
    title = "Predicted biomass"
  }

  # if ( "number" ) {
  #   fn_root = "Predicted_numerical_abundance"
  #   title = "Predicted numerical abundance"
  # }

  # if ("habitat" ) {
  #   fn_root = "Predicted_habitat_probability"
  #   title = "Predicted habitat probability"
  # }

  # if ( "meansize" ) {
  #   fn_root = "Predicted_mean_weight"
  #   title = "Predicted mean weight"
  # }

    # managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )
    # time_match = "2020"
  
  vn = "predictions"
  brks = pretty(  quantile( sims[[vn]], probs=c(0.025,0.975), na.rm=TRUE )  )

  outputdir = file.path( pN$modeldir, pN$carstm_model_label, "predicted.numerical.densitites" )
  
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  # attr( sims[["B"]][["predictions"]], "units")

  for (y in res$year ){
    time_match = as.character(y) 
    
    fn = file.path( outputdir, paste(fn_root, "png", sep=".") )
    carstm_map(  res=sims[["B"]], vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      # breaks=brks,
      # palette="RdYlBu",
      title= "biomass density t/km^2" , #paste(fn_root, time_match, sep="_"),  
      # outfilename=fn,
      background = background,
      # vwidth = 1600,
      # vheight=1000,
      map_mode="view",
      tmap_zoom= c(map_centre, map_zoom)
      #plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" )
    )
  }

  # fit = carstm_model( p=pN, DS="carstm_modelled_fit", sppoly=sppoly  )  
  # res = carstm_model( p=pN, DS="carstm_modelled_summary", sppoly=sppoly )


}



### end


