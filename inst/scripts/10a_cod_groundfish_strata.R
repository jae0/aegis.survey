
# ------------------------------------------------
# Atlantic cod 

# comparison of naive strata-based averages and carstm-based solutions using the same strata
# summer RV surveys only

# NOTE: This replicates standard groundfish strata-based estimation of means and totals
# "standard" random-stratified estimation functions (based on stratanal and bootstrap estimation techniques )


# set up the run parameters
require(aegis)
require(aegis.polygons)
require(aegis.survey)
require(carstm)


spatial_domain = "SSE"
yrs = 1970:2021
groundfish_survey_species_code = 10 # cod

global_output_directory = file.path( data_root, "aegis", "survey", "modelled", "Atlantic_cod" )
if ( !file.exists(global_output_directory)) dir.create( global_output_directory, recursive=TRUE, showWarnings=FALSE )

results_file = file.path( global_output_directory, "RES.RDS" )


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

# basic selection criteria for biologicals and sets 
selection = list(
  biologicals=list(
    spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
  ),
  survey=list(
    data.source="groundfish",
    yr = yrs,      # time frame for comparison specified above
    months=6:8,
    settype = 1,
    gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
    polygon_enforce=TRUE
  )
)
 
# auid to drop to mimic Michelle's extraction
auid_to_drop = strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ) 



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
  outputdir = file.path( global_output_directory, "stratanal" ),
  yrs=1970:2021,
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
  areal_units_overlay = "none",
  areal_units_timeperiod = "pre2014",    # "pre2014" for older
  selection = selection
)
 

# sppoly is used for "stratanal_designated_au" method .. which is the survey.db standard
sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84")   )


# bbox = c(-71.5, 41, -52.5,  50.5 )
additional_features = additional_features_tmap( 
    p=p, 
    isobaths=c( 10, 100, 200, 300, 500, 1000 ), 
    coastline =  c("canada"), 
    xlim=c(-80,-40), 
    ylim=c(38, 60) 
)


if (0) {
  # no data in these areal units: remove .. they seem to be US locations
  plot(sppoly["AUID"], reset=FALSE)
  plot(sppoly[which(sppoly$AUID %in% auid_to_drop), "AUID"], col="darkgray", add=TRUE )
}

# this is to match Michelle's extraction for "Summer RV" 
sppoly = sppoly[ -which( sppoly$AUID %in% auid_to_drop ), ]

plot(sppoly["AUID"] )

RES= list( yr = p$yrs )
for ( data_approach in c( "stratanal_direct", "stratanal_designated_au", "stratanal" ) ) {
for ( tu in c( "standardtow", "towdistance", "sweptarea" ) ) {  
  # c("Standard tow", "Length adjusted", "Length & width adjusted")
  set = stratanal_data( toget=data_approach, selection=selection, trawlable_units=tu, sppoly=sppoly )
  bi = strata_timeseries(
    set=set, variable="totwgt", speciesname=p[["speciesname"]], yrs=p$yrs,
    alpha.t = 0.05 # confidence interval for t-tdist assumption eg. 0.05 = 95%, 0.1 = 90%
  )
  mf = paste(data_approach, tu, sep=".")
  RES[[mf]] = data.frame( year=p$yrs )
  RES[[mf]] = merge( RES[[mf]], bi, by="year", all.x=TRUE, all.y=FALSE )
  RES[[mf]]$label = mf
}}

saveRDS( RES, results_file, compress=TRUE )
# RES = readRDS( results_file )
 

# --------- 
# comparative plots:

dev.new(width=11, height=7)
nvn = setdiff( names(RES), "yr" )
nv = 1:min(10, length(nvn))

col = c("slategray", "turquoise", "darkorange", "green", "blue", "darkred", "cyan", "darkgreen", "purple", "darkgray", "pink" )[nv]
pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20, 19, 23)[nv] 
lty = c(1, 3, 4, 5, 6, 7, 1, 3, 4, 5, 6 )[nv]
lwd = c(2, 4, 6, 2, 4, 6, 2, 4, 6, 5, 4 )[nv]
type =c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l") [nv]

plot( 0, 0, type="n", xlim=range(RES[["yr"]]), ylim=c(0, 280), xlab="Year", ylab="kt", main="Comparing input data treatment and sweptareas")
for (i in nv) {
  lines( mean ~ year, data=RES[[nvn[i]]], lty=lty[i], lwd=lwd[i], col=col[i], pch=pch[i], type=type[i])
}
legend("topright", legend=nvn, lty=lty, col=col, lwd=lwd )

# note: sweptarea methods have lower peak abundance
# note: stratanal discards areas with no sample .. they are zero-values vs carstm (estimates as a funciton of model) so the latter are smaller valued


dev.new(width=6, height=4)
hist( RES[["stratanal.towdistance"]]$mean / RES[["stratanal.standardtow"]]$mean, breaks=20 )

dev.new(width=6, height=4)
hist( RES[["stratanal.sweptarea"]]$mean / RES[["stratanal.standardtow"]]$mean, breaks=20 )

o = data.table(
  RES[["stratanal_direct.towdistance"]]$mean,   # testing stananal transcription 
  RES[["stratanal_direct.sweptarea"]]$mean,     # testing stananal transcription
  RES[["stratanal_direct.standardtow"]]$mean,   # testing stananal transcription
  RES[["stratanal.towdistance"]]$mean, # testing survey.db extraction sanity
  RES[["stratanal.sweptarea"]]$mean,  # testing survey.db extraction sanity 
  RES[["stratanal.standardtow"]]$mean, # testing survey.db extraction sanity
  RES[["stratanal_designated_au.towdistance"]]$mean, # testing survey.db and use of sppoly sanity
  RES[["stratanal_designated_au.sweptarea"]]$mean, # testing survey.db and use of sppoly sanity
  RES[["stratanal_designated_au.standardtow"]]$mean # testing survey.db and use of sppoly sanity
)

cor( o, use="pairwise.complete.obs" )  # all about the same

plot( o )


# as a check: these were Michelle's results: 
# derived from a base access of gcat without correction factors for boat, species, subsampling, etc) 
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
   year   mean     Ylb    Yub
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
    year  mean     Ylb    Yub
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
   year   mean     Ylb    Yub
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

dev.new(); plot( log(totno.mean) ~ log(totno.sd), V ); abline(0,1) ## looks like a Poisson or negative binomial will do..

# overall they look good, some minor variability due to species more sanity checking and imputation of strange data in survey.db 

### end basic stranal comparisons ### 
# ------------------------------------------------



 


# ------------------------------------------------
## Part 2: mimic stratanal with aegis.survey::survey_db and inla .. 

# NOTE:: large areal units so environmental covariates do not make sense 
# This is just a simple random effects models

# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.
# This adds some differences relative to "statanal" (which uses sa in sq nautical miles, btw)

# NOTE:: unlike stratanl, we do not need to remove strata until the last /aggregation step
 

# set up the run parameters
  
p = survey_parameters(
  project_class = "carstm",
  project_name="survey",  # "survey" == keyword used to bring in domain of maritimes boundaries groundfish surveys; otherwise use xydata
  speciesname = "Atlantic_cod",
  label ="Atlantic cod stratanal polygons",
  trawlable_units = "direct_number",  
  outputdir = file.path( p$modeldir, p$carstm_model_label ),
  carstm_model_label="Atlantic_cod_summer_RV_1970_present_stratanal_polygons_iid",   # default = 1970:present, alt: 1999_present 
  carstm_model_type="S_iid.T_iid",
  outputdir = file.path( global_output_directory, "stratanal_iid" ),
  yrs = yrs,
  variabletomodel = "totno",
  vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
  areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_timeperiod = "pre2014",    # "pre2014" for older
  selection = selection  # same as stratanal above
)

# reset sppoly to full domain
# instead of dropping right away, carry the data as it represents neighbourhood information and additional data
sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84")  ) 
sppoly$strata_to_keep = ifelse( sppoly$AUID %in% auid_to_drop, FALSE,  TRUE )

M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=FALSE, quantile_upper_limit=0.95, 
  fn=file.path( p$modeldir, p$speciesname, "carstm_inputs_stratanal_polygons_pre2014.rdata" ) )
ip = which(M$tag == "predictions")
io = which(M$tag == "observations")
iq = unique( c( which( M$totno > 0), ip ) ) # subset to positive definite data (for number and size)
iw = unique( c( which( M$totno > 30), ip ) ) # subset to positive definite data (for number and size) .. mean size needs to have at least 3 individuals
# M$data_offset[io] = M$data_offset[io] *10^6

pN = survey_parameter_list( p=p, mf=p$carstm_model_type, type="abundance" )
pW = survey_parameter_list( p=p, mf=p$carstm_model_type, type="meansize" )
pH = survey_parameter_list( p=p, mf=p$carstm_model_type, type="habitat" )
 
# size model
fit = NULL; gc()
fit = carstm_model( p=pW, data=M[iw,], sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
  num.threads="4:2", mc.cores=2 
)  

# numerical model
fit = NULL; gc()
fit = carstm_model( p=pN, data=M[iq,], sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
  num.threads="4:2", mc.cores=2, inla.mode="classic"
)  

# habitat model
fit = NULL; gc()
fit = carstm_model( p=pH, data=M, sppoly=sppoly, posterior_simulations_to_retain="predictions", 
  num.threads="4:2", mc.cores=2   
) 

fit = NULL; gc()

sims = carstm_posterior_simulations( pN=pN, pW=pW, pH=pH, sppoly=sppoly  )   
sims = sims / 10^6 # 10^6 kg -> kt;; kt/km^2
 

# this is to match Michelle's extraction for "Summer RV" for final aggregation and plotting 
sppoly$au_sa_km2[ which( !sppoly$strata_to_keep ) ] = 0  # set to zero those that are not of interest for aggregation

# aggregate_biomass_from_simulations
SM = colSums( sims * sppoly$au_sa_km2, na.rm=TRUE )

RES = readRDS( results_file )

RES[[p$carstm_model_type]] = data.frame( year = as.numeric(rownames(SM)) )
RES[[p$carstm_model_type]]$mean = apply( simplify2array(SM), 1, mean )
RES[[p$carstm_model_type]]$sd = apply( simplify2array(SM), 1, sd )
RES[[p$carstm_model_type]]$median = apply( simplify2array(SM), 1, median )
RES[[p$carstm_model_type]]$lb025 = apply( simplify2array(SM), 1, quantile, probs=0.025 )
RES[[p$carstm_model_type]]$ub975 = apply( simplify2array(SM), 1, quantile, probs=0.975 )
 
saveRDS( RES, results_file, compress=TRUE )
# RES = readRDS( results_file )
 
outputdir = file.path( carstm_filenames( pN, returnvalue="output_directory"), "aggregated_biomass_timeseries" )
if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

( fn = file.path( outputdir, "atlantic_cod_martimes.png") )
png( filename=fn, width=3072, height=2304, pointsize=12, res=300 )
  plot( mean ~ year, data=RES[[p$carstm_model_type]], lty="solid", lwd=4, pch=20, col="slateblue", type="b", ylab="Biomass index (kt)", xlab="")
  # lines( mean ~ year, data=RES[[p$carstm_model_type]], lty="solid", lwd=4, pch=20, col="green", type="b", ylab="Biomass index (kt)", xlab="")
  lines( lb025 ~ year, data=RES[[p$carstm_model_type]], lty="dotted", lwd=2, col="slategray" )
  lines( ub975 ~ year, data=RES[[p$carstm_model_type]], lty="dotted", lwd=2, col="slategray" )
dev.off()


# if you want to map it ..mean density

  vn = paste("biomass", "predicted", sep=".")

  outputdir = file.path( carstm_filenames( pN, returnvalue="output_directory"), "predicted_biomass_densitites" )

  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  B = apply( sims, c(1,2), mean ) 
  
  brks = pretty( log10( quantile( B[], probs=c(0.05, 0.95) )* 10^6)  )

  
  
  for (i in 1:length(pN$yrs) ){
    y = as.character( pN$yrs[i] )
    sppoly[,vn] = log10( B[,y]* 10^6 )
    outfilename = file.path( outputdir , paste( "biomass", y, "png", sep=".") )
    tmout =  carstm_map(  sppoly=sppoly, vn=vn,
        breaks=brks,
        additional_features=additional_features,
        title=paste( "log_10( Predicted biomass density; kg/km^2 )", y ),
        palette="-RdYlBu",
        plot_elements=c( "compass", "scale_bar", "legend" ), 
        outfilename=outfilename
    )
    tmout
    
  }

# end
# ------------------------------------------------





