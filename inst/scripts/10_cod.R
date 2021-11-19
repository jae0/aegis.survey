
# ------------------------------------------------
# Atlantic cod comparison of naive strata-based averages and carstm-based solutions

# NOTE: This replicates standard groundfish strata-based estimation of means and totals
# "standard" random-stratified estimation functions (based on stratanal and bootstrap estimation techniques )



# set up the run parameters
require(aegis.survey)

spatial_domain = "SSE"
yrs = 1970:2021
groundfish_survey_species_code = 10 # cod

# basic selection criteria
selection = list(
  biologicals=list(
    spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
  ),
  survey=list(
    data.source="groundfish",
    yr = yrs,      # time frame for comparison specified above
    months=6:8,
    # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
    # ranged_data="dyear"
    settype = 1,
    gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
    strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
    polygon_enforce=TRUE
  )
)

# construct basic parameter list defining the main characteristics of the study
# parameter setting used to filter data via 'survey_db( DS="filter")'
# specific selection params required for survey_db(DS="filter") data selection mechanism

p = survey_parameters(
  project_class = "stratanal",
  project_name="survey",  
  label ="Atlantic cod summer",
  speciesname = "Atlantic_cod",
  trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
  carstm_model_label="stratnal",   # default = 1970:present, alt: 1999_present 
  selection = selection,
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
  areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
  areal_units_overlay = "none",
  areal_units_timeperiod = "pre2014"    # "pre2014" for older
)

results_file = file.path( p$modeldir, p$speciesname , "RES_basic_stratanal.rdata" )

RES= list( yr = yrs )



# --------------------------------
# do stratanl for each of the following swept-area assumptions:
sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84")  )
  
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

  save(RES, file=results_file, compress=TRUE )    
  # load( results_file )
  
  
  # ------------------

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
  hist( RES[["stratanal_towdistance"]]$Y / RES[["stratanal_standardtow"]]$Y, breaks=20 )

  dev.new(width=6, height=4)
  hist( RES[["stratanal_sweptarea"]]$Y / RES[["stratanal_standardtow"]]$Y, breaks=20 )

  o = cbind(
    RES[["stratanal_towdistance"]]$Y,
    RES[["stratanal_sweptarea"]]$Y,
    RES[["stratanal_standardtow"]]$Y
  )

  cor( o, use="pairwise.complete.obs" )

  plot( o )



# # ------------------------------------------------
# # these are Michelle's results: (base access of gcat without correction factors for boat, species, etc)  
# NOTE here they are in kg .. but they are now recorded as kt

#         speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length
# 2.5%   COD ATLANTIC 2017  14593959 totwgt_sd    3.4420    3.4258       3.61840   3.3099   3.5451 0.235210 21313 0.81003
# 2.5%34 COD ATLANTIC 2016  27531380 totwgt_sd    6.4932    6.3838      23.36500   6.0890   6.6869 0.597900 20779 0.89443
# 2.5%33 COD ATLANTIC 2015   8915342 totwgt_sd    2.1027    2.0970       0.17429   2.0716   2.1232 0.051683 24031 0.71914
# 2.5%32 COD ATLANTIC 2014  28570078 totwgt_sd    6.7382    6.8005      13.48700   6.5766   7.0328 0.456180 20416 0.88363
# 2.5%31 COD ATLANTIC 2013  12550459 totwgt_sd    2.9600    2.9837       1.13150   2.9189   3.0504 0.131470 24549 0.76574
# 2.5%30 COD ATLANTIC 2012   9538831 totwgt_sd    2.2497    2.2245       0.37251   2.1873   2.2630 0.075729 22789 0.76290
# 2.5%29 COD ATLANTIC 2011  35724538 totwgt_sd    8.4256    8.4033      20.51000   8.1265   8.6906 0.564150 24609 0.79815
# 2.5%28 COD ATLANTIC 2010  44532221 totwgt_sd   10.5030   10.3040      43.71900   9.9038  10.7220 0.817780 28273 0.83744


# # ------------------------------------------------
# # these are with "standard tow" assumptions:
#                  speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length  dwao    gini lower.ci.gini upper.ci.gini mean.3.yr median median.50
# 2.5%  Cod summer standardtow 2017  14863259   totwgt    3.5136    3.4339       3.80320   3.3145   3.5572 0.242740 21451 0.77906
# 2.5%7 Cod summer standardtow 2016  21430734   totwgt    5.0662    5.1501      11.10100   4.9457   5.3587 0.412980 20681 0.84814
# 2.5%6 Cod summer standardtow 2015   8723439   totwgt    2.0622    2.0598       0.16459   2.0347   2.0851 0.050439 23705 0.65717
# 2.5%5 Cod summer standardtow 2014  27156331   totwgt    6.4197    6.4166      13.37500   6.1928   6.6459 0.453100 20786 0.83574
# 2.5%4 Cod summer standardtow 2013  12288438   totwgt    2.9050    2.9377       1.12560   2.8729   3.0045 0.131620 24411 0.73470
# 2.5%3 Cod summer standardtow 2012   9105517   totwgt    2.1525    2.1576       0.38184   2.1198   2.1957 0.075942 22465 0.73825
# 2.5%2 Cod summer standardtow 2011  34542306   totwgt    8.1657    8.1041      20.12700   7.8330   8.3863 0.553380 24584 0.77513
# 2.5%1 Cod summer standardtow 2010  42903020   totwgt   10.1420   10.4090      42.51400  10.0130  10.8180 0.805170 28360 0.82533

# # ------------------------------------------------
# # towed distance
#                  speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length  dwao    gini
# 2.5%  Cod summer towdistance 2017  15025543   totwgt    3.4420    3.4205       3.76150   3.3030   3.5434 0.240360 22219 0.77524
# 2.5%7 Cod summer towdistance 2016  23732430   totwgt    5.5022    5.4666      14.00500   5.2375   5.7001 0.462570 21266 0.84436
# 2.5%6 Cod summer towdistance 2015   8798976   totwgt    2.0690    2.0732       0.15893   2.0490   2.0982 0.049255 23901 0.65307
# 2.5%5 Cod summer towdistance 2014  28503738   totwgt    6.5806    6.6708      12.70500   6.4516   6.8923 0.440700 21401 0.83786
# 2.5%4 Cod summer towdistance 2013  12434510   totwgt    2.8474    2.8555       1.02700   2.7944   2.9199 0.125560 25127 0.73675
# 2.5%3 Cod summer towdistance 2012   9340895   totwgt    2.1474    2.1441       0.37216   2.1066   2.1824 0.075768 23436 0.73386
# 2.5%2 Cod summer towdistance 2011  35721843   totwgt    8.1870    8.0592      21.59400   7.7759   8.3539 0.578040 25474 0.77066
# 2.5%1 Cod summer towdistance 2010  43790809   totwgt    9.9849    9.8188      43.78700   9.4161  10.2420 0.825920 29684 0.81764

# # ------------------------------------------------
# # sweptarea
#                 speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length  dwao
# 2.5%  Cod summer sweptarea 2017  14584703   totwgt    3.5108    3.5984       4.12750   3.4741   3.7256 0.251540 20961
# 2.5%7 Cod summer sweptarea 2016  20677264   totwgt    5.0506    5.0506      11.32400   4.8465   5.2649 0.418400 19806
# 2.5%6 Cod summer sweptarea 2015   7397592   totwgt    1.9220    1.9240       0.13729   1.9011   1.9471 0.045981 20763
# 2.5%5 Cod summer sweptarea 2014  25264103   totwgt    6.5155    6.5552      14.94200   6.3205   6.7999 0.479430 18288
# 2.5%4 Cod summer sweptarea 2013  10290871   totwgt    2.7651    2.7570       1.12270   2.6915   2.8227 0.131200 20251
# 2.5%3 Cod summer sweptarea 2012   8839376   totwgt    2.1085    2.1188       0.36425   2.0816   2.1566 0.074987 22206
# 2.5%2 Cod summer sweptarea 2011  34866336   totwgt    8.4871    8.0913      24.96300   7.7888   8.4061 0.617280 23648
# 2.5%1 Cod summer sweptarea 2010  45648142   totwgt   10.7420   10.6010      52.95200  10.1560  11.0630 0.906710 28503


# TODO basic corelations and plots, summarizing the above

set = stratanal_data(  p=p, toget="stratanal", selection=selection, trawlable_units="sweptarea", sppoly=sppoly )

set$strata_year = paste( set$AUID, set$yr, sep=".")
nn = applySummary( set[, c("strata_year", "totno")]  )

V = expand.grid( AUID=unique(set$AUID), yr=sort( unique(set$yr) ) )
V$strata_year = paste( V$AUID, V$yr, sep=".")
V = merge( V, nn, by="strata_year", all.x=TRUE, all.y=FALSE, suffixes=c("", ".totno") )

dev.new(); plot( log(totno.mean) ~ log(totno.sd), V ); abline(0,1) ## looks like a Poisson ..


### end basic stranal comparisons ###
# ------------------------------------------------



# ------------------------------------------------
## mimic stranal with aegis.survey::survey_db & carstm

glm methods here


### end basic stranal glm comparisons ###
# ------------------------------------------------



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
  require(aegis.survey)

  spatial_domain = "SSE"
  
  yrs = 1970:2021
  runtype = "1970_present"

  if (0) {
    yrs = 1999:2021
    runtype = "1999_present"
  }

  groundfish_survey_species_code = 10 # cod


  # basic selection criteria
  ## note difference from stratanal: all gears kept, and all areas kept
  selection = list(
    biologicals=list(
      spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
    ),
    survey=list(
      data.source = c("groundfish", "snowcrab"),
      yr = yrs,      # time frame for comparison specified above
      months=6:8,
      # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
      # ranged_data="dyear"
      settype = c(1,2,4,5,8),
      # gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
      # strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
      polygon_enforce=TRUE
    )
  )
  p = survey_parameters(
    project_class = "carstm",
    project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
    label ="Atlantic cod summer",
    speciesname = "Atlantic_cod",
    trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  
    carstm_model_label=runtype,   # default = 1970:present, alt: 1999_present 
    runtype=runtype,
    yrs = yrs,
    selection = selection,
    variabletomodel = "totno",
    vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
    areal_units_type = "stratanal_polygons_pre2014",
    areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
    areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
    areal_units_overlay = "none",
    areal_units_timeperiod = "pre2014",    # "pre2014" for older
  )

  results_file = file.path( p$modeldir, p$speciesname , "RES_basic_carstm.rdata" )

  RES= list( yr = yrs )



  model_forms = c(
    # "A.S_fac.T_fac", # almost standard GF strata, no cov, no s, no st ;~ "stratanl"; stratanal_polygons_pre2014  -- results are useless
    # "A.SxT",  # broken: interaction only model (space:time) == stratanl .. will fail as there are missing combinations -- trying to add  a random spacetime iid effect to stabilize computations still does not help .. 
    # "A.SiT.ST_iid", # working . though results are not useful
    # "A.SxT.ST_iid",   # working
    "A.S_iid.T_iid", # working
    "A.S_iid.T_iid.ST_iid",
    "A.S_bym2.T_fac.ST_bym2.env.eco",
    "A.S_bym2.T_ar1.ST_bym2.env.eco",
    "H.S_fac.T_fac",
    "H.S_iid.T_iid",
    "H.S_iid.T_iid.ST_iid",
    "H.S_bym2.T_fac.ST_bym2.env.eco",
    "H.S_bym2.T_ar1.ST_bym2.env.eco"
  )


  redo_sppoly=FALSE
  # redo_sppoly=TRUE 
  sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), 
    areal_units_to_drop=c("5Z3","5Z4","5Z5","5Z6","5Z7","5Z8"), redo=redo_sppoly  )
  # sppoly$strata_to_keep = ifelse( as.character(sppoly$AUID) %in% strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ), FALSE,  TRUE )
      # plot(  sppoly["AUID"])

  # no data in these areal units: remove 
  if (0) {
    plot(sppoly["AUID"], reset=FALSE)
    plot(sppoly[which(sppoly$AUID %in% c("5Z3","5Z4","5Z5","5Z6","5Z7","5Z8") ), "AUID"], col="green", add=TRUE )
  }

  redo_survey_data = FALSE
  # redo_survey_data = TRUE
  M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=redo_survey_data, qupper=0.99 )

  for ( mf in model_forms ) {
    if (0)    mf = model_forms[4]
    loadfunctions("aegis.survey")
    RES[[mf]] = survey_parameter_list( mf=mf,  p=p )
    RES[[mf]] = survey_index( params=RES[[mf]], M=M, sppoly=sppoly, redo_model=TRUE, extrapolation_limit=c(0.025, 0.975) )
    save(RES, file=results_file, compress=TRUE)   # load(results_file)     # store some of the aggregate timeseries in this list
    
    if (0) {
      units = attr( RES[[mf]][["biomass"]], "units")
      plot( RES[[mf]][["biomass"]][["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="blue", type="b", main=mf, ylab=units, xlab="year" )
      lines( RES[[mf]][["biomass"]][["mean"]] ~ RES$yr, lty=1, lwd=2.5, col="blue", type="b" )
    }

  }



  if (0) {
      # map it
      map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
      map_zoom = 7
      background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 

      if (RES[[mf]]$type=="abundance") {
        fn_root = "Predicted_numerical_abundance"
        title = "Predicted numerical abundance"
        pci = RES[[mf]]$pN

      }
      if (RES[[mf]]$type=="habitat") {
        fn_root = "Predicted_habitat_probability"
        title = "Predicted habitat probability"
        pci = RES[[mf]]$pH
      }

      if (RES[[mf]]$type=="meansize") {
        fn_root = "Predicted_mean_weight"
        title = "Predicted mean weight"
        pci = RES[[mf]]$pW
      }

      plot_crs = pci$aegis_proj4string_planar_km
       # managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )
       # time_match = "2020"
      
      vn = "predictions"
      brks = pretty(  quantile( RES[[mf]][[vn]], probs=c(0.025,0.975), na.rm=TRUE )  )

      outputdir = file.path( pci$modeldir, pci$carstm_model_label, "predicted.numerical.densitites" )
      
      if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

      # attr( RES[[mf]][["predictions"]], "units")

      for (y in res$year ){
        time_match = as.character(y) 
        
        fn = file.path( outputdir, paste(fn_root, "png", sep=".") )
        carstm_map(  res=RES[[mf]], vn=vn, tmatch=time_match,
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

  }

  # for non-stratanly polys: 

  # areal_units_type = "lattice",  
  # areal_units_resolution_km = 25, 
  # areal_units_proj4string_planar_km =  projection_proj4string("utm20"),       
  # areal_units_overlay = "none",
  # areal_units_timeperiod = "none",
    



# ------------------------------------------------
# Model lattice 1:
# "INLA Envir AR1|year iid|Strata"	ar1	rw2: temp+depth, no car just iid in space
# simple factorial with totno and poisson; 79 configs; 6 hrs
 
 


#########################################
### PRESENCE-ABSENCE
#########################################


  formula = pa ~ 1
  + f(uid, model="iid", hyper=H$iid)
  + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(year, model="iid", hyper=H$iid)
  + f(strata, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family="binomial",  # alternates family="zeroinflatedbinomial0", family="zeroinflatedbinomial1",
  control.family=list(control.link=list(model="logit")),


# Fixed effects:
#               mean   sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) -0.263 0.35         -1   -0.245      0.373 -0.21   0

# Random effects:
#   Name	  Model
#     uid IID model
#    ti RW2 model
#    zi RW2 model
#    year IID model
#    strata BYM2 model

# Model hyperparameters:
#                            mean       sd 0.025quant 0.5quant 0.975quant   mode
# Precision for uid 321.732 1171.499      7.291   92.214   2026.074 16.637
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



# -------------

# car by year
# 100 hrs; 79 configs
fit = inla(
  formula = pa ~ 1
  + f(uid, model="iid", hyper=H$iid)
  + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
  + f(year, model="iid", hyper=H$iid)
  + f(strata, model="bym2", graph=slot(sppoly, "nb"), group = year, scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family="binomial",  # alternates family="zeroinflatedbinomial0", family="zeroinflatedbinomial1",
  control.family=list(control.link=list(model="logit")),


dev.new(width=11, height=7)
col = c("slategray", "turquoise", "darkorange", "green", "blue", "darkred", "cyan", "darkgreen", "purple" )
pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20)
lty = c(1, 3, 4, 5, 6, 7, 1, 3, 4 )
lwd = c(4, 4, 4, 4, 4, 4, 4, 4, 4 )
type =c("l", "l", "l", "l", "l", "l", "l", "l", "l")

labels = c("Standard tow stratanal" )



plot( pop.total  ~ RES[["yr"]], data=RES[["stratanal_towdistance"]], lty=lty[1], lwd=lwd[1], col=col[1], pch=pch[1], type=type[1], ylim=c(0,0.46e9), xlab="Year", ylab="kg")

model_forms = setdiff( names( RES ), "yr" )
model_forms = model_forms[ - grep("stratanal.*", model_forms) ]

for (rt in 1:length( model_forms) ) {
  mf = model_forms[rt]
  labels = c( labels, RES[[mf]]$label )
  lines( biomass_mean ~ RES[["yr"]], data=RES[[mf]], lty=lty[rt], lwd=lwd[rt], col=col[rt], pch=pch[rt], type=type[rt])
}

legend("topright", legend=labels, lty=lty, col=col, lwd=lwd )


if (0) {
     # save(RES, file=results_file, compress=TRUE)
    # load(results_file)
}




### end


