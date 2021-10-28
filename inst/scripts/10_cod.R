
# ------------------------------------------------
# Atlantic cod comparison of naive strata-based averages and carstm-based solutions

# NOTE: This replicates standard groundfish strata-based estimation of means and totals
# "standard" random-stratified estimation functions (based on stratanal and bootstrap estimation techniques )

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
      settype = 1,
      gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
      strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
      polygon_enforce=TRUE,
      ranged_data="dyear"
    )
  )


  # store some of the aggregate timeseries in this list
  RES= list( yr = yrs )
  if (0) {
    fn = file.path( getwd(), "RES.rdata" )
    # save(RES, file=fn)
    # load(fn)
  }


# --------------------------------
# construct basic parameter list defining the main characteristics of the study
# parameter setting used to filter data via 'survey_db( DS="filter")'
# specific selection params required for survey_db(DS="filter") data selection mechanism

  p = survey_parameters(
    project_class = "stratanal",
    project_name="atlantic_cod",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
    label ="Atlantic cod summer standardtow",
    speciesname = "Atlantic_cod",
    trawlable_units = "__to_be_filled_later__",  # to be filled in below in call to aegis_survey_index
    selection = selection
  )

    

  # do stratanl for each of the following swept-area assumptions:
  for (tu in c( "standardtow", "towdistance", "sweptarea") ) {
    sppoly =  areal_units( p=p  )
    bi = strata_timeseries(
      set=stratanal_data( p=p, toget="stratanal", trawlable_units=tu, sppoly=sppoly ),
      variable="totwgt",
      speciesname=p[["label"]],
      yrs=p$yrs
      # alpha.t = 0.05, # confidence interval eg. 0.05 = 95%, 0.1 = 90%
      # alpha.b = 0.05,
      # nresamp = 1000,
      # prints=TRUE
    )
    runtype = paste("stratanal", tu, sep="_")
    RES[[runtype]] = bi[ match(RES[["yr"]], bi$year), ]
    RES[[runtype]]$label = runtype

    plot( pop.total ~ RES[["yr"]], data=RES[[runtype]], lty=5, lwd=4, col="red", type="b", ylim=c(0,8e8) )
    lines ( pop.total ~ RES[["yr"]], data=RES[[runtype]], lty=5, lwd=4, col="red", type="b", ylim=c(0,8e8))
  }


  # comparative plots:

  dev.new(width=11, height=7)
  col = c("slategray", "turquoise", "darkorange" )
  pch = c(20, 21, 22)
  lty = c(1, 3, 4 )
  lwd = c(4, 4, 4)
  type =c("l", "l", "l")

  plot( pop.total  ~ RES[["yr"]], data=RES[["stratanal_standardtow"]], lty=lty[1], lwd=lwd[1], col=col[1], pch=pch[1], type=type[1], ylim=c(0,2.6e8), xlab="Year", ylab="kg")
  lines( pop.total ~ RES[["yr"]], data=RES[["stratanal_towdistance"]], lty=lty[2], lwd=lwd[2], col=col[2], pch=pch[2], type=type[2])
  lines( pop.total ~ RES[["yr"]], data=RES[["stratanal_sweptarea"]], lty=lty[3], lwd=lwd[3], col=col[3], pch=pch[3], type=type[3])
  legend("topright", legend=c("Standard tow", "Length adjusted", "Length & width adjusted"), lty=lty, col=col, lwd=lwd )


  dev.new(width=6, height=4)
  hist( RES[["stratanal_towdistance"]]$pop.total / RES[["stratanal_standardtow"]]$pop.total, breaks=20 )

  dev.new(width=6, height=4)
  hist( RES[["stratanal_sweptarea"]]$pop.total / RES[["stratanal_standardtow"]]$pop.total, breaks=20 )

  o = cbind(
    RES[["stratanal_towdistance"]]$pop.total,
    RES[["stratanal_sweptarea"]]$pop.total,
    RES[["stratanal_standardtow"]]$pop.total
  )

  cor( o, use="pairwise.complete.obs" )

  plot( o )



# ------------------------------------------------
# these are Michelle's results: (base access of gcat without correction factors for boat, species, etc)
        speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length
2.5%   COD ATLANTIC 2017  14593959 totwgt_sd    3.4420    3.4258       3.61840   3.3099   3.5451 0.235210 21313 0.81003
2.5%34 COD ATLANTIC 2016  27531380 totwgt_sd    6.4932    6.3838      23.36500   6.0890   6.6869 0.597900 20779 0.89443
2.5%33 COD ATLANTIC 2015   8915342 totwgt_sd    2.1027    2.0970       0.17429   2.0716   2.1232 0.051683 24031 0.71914
2.5%32 COD ATLANTIC 2014  28570078 totwgt_sd    6.7382    6.8005      13.48700   6.5766   7.0328 0.456180 20416 0.88363
2.5%31 COD ATLANTIC 2013  12550459 totwgt_sd    2.9600    2.9837       1.13150   2.9189   3.0504 0.131470 24549 0.76574
2.5%30 COD ATLANTIC 2012   9538831 totwgt_sd    2.2497    2.2245       0.37251   2.1873   2.2630 0.075729 22789 0.76290
2.5%29 COD ATLANTIC 2011  35724538 totwgt_sd    8.4256    8.4033      20.51000   8.1265   8.6906 0.564150 24609 0.79815
2.5%28 COD ATLANTIC 2010  44532221 totwgt_sd   10.5030   10.3040      43.71900   9.9038  10.7220 0.817780 28273 0.83744


# ------------------------------------------------
# these are with "standard tow" assumptions:
                 speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length  dwao    gini lower.ci.gini upper.ci.gini mean.3.yr median median.50
2.5%  Cod summer standardtow 2017  14863259   totwgt    3.5136    3.4339       3.80320   3.3145   3.5572 0.242740 21451 0.77906
2.5%7 Cod summer standardtow 2016  21430734   totwgt    5.0662    5.1501      11.10100   4.9457   5.3587 0.412980 20681 0.84814
2.5%6 Cod summer standardtow 2015   8723439   totwgt    2.0622    2.0598       0.16459   2.0347   2.0851 0.050439 23705 0.65717
2.5%5 Cod summer standardtow 2014  27156331   totwgt    6.4197    6.4166      13.37500   6.1928   6.6459 0.453100 20786 0.83574
2.5%4 Cod summer standardtow 2013  12288438   totwgt    2.9050    2.9377       1.12560   2.8729   3.0045 0.131620 24411 0.73470
2.5%3 Cod summer standardtow 2012   9105517   totwgt    2.1525    2.1576       0.38184   2.1198   2.1957 0.075942 22465 0.73825
2.5%2 Cod summer standardtow 2011  34542306   totwgt    8.1657    8.1041      20.12700   7.8330   8.3863 0.553380 24584 0.77513
2.5%1 Cod summer standardtow 2010  42903020   totwgt   10.1420   10.4090      42.51400  10.0130  10.8180 0.805170 28360 0.82533

# ------------------------------------------------
# towed distance
                 speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length  dwao    gini
2.5%  Cod summer towdistance 2017  15025543   totwgt    3.4420    3.4205       3.76150   3.3030   3.5434 0.240360 22219 0.77524
2.5%7 Cod summer towdistance 2016  23732430   totwgt    5.5022    5.4666      14.00500   5.2375   5.7001 0.462570 21266 0.84436
2.5%6 Cod summer towdistance 2015   8798976   totwgt    2.0690    2.0732       0.15893   2.0490   2.0982 0.049255 23901 0.65307
2.5%5 Cod summer towdistance 2014  28503738   totwgt    6.5806    6.6708      12.70500   6.4516   6.8923 0.440700 21401 0.83786
2.5%4 Cod summer towdistance 2013  12434510   totwgt    2.8474    2.8555       1.02700   2.7944   2.9199 0.125560 25127 0.73675
2.5%3 Cod summer towdistance 2012   9340895   totwgt    2.1474    2.1441       0.37216   2.1066   2.1824 0.075768 23436 0.73386
2.5%2 Cod summer towdistance 2011  35721843   totwgt    8.1870    8.0592      21.59400   7.7759   8.3539 0.578040 25474 0.77066
2.5%1 Cod summer towdistance 2010  43790809   totwgt    9.9849    9.8188      43.78700   9.4161  10.2420 0.825920 29684 0.81764

# ------------------------------------------------
# sweptarea
                speciesname year pop.total variable orig.mean boot.mean var.boot.mean lower.ci upper.ci   length  dwao
2.5%  Cod summer sweptarea 2017  14584703   totwgt    3.5108    3.5984       4.12750   3.4741   3.7256 0.251540 20961
2.5%7 Cod summer sweptarea 2016  20677264   totwgt    5.0506    5.0506      11.32400   4.8465   5.2649 0.418400 19806
2.5%6 Cod summer sweptarea 2015   7397592   totwgt    1.9220    1.9240       0.13729   1.9011   1.9471 0.045981 20763
2.5%5 Cod summer sweptarea 2014  25264103   totwgt    6.5155    6.5552      14.94200   6.3205   6.7999 0.479430 18288
2.5%4 Cod summer sweptarea 2013  10290871   totwgt    2.7651    2.7570       1.12270   2.6915   2.8227 0.131200 20251
2.5%3 Cod summer sweptarea 2012   8839376   totwgt    2.1085    2.1188       0.36425   2.0816   2.1566 0.074987 22206
2.5%2 Cod summer sweptarea 2011  34866336   totwgt    8.4871    8.0913      24.96300   7.7888   8.4061 0.617280 23648
2.5%1 Cod summer sweptarea 2010  45648142   totwgt   10.7420   10.6010      52.95200  10.1560  11.0630 0.906710 28503


# TODO basic corelations and plots, summarizing the above

set = stratanal_data( p=p, toget="stratanal", trawlable_units="sweptarea" )

set$strata_year = paste( set$AUID, set$yr, sep=".")
nn = applySummary( set[, c("strata_year", "totno")]  )

V = expand.grid( AUID=unique(set$AUID), yr=sort( unique(set$yr) ) )
V$strata_year = paste( V$AUID, V$yr, sep=".")
V = merge( V, nn, by="strata_year", all.x=TRUE, all.y=FALSE, suffixes=c("", ".totno") )

dev.new(); plot( log(totno.mean) ~ log(totno.sd), V ); abline(0,1) ## looks like a Poisson ..


### end basic stranal comparisons ###
# ------------------------------------------------



# ------------------------------------------------
# Atlantic cod comparison of CAR (ICAR/BYM) Poisson process models
# using sweptarea only on a lattice system with environmental covariates.
# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.
# This adds some differences relative to "statanal" (which uses sa in sq nautical miles, btw)

# NOTE:: unlike stratanl, we do not need to remove strata until the last /aggregation step

# the variations examined here:

, "Basic IID", "Envir", "Envir AR1", "Envir CAR", "Envir AR1 CAR", "Envir AR1 CAR|year", "Envir AR1|strata CAR", "Envir AR1|strata CAR|year", "Envir CAR|year", leroux, besag, overdispered,




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
      settype = 1,
      gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
      #  strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
      polygon_enforce=TRUE,
      ranged_data="dyear"
    )
  )

     

# ----------------------------------------------
# define runtypes: params are stored in  survey_parameter_list()

  runtypes = c(
    "abundance.space_iid.year_iid",  # standard GF strata, no cov, no s, no st ;~ "stratanl"; stratanal_polygons_pre2014 
    "abundance.space_iid.year_iid.envir",
    "abundance.space_iid.year_ar1.envir", 
    "abundance.space_bym2.year_ar1.envir", 
    "abundance.space_bym2.year_ar1.spacetime.envir"
  )
 

  redo = FALSE
  # redo = TRUE  # if new year/ new data

  for ( runtype in runtypes ) {

    RES[[runtype]] = survey_parameter_list( runtype=runtype, yrs=yrs, selection=selection, project_name="atlantic_cod" )

    RES[[runtype]] = survey_index( params=RES[[runtype]], M=M, redo=redo )

    str( RES[[runtype]] )

    plot( biomass_mean ~ yr, data=RES[[runtype]], lty=1, lwd=2.5, col="blue", type="b" )

    if (0) {
      # map it
      plot_crs = pN$aegis_proj4string_planar_km
      coastline=aegis.coastline::coastline_db( DS="eastcoast_gadm", project_to=plot_crs )
      isobaths=aegis.bathymetry::isobath_db( depths=c(50, 100, 200, 400, 800), project_to=plot_crs )
      managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )

      time_match = list( year=as.character(2020)  )

      res = carstm_model( p=RES[[runtype]]$pW, DS="carstm_modelled_summary" )   # weights (mean)
      res = carstm_model( p=RES[[runtype]]$pN, DS="carstm_modelled_summary" )   # numbers (density)

      carstm_map(  res=res,
        vn="predictions",
        time_match=time_match,
        coastline=coastline,
        managementlines=managementlines,
        isobaths=isobaths,
        main=paste("Predicted numerical abundance", paste0(time_match, collapse="-") )
      )

      # map all :
      vn = "predictions"
      outputdir = file.path( RES[[runtype]]$pN$modeldir, RES[[runtype]]$pN$carstm_model_label, "predicted.numerical.densitites" )
      if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )
      brks = pretty(  quantile(res[[vn]], probs=c(0,0.975))  )

      for (y in res$year ){
        time_match = list( year=as.character(y)  )
        fn_root = paste("Predicted_numerical_abundance", paste0(time_match, collapse="-"), sep="_")
        fn = file.path( outputdir, paste(fn_root, "png", sep=".") )
        carstm_map(  res=res, vn=vn, time_match=time_match,
          breaks =brks,
          coastline=coastline,
          isobaths=isobaths,
          managementlines=managementlines,
          main=paste("Predicted numerial abundance", paste0(time_match, collapse="-") ),
          outfilename=fn
        )
      }

    }

  }




# ----------------------------------------------
# 6. Habitat data-based areal-units solution

    # areal_units_type="tesselation"
    runtype = "habitat.space_bym2.year_ar1.spacetime.envir"
    RES[[runtype]]$label = ""
    RES[[runtype]]$pH = p
    RES[[runtype]]$pH$variabletomodel = "pa"  # could be picked up later from formula but better to specify explicitly
    RES[[runtype]]$pH$areal_units_type="stratanal_polygons_pre2014"
    RES[[runtype]]$pN$trawlable_units="sweptarea"
    RES[[runtype]]$pH$formula = formula( pa ~ 1
        + f(strata, model="iid", group=year, hyper=H$iid)
        + f(year, model="iid", hyper=H$iid )
    )

    RES[[runtype]]$pH$family =  "binomial"  # alternates family="zeroinflatedbinomial0", family="zeroinflatedbinomial1",

    RES[[runtype]]$sppoly = areal_units( p=pH  )

# ----------------------------------------------
# 7. data-based areal-units solution
    # areal_units_type="tesselation"
    runtype = "habitat.space_bym2.year_ar1.spacetime.envir"
    RES[[runtype]]$label = ""
    RES[[runtype]]$pH = p
    RES[[runtype]]$pH$variabletomodel = "pa"  # could be picked up later from formula but better to specify explicitly
    RES[[runtype]]$pH$areal_units_type="stratanal_polygons_pre2014"
    RES[[runtype]]$pN$trawlable_units="sweptarea"
    RES[[runtype]]$pH$formula = formula( pa ~ 1
        + f(strata, model="iid", group=year, hyper=H$iid)
        + f(year, model="iid", hyper=H$iid )
    )

    RES[[runtype]]$pH$family =  "binomial"  # alternates family="zeroinflatedbinomial0", family="zeroinflatedbinomial1",
    RES[[runtype]]$sppoly = areal_units( p=pH  )

... etc


# ----------------------------------------------

  runtypes = names(RES) [ grep("habitat.*", names(RES) ) ]

  redo = FALSE
  # redo = TRUE  # if new year/ new data

  for ( runtype in runtypes ) {

    params$M = survey_db( p=RES[[runtype]]$pH, DS="carstm_inputs", sppoly=params$sppoly, redo=redo )

    RES[[runtype]] = survey_index( type="habitat", params=RES[[runtype]], redo=FALSE )
    str( RES[[runtype]] )
    plot( biomass_mean ~ yr, data=RES[[runtype]], lty=1, lwd=2.5, col="blue", type="b")

    if (0) {
      # map it
      plot_crs = pN$aegis_proj4string_planar_km
      coastline=aegis.coastline::coastline_db( DS="eastcoast_gadm", project_to=plot_crs )
      isobaths=aegis.bathymetry::isobath_db( depths=c(50, 100, 200, 400, 800), project_to=plot_crs )
      managementlines = aegis.polygons::area_lines.db( DS="cfa.regions", returntype="sf", project_to=plot_crs )

      time_match = list( year=as.character(2020)  )

      res = carstm_model( p=RES[[runtype]]$pW, DS="carstm_modelled_summary" )   # weights (mean)
      res = carstm_model( p=RES[[runtype]]$pN, DS="carstm_modelled_summary" )   # numbers (density)

      carstm_map(  res=res,
        vn="predictions",
        time_match=time_match,
        coastline=coastline,
        managementlines=managementlines,
        isobaths=isobaths,
        main=paste("Predicted habitat probability", paste0(time_match, collapse="-") )
      )

      # map all :
      vn = "predictions"
      outputdir = file.path( RES[[runtype]]$pN$modeldir, RES[[runtype]]$pN$carstm_model_label, "predicted.numerical.densitites" )
      if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )
      brks = pretty(  quantile(res[[vn]], probs=c(0,0.975))  )

      for (y in res$year ){
        time_match = list( year=as.character(y)  )
        fn_root = paste("Predicted_habitat_probability", paste0(time_match, collapse="-"), sep="_")
        fn = file.path( outputdir, paste(fn_root, "png", sep=".") )
        carstm_map(  res=res, vn=vn, time_match=time_match,
          breaks =brks,
          coastline=coastline,
          isobaths=isobaths,
          managementlines=managementlines,
          main=paste("Predicted habitat probability", paste0(time_match, collapse="-") ),
          outfilename=fn
        )
      }

    }

  }





# ------------------------------------------------
# Model lattice 1:
# "INLA Envir AR1|year iid|Strata"	ar1	rw2: temp+depth, no car just iid in space
# simple factorial with totno and poisson; 79 configs; 6 hrs

pN$formula =  formula(
  totno ~ 1 + offset( log( data_offset) )
    + f(strata, model="iid", group=year, hyper=H$iid)
    + f(year, model="ar1", hyper=H$ar1 )
    + f(uid, model="iid", hyper=H$iid)
    + f(ti, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
    + f(zi, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
)
pN$family = "poisson"
RES[["space_iid.year_ar1"]]$pN =pN

pW$formula =  formula(
  totno ~ 1
    + f(strata, model="iid", group=year, hyper=H$iid)
    + f(year, model="ar1", hyper=H$ar1 )
    + f(uid, model="iid", hyper=H$iid)
    + f(ti, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
    + f(zi, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
)
pW$family = "gaussian"
RES[["space_iid.year_ar1"]]$pW =pW




# ------------------------------------------------
#  Model lattice 2: CAR simple and year iid
# 46hr; 45 configs

  formula = totno ~ 1
    + offset( log(data_offset) )
    + f(uid, model="iid", hyper=H$iid)
    + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(year, model="iid", hyper=H$iid)
    + f(strata, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family = "poisson",

RES$space_car.year_iid = colSums( {out * weight_year * sppoly$au_sa_km2}[sppoly$strata_to_keep, ], na.rm=TRUE )



# ------------------------------
# Model 3 Lattice  CAR in space grouped by year and ar1 in time (year)
# 81 configs and about 97 hrs!

  formula = totno ~ 1
    + offset( log(data_offset) )
    + f(uid, model="iid", hyper=H$iid)
    + f(ti, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(zi, model="rw2", scale.model=TRUE, hyper=H$rw2)
    + f(year, model="ar1", hyper=H$ar1)
    + f(strata, model="bym2", graph=slot(sppoly, "nb"), group = year, scale.model=TRUE, constr=TRUE, hyper=H$bym2),
  family = "poisson",

RES$space_car.year_ar1 = colSums( {out * weight_year * sppoly$au_sa_km2}[sppoly$strata_to_keep, ], na.rm=TRUE )




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

runtypes = setdiff( names( RES ), "yr" )
runtypes = runtypes[ - grep("stratanal.*", runtypes) ]

for (rt in 1:length( runtypes) ) {
  runtype = runtypes[rt]
  labels = c( labels, RES[[runtype]]$label )
  lines( biomass_mean ~ RES[["yr"]], data=RES[[runtype]], lty=lty[rt], lwd=lwd[rt], col=col[rt], pch=pch[rt], type=type[rt])
}

legend("topright", legend=labels, lty=lty, col=col, lwd=lwd )


if (0) {
    fn = file.path( getwd(), "RES.rdata" )
    # save(RES, file=fn)
    # load(fn)
}




### end


