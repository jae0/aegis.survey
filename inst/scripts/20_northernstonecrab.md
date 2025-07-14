---
title: "northern_stone_crab ... placeholder .. incomplete"
author: "Jae S. Choi"
toc: true
number-sections: true
highlight-style: pygments
editor:
  render-on-save: false
format:
  html: 
    code-fold: true
    html-math-method: katex
    embed-resources: true
  pdf:
    pdf-engine: lualatex
  docx: default 
---
 

<!-- This is a Markdown/Quarto document -->

<!-- 
Copy this file to a work directory (e.g., ~/tmp/ ) 
and run Quarto from there:

# quarto render *.qmd --to html 

Can add "--to docx --to pdf" as additional documents, but their formatting is awkward and will require more work.  
-->

```r 
# ------------------------------------------------
# XXX comparison of CAR (ICAR/BYM) Poisson process models
# using sweptarea only on a lattice system with environmental covariates.
# Here we compute surface area of each polygon via projection to utm or some other appropriate planar projection.

# This adds some differences relative to "statanal" (which uses sa in sq nautical miles, btw)

# NOTE:: unlike stratanl, we do not need to remove strata until the last /aggregation step

# the variations examined here:

# ----------------------------------------------
# define model_forms: params are stored in  survey_parameter_list()

# adding settype 2 and 5 (comparative tows, and generic surveys) 

 
# set up the run parameters

# parameter_set = "stratanal_iid"  # used by 10_cod_workspace to load a defined parameter subset
parameter_set = "tesselation"  # used by 10_cod_workspace to defined parameter subsets

year.assessment = 2022

species = northern_stone_crab_spec = 2523 # (itis_tsn=97943)
 
figure.timeseries.bycatch(p=p, species=northern_stone_crab_spec, plotyears=2004:p$year.assessment, outdir=file.path(p$annual.results,"timeseries", "survey") )

bc.vars = c(paste("ms.mass", northern_stone_crab_spec, sep='.'),paste("ms.no", northern_stone_crab_spec, sep='.'))
outdir.bc= file.path( p$project.outputdir, "maps", "survey", "snowcrab","annual", "bycatch" )
map.set.information( p, variables=bc.vars, mapyears=1999:p$year.assessment, outdir=outdir.bc,probs=c(0,0.975)) #


  require(aegis)
  require(aegis.polygons)
  require(aegis.survey)
  require(carstm)


  groundfish_survey_species_code = northern_stone_crab_spec

  spatial_domain = "SSE"
  
  yrs = 1970:year.assessment

  carstm_model_label = "default" #   NOTE:  must use "default" for cod

 
  global_output_directory = file.path( data_root, "aegis", "survey", "modelled", "NorthernStoneCrab" )
  if ( !file.exists(global_output_directory)) dir.create( global_output_directory, recursive=TRUE, showWarnings=FALSE )

  results_file = file.path( global_output_directory, "RES.rdz" )


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

  # basic selection criterias


  if (parameter_set=="habitat_paper") {
  
    # note difference from stratanal: all gears kept, and all areas kept

    p = survey_parameters(
      project_class = "carstm",
      project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
      label ="NorthernStoneCrab summer",
      speciesname = "NorthernStoneCrab",
      trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  
      carstm_model_label=carstm_model_label,   # default = 1970:present, alt: 1999_present 
      yrs = yrs,
      type="habitat",
      variabletomodel = "pa",  
      vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
      selection = list(
        biologicals=list(
          spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
        ),
        survey=list(
          data.source = c("groundfish", "snowcrab"),
          yr = yrs,      # time frame for comparison specified above
          # months=6:11,   #"summer"
          # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
          # ranged_data="dyear"
          settype = c( 1,2,5,8 ),
          # gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
          # strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
          polygon_enforce=TRUE
        )
      )
    )

    results_file = file.path( p$modeldir, p$speciesname , "RES_habitat_comparisons.rdz" )
    RES= list( yr = yrs )

  }




  if ( parameter_set=="stratanal" ) {

    # basic selection criteria for biologicals and sets 

    p = survey_parameters(
      project_class = "stratanal",
      project_name="survey",  
      speciesname = "NorthernStoneCrab",
      trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
      carstm_model_label="NorthernStoneCrab_summer_RV_default_stratanal",   # default = 1970:present, alt: 1999_present 
      outputdir = file.path( global_output_directory, "stratanal" ),
      yrs=1970:year.assessment,
      areal_units_type = "stratanal_polygons_pre2014",
      areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
      areal_units_overlay = "none",
      areal_units_timeperiod = "pre2014",    # "pre2014" for older
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
    )


  # bbox = c(-71.5, 41, -52.5,  50.5 )
   additional_features = features_to_add( 
      p=p, 
      isobaths=c( 100, 200, 300, 400, 500 ), 
      coastline = c("united states of america", "canada"), 
      xlim=c(-80,-40), 
      ylim=c(38, 60) 
  )
    

    # ------------------------------------------------
    # PART 1 -- simple "stratanal" 

    # construct basic parameter list defining the main characteristics of the study
    # parameter setting used to filter data via 'survey_db( DS="filter")'
    # specific selection params required for survey_db(DS="filter") data selection mechanism

    # sppoly is used for "stratanal_designated_au" method .. which is the survey.db standard

    # auid to drop to match Michelle's extraction for "stratanal"
    auid_to_drop = strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ) 

    if (redo_data) {
      sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84"), redo=TRUE )
      # no data in these areal units: remove .. they seem to be US locations
      plot(sppoly["AUID"], reset=FALSE)
      plot(sppoly[which(sppoly$AUID %in% auid_to_drop), "AUID"], col="darkgray", add=TRUE )
    }


    # this is to match Michelle's extraction for "Summer RV" 
    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84")   )
    plot(sppoly["AUID"] )

    sppoly = sppoly[ -which( sppoly$AUID %in% auid_to_drop ), ]
    plot(sppoly["AUID"] )




    # plot figure for ms following just creates the background map 
      
    sppoly$dummy_var = 1:nrow(sppoly)

    carstm_map(  sppoly=sppoly, vn="dummy_var",
        colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
        additional_features = additional_features
          ) 



    # plot figure for ms following just creates the background map 
    # .. must send boundaries of areal units again as a separate feature to plot
 
    sppoly$dummy_var = NA
    outfilename = file.path( outputdir , "areal_units_groundfish_strata.png" )
    carstm_map(  sppoly=sppoly, vn="dummy_var",
        colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
        additional_features = additional_features
        outfilename=outfilename
    ) 



    # RES= list( yr = p$yrs )
    RES = aegis::read_write_fast( results_file )

    # for ( data_approach in c( "stratanal_direct", "stratanal_designated_au", "stratanal" ) ) {
    
    # for ( tu in c( "standardtow", "towdistance", "sweptarea" ) ) {  
      
      data_approach = "stratanal"
      tu = "standardtow"
      # data selection (depending upon methods, sa estimates, etc)
      set = stratanal_data( toget=data_approach, selection=p$selection, trawlable_units=tu, sppoly=sppoly ) 
      # compute stratanal (stratum-surface area weighted sums)
      bi = strata_timeseries(
        set=set, variable="totwgt", speciesname=p[["speciesname"]], yrs=p$yrs,
        alpha.t = 0.05 # confidence interval for t-tdist assumption eg. 0.05 = 95%, 0.1 = 90%
      )
      model_label = paste(data_approach, tu, sep=".")
      RES[[model_label]] = data.frame( year=p$yrs )
      RES[[model_label]] = merge( RES[[model_label]], bi, by="year", all.x=TRUE, all.y=FALSE )
      RES[[model_label]]$label = model_label
    
    # }}

    read_write_fast( data=RES, results_file )
    # RES = aegis::read_write_fast( results_file )
    

    # --------- 

    # single plot
    dev.new(width=11, height=7)
    nvn = setdiff( names(RES), "yr" )
    nv = which( nvn == "stratanal.standardtow" )

    col = c("darkslategray", "turquoise", "darkorange", "green", "blue", "darkred", "slategray", "darkgreen", "purple", "darkgray", "pink" )
    pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20, 19, 23)  
    lty = c(1, 3, 4, 5, 6, 7, 1, 3, 4, 5, 6 ) 
    lwd = c(2, 4, 6, 2, 4, 6, 6, 4, 6, 5, 4 ) 
    type =c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l")

    plot( 0, 0, type="n", xlim=range(RES[["yr"]]), ylim=c(0, 280), xlab="Year", ylab="kt", main="Comparing input data treatment and sweptareas")
    for (i in nv) {
      lines( mean ~ year, data=RES[[nvn[i]]], lty=lty[i], lwd=lwd[i], col=col[i], pch=pch[i], type=type[i])
      lines( lb025 ~ year, data=RES[[nvn[i]]], lty="dotted", lwd=1, col=col[i] )
      lines( ub975 ~ year, data=RES[[nvn[i]]], lty="dotted", lwd=1, col=col[i] )
    }





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

    setDT(set)
    nn = set[, 
      .(  mean=mean(totno, na.rm=TRUE),
          sd  = sd (totno, na.rm=TRUE),
          n   = length( which(is.finite(totno)))
      ),
      by = .(strata_year)
    ]

    V = expand.grid( AUID=unique(set$AUID), yr=sort( unique(set$yr) ) )
    V$strata_year = paste( V$AUID, V$yr, sep=".")
    V = merge( V, nn, by="strata_year", all.x=TRUE, all.y=FALSE, suffixes=c("", ".totno") )

    dev.new(); plot( log(totno.mean) ~ log(totno.sd), V ); abline(0,1) ## looks like a Poisson or negative binomial will do..

    # overall they look good, some minor variability due to species more sanity checking and imputation of strange data in survey.db 

    ### end basic stranal comparisons ### 
    # ------------------------------------------------


    # Full Martimes domain (above was for "summer strata")


    

    
  }



  if ( parameter_set=="stratanal_iid" ) {

    # basic selection criteria for biologicals and sets 
    # "selection" is the same as for stratanal (above)

    p = survey_parameters(
      project_class = "carstm",
      project_name="survey",  # "survey" == keyword used to bring in domain of maritimes boundaries groundfish surveys; otherwise use xydata
      speciesname = "NorthernStoneCrab",
      label ="NorthernStoneCrab stratanal polygons",
      trawlable_units = "direct_number",  
      carstm_model_label="NorthernStoneCrab_summer_RV_default_stratanal_polygons_iid",   # default = 1970:present, alt: 1999_present 
      carstm_model_type="S_iid.T_iid",
      outputdir = file.path( global_output_directory, "stratanal_iid" ),
      yrs = yrs,
      variabletomodel = "totno",
      vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
      areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
      areal_units_type = "stratanal_polygons_pre2014",
      areal_units_timeperiod = "pre2014",    # "pre2014" for older
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
    )
    
  }



  if ( parameter_set=="tesselation" ) {

    # note difference from stratanal: all gears kept, and all areas kept

    p = survey_parameters(
      project_class = "carstm",
      project_name="survey",  # "survey" == keyword used to bring in domain of maritimes boundaries groundfish surveys; otherwise use xydata
      speciesname = "NorthernStoneCrab",
      label ="NorthernStoneCrab tesselation",
      trawlable_units = "direct_number",  
      carstm_model_label="NorthernStoneCrab_summer_RV_default_tesselation",   # default = 1970:present, alt: 1999_present 
      carstm_model_type="full_model",
      outputdir = file.path( global_output_directory, "full_model" ),
      yrs = yrs,
      variabletomodel = "totno",
      vars_to_retain = c("totwgt", "totno", "pa", "meansize", "data_offset", "gear", "data.source", "id"),  # to compute mean size, etc
      areal_units_proj4string_planar_km = projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm; alt projection_proj4string("omerc_nova_scotia")   
      areal_units_type = "tesselation",
      areal_units_constraint_ntarget = length(yrs),  # n time slices req in each au
      areal_units_constraint_nmin = 5,   # n time slices req in each au
      areal_units_resolution_km = 1,  # starting resolution .. if using tesselation/ otherwise grid size ()
      areal_units_overlay = "none",
      areal_units_timeperiod = "none",  # only relevent for groundfish polys
      tus="yr",
      fraction_todrop = 0.05,
      fraction_cv = 1.0,  # just under poisson (binomial)
      fraction_good_bad = 0.9,
      nAU_min = 30,
      selection = list(
        biologicals=list(
          spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
        ),
        survey=list(
          data.source = c("groundfish", "snowcrab"),
          yr = yrs,      # time frame for comparison specified above
          # months=6:11,   #"summer"
          # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
          # ranged_data="dyear"
          settype = c( 1,2,5,8 ),
          # gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
          # strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
          polygon_enforce=TRUE
        )
      )
    )

  # bbox = c(-71.5, 41, -52.5,  50.5 )
  additional_features = features_to_add( 
      p=p, 
      isobaths=c( 100, 200, 300, 400, 500 ), 
      xlim=c(-80,-40), 
      ylim=c(38, 60) 
  )
    

    outputdir = file.path( dirname(results_file), p$carstm_model_label  )
    if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )
    
      
    # ----------------------------------------------
    # NorthernStoneCrab with a CAR (ICAR/BYM) Poisson process models with tesselation


    # instead of dropping right away, carry the data as it represents neighbourhood information and additional data
    # reset sppoly to full domain

    # auid to drop to match Michelle's extraction for "stratanal"

    if (redo_data) {
    
      xydata = survey_db( p=p, DS="areal_units_input", redo=TRUE )
      
      # tesselation
      sppoly = areal_units( p=p, xydata=xydata[ which(xydata$yr %in% p$yrs), ], redo=TRUE, verbose=TRUE, hull_noise=1e-2 )  # create constrained polygons with neighbourhood as an attribute
    

      M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE, quantile_upper_limit=0.99, 
        fn=file.path( p$modeldir, p$speciesname, "carstm_inputs_tesselation.rdz" ) )
    }


    sppoly = areal_units( p=p, return_crs=projection_proj4string("lonlat_wgs84") )

    # auid to drop to match Michelle's extraction for "stratanal"
    auid_to_drop = strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ) 
    sppoly = set_surface_area_to_NA( sppoly, auid_to_drop )  # do not drop data .. only set areas beyond domain to NA
    sppoly$filter = ifelse(is.finite( sppoly$au_sa_km2 ), 1, NA)



    # plot figure for ms following just creates the background map 
    # .. must send boundaries of areal units again as a separate feature to plot
    aus =  
      tm_shape( sppoly[ which(sppoly$filter==1), ] ) + 
        tm_borders( col="plum", alpha=0.75, lwd=1)  +
      tm_shape( sppoly[ which(is.na(sppoly$filter) ), ] ) + 
        tm_borders( col="lightgray", alpha=0.75, lwd=1)  

    sppoly$dummy_var = NA
    outfilename = file.path( outputdir , "areal_units_tesselation.png" )
    carstm_map(  sppoly=sppoly, vn="dummy_var",
        additional_features=additional_features+aus,
        colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
        scale=1.5,
        outfilename=outfilename
    ) 


    M = survey_db( p=p, DS="carstm_inputs", sppoly=sppoly, quantile_upper_limit=0.99, 
        fn=file.path( p$modeldir, p$speciesname, "carstm_inputs_tesselation.rdz" ) )

    ip = which(M$tag == "predictions")
    io = which(M$tag == "observations")
    iq = unique( c( which( M$totno > 0), ip ) ) # subset to positive definite data (for number and size)
    iw = unique( c( which( M$totno > 30), ip ) ) # subset to positive definite data (for number and size)

    pN = survey_parameter_list( p=p, model_label=p$carstm_model_type, type="abundance" )
    pW = survey_parameter_list( p=p, model_label=p$carstm_model_type, type="meansize" )
    pH = survey_parameter_list( p=p, model_label=p$carstm_model_type, type="habitat" )



      pN$space_name = sppoly$AUID 
      pN$space_id = 1:nrow(sppoly)  # must match M$space

      pN$time_name = as.character(pN$yrs)
      pN$time_id =  1:pN$ny

      pN$cyclic_name = as.character(pN$cyclic_levels)
      pN$cyclic_id = 1:pN$nw

      pW$space_name = sppoly$AUID 
      pW$space_id = 1:nrow(sppoly)  # must match M$space

      pW$time_name = as.character(pW$yrs)
      pW$time_id =  1:pW$ny

      pW$cyclic_name = as.character(pW$cyclic_levels)
      pW$cyclic_id = 1:pW$nw

      pH$space_name = sppoly$AUID 
      pH$space_id = 1:nrow(sppoly)  # must match M$space

      pH$time_name = as.character(pH$yrs)
      pH$time_id =  1:pH$ny

      pH$cyclic_name = as.character(pH$cyclic_levels)
      pH$cyclic_id = 1:pH$nw



    if (0) {
      # debugging for windows
      inla.setOption(
        num.threads="1:1",
        mc.cores=1, 
        blas.num.threads=1,
        inla.mode="classic",
        mkl=TRUE
      )
    }
    # size model
    res = NULL; gc()
    res = carstm_model( p=pW, data=M[iw,], sppoly=sppoly,  posterior_simulations_to_retain="predictions", 
        theta= c( 0.088, 2.950, 0.943, 3.230, 3.676, 4.382, 3.781, 3.952, 3.313, 2.603, -0.044, 2.566, 3.194),
      # control.inla = list( strategy='adaptive' ), 
      num.threads="4:2" 
    )  

    # numerical model
    res = NULL; gc()
    res = carstm_model( p=pN, data=M[iq,], sppoly=sppoly,  posterior_simulations_to_retain="predictions",
      #theta=c(1.131, 0.767, 2.593, -0.659, -1.411, -1.689, -0.254, -2.234, 3.394, -2.381, -1.399, 0.371) ,
      # control.inla = list( strategy='adaptive', int.strategy="eb" ), 
      num.threads="1:1" 
    )  

    # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

    # habitat model
    res = NULL; gc()
    res = carstm_model( p=pH, data=M, sppoly=sppoly, posterior_simulations_to_retain="predictions", 
      # control.inla = list( strategy='adaptive' ), 
      num.threads="4:2"    
    ) 
    # plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )


    # reload collator
    RES = aegis::read_write_fast( results_file )

    # NOTE: below we divide by 10^6 to convert  kg -> kt;; kt/km^2
    # with "habitat" at habitat definition of prob=0.05 (hurdle process)  
    sims = carstm_posterior_simulations( pN=pN, pW=pW, pH=pH, pa_threshold=0.05 ) * sppoly$au_sa_km2 / 10^6  
    RES[[p$carstm_model_type]] = carstm_posterior_simulations_summary( sims ) 


    read_write_fast( data=RES, fn=results_file )
    # RES = aegis::read_write_fast( results_file )
      


    ( fn = file.path( outputdir, "biomass_timeseries.png") )
    png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
      plot( mean ~ year, data=RES[[p$carstm_model_type]], col="slategray", pch=19, lty=1, lwd=2.5, 
      type="b", ylab="Biomass index (kt)", xlab="", ylim=c(0,190))
      lines( lb025 ~ year, data=RES[[p$carstm_model_type]], lty="dashed", col="gray" )
      lines( ub975 ~ year, data=RES[[p$carstm_model_type]], lty="dashed", col="gray" )
    dev.off()
    

    # map it ..mean density (choose appropriate p$carstm_model_type/"sims", above)
    
    # equivalent of the persistent spatial effect (global spatial mean)
    Bg = apply( sims, c(1), mean, na.rm=TRUE )  # global spatial means  
    brks = pretty( log10( quantile( Bg, probs=c(0.05, 0.95), na.rm=TRUE ))  )
    vn =  "biomass_mean_global" 
    sppoly[,vn] = log10( Bg )

    outfilename = file.path( outputdir , "predictions", paste( "biomass", "spatial_effect", "png", sep=".") )
    carstm_map(  sppoly=sppoly, vn=vn,
        breaks=brks,
        additional_features=additional_features,
    #    title= y, #paste( "log_10( Predicted biomass density; kg/km^2 )", y ),
        colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
        scale=1.5,
        outfilename=outfilename
    ) 
    
    B = apply( sims, c(1,2), mean, na.rm=TRUE  ) # means by year
    brks = pretty( log10( quantile( B[], probs=c(0.05, 0.95), na.rm=TRUE  ))  )
    vn = paste("biomass", "predicted", sep=".")
    for (i in 1:length(pN$yrs) ){
      y = as.character( pN$yrs[i] )
      sppoly[,vn] = log10( B[,y] )
      outfilename = file.path( outputdir, "predictions",  paste( "biomass", y, "png", sep=".") )
      carstm_map(  sppoly=sppoly, vn=vn,
          breaks=brks,
          additional_features=additional_features,
          title= y, #paste( "log_10( Predicted biomass density; kg/km^2 )", y ),
          colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
          scale=1.5,
          outfilename=outfilename
      )
    }


    # comparative plots of timeseries:
    # dev.new(width=11, height=7)
    # nvn = setdiff( names(RES), "yr" )
    # vc = paste( "full_model" )

    # nv  = which( nvn %in% c( "full_model",  "stratanal.towdistance")  )
    # col = c("slategray", "turquoise", "darkorange", "lightgreen", "navyblue", "darkred",  "turquoise", "cyan", "darkgreen", "purple", "darkgray", "pink" )
    # pch = c(20, 21, 22, 23, 24, 25, 26, 27, 20, 19, 23)
    # lty = c(2, 1, 4, 5, 6, 7, 1, 3, 4, 5, 6 )
    # lwd = c(3, 6, 6, 2, 4, 6, 2, 4, 6, 5, 4 )
    # type =c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l")

    # plot( 0, 0, type="n", xlim=range(RES[["yr"]]), ylim=c(0, 320), xlab="Year", ylab="kt", main="Comparing input data treatment and sweptareas")
    # for (j in 1:length(nv) ) {
    #   i = nv[j]
    #   lines( mean ~ year, data=RES[[nvn[i]]], lty=lty[j], lwd=lwd[j], col=col[j], pch=pch[j], type=type[j])
    #   lines( lb025 ~ year, data=RES[[nvn[i]]], lty="dotted", lwd=1, col=col[j] )
    #   lines( ub975 ~ year, data=RES[[nvn[i]]], lty="dotted", lwd=1, col=col[j] )
    # }

    # legend("topright", legend=nvn[nv], lty=lty, col=col, lwd=lwd )

    # alternative plot (Figure 2)
    dev.new(width=14, height=8, pointsize=20)

    library(ggplot2)

    r1 = RES[["stratanal.standardtow"]]
    r1$Method = "Stratanal" 

    r2 = RES[["full_model"]]
    r2$Method = "CAR/Hurdle"

    keep = intersect( names(r1), names(r2) )
    dta = rbind( r1[, keep ], r2[, keep] ) 
    dta[dta<0] = 0
    
    ggplot( dta, aes(year, mean, fill=Method, colour=Method) ) +
      geom_ribbon(aes(ymin=lb025, max=ub975), alpha=0.2, colour=NA) +
      geom_line() +
      labs(x="Year", y="Biomass (kt)", size = rel(1.5)) +
      # scale_y_continuous( limits=c(0, 300) )  
      theme_light( base_size = 22 ) 




    # --- simple ribbon plot

    # start from a vanilla R session (plotly does not like startup)
    

    # end
    # ------------------------------------------------
    
    # Figure 1 alt. average bottom temperature of prediction surface (whole year spatial and temporal variability)
      pt = temperature_parameters( 
          project_class="carstm", 
          yrs=1970:year.assessment, 
          carstm_model_label="default" 
        ) 
      tspol = areal_units( p=pt )
      tspol = set_surface_area_to_NA( tspol, auid_to_drop )  # do not drop data .. only set areas beyond domain to NA

      res = carstm_model( p=pt, DS="carstm_modelled_summary", sppoly=tspol  ) # to load currently saved results

      aufilter = ifelse( is.finite(tspol$au_sa_km2), 1, NA )
      res = res$predictions[,,,"mean"] * aufilter  # subannual means
      res_mean = apply(res, 2, mean, na.rm=TRUE )
      res_q025 = apply(res, 2, quantile, probs=0.025, na.rm=TRUE )
      res_q975 = apply(res, 2, quantile, probs=0.975, na.rm=TRUE )
      tyrs = as.numeric( dimnames(res)[["time"]] )

      trange = range(  c(res_q975, res_q025) ) * c(0.9, 1.1)
      plot( res_mean ~ tyrs, type="b", pch=19, col="slategray", ylim = trange, ylab="Bottom temperature, Celsius", xlab="Year", lwd=1.5)
      lines( res_q025 ~tyrs, col="darkgray", lty="dashed")
      lines( res_q975 ~tyrs, col="darkgray", lty="dashed")




    # --------------------------------  
    # maps and plots
    
      p = pH
      fn_root = "Predicted_habitat_probability"
      
      carstm_plot_marginaleffects( p, outputdir, fn_root)

      

      # posterior predictions: timeseries 
      preds = res[["predictions"]] * sppoly$filter # space x year (in 1 JULY)

      # spatial CI
        preds = data.table(
          mean = apply( preds, 2, mean, na.rm=TRUE) ,
          q025 = apply( preds, 2, quantile, probs=0.025, na.rm=TRUE) ,
          q975 = apply( preds, 2, quantile, probs=0.975, na.rm=TRUE) 
        )

        plot( 0 , 0, type="n", ylab="Probability", xlab="Year", ylim=c(0, 1), xlim=range( RES$yr)   )
        lines( preds$mean ~ res$yr, lty=1, lwd=2.5, col="slategray" )
        lines( preds$q025 ~ res$yr, lty="dotted", lwd=1, col="slategray"  )
        lines( preds$q975 ~ res$yr, lty="dotted", lwd=1, col="slategray"  )

        abline( h=0.5, lty="dashed",  col="slategray" )

      
      # from sims:
      
        # with "habitat" at habitat definition of prob=0.05 (hurdle process)  
        sims = carstm_posterior_simulations( pH=pH, pa_threshold=0.05 ) 
        sims = sims * sppoly$au_sa_km2 / sum(  sppoly$au_sa_km2, na.rm=TRUE )  # area weighted average

        lab = paste(p$carstm_model_type, "habitat", sep="_")
        RES[[lab]] = carstm_posterior_simulations_summary( sims )  # sum area weighted probs
      
        ( fn = file.path( outputdir, "habitat_timeseries.png") )
        png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
          plot( mean ~ year, data=RES[[lab]], col="slategray", pch=19, lty=1, lwd=2.5, 
          type="b", ylab="Probability", xlab="", ylim=c(0,1) )
          lines( lb025 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
          lines( ub975 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
        dev.off()
        


      p = pN
      fn_root = "Predicted_numerical_density"
       
      carstm_plot_marginaleffects( p, outputdir, fn_root)

    # from sims:
      
        # with "habitat" at habitat definition of prob=0.05 (hurdle process)  
        sims = carstm_posterior_simulations( pN=pN, pa_threshold=0.05 ) 
        sims = sims * sppoly$au_sa_km2  / 10^6 # n -> G n  # expand densities to number and then sum below (carstm_posterior_simulations_summary)

        lab = paste(p$carstm_model_type, "number", sep="_")
        RES[[lab]] = carstm_posterior_simulations_summary( sims )  # sum area weighted probs
      
        ( fn = file.path( outputdir, "number_timeseries.png") )
        png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
          plot( mean ~ year, data=RES[[lab]], col="slategray", pch=19, lty=1, lwd=2.5, 
          type="b", ylab=bquote("Number" ~ 10^6), xlab="", ylim=c(0,160) )
          lines( lb025 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
          lines( ub975 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
        dev.off()
    
        

      p = pW
      fn_root = "Predicted_mean_weight"
     
      carstm_plot_marginaleffects( p, outputdir, fn_root)

        sims = carstm_posterior_simulations( pW=pW, pa_threshold=0.05 ) 
        sims = sims * sppoly$au_sa_km2 / sum(  sppoly$au_sa_km2, na.rm=TRUE )  # area weighted average

        lab = paste(p$carstm_model_type, "weight", sep="_")
        RES[[lab]] = carstm_posterior_simulations_summary( sims )  # sum area weighted probs
      
        ( fn = file.path( outputdir, "weight_timeseries.png") )
        png( filename=fn, width=1000, height=800, pointsize=10, res=192 )
          plot( mean ~ year, data=RES[[lab]], col="slategray", pch=19, lty=1, lwd=2.5, 
          type="b", ylab="Weight (kg)", xlab="", ylim=c(0, 2.75) )
          lines( lb025 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
          lines( ub975 ~ year, data=RES[[lab]], lty="dashed", col="gray" )
        dev.off()
    

      read_write_fast( data=RES, fn=results_file  )
      # RES = aegis::read_write_fast( results_file )
        

      if (0) {
        fit = carstm_model( p=p, DS="modelled_fit", sppoly=sppoly )
        names( fit$summary.random)
        res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale
        names( res[["random"]])
        # "time"  
        # "cyclic" 
        # "gear" sppoly$filter 
        # "inla.group(t, method = \"quantile\", n = 11)"
        # "inla.group(z, method = \"quantile\", n = 11)"
        # etc 
      }



    # --------------------------------  
    # Figure  3D plot of habitat vs temperature vs depth  via splines .. slow (skip if not required)

      p = pH
      fn_root = "Predicted_habitat_probability"
      title = "Predicted habitat probability"
      res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  )  # NOTE: res contains estimates on user scale

      o = carstm_2D_effects_probability( 
        res=res,
        xvar = "inla.group(t, method = \"quantile\", n = 11)",  
        yvar = "inla.group(z, method = \"quantile\", n = 11)" ,
        xgrid = seq( -1, 10.5, by=0.5),
        ygrid = seq( 25, 350, by=25),
        xslice = 4,
        yslice = -75,
        nx=200, ny=200,
        theta = -40,
        phi = 10
      )
      
      # add coastline to the data domain (sppoly)
      crs_plot = st_crs( sppoly )
      domain = polygon_managementareas( species="maritimes" )
      domain = st_transform( domain, crs_plot )
      data_mask = st_union( sppoly[which(sppoly$filter==1),1] ) 
      # all = st_union( domain, data_mask )
      nearshore = st_cast( st_difference( domain, data_mask ), "POLYGON")[1]
      domain_new = st_union( data_mask, nearshore )


      if (0) {

        loadfunctions("aegis.survey")
      
        year.assessment = year.assessment  
        xvar = "inla.group(t, method = \"quantile\", n = 11)"   
        yvar = "inla.group(z, method = \"quantile\", n = 11)" 
        depths = c( 10, 150 )   # range of survey data for mean habitat estimates 150m ~= prob at 0.5
        nsims = 100
        domain=domain_new 
        probability_limit = 0.25

      }

      # WARNING: This is very slow and RAM intensive too .. temperature posterior sims are very large ~100 GB, several hours 
      # worth running in parallel .. eventually 
      o = carstm_optimal_habitat( 
        res = res,
        year.assessment = year.assessment ,
        xvar = "inla.group(t, method = \"quantile\", n = 11)",  
        yvar = "inla.group(z, method = \"quantile\", n = 11)",
        depths = c( 10, 100 ),   # range of survey data for mean habitat estimates ;; 150m ~= prob at 0.5
        probability_limit =0.25,
        nsims = 100,
        domain=domain_new 
      ) 

      if (0) {
        u = aegis::read_write_fast('/home/jae/tmp/temp_depth_habitat.rdz')
        dev.new()
        plot( habitat~yr, u, type="b", ylim=c(0.29, 0.4))
        lines( habitat_lb~yr, u)
        lines( habitat_ub~yr, u)
        abline(v=1990)
        abline(v=2012)
      
        dev.new()
        plot( habitat_sa~yr, u, type="b", ylim=c( 55000, 78000))
        lines( habitat_sa_lb~yr, u)
        lines( habitat_sa_ub~yr, u)
        abline(v=1990)
        abline(v=2012)

        ll = loess(habitat~yr, u, span=0.25 )
        pp = predict( ll, u )
        lines(pp ~ u$yr)

      }

      fn_optimal = file.path( outputdir, "optimal_habitat.rdz" )
      read_write_fast( data=o, fn=fn_optimal )
      o = aegis::read_write_fast(fn_optimal)
    
      if (plot_map) {

        require(ggplot2)
        proj_stmv =  "+proj=utm +ellps=WGS84 +zone=20 +units=km"  # crs of depth from stmv
        crs_domain = st_crs( proj_stmv )

        domain = st_transform( st_union( st_as_sf(domain_new) ), crs_domain  )

        isobaths = c( 0, 100, 200, 300, 400, 800 )
        isobs = aegis.bathymetry::isobath_db( depths=isobaths, project_to=crs_domain )
        isobs = st_intersection(isobs, domain)

        # coastline = st_transform( polygons_rnaturalearth(), st_crs("+proj=utm +ellps=WGS84 +zone=20 +units=km" ) )
        # coastline = st_intersection(coastline, domain)  
        # coastline = st_cast( coastline, "MULTILINESTRING" )
        
        coastline = st_transform( polygons_rnaturalearth(countries=c("United States of America", "Canada"),
          xlim=c(-80,-40), ylim=c(38, 60)), st_crs(crs_domain) )   

        if (is.null(probability_limit)) {
          zprob = 0
        } else {
          zprob = probability_limit
        }

        # incomplete:: must add Z (prob hab)
        plt = ggplot() +
            geom_sf( data=coastline, aes(alpha=0.1), colour="gray90" )  +
            geom_sf( data=isobs, aes(alpha=0.1), colour="lightgray" ) +
            geom_raster(data = Z[Z$Zprob>= zprob ,], aes(x=plon, y=plat, fill=Zprob, alpha=1.0) ) +
            scale_fill_gradientn(name = "Probability (depth)", colors =color.code( "seis", seq( 0, 1, by=0.1 )), na.value=NA ) +
            guides(fill = guide_colorbar(
              title.theme = element_text(size = 20),
              label.theme = element_text(size = 18) ) ) +
            scale_alpha(range = c(0.9, 0.95), guide = "none") +
            theme(
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(), 
              legend.position="inside", legend.position.inside=c( 0.1, 0.8 ),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank() )
              ggtitle("Cod depth") +
              coord_fixed()
        
        dev.new(width=14, height=8, pointsize=20)
        print(plt)  

        gc()
        
      }
    
      
      u = RES[["full_model_number"]]
      u$sa = o[["temperature_depth"]]$habitat_sa
      u$density = u$mean / u$sa
      plot(density ~ year, u)

      dev.new(width=14, height=8, pointsize=20)
      library(ggplot2)


      ggplot( o[["temperature_depth"]], aes(yr, habitat ) ) +
        geom_ribbon(aes(ymin=habitat_lb, max=habitat_ub), alpha=0.2, colour=NA) +
        geom_line() +
        labs(x="Year", y="Habitat probabtility", size = rel(1.5)) +
        # scale_y_continuous( limits=c(0, 300) )  
        theme_light( base_size = 22 ) 
      

      dev.new(width=14, height=8, pointsize=20)
      library(ggplot2)
      ggplot( o[["temperature_depth"]], aes(yr, habitat_sa ) ) +
        geom_ribbon(aes(ymin=habitat_sa_lb, max=habitat_sa_ub), alpha=0.2, colour=NA) +
        geom_line() +
        labs(x="Year", y=bquote("Habitat surface area;" ~ km^2), size = rel(1.5)) +
        # scale_y_continuous( limits=c(0, 300) )  
        theme_light( base_size = 22 ) 



      # estimate of surface area of optimal habitat (depth only):
      # SA = 63819 km^2  (max) 





    # ---------------------------
    # operating directly upon posterior samples:

    np = 5000
    psims = inla.posterior.sample( np, fit  ) 


    # order of effects gets messed up .. must use names
    psims_rn = gsub( "[:].*$", "", rownames(psims[[1]]$latent) )
    i_b0 = grep("\\(Intercept\\)", psims_rn )
    i_temp = grep( "inla.group\\(t", psims_rn )
    i_depth = grep( "inla.group\\(z", psims_rn )
    i_cyclic = grep( "^cyclic$", psims_rn )
    i_gear = grep( "^gear$", psims_rn )
    i_time = grep( "^time$", psims_rn )
    

    ns = length( res$space )

    iid = 1:ns
    bym = (ns+1) : (2*ns)
    i_space = grep( "^space$", psims_rn )
    i_space_iid = i_space[iid]
    i_space_bym = i_space[bym]


    sti = expand.grid( space=res$space, type = c("iid", "bym"), time=res$time, stringsAsFactors =FALSE ) # bym2 effect: bym and iid with annual results
    iid = which(sti$type=="iid") #  spatiotemporal interaction effects  iid
    bym = which(sti$type=="bym") #  spatiotemporal interaction effects bym

    i_space_time = grep( "^space_time$", psims_rn )
    i_space_time_iid = i_space_time[iid]
    i_space_time_bym = i_space_time[bym]

    tempsQ  = res$random[[4]]$ID
    depthsQ = res$random[[5]]$ID

    matchto   = list( space=res$space, time=res$time  )
    matchfrom = list( space=sti[["space"]][iid], time=sti[["time"]][iid]  )



    pred_func = function(x, threshold =NULL) {
      Y = x$latent
      yrr = matrix(  Y[i_time], ncol=length(res$time), nrow=length(res$space), byrow=TRUE )
      depth_fn = splinefun( depthsQ,  Y[i_depth], method="monoH.FC" )
      temp_fn  = splinefun( tempsQ,   Y[i_temp], method="monoH.FC"  )
      depths = depth_fn( res$data$z[which(res$data$tag=="predictions")] )
      temps = temp_fn(  res$data$t[which(res$data$tag=="predictions")] )
      depths = reformat_to_array(  input =depths , matchfrom = matchfrom, matchto = matchto )
      temps = reformat_to_array(  input =temps, matchfrom = matchfrom, matchto = matchto )
      Wbym = Wiid = array( NA, dim=c( length( res$space), length(res$time) ), dimnames=list( space=res$space, time=res$time ) )
      Wiid = reformat_to_array(  input = unlist(Y[i_space_time_iid]), matchfrom = matchfrom, matchto = matchto )
      Wbym = reformat_to_array(  input = unlist(Y[i_space_time_bym]), matchfrom = matchfrom, matchto = matchto )
      # pred = Y[i_b0]  + yrr+ Y[i_cyclic[7]]  + Y[i_space_iid] + Y[i_space_bym]  + temps + depths 
      # pred = Y[i_b0] + yrr + Y[i_cyclic[7]]  + Y[i_space_iid] + Y[i_space_bym]  + Wbym + Wiid + temps + depths 
      pred =   Y[i_b0] + Y[i_cyclic[7]]  + Y[i_space_iid] + Y[i_space_bym] + Wbym + Wiid + temps + depths 
      
      if (!is.null(threshold)) {
        il = which( pred <= threshold )
        iu = which( pred > threshold )
        pred[ il ] = 0
        pred[ iu ] = 1
      }
      pred
    } 

    oo =  ( simplify2array( lapply( psims, pred_func ) ) )
    gg = apply( inverse.logit(oo)*sppoly$au_sa_km2, 2, mean, na.rm=TRUE ) 
    plot( gg ~ res$time, type="b")
    abline(v=1990)


    oo = simplify2array( lapply( psims, pred_func, threshold = 0 ) )
    gg = apply( oo*sppoly$au_sa_km2, 2, sum, na.rm=TRUE ) /  sum(sppoly$au_sa_km2)/ np
    plot(gg ~ res$time, type="b")
    abline(v=1990)

    gg = apply( oo, 2, median, na.rm=TRUE )
    lines(gg ~ res$time, col="green")
    gg = apply( oo, 2, quantile, probs=0.025, na.rm=TRUE )
    lines(gg ~ res$time, col="red")


    gg = inverse.logit( apply( oo, 2, mean, na.rm=TRUE )) 


    g = invlink( g ) 

    lnk_function = inla.link.logit
    lnk_function_predictions = inla.link.identity  # binomial seems to be treated differently by INLA



  }

```