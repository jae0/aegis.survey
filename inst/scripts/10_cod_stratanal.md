---
title: "Cod analysis stratanal"
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



## Set up generic run parameters

```r
  require(aegis)
  require(aegis.polygons)
  require(aegis.survey)
  require(carstm)


  groundfish_survey_species_code = 10 # cod

  spatial_domain = "SSE"
  
  yrs = 1970:year.assessment

  carstm_model_label = "default" #   NOTE:  must use "default" for cod

 
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

  # basic selection criterias


  if (parameter_set=="habitat_paper") {
  
    # note difference from stratanal: all gears kept, and all areas kept

    p = survey_parameters(
      project_class = "carstm",
      project_name="survey",  # "survey" == keyword used to bring in domain of martimes boundaries groundfish surveys; otherwise use xydata
      label ="Atlantic cod summer",
      speciesname = "Atlantic_cod",
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

    results_file = file.path( p$modeldir, p$speciesname , "RES_habitat_comparisons.rdata" )
    RES= list( yr = yrs )

  }


  if ( parameter_set=="tesselation" ) {

    # note difference from stratanal: all gears kept, and all areas kept

    p = survey_parameters(
      project_class = "carstm",
      project_name="survey",  # "survey" == keyword used to bring in domain of maritimes boundaries groundfish surveys; otherwise use xydata
      speciesname = "Atlantic_cod",
      label ="Atlantic cod tesselation",
      trawlable_units = "direct_number",  
      carstm_model_label="Atlantic_cod_summer_RV_default_tesselation",   # default = 1970:present, alt: 1999_present 
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

  }



  if ( parameter_set=="stratanal" ) {

    # basic selection criteria for biologicals and sets 

    p = survey_parameters(
      project_class = "stratanal",
      project_name="survey",  
      speciesname = "Atlantic_cod",
      trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
      carstm_model_label="Atlantic_cod_summer_RV_default_stratanal",   # default = 1970:present, alt: 1999_present 
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
    
  }



  if ( parameter_set=="stratanal_iid" ) {

    # basic selection criteria for biologicals and sets 
    # "selection" is the same as for stratanal (above)

    p = survey_parameters(
      project_class = "carstm",
      project_name="survey",  # "survey" == keyword used to bring in domain of maritimes boundaries groundfish surveys; otherwise use xydata
      speciesname = "Atlantic_cod",
      label ="Atlantic cod stratanal polygons",
      trawlable_units = "direct_number",  
      carstm_model_label="Atlantic_cod_summer_RV_default_stratanal_polygons_iid",   # default = 1970:present, alt: 1999_present 
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



# bbox = c(-71.5, 41, -52.5,  50.5 )
additional_features = features_to_add( 
    p=p, 
    isobaths=c( 100, 200, 300, 400, 500 ), 
    xlim=c(-80,-40), 
    ylim=c(38, 60) 
)


map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
map_zoom = 7

background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 


```

## Groundfish analysis using Stratanal (historical framework)

```r

# ------------------------------------------------
# Atlantic cod 

# comparison of naive strata-based averages and carstm-based solutions using the same strata
# summer RV surveys only

# NOTE: This replicates standard groundfish strata-based estimation of means and totals
# "standard" random-stratified estimation functions (based on stratanal and bootstrap estimation techniques )


# set up the run parameters

parameter_set = "stratanal"  # used by 10_cod_workspace to defined parameter subsets
year.assessment = 2022

source( file.path( code_root, "aegis.survey", "inst", "scripts", "10_cod_workspace.R" ) )

 
outputdir = file.path( dirname(results_file), p$carstm_model_label  )
if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )


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
  
sppoly$dummy_var = NA
outfilename = file.path( outputdir , "areal_units.png" )
carstm_map(  sppoly=sppoly, vn="dummy_var",
    additional_features=additional_features,
    # palette="-RdYlBu",
    plot_elements=c( "compass", "scale_bar", "legend"  ), 
    scale=1.5,
    map_mode="plot",
    tmap_zoom= c(map_centre, map_zoom),
    outfilename=outfilename
) 



# plot figure for ms following just creates the background map 
# .. must send boundaries of areal units again as a separate feature to plot
aus =  tm_shape( sppoly ) + tm_borders( col="plum", alpha=0.9, lwd=2)  
 
sppoly$dummy_var = NA
outfilename = file.path( outputdir , "areal_units_groundfish_strata.png" )
carstm_map(  sppoly=sppoly, vn="dummy_var",
    additional_features=additional_features+aus,
    # palette=NA,
    plot_elements=c( "compass", "scale_bar", "legend"  ), 
    scale=1.5,
    alpha=0.5,
    map_mode="plot",
    tmap_zoom= c(map_centre, map_zoom),
    outfilename=outfilename
) 



# RES= list( yr = p$yrs )
RES = aegis::read_write_fast( results_file )

for ( data_approach in c( "stratanal_direct", "stratanal_designated_au", "stratanal" ) ) {
for ( tu in c( "standardtow", "towdistance", "sweptarea" ) ) {  
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
}}

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
nn = applySummary( set[, c("strata_year", "totno")]  )

V = expand.grid( AUID=unique(set$AUID), yr=sort( unique(set$yr) ) )
V$strata_year = paste( V$AUID, V$yr, sep=".")
V = merge( V, nn, by="strata_year", all.x=TRUE, all.y=FALSE, suffixes=c("", ".totno") )

dev.new(); plot( log(totno.mean) ~ log(totno.sd), V ); abline(0,1) ## looks like a Poisson or negative binomial will do..

# overall they look good, some minor variability due to species more sanity checking and imputation of strange data in survey.db 

### end basic stranal comparisons ### 
# ------------------------------------------------


# Full Martimes domain (above was for "summer strata")


 

 
```
