


  # set up generic run parameters


  require(aegis)
  require(aegis.polygons)
  require(aegis.survey)
  require(carstm)


  groundfish_survey_species_code = 10 # cod

  spatial_domain = "SSE"
  yrs = 1970:2021
  runtype = "1970_present"
 


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
      # carstm_model_label = "full_model"   ## <<----- 
      carstm_model_label=runtype,   # default = 1970:present, alt: 1999_present 
      runtype=runtype,
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
      carstm_model_label="Atlantic_cod_summer_RV_1970_present_tesselation",   # default = 1970:present, alt: 1999_present 
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
      hull_alpha = 15,
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
      carstm_model_label="Atlantic_cod_summer_RV_1970_present_stratanal",   # default = 1970:present, alt: 1999_present 
      outputdir = file.path( global_output_directory, "stratanal" ),
      yrs=1970:2021,
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
      carstm_model_label="Atlantic_cod_summer_RV_1970_present_stratanal_polygons_iid",   # default = 1970:present, alt: 1999_present 
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
additional_features = additional_features_tmap( 
    p=p, 
    isobaths=c( 10, 100, 200, 300, 500, 1000 ), 
    coastline =  c("canada"), 
    xlim=c(-80,-40), 
    ylim=c(38, 60) 
)


map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
map_zoom = 7
background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 


