

stratanal_data = function( selection=NULL, toget="", sppoly=NULL, trawlable_units=NULL, ... ) {

  if ( toget=="stratanal_direct") {
    # used for DEBUGGING:  access via strata_dataformat .. ie. directly from groundfish_survey_db .. gscat
    # gscat does not have the data corrections due to miscoding etc, vessel-species "catchability" corrections, etc
    # .. but the totals are nearly identical to survey_db access

    # NOTE polygon areas of strata are predetermined in GSSTRATUM and used by "stratanal"
    # This behaviour is mimicked in survey_db .. although it does not have to be
    # .. The CAR variation computes surfaces directly from polygons
    # Using this approach is better in that there is more filter control
    # Results for the basic test cases are essentially identical to "stratanal" (via "strata_dataformat", above)
    # but faster and more QA/QC done on the input data

    set = strata_dataformat( selection=selection )   # return values in kg or no per set
      # dim(set) # [1] 1682   45
      # sum(set$totwgt) # [1] 9683
      # sum(set$totno)  # [1] 15261

    # compute no of trawlable units for each stratum

  } else if ( toget=="stratanal") {

    # integrated with aegis.survey access methods .. i.e., more flexible

    # categorize Strata based on lon/lat
  
    if (exists( "survey", selection )) {
      if(exists( "strata_toremove", selection$survey )) {
        i = which( as.character(sppoly$AUID) %in%  strata_definitions( selection$survey$strata_toremove ) )
        if (length(i) > 0 ) sppoly$strata_to_keep[i] = FALSE
      }
    }

    set = survey_db( p=p, DS="filter" )

    set$AUID = st_points_in_polygons(
      pts = st_as_sf( set, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) ),
      polys = sppoly[, "AUID"],
      varname="AUID"
    )
    set = set[ which(!is.na(set$AUID)),]

  } else if ( toget=="stratanal_designated_au") {
    # alternatively, merge based upon AUID's designated in groundfish tables.
    set =  survey_db( p=p, DS="filter", add_groundfish_strata=TRUE )   # return values in kg or no per set

  } else {
    # default :
    set =  survey_db( p=p, DS="filter"  )   # return values in kg or no per set

  }

  spp = st_drop_geometry( sppoly )
  attributes(spp$au_sa_km2) = NULL
  
  set = merge( set, spp, by="AUID", all.x=TRUE, all.y=FALSE, suffixes=c(".set",""))

  # compute no of trawlable units for each stratum
  # "strat" and "nh" are used by "stratanal"
  set$strat = set$AUID

  ft2m = 0.3048
  m2km = 1/1000
  nmi2mi = 1.1507794
  mi2ft = 5280
  standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm
  # = 0.0405 and NOT 0.011801 .. where did this come from?
  # set up trawlable units used in stratanal

  if (!exists("sa_towdistance", set)) {
    dtow_km = set$dist_pos
    i = which(!is.finite(dtow_km)) 
    if (length(i) > 0) dtow_km[i] = set$dist_km[i]
    i = which(!is.finite(dtow_km)) 
    if (length(i) > 0) dtow_km[i] = standardtow_sakm2
    set$sa_towdistance = (41 * ft2m * m2km ) * dtow_km
  }

  

  set$nh = switch( trawlable_units,
    sweptarea = as.numeric(set$au_sa_km2) * set$vessel_correction / set$sweptarea, # convert strata area to trawlable units 41ft by 1.75 nm, divide area by sweptarea
    standardtow = as.numeric(set$au_sa_km2) / standardtow_sakm2, # convert strata area to trawlable units 41ft by 1.75 nm, divide area by 0.011801
    towdistance = as.numeric(set$au_sa_km2) / set$sa_towdistance # convert strata area to trawlable units 41ft by 1.75 nm, divide area by 0.011801
  )

  i = which(!is.finite(set$nh))
  set$nh[i] = as.numeric(set$au_sa_km2[i]) / standardtow_sakm2  # override missing with "standard set" .. also if no trawlable_units set

  return(set)

}
