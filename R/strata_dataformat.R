strata_dataformat = function( selection, areal_units_proj4string_planar_km="+proj=utm +ellps=WGS84 +zone=20 +units=km" ) {

    #Extract data from groundfish survey
    #Extract Cat data from groundfish survey
    k = groundfish_survey_db(DS="gscat", yrs=selection$survey$yr ) #export from grounfish survey database .. weight (kg) and num per unit area (km^2)

    # # add required variables .. not sure if still required JC
    u = data.frame( matrix( unlist(strsplit( k$id, ".", fixed=TRUE)), ncol=2, byrow=TRUE), stringsAsFactors=FALSE )
    k$mission = as.character( u[,1] )
    k$setno = as.numeric( u[,2] )

    # apply selection criteria
    if (exists("survey", selection)) {  # filter survey information
      if (exists("settype", selection$survey) )  k = k[ which(k$settype %in% selection$survey[["settype"]] ) , ]  # k = k[ which(k$settype %in% c(1,2,5)) , ]
      if (exists("strata_toremove", selection$survey) ) k = k[-which( k$strat %in% strata_definitions( selection$survey[["strata_toremove"]] ) ) , ]
      if (exists("strata_tokeep", selection$survey) ) k = k[which( k$strat %in% selection$survey[["strata_to_keep"]] ) , ]
      if (exists("yr", selection$survey) ) k = k[ which(k$yr %in% selection$survey[["yr"]] ), ]
      if (exists("months", selection$survey) ) k = k[ which(month(k$timestamp) %in% selection$survey[["months"]] ), ]
      if (exists("gear", selection$survey )) k = k[ which(k$geardesc %in% selection$survey[["gear"]] ), ]
    }

    # split biological and set specific variables to redo a join
    vars_biologicals = c("totwgt", "totno", "spec" )
    vars_set = setdiff( names(k), vars_biologicals )

    if (length(selection$species) > 1) {
      warning("you probably want to aggregate and create new species id code before running this...")
      warning(" or modify code here to do the aggregation")
    }

    species.specific = k[ which(k$spec == selection$biologicals$spec ), c("id", vars_biologicals) ]  # keep id to join back ..

    set.specific = k[ match( sort(unique(k[,"id"])), k[,"id"] ), vars_set] # unique first locations that match .. set variables only

    # ensure all species-specific entries are consistent
    set = merge(set.specific, species.specific, by="id", all.x=TRUE )
    set$spec = na.omit(unique(set$spec))[1]
    set$name.scientific = na.omit(unique(set$name.scientific))[1]
    set$name.common = na.omit(unique(set$name.common))[1]
    set$itis.tsn = na.omit(unique(set$itis.tsn))[1]

    # totno and towgt are not adjusted .. they are simply the values from the catch tables ("cat")

    set$totno[is.na(set$totno)] = 0
    set$totwgt[is.na(set$totwgt)] = 0

    areal_units_timeperiod = "pre2014"  # "pre2014" for older
    sppoly = maritimes_groundfish_strata( areal_units_timeperiod=areal_units_timeperiod )
    set = maritimes_groundfish_strata_identify( Y=set, sppoly=sppoly, xyvars=c("lon", "lat"), planar_crs_km=areal_units_proj4string_planar_km  )

    if (exists("survey", selection)) {  # filter survey information
      if (exists("polygon_enforce", selection$survey) ) {
        set = set[ which(!is.na(set$AUID)), ] # remove unsetegorized sets
      }
      if (exists("months", selection$survey) ) set = set[ which(month(set$timestamp) %in% selection$survey[["months"]] ), ]
      if (exists("strata_toremove", selection$survey) ) {
        todrop = which(set$AUID %in% strata_definitions( selection$survey[["strata_toremove"]] ) )
        if (length(todrop) > 0) set = set[- todrop , ]
      }
      isc = filter_data( set, selection$survey )
      if (length(isc) > 0) set = set[isc,]
      isc = NULL
    }
  

    return(set)

      # dim(k)  # after last filter
      # [1] 35990    68
      #
      # dim(species.specific)
      # [1] 817   7
      # > dim(set.specific)
      # [1] 1686   62   # 2 more .. due to direct categorization of strata (vs via coordinates)
      # R> dim(set)
      # [1] 1686   62
      # sum(set$totno)
      # [1] 15261
      # sum(set$totwgt)
      # [1]  9682.6
  }
