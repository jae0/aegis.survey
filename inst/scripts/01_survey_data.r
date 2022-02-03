
# update all incoming raw survey data that exist "outside of aegis"
# requires DFO oracle database connectivity

# these are here to show the dependencies of aegis.survey::survey_db()
# run only if they have not already been updated
# .. show other sources here as they become available

  require( aegis.survey )

  year.assessment = 2021
  yrs = 1970:year.assessment

# ----------------------------------------
# absorb groundfish data -- requires DFO oracle database connectivity
 
  if ( data_dump_from_a_full_installation ) {
    # full data dump of all years in correct locations
    # rawdata data dump of aegis tables
    groundfish_survey_db( DS="gscat.rawdata.redo", yrs=yrs )
    groundfish_survey_db( DS="gsdet.rawdata.redo", yrs=yrs )
    groundfish_survey_db( DS="gsinf.rawdata.redo", yrs=yrs )

    # groundfish_survey_db( DS="gshyd.profiles.rawdata.redo", yrs=p$yrs )

    groundfish_survey_db( DS="gsmissions.rawdata.redo" ) #  not working?

    update.infrequently = FALSE
    if (update.infrequently) {
      # the following do not need to be updated annually
      groundfish_survey_db( DS="spcodes.rawdata.redo"  )
      groundfish_survey_db( DS="gslist.rawdata.redo"  )
      groundfish_survey_db( DS="gsgear.rawdata.redo"  )
      groundfish_survey_db( DS="gsstratum.rawdata.redo"  )
    }
  }



# ----------------------------------------
# process groundfish data 

  require(bio.taxonomy)

  groundfish_survey_db( DS="gscat.base.redo", yrs=yrs )
  groundfish_survey_db( DS="gsdet.redo", yrs=yrs )
  groundfish_survey_db( DS="gsinf.redo", yrs=yrs )


  if (redo_historical_netmensuration_estimates ) {
    
    WARNING::: DO NOT RUN THIS PART 

    # only relevent for a short window of time .. no need to rerun
    # source ( file.path( find.package(package="bio.snowcrab"), "scripts", "99_netmensuration_historical_data.R" ) )  # in aegis_survey:: scripts directory

      # requires gsinf
      p = groundfish_parameters()
      p$netmensuration.years = intersect( p$netmensuration.years, p$yrs ) #  1990:1992, 2004:max(p$yrs)

      # set id's that should be skipped:: corrupted/improper due to faulty sensor data, etc.
      p$problem.sets = c("NED2014018.27", "NED2014101.11", "NED2014101.12", "NED2014101.13",  "NED2014101.14",
              "NED2010027.143")

      # the following works upon many or annual time slices ( defined in p$netmensuration.years )
      netmensuration.scanmar( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
      netmensuration.scanmar( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
      netmensuration.scanmar( DS="sanity.checks.redo",  p=p )      # QA/QC of data
      netmensuration.scanmar( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

      # netmind base data filtered for fishing periods .. not really used except for some plots
      netmensuration.scanmar( DS="scanmar.filtered.redo",  p=p )

      netmensuration.figures( DS="all", p=p )
      # netmensuration.figures( DS="all", p=p, outdir=file.path(loc, "output")  )
      # see scripts/99.example.netmensuration.r for some additional figures, etc.

  }

  # swept areas are computed in bottom.contact.redo ..
  # this step estimates swept area for those where there was insufficient data to compute SA directly from logs,
  # estimate via approximation using speed etc.
  groundfish_survey_db( DS="sweptarea.redo", yrs=yrs )  ## this is actually gsinf with updated data, etc.

  # these are now obsolete:
  #groundfish_survey_db( DS="gshyd.profiles.redo", yrs=yrs  )
  #groundfish_survey_db( DS="gshyd.redo", yrs=yrs  )
  #groundfish_survey_db( DS="gshyd.georef.redo", yrs=yrs  )  # not used here but used in temperature re-analysis

    if (0) {

      # lookupregion = lookup.strata()  # create strata vs region lookup table

      Vn = as.character(c(440:442))   # these are alphanumeric codes
      Vs = as.character(c(443:452))   # these are alphanumeric codes
      W  = as.character(c(453:466))   # these are alphanumeric codes
      X  = as.character(c(470:495))   # these are alphanumeric codes
      Ge = c("5Z1","5Z2","5Z3","5Z4","5Z5","5Z6","5Z7","5Z8")

      lookupregion = data.frame(strat=c(Vn, Vs, W, X, Ge), region=NA)
      lookupregion$strat = as.character(lookupregion$strat)

      lookupregion$region[lookupregion$strat %in% Vn] = "Vn"
      lookupregion$region[lookupregion$strat %in% Vs] = "Vs"
      lookupregion$region[lookupregion$strat %in% W ] = "W"
      lookupregion$region[lookupregion$strat %in% X ] = "X"
      lookupregion$region[lookupregion$strat %in% Ge] = "Ge"

    }

  # merged data sets
  groundfish_survey_db( DS="set.base.redo", yrs=yrs  ) # set info .. includes netmensuration.scanmar("sweptarea")
  groundfish_survey_db( DS="gscat.redo", yrs=yrs  ) # catches .. add correction factors




# ----------------------------------------
# prepare snow crab data -- requires DFO oracle database connectivity

  if ( data_dump_from_a_full_installation ) {
    # full data dump of all years in correct locations
    run relevant  items in: 
     
     file.path( find.package(package="bio.snowcrab"), "scripts", "01.snowcrab.r") 

  }



# ----------------------------------------
# add other data such as biochem, etc  here
# see 99.05.biochem.R ... mostl;y done , awaiting database clean up



  # etc


# ----------------------------------------
# assimilate survey raw data into aegis and lookup some environmental that has been processed by aegis.*
## NOTE resolution is fixed at SSE for the following



# ----------------------------------------------------------
# FINALLY: glue biological data sets together from various surveys and lookup environmental data where possible
  require(aegis.survey)

  p = survey_parameters( yrs=yrs )

  o = survey_db( DS="set.init", p=p, redo=TRUE ) ; head(o)
  o = survey_db( DS="cat.init", p=p, redo=TRUE  ) ; head(o)
  o = survey_db( DS="det.init", p=p, redo=TRUE  ) ; head(o)

  # the following does a lookup of env data ...
  # want to make sure the relevent ones are complete (t, z, etc.)
  o = survey_db( DS="lengthweight.parameters", p=p, redo=TRUE   ) ; head(o) # # TODO:: parallelize me ... update the local tables (not necessary)
  o = survey_db( DS="set.base", p=p, redo=TRUE  ); head(o) # adds temperature required for metabolism lookup in "det"
  o = survey_db( DS="det", p=p, redo=TRUE  ) ; head(o) # mass/length imputation and sanity checking
  o = survey_db( DS="cat", p=p, redo=TRUE  ) ; head(o) # sanity checking and fixing mass estimates from det etc ...
  o = survey_db( DS="set", p=p, redo=TRUE  ) ; head(o) # sanity checking and year filtering to 1999 - present

  # aegis.mpa::figure.bio.map.survey.locations(p=p)  # see mpa/src/_Rfunctions/figure.trawl.density for more control



# ----------------------------------------------------------
# landings are like surveys -- but we do not want to interpolate this
# .. rather aggregate into meaningful areas, model patterns and map
# or modify as supplemntary data for distributional models
  if (0) {

    pl = aegis.survey::landings_parameters( yrs=1970:year.assessment )  # these are default years
    landings_db( DS="rawdata", p=pl )

    for ( vn in p$varstomodel) {
      print(vn)
      aegis_db ( DS="complete.redo", p=p )
      aegis_db_map( p=p )
    }

  }


### end

