
# update all incoming raw survey data that exist "outside of aegis"
# requires DFO oracle database connectivity

# these are here to show the dependencies of aegis.survey::survey_db()
# run only if they have not already been updated
# .. show other sources here as they become available

# 02.surveys.*.r assimilates and models these data
   year.assessment = 2020


  # prepare groundfish data -- requires DFO oracle database connectivity
  aegis.survey::groundfish_survey_db( DS="refresh.all.data.tables", yrs=1970:year.assessment )


  # prepare snow crab data -- requires DFO oracle database connectivity
  source( file.path( find.package(package="bio.snowcrab"), "scripts", "01.snowcrab.r") )




  # add other data such as biochem, etc  here
  # see 99.05.biochem.R ... mostl;y done , awaiting database clean up



# etc


  # assimilate survey raw data into aegis and lookup some environmental that has been processed by aegis.*
    ## NOTE resolution is fixed at SSE for the following



  # ----------------------------------------------------------
  # glue biological data sets together from various surveys and lookup environmental data where possible

  p = aegis.survey::survey_parameters( yrs=1970:year.assessment )

  survey_db( DS="set.init.redo", p=p )
  survey_db( DS="cat.init.redo", p=p )
  survey_db( DS="det.init.redo", p=p )

  # the following does a lookup of env data ...
  # want to make sure the relevent ones are complete (t, z, etc.)
  survey_db( DS="lengthweight.redo", p=p  )  # # TODO:: parallelize me ... update the local tables (not necessary)
  survey_db( DS="set.base.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
  survey_db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  survey_db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ...
  survey_db( DS="set.redo", p=p ) # sanity checking and year filtering to 1999 - present

  aegis.mpa::figure.bio.map.survey.locations(p=p)  # see mpa/src/_Rfunctions/figure.trawl.density for more control



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

