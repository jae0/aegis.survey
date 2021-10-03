
# update all incoming raw survey data that exist "outside of aegis"
# requires DFO oracle database connectivity

# these are here to show the dependencies of aegis.survey::survey_db()
# run only if they have not already been updated
# .. show other sources here as they become available

# 02.surveys.*.r assimilates and models these data
   year.assessment = 2021


      if (0) {
        # to manually extract data from Oracle on MSWindows: (use R - 3.6.3 in virutalbox ..)
        # the following uses minimal libraries and parameter settings 
        
        require(ROracle)

        years_to_extract = 2018:2021
        
        fn.root =  file.path( getwd(), "trawl", "gscat" )
        dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

        for ( YR in years_to_extract ) {
          fny = file.path( fn.root, paste( YR,"rdata", sep="."))
          gscat = ROracle::dbGetQuery( connect,  paste(
            "select i.*, substr(mission,4,4) year " ,
            " from groundfish.gscat i " ,
            " where substr(MISSION,4,4)=", YR)
          )

          names(gscat) =  tolower( names(gscat) )
          print(fny)
          save(gscat, file=fny, compress=T)
          gc()  # garbage collection
          print(YR)
        }
      ROracle::dbDisconnect(connect)

    

        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

        fn.root =  file.path( getwd(), "trawl", "gsdet" )
        dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

        for ( YR in years_to_extract ) {
          fny = file.path( fn.root, paste( YR,"rdata", sep="."))
          gsdet = ROracle::dbGetQuery( connect,  paste(
            "select i.*, substr(mission,4,4) year" ,
            " from groundfish.gsdet i " ,
            " where substr(mission,4,4)=", YR)
          )
          names(gsdet) =  tolower( names(gsdet) )
          gsdet$mission = as.character( gsdet$mission )
          save(gsdet, file=fny, compress=T)
          print(fny)
          gc()  # garbage collection
          print(YR)
        }
      
      ROracle::dbDisconnect(connect)

      

        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

        fn.root =  file.path(getwd(), "trawl", "gsinf" )
        dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

        for ( YR in years_to_extract ) {
          fny = file.path( fn.root, paste( YR,"rdata", sep="."))
          gsinf = ROracle::dbGetQuery( connect,  paste(
            "select * from groundfish.gsinf where EXTRACT(YEAR from SDATE) = ", YR )
          )
          names(gsinf) =  tolower( names(gsinf) )
          save(gsinf, file=fny, compress=T)
          print(fny)
          gc()  # garbage collection
          print(YR)
        }
      ROracle::dbDisconnect(connect)

    
        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

        fn = file.path( getwd(),  "trawl", "gsgear.rdata")
        gsgear =  ROracle::dbGetQuery(connect, "select * from groundfish.gsgear", as.is=T)
        ROracle::dbDisconnect(connect)
        names(gsgear) =  tolower( names(gsgear) )
        save(gsgear, file=fn, compress=T)
        print(fn)

      ROracle::dbDisconnect(connect)

    
      
        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)
      fn = file.path( getwd(),  "trawl", "gslist.rdata")
        gslist = ROracle::dbGetQuery(connect, "select * from groundfish.gs_survey_list")
        ROracle::dbDisconnect(connect)
        names(gslist) =  tolower( names(gslist) )
        save(gslist, file=fn, compress=T)
        print(fn)
      ROracle::dbDisconnect(connect)

    
        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

        fnmiss = file.path( getwd(),  "trawl", "gsmissions.rdata")
        gsmissions = ROracle::dbGetQuery(connect, "select MISSION, VESEL, CRUNO from groundfish.gsmissions")
        ROracle::dbDisconnect(connect)
        names(gsmissions) =  tolower( names(gsmissions) )
        save(gsmissions, file=fnmiss, compress=T)
        print(fnmiss)

      ROracle::dbDisconnect(connect)

    
        connect = ROracle::dbConnect( DBI::dbDriver("Oracle"), dbname=oracle.groundfish.server, username=oracle.personal.user, password=oracle.personal.password, believeNRows=F)

        fnspc = file.path( getwd(),  "trawl", "spcodes.rdata" )
        spcodes = ROracle::dbGetQuery(connect, "select * from groundfish.gsspecies", as.is=T)
        ROracle::dbDisconnect(connect)
        names(spcodes) =  tolower( names(spcodes) )
        save(spcodes, file=fnspc, compress=T)
        print( fnspc )
        print("Should follow up with a refresh of the taxonomy.db " )

        ROracle::dbDisconnect(connect)


      }




  # prepare groundfish data -- requires DFO oracle database connectivity
  aegis.survey::groundfish_survey_db( DS="refresh.all.data.tables", yrs=1970:year.assessment )
    if (0) {
      # or in manual mode:
      # the following are done in "refresh.all.data.tables
       p = aegis.survey::groundfish_parameters( yrs=1970:year.assessment )
    
        groundfish_survey_db(p=p, DS="gscat.base.redo" )
        groundfish_survey_db(p=p, DS="gsdet.redo" )
        groundfish_survey_db(p=p, DS="gsinf.redo" )
        groundfish_survey_db(p=p, DS="sweptarea.redo" )  ## this is actually gsinf with updated data, etc.
        # merged data sets
        groundfish_survey_db(p=p, DS="set.base.redo"  ) # set info .. includes netmensuration.scanmar("sweptarea")
        groundfish_survey_db(p=p, DS="gscat.redo"  ) # catches .. add correction factors
      }



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

