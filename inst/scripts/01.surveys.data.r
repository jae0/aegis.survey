
# update all incoming raw survey data that exist "outside of aegis"
# requires DFO oracle database connectivity

# these are here to show the dependencies of aegis::survey.db()
# run only if they have not already been updated
# .. show other sources here as they become available

# 02.surveys.*.r assimilates and models these data

  if (!exists("year.assessment")) {
    year.assessment=lubridate::year(Sys.Date()) - 1  # choose one
    year.assessment=lubridate::year(Sys.Date())
  }


  # prepare groundfish data -- requires DFO oracle database connectivity
  aegis.survey::groundfish.db(p=p, DS="refresh.all.data.tables", yrs=1970:year.assessment )


  # prepare snow crab data -- requires DFO oracle database connectivity
  source( system.file( "scripts", "01.snowcrab.R", package = "bio.snowcrab") )


  # add other data such as biochem, etc  here
  # see 99.05.biochem.R ... mostl;y done , awaiting database clean up


# etc
