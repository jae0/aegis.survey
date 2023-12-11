
# Prep landings data for assimilation
#todo .. incomplete

  require( aegis.survey )

  year.assessment = 2023
  yrs = 1970:year.assessment


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

