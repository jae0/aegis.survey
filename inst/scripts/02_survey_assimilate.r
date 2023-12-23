
# assimilate all incoming raw survey data that exist "outside of aegis"
# currently this means: groundfish and snowcrab data 
# the above requires DFO oracle database connectivity

  require( aegis.survey )

  year.assessment = 2023
  yrs = 1970:year.assessment
 

# ----------------------------------------
# assimilate survey raw data into aegis and lookup some environmental that has been processed by aegis.*
## NOTE resolution is fixed at SSE for the following
 
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



### end

