
# aegis.survey

This R-package integrates data from various surveys. Currently they include:

- ![Groundfish RV surveys](https://github.com/jae0/aegis.survey/blob/master/R/groundfish_survey_db.R); and,

- ![Snow crab surveys](https://github.com/jae0/bio.snowcrab/blob/master/README.md).

but of course is readily extended to any other survey. Use the above as guides.


The main categories of information in the internal data structures operation upon:
- "set" for sample events at a set-level (location, time, etc.)
- "cat" for information on captured organisms (biomass, number) by species,
- "det" for information measured from individual orgamisms (length, width, mass).

For data extraction, a structured list of selection criteria are required and correspond to the above levels of informtion:
- "biologicals" define individual level data and correspond to "cat" and "det" information
- "surveys" define sample event level data and correspond wit "set" information .

They are simple and easily extensible.

- "aggregate" is a list that has options related to:
  - drop.unreliable.zeros.groundfish.data - TRUE/FALSE : unreliable zero's for snowcrab in the groundfish data
  - pattern matching on any set-level information defined by a "variable_name = list()" 
- "survey" is a list with the following options:
  - polygon_enforce : ensure AUID is specified and drop any that are not
  - strata_to_remove : drop areal units that match
  - strata_to_keep : ensure that specified areal units are retained
  - months : retain data that match specified months 
  - pattern matching on any set-level information defined by a "variable_name = list()"
- "biologicals" is a list with the following options:
  - pattern matching on any catt-level information defined by a "variable_name = list()"
  - pattern matching on any det-level information defined by a "variable_name = list()"
  - spec_bio : internal species code ( accessed via groundfish code with a helper function, bio.taxonomy::taxonomy.recode ) 

filter_data is the core function that does the pattern matching. However, it also can make ranged selections using the following syntax:
  - "ranged_data", "less_than", "greater_than"

For example, with Atlantic cod in the Martimes Region of Canada, one can specify:

```r
require(aegis.survey)

spatial_domain = "SSE"  # short for Scotian Shelf Ecosystem or NAFO Div. 4VWX)
yrs = 1970:2021
groundfish_survey_species_code = 10 # cod

# basic selection criteria
selection = list(
  biologicals=list(
    spec_bio = bio.taxonomy::taxonomy.recode( from="spec", to="parsimonious", tolookup=groundfish_survey_species_code )
  ),
  survey=list(
    data.source="groundfish",
    yr = yrs,      # time frame for comparison specified above
    months=6:8,
    # dyear = c(150,250)/365, #  summer = which( (x>150) & (x<250) ) , spring = which(  x<149 ), winter = which(  x>251 )
    # ranged_data="dyear"
    settype = 1,
    gear = c("Western IIA trawl", "Yankee #36 otter trawl"),
    strata_toremove=c("Gulf", "Georges_Bank", "Spring", "Deep_Water"),  # <<<<< strata to remove from standard strata-based analysis
    polygon_enforce=TRUE
  )
)

```

The criteria are then passed along with other parameters related to spatial domain and other parameters related to the polygons of the areal units in the domain, normalizing factors (e.g., towdistance) and other treatment of the data:

```r

p = survey_parameters(
  project_class = "stratanal",
  project_name="survey",
  label ="Atlantic cod summer",
  speciesname = "Atlantic_cod",
  trawlable_units = c( "standardtow", "towdistance", "sweptarea")[2],  # arbitrary for below
  carstm_model_label="stratnal",   # default = 1970:present, alt: 1999_present
  selection = selection,
  areal_units_type = "stratanal_polygons_pre2014",
  areal_units_resolution_km = 25, # meaningless here .. just a placeholder for filenaming convention
  areal_units_proj4string_planar_km = projection_proj4string("utm20"), #projection_proj4string("omerc_nova_scotia") ,
  areal_units_overlay = "none",
  areal_units_timeperiod = "pre2014"    # "pre2014" for older
)

sppoly = areal_units( p=p  )  # polygons with areal designations as variable "AUID"

set = stratanal_data(
  toget="stratanal",
  selection=selection,
  trawlable_units="towdistance",
  sppoly=sppoly
)


```

To replicate the "stranal" method used by many groundfish assessments,

```r
results = strata_timeseries(
  set=set,
  variable="totwgt",
  speciesname=p[["speciesname"]],
  yrs=p$yrs,
  alpha.t = 0.05 # confidence interval for t-dist assumption eg. 0.05 = 95%, 0.1 = 90%
)

plot( results$Y ~ p[["yrs"]] )

```

Usually, one just wants the data, aggregated at each set level and not the backwards compatible "Stratanal" solution. This is accomplished via:

```r
set = survey_db( p=p, DS="filter"  ) 

```

If catch-level or individual-level results are required: 

```r
cat = survey_db( p=p, DS="cat.filter"  ) 
det = survey_db( p=p, DS="det.filter"  )
```


<!--

The folowing is the code to be run to generate some plots :

```

  outdir = file.path( "~", "bio", "aegis.survey", "inst", "doc")

  grDevices::png(filename = file.path(outdir, "stratanl_timeseries.png"))
    # using cubic folded root to visualize
    frp = 1/2  # folded root power
    xvals = seq(0, nx, by=20)
    yrange = folded_root(c(0.96, 1), frp)
    yticks = seq( yrange[1], yrange[2], length.out=8)
    yvals = round( folded_root( yticks, frp, inverse=TRUE) *100 , 2)  # convert to %
    graphics::plot( 0,0, type="n", xlab="", ylab="", ylim=yrange, xlim=c(0, nx), axes=FALSE)
    i = 1
    graphics::lines( folded_root(results$Y, frp) ~ p[["yrs"]], col=scales::alpha(colours[i], 0.9), lty=ltypes[i]   )
    graphics::axis( 1, at=xvals )
    graphics::axis( 2, at=yticks, labels=yvals )
    graphics::legend( "bottomleft", legend=aus, col=colours, lty=ltypes, bty="n" )
    graphics::title( xlab="Time (days)", ylab=" XXX (Folded root=1/2)" )
  grDevices::dev.off()


```
 
-->


The utility of the above is that it is quite simple once the parameters are understood. Further, individual level information can e filtered resulting in a fast interface for data extraction and routine modelling. Examples of the latter are found in the ![scripts directory](./inst/README.md)


## Installation


To install, run the following:

```r
  remotes::install_github( "jae0/aegis")  # helper functions
  remotes::install_github( "jae0/aegis.survey")
``` 

You probably will want to have an Rprofile set up properly such as:

```r
homedir = path.expand("~")
code_root = file.path( homedir, "bio" )   ### replace with correct path to the parent directory of your git-projects
data_root = file.path( homedir, "bio.data" )   ### replace with correct path to your data

require( aegis )
require( aegis.survey )

```
 