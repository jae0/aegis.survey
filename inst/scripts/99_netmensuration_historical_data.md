---
title: "Netmensuration Historical Data"
author: "Jae S. Choi"
toc: true
number-sections: true
highlight-style: pygments
editor:
  render-on-save: false
format:
  html: 
    code-fold: true
    html-math-method: katex
    embed-resources: true
  pdf:
    pdf-engine: lualatex
  docx: default 
---
 

<!-- This is a Markdown/Quarto document -->

<!-- 
Copy this file to a work directory (e.g., ~/tmp/ ) 
and run Quarto from there:

# quarto render *.qmd --to html 

Can add "--to docx --to pdf" as additional documents, but their formatting is awkward and will require more work.  
-->

```r 
    # set up libs and bio functions
    p = aegis.survey::groundfish_parameters( year.assessment=2016 )

    # steps required to recreate a local database of all data
    recreate.perley.db = FALSE
    if ( recreate.perley.db ) {
      # define these in your Rprofile
      # oracle.perley.user ="username"
      # oracle.perley.password = "password"
      # oracle.perley.db = "servername"
      netmensuration.scanmar( DS="perley.datadump", p=p ) # rawdata data dump .. this step requires definition of password etc
      netmensuration.scanmar( DS="perley.redo", p=p )    # perley had two db's merge them together: from XXXX-2002 and 2006 to 200X
      ## NOTE:: A cautionary note.. I am suspicious of the ID's that are given in the Perley data series ...
      ##        Defualt matches using time/location suggest there might have been some errors when entered into the database..
    }


    # the following works upon many or annual time slices ( defined in p$netmensuration.years )
    netmensuration.scanmar( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
    netmensuration.scanmar( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
    netmensuration.scanmar( DS="sanity.checks.redo",  p=p )      # QA/QC of data


    # WARNING:: the following may crash as INLA does not exit gracefully from some errors
    # and R cannot catch the faults .. restart R or reboot the system (it can happen)
    # doing it year by year is probably wise for now
    # usually insufficient data for these or just flat-lines .. no reliable data

    if (FALSE) {
      # challenging tests
      totest = c("TEL2004529.20", "TEL2004529.16", "TEL2004530.84",  "TEL2004530.85", "NED2015002.12",
                 "NED2010001.36", "NED2010002.1",
       )

      # nomatch in gsinf?
      totest = c( "NED2010027.62", "NED2010027.117", "NED2010027.118",  "NED2010027.201",
       "NED2014101.16", "NED2014101.21", "NED2014101.14", "NED2014101.16", "NED2014101.21",
      "NED2014002.1", "NED2014102.5",
      "NED2014102.29", "NED2014102.33", "NED2014102.34", "NED2014102.37", "NED2014102.39", "NED2015002.57")

      bc = netmensuration.scanmar( DS="bottom.contact.redo",  p=p , debugid=totest )  ## a lot of data ???

      # lots of 20 min tows in 2012 and 2014
      # and some are really 15 min !

      # to  load a single result and view
      # bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2010027.53") # strange wingspreads
      # bottom.contact.plot( bc, netspread=TRUE )
    }

    netmensuration.scanmar( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

    # swept areas are computed in bottom.contact.redo ..
    # this step estimates swept area for those where there was insufficient data to compute SA directly from logs,
    # estimate via approximation using speed etc.
    groundfish_survey_db( DS="sweptarea.redo" )

    netmensuration.figures( p=p, DS="all" )


    # netmind base data filtered for fishing periods .. not really used except for some plots
    netmensuration.scanmar( DS="scanmar.filtered.redo",  p=p )

    create.marport.database = FALSE
    if (create.marport.database ) {
      p$netmensuration.years = 2013:2014
      netmensuration.marport( DS="basedata.redo",  p=p )      # load data from raw data files
      netmensuration.marport( DS="gated.redo",  p=p )      # QA/QC of data
      marport = netmensuration.marport( DS="gated",  p=p )
    }


    # --- misc stats / analysis

    gs0 = netmensuration.scanmar( DS="bottom.contact", p=p )  # bring in estimates of bottom contact times from scanmar



    gs0$timediff_official =  as.numeric(gs0$edate - gs0$sdate) / 60 # that which would have been used (if at all .. )
    gs0$bottom.dist = geosphere::distGeo( gs0[, c("bc.lon0","bc.lat0")], gs0[, c("bc.lon1", "bc.lat1")] )/1000


    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
            #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
            #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

    # setype
    table(gs0$settype)
    #    1    3    4    5    8    9
    # 5333  294    1  608  147   59


    # settype vs mean duration
    tapply( gs0$bottom_duration/60, gs0$settype, mean, na.rm=TRUE )
    #        1        3        4        5        8        9
    # 29.76567      NaN      NaN 28.45052      NaN      NaN



    w2a = which( gs0$geardesc == "Western IIA trawl" & gs0$settype %in% c(1,2,5) )     # for distribution checks for western IIA trawl
    gs = gs0[ w2a, ]

    # trawl duration bias
    plot( jitter(gs$timediff_official), jitter(gs$bottom_duration/60), xlim=c(10,40), cex=0.65, col="slateblue",
         xlab="Trawl duration: official (minutes)", ylab="Trawl duration: computed (minutes)" )
    abline(a=0,b=1, col="grey" )


    # distance bias
    plot( jitter(gs$dist_km), jitter(gs$bottom.dist), xlim=c(1.5,4),ylim=c(0,5),cex=0.65, col="slateblue",
         xlab="Trawl length: official (km)", ylab="Trawl length: computed from end points (km)" )
    abline(a=0,b=1, col="grey" )


    # distance bias
    plot( jitter(gs$bc.dist), jitter(gs$bottom.dist), xlim=c(1.5,6),ylim=c(0,6),cex=0.65, col="slateblue",
         xlab="Trawl length: official (km)", ylab="Trawl length: computed from track(km)" )
    abline(a=0,b=1, col="grey" )


    # distance bias ( vert has no influence)
    plot( jitter(gs$bc.dist), jitter(gs$bc.dist.h), xlim=c(1.5,4),ylim=c(0,5),cex=0.65, col="slateblue",
         xlab="Trawl length: total (km)", ylab="Trawl length: horiz (km)" )
    abline(a=0,b=1, col="grey" )




    # swept area (door width X distance)
    plot( door.sa ~ as.factor(yr), gs[ gs$yr> 2009,], ylab="Door width (m)", xlab="Year" )

    plot( door.sa ~ log(bottom_depth), gs[ ,], xlim=log(c(20,650)) )

    # Swept area vs depth
    plot( (door.sa) ~ log(bc.depth.mean), gs[ ,], col="slategray", cex=0.5, xlab="log Depth (m)", ylab="Swept area (km^2)" )

    # points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2010 ,], pch="*", col="red" )
    points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2011 ,], pch=20, col="steelblue", cex=1.25 )
    #points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2012 ,], pch=22, col="orange" )
    #points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2013 ,], pch=19, col="brown", cex= 0.8 )
    #points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2014 ,], pch=19, col="brown", cex= 0.8 )
    #points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2015 ,], pch=19, col="red", cex= 0.8 )


    plot( bottom_duration ~ as.factor( strat), gs[ gs$yr> 2008,] )
    plot( bottom_duration ~ bottom_salinity, gs[ ,], xlim=c(31,36) )
    plot( door.sa ~ bottom_salinity, gs[ ,], xlim=c(31,36) )
    plot( door.sa ~ bottom_depth, gs[ ,], xlim=c(31,36) )

    plot( door.sa ~ log(bottom_depth), gs[ ,], xlim=log(c(20,650)) )




    plot( bottom_duration ~ as.factor(yr), gs[ gs$yr> 2008,] )

    plot( (bottom_duration) ~ bc.depth.mean, gs[ ,], pch=".", col="red" )
    points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2010 ,], pch="*", col="red" )
    points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2011 ,], pch="*", col="blue" )
    points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2012 ,], pch=22, col="orange" )
    points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2013 ,], pch=19, col="brown", cex= 0.8 )
    points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2014 ,], pch=19, col="brown", cex= 0.8 )
    points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2015 ,], pch=19, col="red", cex= 0.8 )

 

    pp = tapply( gs$id, year(gs$bc0.datetime), function(x) { length(unique(x))} )
    pp = data.frame( yr= rownames(pp), n.bc=pp)
    #netmensuration.scanmar( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

    oo = tapply( gs$id, year(gs$sdate), function(x) { length(unique(x))} )
    oo = data.frame( yr = rownames(oo), n.gs= oo)
    length( gs[ which(is.finite(gs$bottom_duration) ), "id" ] )
    length( gs[ which(is.finite(gs$bottom_duration) & gs$bc0.sd<30 & gs$bc1.sd<30), "id" ] )


    sc = netmensuration.scanmar( DS="sanity.checks",  p=p )      # QA/QC of data
    rr = tapply( sc$id, sc$year, function(x) { length(unique(x))} )
    rr = data.frame( yr= rownames(rr), n.scanmar=rr )

    nm = netmensuration.scanmar( DS="scanmar.filtered",  p=p )  # bring in estimates of bottom contact times from scanmar
    qq = tapply( nm$id, nm$year, function(x) { length(unique(x))} )
    qq = data.frame( yr= rownames(qq), n.filtered=qq )


    res = merge ( oo, pp, by="yr")
    res = merge ( res, qq, by="yr")
    res = merge ( res, rr, by="yr")





    # ----
    # debugging SA estimates

    g = netmensuration.scanmar( DS="bottom.contact",  p=p )
    plot( dist_km~ I(wing.sa / wing.mean*1000), g, ylim=c(0,5) )
    gr = abs(g$dist_km - (g$wing.sa / g$wing.mean)*1000)
    strange = which ( gr > 1 & g$gear==9 & g$settype==1 )
    g[strange, "id"]

#    "NED2014101.23" "NED2011002.6"  "NED2014101.15" "NED2015002.20"

    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2014018.71")  # depth sensor not working
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2013022.192") # large depth range
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2013022.205") # depth sensor not working
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2013022.208") # GPS not working
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2011002.53")  #
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2011002.45")  # doorspread failure and almost no wingspread
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2014101.23")
    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2011002.6")


    bottom.contact.plot( bc, netspread=TRUE )


    bc = netmensuration.scanmar( DS="bottom.contact.redo",  p=p , debugid= "NED2011002.45") # GPS not working
    bc = netmensuration.scanmar( DS="bottom.contact.redo",  p=p , debugid= "NED2013022.208") # GPS not working

    bc = netmensuration.scanmar( DS="bottom.contact",  p=p , setid= "NED2009002.17")  # doorspread failure and almost no wingspread


    bottom.contact.plot( bc, netspread=TRUE )
    plot( longitude ~ latitude, bc$plotdata )
    plot( wingspread ~ ts, bc$plotdata )
    plot( doorspread ~ ts, bc$plotdata )

    uu = netmensuration.scanmar( DS="sanity.checks",  p=p, YR=2011 )
    uu = netmensuration.scanmar( DS="basedata", p=p, YR=2011 )        # Assimilate Scanmar files in raw data saves *.set.log files
    vv = netmensuration.scanmar( DS="basedata.lookuptable", p=p )
    ww = which( vv$id=="NED2009027.25" )
    xx = uu[ which( uu$nm_id == vv$nm_id[ww] ) , ]
    plot( wingspread ~ timestamp, xx )
    plot( doorspread ~ timestamp, xx )

```