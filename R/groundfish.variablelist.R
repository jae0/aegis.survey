
  groundfish.variablelist = function(component="all.data") {
    
    V = switch( EXPR=component,
      
      sp.list = c( 
        "forage.fish", "all", "allfish", "elasmobranchs", "gadoid", "flatfish",
        "demersal", "large.demersal", "small.demersal",
        "pelagic", "large.pelagic", "small.pelagic",
        "commercial", "noncommercial", 
        "cod", "haddock", "american.plaice", "silver.hake", "white.hake", 
        "capelin", "herring", "mackerel", "sandlance", "redfish", "wolffish",
        "winter.flounder", 
        "spiny.dogfish",  "thornyskate",
        "crabs", "snowcrab", "northernshrimp", "squid" 
      ), 
     
      multispecies = c( "all", "elasmobranchs", "demersal", "large.demersal", "small.demersal",
        "pelagic", "large.pelagic", "small.pelagic", "flatfish", "commercial", "noncommercial"
      ),
     
      days = c("all.1km.10day", "all.50km.10day", "all.1km.50day", "all.50km.50day" 
      ),

      all = c(
        paste( "totno", groundfish.variablelist("sp.list"), sep="." ),
        paste( "totwgt", groundfish.variablelist("sp.list"),  sep="." ),
        paste( "ntaxa", groundfish.variablelist("multispecies"),  sep="." ),
        paste( "rmean", groundfish.variablelist("sp.list"),  sep="." ),
        paste( "pmean", groundfish.variablelist("sp.list"),  sep="." ),
        paste( "mmean", groundfish.variablelist("sp.list"),  sep="." ),
  #      paste( "lmean", groundfish.variablelist("sp.list"),  sep="." ),
        paste( "nss.rsquared", groundfish.variablelist("days"), sep="."),
        paste( "nss.df", groundfish.variablelist("days"), sep="."),
        paste( "nss.b0", groundfish.variablelist("days"), sep="."),
        paste( "nss.b1", groundfish.variablelist("days"), sep="."),
        paste( "nss.shannon", groundfish.variablelist("days"), sep="."),
        paste( "nss.evenness", groundfish.variablelist("days"), sep="."),
        paste( "nss.Hmax", groundfish.variablelist("days"), sep="."),
        paste( "ntaxa", "annual",c(1,seq(20,200,20)), sep="."),
        "C", "Z", "sar.rsq", "Npred", 
        "mr", "mrT", "smr", "smrT", "mrPvalue", "mrPvalueT",
        "ca1", "ca2", "shannon", "evenness", "Hmax",
        "sdepth", "temp", "sal", "oxyml", "julian"
      ),

      catch.summary = groundfish.variablelist("sp.list") ,
  
               
      physical = c("z", "t", "julian"),

      scaled.centered =c ("dummyvariable"), # swtich does not like a null vector

      log.transform = c(
        paste( "totno", groundfish.variablelist("sp.list"), sep="." ),
        paste( "totwgt", groundfish.variablelist("sp.list"),  sep="." ),
        "Npred", "mr", "mrT",
        "landings", "dZ", "ddZ"
      )
    ) # end switch
    
    V = sort( unique(  V ) )
    return (V)
  }


