
survey_parameter_list = function( mf, p ) {

  p$label = mf 

  if (p$speciesname == "Atlantic_cod") {
    
    if (mf == "A.S_fac.T_fac" ) {
      
      p$type="abundance"
      
      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
        totno ~ 1 + offset( data_offset )
          + as.factor(space)
          + as.factor(time)
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "gaussian" 
      p$pW$formula = formula ( 
        meansize ~ 1 + offset( data_offset )
          + as.factor(space)
          + as.factor(time)
      )

      return(p)

    }
  

    if (mf == "A.SxT" ) {
      
      p$type="abundance"

      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space) * as.factor(time)
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "gaussian" 
      p$pW$formula = formula ( 
          meansize ~ 1  
            + as.factor(space) * as.factor(time)
      )


      return(p)

    }
  
    if (mf == "A.SiT.ST_iid" ) {
      
      p$type="abundance"

      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space) : as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid )  # required to stabilize missing combos
#            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="ar1", hyper=H$ar1_group))  # required to stabilize missing combos
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "gaussian" 
      p$pW$formula = formula ( 
          meansize ~ 1 
            + as.factor(space) : as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid )  # required to stabilize missing combos
#            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="ar1", hyper=H$ar1_group))  # required to stabilize missing combos
      )
      
      return(p)

    }
  

    if (mf == "A.SxT.ST_iid" ) {
      
      p$type="abundance"

      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space) * as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="iid", hyper=H$iid))  # required to stabilize missing combos
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "gaussian" 
      p$pW$formula = formula ( 
          meansize ~ 1 
            + as.factor(space) * as.factor(time)
            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="iid", hyper=H$iid))  # required         )
      )
      
      return(p)

    }
  
    if (mf == "A.S_iid.T_iid" ) {
      
      p$type="abundance"

      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
          totno ~ 1 + offset( data_offset )
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "gaussian" 
      p$pW$formula = formula ( 
          meansize ~ 1  
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
      )

      return(p)
    }


    if (mf == "A.S_iid.T_iid.ST_iid" ) {
 
      p$type="abundance"

      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
          totno ~ 1 + offset( data_offset )
          + f(space, model="iid", hyper=H$iid)
          + f(time,  model="iid", hyper=H$iid )
          + f(space_time, model="iid", group=time_space, hyper=H$iid)
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "gaussian" 
      p$pW$formula = formula ( 
          meansize ~ 1  
          + f(space, model="iid", hyper=H$iid)
          + f(time,  model="iid", hyper=H$iid )
          + f(space_time, model="iid", group=time_space, hyper=H$iid)
      )

      return(p)
    }


    if (mf == "A.S_bym2.T_fac.ST_bym2.env.eco" ) {
 
      p$type="abundance"

      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(time)  
#               + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
#            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )

      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "lognormal" 
      p$pW$formula = formula ( 
          meansize ~ 1  
            + as.factor(time)   
#               + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
#            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )
      return(p)
    }



    if (mf == "A.S_bym2.T_ar1.ST_bym2.env.eco" ) {
 
      p$type="abundance"
      
      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
        totno ~ 1 + offset( data_offset )
            + f( gear, model="iid",  hyper=H$iid ) 
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "lognormal" 
      p$pW$formula = formula ( 
          meansize ~ 1  
            + f( gear, model="iid",  hyper=H$iid ) 
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )
  
      return(p)
  
    } 
    

    if (mf == "A.full_model" ) {
 
      p$type="abundance"
      
      p$pN = p
      p$pN$label ="Atlantic cod summer standardtow totno"
      p$pN$carstm_model_label = mf
      p$pN$variabletomodel = "totno"  
      p$pN$family = "poisson" 
      p$pN$formula = formula( 
        totno ~ 1 + offset( data_offset )
#            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid)) 
            + f( gear, model="iid",  hyper=H$iid ) 
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )


      p$pW = p
      p$pW$label ="Atlantic cod summer standardtow meansize"
      p$pW$carstm_model_label = mf
      p$pW$variabletomodel = "meansize"  
      p$pW$family = "lognormal" 
      p$pW$formula = formula ( 
          meansize ~ 1  
#            + f( vessel, model="iid",  hyper=H$iid, group=gear, control.group=list(model="iid", hyper=H$iid)) 
            + f( gear, model="iid",  hyper=H$iid ) 
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )
  
      return(p)
  
    }  # end abundance


    ## habitat

    if (mf == "H.S_fac.T_fac" ) {
      
      p$type="habitat"
    
      p$pH = p
      p$pH$label ="Atlantic cod summer standardtow habitat"
      p$pH$carstm_model_label = mf
      p$pH$variabletomodel = "pa"  
      p$pH$family = "binomial" 
      p$pH$formula = formula( 
          pa ~ 1  
            + as.factor(space)
            + as.factor(time)
      )
  
      return(p)

    }
  
    if (mf == "H.S_iid.T_iid" ) {
      
      p$type="habitat"

      p$pH = p
      p$pH$label ="Atlantic cod summer standardtow habitat"
      p$pH$carstm_model_label = mf
      p$pH$variabletomodel = "pa"  
      p$pH$family = "binomial" 
      p$pH$formula = formula( 
          pa ~ 1  
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
      )

      return(p)

 
    }
  


    if (mf == "H.S_iid.T_iid.ST_iid" ) {
 
      p$type="habitat"

      p$pH = p
      p$pH$label ="Atlantic cod summer standardtow habitat"
      p$pH$carstm_model_label = mf
      p$pH$variabletomodel = "pa"  
      p$pH$family = "binomial" 
      p$pH$formula = formula( 
          pa ~ 1 
          + f(space, model="iid", hyper=H$iid)
          + f(time,  model="iid", hyper=H$iid )
          + f(space_time, model="iid", group=time_space, hyper=H$iid)
      )

      return(p)
 

    if (mf == "H.S_bym2.T_fac.ST_bym2.env.eco" ) {
 
      p$type="habitat"

      p$pH = p
      p$pH$label ="Atlantic cod summer standardtow habitat"
      p$pH$carstm_model_label = mf
      p$pH$variabletomodel = "pa"  
      p$pH$family = "binomial" 
      p$pH$formula = formula( 
          pa ~ 1 
            + as.factor(time)  
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )
      
      return(p)
    }


    if (mf == "H.S_bym2.T_ar1.ST_bym2.env.eco" ) {
 
      p$type="habitat"

      p$pH = p
      p$pH$label ="Atlantic cod summer standardtow habitat"
      p$pH$carstm_model_label = mf
      p$pH$variabletomodel = "pa"  
      p$pH$family = "binomial" 
      p$pH$formula = formula( 
          pa ~ 1 
            + f( time, model="ar1",  hyper=H$ar1 ) 
            + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
            + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
            + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
      )
      
      return(p)
    }

  }  # end habitat


 }  # end atlantic cod


}