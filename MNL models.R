rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","wind_perception","tfeel", "greenness", "environ_lifestyle")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250 & dist>0.5)
database <-  subset(database, trip_duration<250 & trip_duration>4)
summary(database)
count(database)
apollo_initialise()

apollo_control=list(modelName="Alternative Spec Constant",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_time_w             = 0,
              b_time_b             = 0,
              b_time_p             = 0,
              b_maxAT_w            = 0,
              b_maxAT_b            = 0,
              b_maxAT_p            = 0,
              b_maxWS_w            = 0,
              b_maxWS_b            = 0,
              b_maxWS_p            = 0,
              b_avgPS_w            = 0,
              b_avgPS_b            = 0,
              b_avgPS_p            = 0,
              b_motive_ww          = 0,
              b_motive_wb          = 0,
              b_motive_wp          = 0,
              b_motive_ew          = 0,
              b_motive_eb          = 0,
              b_motive_ep          = 0,
              b_motive_sw          = 0,
              b_motive_sb          = 0,
              b_motive_sp          = 0,
              b_weekend_w          = 0,
              b_weekend_b          = 0,
              b_weekend_p          = 0
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
 
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_time_w * log(trip_duration)  + b_maxAT_w * Ta_max + b_maxWS_w * Ws_avg18h + b_avgPS_w * Psum_avg18h + b_motive_ww * (motive4==1) + b_motive_ew * (motive4==2) + b_motive_sw * (motive4==3) + b_weekend_w * weekend 
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_time_b * log(trip_duration)  + b_maxAT_b * Ta_max + b_maxWS_b * Ws_avg18h + b_avgPS_b * Psum_avg18h + b_motive_wb * (motive4==1) + b_motive_eb * (motive4==2) + b_motive_sb * (motive4==3) + b_weekend_b * weekend
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_time_p * log(trip_duration)  + b_maxAT_p * Ta_max + b_maxWS_p * Ws_avg18h + b_avgPS_p * Psum_avg18h + b_motive_wp * (motive4==1) + b_motive_ep * (motive4==2) + b_motive_sp * (motive4==3) + b_weekend_p * weekend
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.1= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.1)
apollo_saveOutput(model.1)

#-------------------------------------------------------------------------------------------------

apollo_initialise()

apollo_control=list(modelName="interaction bet greenness and insun",
                    modelDescr="Considering the motive of trip",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
              b_dist_w = 0,
              b_dist_b = 0,
              b_dist_p = 0,
              b_time_w   = 0,
              b_time_b   = 0,
              b_time_p   = 0,
              
            
              b_insun_w   = 0,
              b_insun_b   = 0,
              b_insun_p   = 0,
              b_greenness_w = 0,
              b_greenness_b = 0,
              b_greenness_p = 0,
            
              
              
              b_g_i_w = 0,
              b_g_i_b = 0,
              b_g_i_p = 0
              
              
             
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_time_w * log(trip_duration)  +  b_insun_w * insun + b_greenness_w * greenness + b_g_i_w * insun * greenness
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_time_b * log(trip_duration)  +  b_insun_b * insun + b_greenness_b * greenness + b_g_i_b * insun * greenness
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_time_p * log(trip_duration)  +  b_insun_p * insun + b_greenness_p * greenness + b_g_i_p * insun * greenness
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.2= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.2)
apollo_saveOutput(model.2)

#-------------------------------------------------------------------------------------------------

apollo_initialise()

apollo_control=list(modelName="Interaction bet sun_percept & insun",
                    modelDescr="Considering the motive of trip",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
              b_dist_w = 0,
              b_dist_b = 0,
              b_dist_p = 0,
              b_time_w   = 0,
              b_time_b   = 0,
              b_time_p   = 0,
              
              
              b_insun_w   = 0,
              b_insun_b   = 0,
              b_insun_p   = 0,
              b_sunshine_perception_w = 0,
              b_sunshine_perception_b = 0,
              b_sunshine_perception_p = 0,
              
              
              
              b_sp_i_w = 0,
              b_sp_i_b = 0,
              b_sp_i_p = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_time_w * log(trip_duration)  +  b_insun_w * insun + b_sunshine_perception_w * sunshine_perception + b_sp_i_w * insun * sunshine_perception
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_time_b * log(trip_duration)  +  b_insun_b * insun + b_sunshine_perception_b * sunshine_perception + b_sp_i_b * insun * sunshine_perception
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_time_p * log(trip_duration)  +  b_insun_p * insun + b_sunshine_perception_p * sunshine_perception + b_sp_i_p * insun * sunshine_perception
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.3= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.3)
apollo_saveOutput(model.3)

#-------------------------------------------------------------------------------------------------


apollo_initialise()

apollo_control=list(modelName="Interaction bet wind perc & wind speed",
                    modelDescr="Considering the motive of trip",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
              b_wind_perception_1_w   = 0,
              b_wind_perception_1_b   = 0,
              b_wind_perception_1_p   = 0,
              b_wind_perception_5_w   = 0,
              b_wind_perception_5_b   = 0,
              b_wind_perception_5_p   = 0,
              b_Ws_avg18h_w = 0,
              b_Ws_avg18h_b = 0,
              b_Ws_avg18h_p = 0,
              b_wp_ws_1_w = 0,
              b_wp_ws_1_b = 0,
              b_wp_ws_1_p = 0,
              b_wp_ws_5_w = 0,
              b_wp_ws_5_b = 0,
              b_wp_ws_5_p = 0
      
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     +  b_Ws_avg18h_w * Ws_avg18h + b_wind_perception_1_w * (wind_perception==1) + b_wind_perception_5_w * wind_perception==5 + b_wp_ws_1_w * Ws_avg18h * wind_perception==1 + b_wp_ws_5_w * Ws_avg18h * wind_perception==5
  V[['bicycle']] = asc_bicycle  +  b_Ws_avg18h_b * Ws_avg18h + b_wind_perception_1_b * (wind_perception==1) + b_wind_perception_5_b * wind_perception==5 + b_wp_ws_1_b * Ws_avg18h * wind_perception==1 + b_wp_ws_5_b * Ws_avg18h * wind_perception==5
  V[['pt']]      = asc_pt       +  b_Ws_avg18h_p * Ws_avg18h + b_wind_perception_1_p * (wind_perception==1) + b_wind_perception_5_p * wind_perception==5 + b_wp_ws_1_p * Ws_avg18h * wind_perception==1 + b_wp_ws_5_p * Ws_avg18h * wind_perception==5
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.4= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.4)
apollo_saveOutput(model.4)

---------------------------------------------------------------------------------------------------------------------------
  
  apollo_initialise()

apollo_control=list(modelName="Interaction bet Ta_max & tfeel",
                    modelDescr="Considering the motive of trip",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
              b_dist_w = 0,
              b_dist_b = 0,
              b_dist_p = 0,
              b_time_w   = 0,
              b_time_b   = 0,
              b_time_p   = 0,
              
              
              b_tfeel_w   = 0,
              b_tfeel_b   = 0,
              b_tfeel_p   = 0,
              b_Ta_max_w = 0,
              b_Ta_max_b = 0,
              b_Ta_max_p = 0,
              
              
              
              b_tf_Ta_w = 0,
              b_tf_Ta_b = 0,
              b_tf_Ta_p = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_time_w * log(trip_duration)  +  b_Ta_max_w * Ta_max + b_tfeel_w * tfeel + b_tf_Ta_w * Ta_max * tfeel
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_time_b * log(trip_duration)  +  b_Ta_max_b * Ta_max + b_tfeel_b * tfeel + b_tf_Ta_b * Ta_max * tfeel
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_time_p * log(trip_duration)  +  b_Ta_max_p * Ta_max + b_tfeel_p * tfeel + b_tf_Ta_p * Ta_max * tfeel
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.5= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.5)
apollo_saveOutput(model.5)

-------------------------------------------------------------------------------------------------------------------------------------
  
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","wind_perception","tfeel", "greenness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250 & dist>0.5)
database <-  subset(database, trip_duration<250 & trip_duration>4)
database <-  subset(database, hhincome < 5 )
summary(database)
count(database)
apollo_initialise()

apollo_control=list(modelName="Weekend & environ_lifestyle",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_avgPS_p            = 0,
              b_environ_lifestyle_w = 0,
              b_environ_lifestyle_b = 0,
              b_environ_lifestyle_p = 0,
              b_w_el_w              = 0,
              b_w_el_b              = 0,
              b_w_el_p              = 0
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_time_w * log(trip_duration)  + b_maxAT_w * Ta_max + b_maxWS_w * Ws_avg18h + b_avgPS_w * Psum_avg18h + b_motive_ww * (motive4==1) + b_motive_ew * (motive4==2) + b_motive_sw * (motive4==3) + b_weekend_w * weekend + b_environ_lifestyle_w * environ_lifestyle +  b_w_el_w * weekend * environ_lifestyle
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_time_b * log(trip_duration)  + b_maxAT_b * Ta_max + b_maxWS_b * Ws_avg18h + b_avgPS_b * Psum_avg18h + b_motive_wb * (motive4==1) + b_motive_eb * (motive4==2) + b_motive_sb * (motive4==3) + b_weekend_b * weekend + b_environ_lifestyle_b * environ_lifestyle +  b_w_el_b * weekend * environ_lifestyle
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_time_p * log(trip_duration)  + b_maxAT_p * Ta_max + b_maxWS_p * Ws_avg18h + b_avgPS_p * Psum_avg18h + b_motive_wp * (motive4==1) + b_motive_ep * (motive4==2) + b_motive_sp * (motive4==3) + b_weekend_p * weekend + b_environ_lifestyle_p * environ_lifestyle +  b_w_el_p * weekend * environ_lifestyle
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.6= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.6)
apollo_saveOutput(model.6)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
database <-  subset(database, wind_perception==1 | wind_perception==3 | wind_perception==5)
summary(database)
count(database)
  
apollo_control=list(modelName="interaction bet Wind perception and Wind speed",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_maxWS_w            = 0,
              b_maxWS_b            = 0,
              b_maxWS_p            = 0,
              b_wind_perception_1_w  = 0,
              b_wind_perception_1_b  = 0,
              b_wind_perception_1_p  = 0,
              b_wind_perception_5_w  = 0,
              b_wind_perception_5_b  = 0,
              b_wind_perception_5_p  = 0,
              b_ws_wp_1_w            = 0,
              b_ws_wp_1_b            = 0,
              b_ws_wp_1_p            = 0,
              b_ws_wp_5_w            = 0,
              b_ws_wp_5_b            = 0,
              b_ws_wp_5_p            = 0
              
 

)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_maxWS_w * Ws_avg18h + b_wind_perception_1_w * (wind_perception==1) + b_wind_perception_5_w * (wind_perception==5) + b_ws_wp_1_w * (wind_perception==1) * Ws_avg18h + b_ws_wp_5_w * (wind_perception==5) * Ws_avg18h
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_maxWS_b * Ws_avg18h + b_wind_perception_1_b * (wind_perception==1) + b_wind_perception_5_b * (wind_perception==5) + b_ws_wp_1_b * (wind_perception==1) * Ws_avg18h + b_ws_wp_5_b * (wind_perception==5) * Ws_avg18h
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_maxWS_p * Ws_avg18h + b_wind_perception_1_p * (wind_perception==1) + b_wind_perception_5_p * (wind_perception==5) + b_ws_wp_1_p * (wind_perception==1) * Ws_avg18h + b_ws_wp_5_p * (wind_perception==5) * Ws_avg18h
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.7= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.7)
-------------------------------------------------------------------------------------------------------------------------------------------
  
rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle","tiredness","irritation")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
database <-  subset(database, wind_perception==1 | wind_perception==3 | wind_perception==5)
summary(database)
count(database)

apollo_control=list(modelName="Tiredness",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              
              b_tiredness_1_w      = 0,
              b_tiredness_1_b      = 0,
              b_tiredness_1_p      = 0,
              b_tiredness_5_w      = 0,
              b_tiredness_5_b      = 0,
              b_tiredness_5_p      = 0
            
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_tiredness_1_w * (tiredness==1)  + b_tiredness_5_w * (tiredness==5) 
  V[['bicycle']] = asc_bicycle  + b_tiredness_1_b * (tiredness==1)  + b_tiredness_5_b * (tiredness==5) 
  V[['pt']]      = asc_pt       + b_tiredness_1_p * (tiredness==1)  + b_tiredness_5_p * (tiredness==5) 
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.8= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.8)
apollo_saveOutput(model.8)

---------------------------------------------------------------------------------------------------------------------------------------------------------
  
--------------------------------------------------------------------------------------------------------------------------------
  
rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
database <-  subset(database, wind_perception==1 | wind_perception==3 | wind_perception==5)
database <-  subset(database, happiness==1 | happiness==3 | happiness==5)
summary(database)
count(database)

apollo_control=list(modelName="interaction bet Wind perception and happiness",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_happiness_1_w      = 0,
              b_happiness_1_b      = 0,
              b_happiness_1_p      = 0,
              b_happiness_5_w      = 0,    
              b_happiness_5_b      = 0,
              b_happiness_5_p      = 0,
              b_wind_perception_1_w  = 0,
              b_wind_perception_1_b  = 0,
              b_wind_perception_1_p  = 0,
              b_wind_perception_5_w  = 0,
              b_wind_perception_5_b  = 0,
              b_wind_perception_5_p  = 0,
              b_hp_wp_1_w            = 0,
              b_hp_wp_1_b            = 0,
              b_hp_wp_1_p            = 0,
              b_hp_wp_5_w            = 0,
              b_hp_wp_5_b            = 0,
              b_hp_wp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)   + b_happiness_1_w * (happiness==1) + b_happiness_5_w * (happiness==5) + b_wind_perception_1_w * (wind_perception==1) + b_wind_perception_5_w * (wind_perception==5) + b_hp_wp_1_w  * (happiness==1) * (wind_perception==1) + b_hp_wp_5_w  * (happiness==5) * (wind_perception==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)   + b_happiness_1_b * (happiness==1) + b_happiness_5_b * (happiness==5) + b_wind_perception_1_b * (wind_perception==1) + b_wind_perception_5_b * (wind_perception==5) + b_hp_wp_1_b  * (happiness==1) * (wind_perception==1) + b_hp_wp_5_b  * (happiness==5) * (wind_perception==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)   + b_happiness_1_p * (happiness==1) + b_happiness_5_p * (happiness==5) + b_wind_perception_1_p * (wind_perception==1) + b_wind_perception_5_p * (wind_perception==5) + b_hp_wp_1_p  * (happiness==1) * (wind_perception==1) + b_hp_wp_5_p  * (happiness==5) * (wind_perception==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.9= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.9)
apollo_saveOutput(model.9)

---------------------------------------------------------------------------------------------
  

rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
database <-  subset(database, sunshine_perception==1 | sunshine_perception==3 | sunshine_perception==5)
database <-  subset(database, happiness==1 | happiness==3 | happiness==5)
summary(database)
count(database)

apollo_control=list(modelName="interaction bet sunshine_perception and happiness",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_happiness_1_w      = 0,
              b_happiness_1_b      = 0,
              b_happiness_1_p      = 0,
              b_happiness_5_w      = 0,    
              b_happiness_5_b      = 0,
              b_happiness_5_p      = 0,
              b_sunshine_perception_1_w  = 0,
              b_sunshine_perception_1_b  = 0,
              b_sunshine_perception_1_p  = 0,
              b_sunshine_perception_5_w  = 0,
              b_sunshine_perception_5_b  = 0,
              b_sunshine_perception_5_p  = 0,
              b_hp_ss_1_w            = 0,
              b_hp_ss_1_b            = 0,
              b_hp_ss_1_p            = 0,
              b_hp_ss_5_w            = 0,
              b_hp_ss_5_b            = 0,
              b_hp_ss_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_happiness_1_w * (happiness==1) + b_happiness_5_w * (happiness==5) + b_sunshine_perception_1_w * (sunshine_perception==1) + b_sunshine_perception_5_w * (sunshine_perception==5) + b_hp_ss_1_w * (sunshine_perception==1) * (happiness==1) + b_hp_ss_5_w * (sunshine_perception==5) * (happiness==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_happiness_1_b * (happiness==1) + b_happiness_5_b * (happiness==5) + b_sunshine_perception_1_b * (sunshine_perception==1) + b_sunshine_perception_5_b * (sunshine_perception==5) + b_hp_ss_1_b * (sunshine_perception==1) * (happiness==1) + b_hp_ss_5_b * (sunshine_perception==5) * (happiness==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_happiness_1_p * (happiness==1) + b_happiness_5_p * (happiness==5) + b_sunshine_perception_1_p * (sunshine_perception==1) + b_sunshine_perception_5_p * (sunshine_perception==5) + b_hp_ss_1_p * (sunshine_perception==1) * (happiness==1) + b_hp_ss_5_p * (sunshine_perception==5) * (happiness==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.10= apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(model.10)
apollo_saveOutput(model.10)

-----------------------------------------------------------------------------------------
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, precipitation_perception==1 | precipitation_perception==3 | precipitation_perception==5)
#database <-  subset(database, happiness==1 | happiness==3 | happiness==5)
summary(database)
count(database)
  
    
apollo_control=list(modelName="interaction bet precipitation_perception and happiness",
                      modelDescr="Considering the daily weather conditions",
                      indivID="respid",
                      panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_happiness_1_w      = 0,
              b_happiness_1_b      = 0,
              b_happiness_1_p      = 0,
              b_happiness_5_w      = 0,    
              b_happiness_5_b      = 0,
              b_happiness_5_p      = 0,
              b_precipitation_perception_1_w  = 0,
              b_precipitation_perception_1_b  = 0,
              b_precipitation_perception_1_p  = 0,
              b_precipitation_perception_5_w  = 0,
              b_precipitation_perception_5_b  = 0,
              b_precipitation_perception_5_p  = 0,
              b_hp_pp_1_w            = 0,
              b_hp_pp_1_b            = 0,
              b_hp_pp_1_p            = 0,
              b_hp_pp_5_w            = 0,
              b_hp_pp_5_b            = 0,
              b_hp_pp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_happiness_1_w * (happiness==1) + b_happiness_5_w * (happiness==5) + b_precipitation_perception_1_w * (precipitation_perception==1) + b_precipitation_perception_5_w * (precipitation_perception==5) + b_hp_pp_1_w * (precipitation_perception==1) * (happiness==1) + b_hp_pp_5_w * (precipitation_perception==5) * (happiness==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_happiness_1_b * (happiness==1) + b_happiness_5_b * (happiness==5) + b_precipitation_perception_1_b * (precipitation_perception==1) + b_precipitation_perception_5_b * (precipitation_perception==5) + b_hp_pp_1_b * (precipitation_perception==1) * (happiness==1) + b_hp_pp_5_b * (precipitation_perception==5) * (happiness==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_happiness_1_p * (happiness==1) + b_happiness_5_p * (happiness==5) + b_precipitation_perception_1_p * (precipitation_perception==1) + b_precipitation_perception_5_p * (precipitation_perception==5) + b_hp_pp_1_p * (precipitation_perception==1) * (happiness==1) + b_hp_pp_5_p * (precipitation_perception==5) * (happiness==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.11= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.11)
apollo_saveOutput(model.11)

---------------------------------------------------------------------------------------------------------------------
  
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, precipitation_perception==1 | precipitation_perception==3 | precipitation_perception==5)
#database <-  subset(database, happiness==1 | happiness==3 | happiness==5)
summary(database)
  
apollo_control=list(modelName="interaction bet precipitation_perception and irritation",
                      modelDescr="Considering the daily weather conditions",
                      indivID="respid",
                      panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_irritation_1_w      = 0,
              b_irritation_1_b      = 0,
              b_irritation_1_p      = 0,
              b_irritation_5_w      = 0,    
              b_irritation_5_b      = 0,
              b_irritation_5_p      = 0,
              b_precipitation_perception_1_w  = 0,
              b_precipitation_perception_1_b  = 0,
              b_precipitation_perception_1_p  = 0,
              b_precipitation_perception_5_w  = 0,
              b_precipitation_perception_5_b  = 0,
              b_precipitation_perception_5_p  = 0,
              b_it_pp_1_w            = 0,
              b_it_pp_1_b            = 0,
              b_it_pp_1_p            = 0,
              b_it_pp_5_w            = 0,
              b_it_pp_5_b            = 0,
              b_it_pp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_irritation_1_w * (irritation==1) + b_irritation_5_w * (irritation==5) + b_precipitation_perception_1_w * (precipitation_perception==1) + b_precipitation_perception_5_w * (precipitation_perception==5) + b_it_pp_1_w * (precipitation_perception==1) * (irritation==1) + b_it_pp_5_w * (precipitation_perception==5) * (irritation==5) 
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_irritation_1_b * (irritation==1) + b_irritation_5_b * (irritation==5) + b_precipitation_perception_1_b * (precipitation_perception==1) + b_precipitation_perception_5_b * (precipitation_perception==5) + b_it_pp_1_b * (precipitation_perception==1) * (irritation==1) + b_it_pp_5_b * (precipitation_perception==5) * (irritation==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_irritation_1_p * (irritation==1) + b_irritation_5_p * (irritation==5) + b_precipitation_perception_1_p * (precipitation_perception==1) + b_precipitation_perception_5_p * (precipitation_perception==5) + b_it_pp_1_p * (precipitation_perception==1) * (irritation==1) + b_it_pp_5_p * (precipitation_perception==5) * (irritation==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.12= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.12)
apollo_saveOutput(model.12)

------------------------------------------------------------------------------------------------
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, sunshine_perception==1 | sunshine_perception==3 | sunshine_perception==5)
#database <-  subset(database, irritation==1 | irritation==3 | irritation==5)
summary(database)
count(database)   
  
apollo_control=list(modelName="interaction bet sunshine_perception and irritation",
                      modelDescr="Considering the daily weather conditions",
                      indivID="respid",
                      panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_irritation_1_w      = 0,
              b_irritation_1_b      = 0,
              b_irritation_1_p      = 0,
              b_irritation_5_w      = 0,    
              b_irritation_5_b      = 0,
              b_irritation_5_p      = 0,
              b_sunshine_perception_1_w  = 0,
              b_sunshine_perception_1_b  = 0,
              b_sunshine_perception_1_p  = 0,
              b_sunshine_perception_5_w  = 0,
              b_sunshine_perception_5_b  = 0,
              b_sunshine_perception_5_p  = 0,
              b_it_sp_1_w            = 0,
              b_it_sp_1_b            = 0,
              b_it_sp_1_p            = 0,
              b_it_sp_5_w            = 0,
              b_it_sp_5_b            = 0,
              b_it_sp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_irritation_1_w * (irritation==1) + b_irritation_5_w * (irritation==5) + b_sunshine_perception_1_w * (sunshine_perception==1) + b_sunshine_perception_5_w * (sunshine_perception==5) + b_it_sp_1_w * (sunshine_perception==1) * (irritation==1) + b_it_sp_5_w * (sunshine_perception==5) * (irritation==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_irritation_1_b * (irritation==1) + b_irritation_5_b * (irritation==5) + b_sunshine_perception_1_b * (sunshine_perception==1) + b_sunshine_perception_5_b * (sunshine_perception==5) + b_it_sp_1_b * (sunshine_perception==1) * (irritation==1) + b_it_sp_5_b * (sunshine_perception==5) * (irritation==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_irritation_1_p * (irritation==1) + b_irritation_5_p * (irritation==5) + b_sunshine_perception_1_p * (sunshine_perception==1) + b_sunshine_perception_5_p * (sunshine_perception==5) + b_it_sp_1_p * (sunshine_perception==1) * (irritation==1) + b_it_sp_5_p * (sunshine_perception==5) * (irritation==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.13= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.13)
apollo_saveOutput(model.13)

--------------------------------------------------------------------------------------------------------------------
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, wind_perception==1 | wind_perception==3 | wind_perception==5)
#database <-  subset(database, irritation==1 | irritation==3 | irritation==5)
summary(database)
count(database)  
  
  
apollo_control=list(modelName="interaction bet wind_perception and irritation",
                      modelDescr="Considering the daily weather conditions",
                      indivID="respid",
                      panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_irritation_1_w      = 0,
              b_irritation_1_b      = 0,
              b_irritation_1_p      = 0,
              b_irritation_2_w      = 0,
              b_irritation_2_b      = 0,
              b_irritation_2_p      = 0,
              b_irritation_4_w      = 0,
              b_irritation_4_b      = 0,
              b_irritation_4_p      = 0,
              b_irritation_5_w      = 0,    
              b_irritation_5_b      = 0,
              b_irritation_5_p      = 0,
              b_wind_perception_1_w  = 0,
              b_wind_perception_1_b  = 0,
              b_wind_perception_1_p  = 0,
              b_wind_perception_2_w  = 0,
              b_wind_perception_2_b  = 0,
              b_wind_perception_2_p  = 0,
              b_wind_perception_4_w  = 0,
              b_wind_perception_4_b  = 0,
              b_wind_perception_4_p  = 0,
              b_wind_perception_5_w  = 0,
              b_wind_perception_5_b  = 0,
              b_wind_perception_5_p  = 0,
              b_it_wp_1_w            = 0,
              b_it_wp_1_b            = 0,
              b_it_wp_1_p            = 0,
              b_it_wp_2_w            = 0,
              b_it_wp_2_b            = 0,
              b_it_wp_2_p            = 0,
              b_it_wp_4_w            = 0,
              b_it_wp_4_b            = 0,
              b_it_wp_4_p            = 0,
              b_it_wp_5_w            = 0,
              b_it_wp_5_b            = 0,
              b_it_wp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_irritation_1_w * (irritation==1) + b_irritation_2_w * (irritation==2) + b_irritation_4_w * (irritation==4) + b_irritation_5_w * (irritation==5) + b_wind_perception_1_w * (wind_perception==1) + b_wind_perception_2_w * (wind_perception==2) + b_wind_perception_4_w * (wind_perception==4) + b_wind_perception_5_w * (wind_perception==5) + b_it_wp_1_w * (wind_perception==1) * (irritation==1) + b_it_wp_2_w * (wind_perception==2) * (irritation==2) + b_it_wp_4_w * (wind_perception==4) * (irritation==4) + b_it_wp_5_w * (wind_perception==5) * (irritation==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_irritation_1_b * (irritation==1) + b_irritation_2_b * (irritation==2) + b_irritation_4_b * (irritation==4) + b_irritation_5_b * (irritation==5) + b_wind_perception_1_b * (wind_perception==1) + b_wind_perception_2_b * (wind_perception==2) + b_wind_perception_4_b * (wind_perception==4) + b_wind_perception_5_b * (wind_perception==5) + b_it_wp_1_b * (wind_perception==1) * (irritation==1) + b_it_wp_2_b * (wind_perception==2) * (irritation==2) + b_it_wp_4_b * (wind_perception==4) * (irritation==4) + b_it_wp_5_b * (wind_perception==5) * (irritation==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_irritation_1_p * (irritation==1) + b_irritation_2_p * (irritation==2) + b_irritation_4_p * (irritation==4) + b_irritation_5_p * (irritation==5) + b_wind_perception_1_p * (wind_perception==1) + b_wind_perception_2_p * (wind_perception==2) + b_wind_perception_4_p * (wind_perception==4) + b_wind_perception_5_p * (wind_perception==5) + b_it_wp_1_p * (wind_perception==1) * (irritation==1) + b_it_wp_2_p * (wind_perception==2) * (irritation==2) + b_it_wp_4_p * (wind_perception==4) * (irritation==4) + b_it_wp_5_p * (wind_perception==5) * (irritation==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.14= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.14)
apollo_saveOutput(model.14)

--------------------------------------------------------------------------------------------------------------
  
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, wind_perception==1 | wind_perception==3 | wind_perception==5)
#database <-  subset(database, tiredness==1 | tiredness==3 | tiredness==5)
summary(database)
count(database)    
  
  apollo_control=list(modelName="interaction bet wind_perception and tiredness",
                      modelDescr="Considering the daily weather conditions",
                      indivID="respid",
                      panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_tiredness_1_w      = 0,
              b_tiredness_1_b      = 0,
              b_tiredness_1_p      = 0,
              b_tiredness_5_w      = 0,    
              b_tiredness_5_b      = 0,
              b_tiredness_5_p      = 0,
              b_wind_perception_1_w  = 0,
              b_wind_perception_1_b  = 0,
              b_wind_perception_1_p  = 0,
              b_wind_perception_5_w  = 0,
              b_wind_perception_5_b  = 0,
              b_wind_perception_5_p  = 0,
              b_td_wp_1_w            = 0,
              b_td_wp_1_b            = 0,
              b_td_wp_1_p            = 0,
              b_td_wp_5_w            = 0,
              b_td_wp_5_b            = 0,
              b_td_wp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_tiredness_1_w * (tiredness==1) + b_tiredness_5_w * (tiredness==5) + b_wind_perception_1_w * (wind_perception==1) + b_wind_perception_5_w * (wind_perception==5) + b_td_wp_1_w * (wind_perception==1) * (tiredness==1) + b_td_wp_5_w * (wind_perception==5) * (tiredness==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_tiredness_1_b * (tiredness==1) + b_tiredness_5_b * (tiredness==5) + b_wind_perception_1_b * (wind_perception==1) + b_wind_perception_5_b * (wind_perception==5) + b_td_wp_1_b * (wind_perception==1) * (tiredness==1) + b_td_wp_5_b * (wind_perception==5) * (tiredness==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_tiredness_1_p * (tiredness==1) + b_tiredness_5_p * (tiredness==5) + b_wind_perception_1_p * (wind_perception==1) + b_wind_perception_5_p * (wind_perception==5) + b_td_wp_1_p * (wind_perception==1) * (tiredness==1) + b_td_wp_5_p * (wind_perception==5) * (tiredness==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.15= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.15)
apollo_saveOutput(model.15)

--------------------------------------------------------------

  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, sunshine_perception==1 | sunshine_perception==3 | sunshine_perception==5)
#database <-  subset(database, tiredness==1 | tiredness==3 | tiredness==5)
summary(database)
count(database)  
  
   apollo_control=list(modelName="interaction bet sunshine_perception and tiredness",
                      modelDescr="Considering the daily weather conditions",
                      indivID="respid",
                      panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_tiredness_1_w      = 0,
              b_tiredness_1_b      = 0,
              b_tiredness_1_p      = 0,
              b_tiredness_5_w      = 0,    
              b_tiredness_5_b      = 0,
              b_tiredness_5_p      = 0,
              b_sunshine_perception_1_w  = 0,
              b_sunshine_perception_1_b  = 0,
              b_sunshine_perception_1_p  = 0,
              b_sunshine_perception_5_w  = 0,
              b_sunshine_perception_5_b  = 0,
              b_sunshine_perception_5_p  = 0,
              b_td_sp_1_w            = 0,
              b_td_sp_1_b            = 0,
              b_td_sp_1_p            = 0,
              b_td_sp_5_w            = 0,
              b_td_sp_5_b            = 0,
              b_td_sp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_tiredness_1_w * (tiredness==1) + b_tiredness_5_w * (tiredness==5) + b_sunshine_perception_1_w * (sunshine_perception==1) + b_sunshine_perception_5_w * (sunshine_perception==5) + b_td_sp_1_w * (sunshine_perception==1) * (tiredness==1) + b_td_sp_5_w * (sunshine_perception==5) * (tiredness==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_tiredness_1_b * (tiredness==1) + b_tiredness_5_b * (tiredness==5) + b_sunshine_perception_1_b * (sunshine_perception==1) + b_sunshine_perception_5_b * (sunshine_perception==5) + b_td_sp_1_b * (sunshine_perception==1) * (tiredness==1) + b_td_sp_5_b * (sunshine_perception==5) * (tiredness==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_tiredness_1_p * (tiredness==1) + b_tiredness_5_p * (tiredness==5) + b_sunshine_perception_1_p * (sunshine_perception==1) + b_sunshine_perception_5_p * (sunshine_perception==5) + b_td_sp_1_p * (sunshine_perception==1) * (tiredness==1) + b_td_sp_5_p * (sunshine_perception==5) * (tiredness==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.16= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.16)
apollo_saveOutput(model.16)

------------------------------------------------------------------------------------------------------------------
  
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, precipitation_perception==1 | precipitation_perception==3 | precipitation_perception==5)
#database <-  subset(database, tiredness==1 | tiredness==3 | tiredness==5)
summary(database)
count(database)  

  
  apollo_control=list(modelName="interaction bet precipitation_perception and tiredness",
                     modelDescr="Considering the daily weather conditions",
                     indivID="respid",
                     panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_tiredness_1_w      = 0,
              b_tiredness_1_b      = 0,
              b_tiredness_1_p      = 0,
              b_tiredness_5_w      = 0,    
              b_tiredness_5_b      = 0,
              b_tiredness_5_p      = 0,
              b_precipitation_perception_1_w  = 0,
              b_precipitation_perception_1_b  = 0,
              b_precipitation_perception_1_p  = 0,
              b_precipitation_perception_5_w  = 0,
              b_precipitation_perception_5_b  = 0,
              b_precipitation_perception_5_p  = 0,
              b_td_pp_1_w            = 0,
              b_td_pp_1_b            = 0,
              b_td_pp_1_p            = 0,
              b_td_pp_5_w            = 0,
              b_td_pp_5_b            = 0,
              b_td_pp_5_p            = 0
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_tiredness_1_w * (tiredness==1) + b_tiredness_5_w * (tiredness==5) + b_precipitation_perception_1_w * (precipitation_perception==1) + b_precipitation_perception_5_w * (precipitation_perception==5) + b_td_pp_1_w * (precipitation_perception==1) * (tiredness==1) + b_td_pp_5_w * (precipitation_perception==5) * (tiredness==5)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_tiredness_1_b * (tiredness==1) + b_tiredness_5_b * (tiredness==5) + b_precipitation_perception_1_b * (precipitation_perception==1) + b_precipitation_perception_5_b * (precipitation_perception==5) + b_td_pp_1_b * (precipitation_perception==1) * (tiredness==1) + b_td_pp_5_b * (precipitation_perception==5) * (tiredness==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_tiredness_1_p * (tiredness==1) + b_tiredness_5_p * (tiredness==5) + b_precipitation_perception_1_p * (precipitation_perception==1) + b_precipitation_perception_5_p * (precipitation_perception==5) + b_td_pp_1_p * (precipitation_perception==1) * (tiredness==1) + b_td_pp_5_p * (precipitation_perception==5) * (tiredness==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.17= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.17)
apollo_saveOutput(model.17)
---------------------------------------------------------------------------------------------------------
  
  
  rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library("ggpubr")

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr", "mode1","tfeel","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h", "country_lifestyle", "environ_lifestyle", "hhincome", "insun", "sunshine_perception","precipitation_perception","wind_perception","tfeel", "greenness", "environ_lifestyle", "happiness", "irritation", "tiredness")]
database <- database[order(database$respid),]
#database <- subset((database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
summary(database)
database <-  subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
database <-  subset(database, precipitation_perception==1 | precipitation_perception==3 | precipitation_perception==5)
database <-  database$mutate(database, age_50=ifelse(age > 50,1,0))
summary(database)
count(database) 

apollo_control=list(modelName="interaction bet precipitation_perception",
                    modelDescr="Considering the daily weather conditions",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk             = 0,
              asc_bicycle          = 0,
              asc_pt               = 0,
              b_dist_w             = 0,
              b_dist_b             = 0,
              b_dist_p             = 0,
              b_precipitation_perception_1_w      = 0,
              b_precipitation_perception_1_b      = 0,
              b_precipitation_perception_1_p      = 0,
              b_precipitation_perception_5_w      = 0,    
              b_precipitation_perception_5_b      = 0,
              b_precipitation_perception_5_p      = 0
             
              
              
              
)

#all coefficients may be altered, none is fixed

apollo_fixed=NULL


#check if you have defined everything necessary 

apollo_inputs = apollo_validateInputs()


apollo_probabilities= function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)			 ### Attach inputs and detach after
  on.exit(apollo_detach(apollo_beta, apollo_inputs))		 ### function exit		
  
  P = list()								 ### Create list of probabilities P
  
  V = list()	### List of utilities
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_precipitation_perception_1_w * (precipitation_perception==1) + b_precipitation_perception_5_w * (precipitation_perception==5) 
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_precipitation_perception_1_b * (precipitation_perception==1) + b_precipitation_perception_5_b * (precipitation_perception==5)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_precipitation_perception_1_p * (precipitation_perception==1) + b_precipitation_perception_5_p * (precipitation_perception==5)
  V[['car']]     = 0                            
  
  
  mnl_settings = list(
    alternatives = c(walk=1, bicycle=2, pt=3, car=4),					 ### component	
    avail        = list(pt= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar    = mode1,
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)	 
  P = apollo_panelProd(P, apollo_inputs, functionality)	 
  P = apollo_prepareProb(P, apollo_inputs, functionality)	 
  
  return(P)
} 

model.20= apollo_estimate(apollo_beta,
                          apollo_fixed,
                          apollo_probabilities,
                          apollo_inputs)

apollo_modelOutput(model.20)