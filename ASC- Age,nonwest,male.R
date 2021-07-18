rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library(fastDummies)
database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr","country_lifestyle", "mode1","hhincome","insun","sunshine_perception",
                       "precipitation_perception","wind_perception","tfeel","summer_person","dest_duration","trip_duration",
                       "motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa",
                       "Ta_max","Ws_avg18h","Psum_avg18h","Fog_dum18h","Snow_dum18h","Thunder_dum18h","nonwestern")]
database <- database[order(database$respid),]
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
#summary(database)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)


### create new variable for age > 50
database <- mutate(database, age_50=ifelse(age > 50,1,0))


summary(database)
head(database)
count(database)
apollo_initialise()

apollo_control=list(modelName="Alternative Spec Constant - Age50,nonwest,male",
                    modelDescr="Considering the characteristics",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
              b_dist_w = 0,
              b_dist_b = 0,
              b_dist_p = 0,
              b_time_w    = 0,
              b_time_b    = 0,
              b_time_p    = 0,
              b_male_w    = 0,
              b_male_b    = 0,
              b_male_p    = 0,
              b_nonwest_w = 0,
              b_nonwest_b = 0,
              b_nonwest_p = 0,
              b_age50_w   = 0,
              b_age50_b   = 0,
              b_age50_p   = 0
              
              
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
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist)  + b_time_w * log(trip_duration) + b_male_w * male +
                    b_nonwest_w*nonwestern + b_age50_w*age_50
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist)  + b_time_b * log(trip_duration) + b_male_b * male +
                    b_nonwest_b*nonwestern + b_age50_b*age_50
  V[['pt']]      = asc_pt       + b_dist_p * log(dist)  + b_time_p * log(trip_duration) + b_male_p * male +
                    b_nonwest_p*nonwestern + b_age50_p*age_50
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