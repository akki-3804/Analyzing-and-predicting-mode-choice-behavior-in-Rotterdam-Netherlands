rm(list = ls())
library(apollo)# run apollo package
library(tidyverse)
library(dplyr)
library(readxl)
library(fastDummies)
database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr","country_lifestyle","happiness","education", "mode1","hhincome","tfeel","summer_person","dest_duration","trip_duration","motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa","Ta_max","Ws_avg18h","Psum_avg18h")]
database <- database[order(database$respid),]
#database <- subset(database, mode1 < 5)
write.table(database, "rotterdam_database.txt", sep="\t") # excel does some strange things: therefore write to text and re-read-in
rm(list = ls())

## new data
database <- read.delim("rotterdam_database.txt", stringsAsFactors=TRUE)
database <- subset(database,!(mode1==3 & public_transport_card==0))
database <-  subset(database, dist < 250&dist>0.5)
database <-  subset(database, trip_duration<250&trip_duration>4)
#database <-  subset(database, hhincome<5)
#database <-  subset(database, education<99999)

#add dummy variables for household income variable
database <- dummy_cols(database, select_columns = 'hhincome')

apollo_initialise()

apollo_control=list(modelName="Alternative Spec Constant",
                    modelDescr="Considering the household income",
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
              b_hhincome_2_w= 0,
              b_hhincome_3_w= 0,
              b_hhincome_4_w= 0,
              b_hhincome_2_b= 0,
              b_hhincome_3_b= 0,
              b_hhincome_4_b= 0,
              b_hhincome_2_p= 0,
              b_hhincome_3_p= 0,
              b_hhincome_4_p= 0
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
  
  V[['walk']]    = asc_walk     + b_dist_w * log(dist + 1)  + b_time_w * log(trip_duration+1) + b_hhincome_2_w * (hhincome==2) + b_hhincome_3_w * (hhincome==3) + b_hhincome_4_w * (hhincome==4)
  V[['bicycle']] = asc_bicycle  + b_dist_b * log(dist + 1)  + b_time_b * log(trip_duration+1) + b_hhincome_2_b * (hhincome==2) + b_hhincome_3_b * (hhincome==3) + b_hhincome_4_b * (hhincome==4)
  V[['pt']]      = asc_pt       + b_dist_p * log(dist + 1)  + b_time_p * log(trip_duration+1) + b_hhincome_2_p * (hhincome==2) + b_hhincome_3_p * (hhincome==3) + b_hhincome_4_p * (hhincome==4)
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

Model.4.pred = apollo_prediction(model.4, apollo_probabilities, apollo_inputs )
head(Model.4.pred, 20)

#-----------------------------------------------------------------------------------------------------

apollo_initialise()

apollo_control=list(modelName="Alternative Spec Constant model - education",
                    modelDescr="Considering the education level",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
              b_edulevel_2_w= 0,
              b_edulevel_3_w= 0,
              b_edulevel_2_b= 0,
              b_edulevel_3_b= 0,
              b_edulevel_2_p= 0,
              b_edulevel_3_p= 0
             
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
  
  V[['walk']]    = asc_walk   +  b_edulevel_2_w * (education==2) + b_edulevel_3_w * (education==3) 
  V[['bicycle']] = asc_bicycle  + b_edulevel_2_b * (education==2) + b_edulevel_3_b * (education==3)
  V[['pt']]      = asc_pt      +  b_edulevel_2_p * (education==2) + b_edulevel_3_p * (education==3)
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

#-----------------------------------------------------------------------------------------------------------------------------

apollo_initialise()

apollo_control=list(modelName="Alternative Spec Constant model - happiness",
                    modelDescr="Considering the happiness level",
                    indivID="respid",
                    panelData = TRUE)

#Define name and starting values for the coefficients to be estimated

apollo_beta=c(asc_walk              =0,
              asc_bicycle           =0,
              asc_pt                =0,
          
             
             
              b_happy_2_w= 0,
              b_happy_2_b= 0,
              b_happy_2_p= 0,
              b_happy_3_w= 0,
              b_happy_3_b= 0,
              b_happy_3_p= 0,
              b_happy_4_w= 0,
              b_happy_4_b= 0,
              b_happy_4_p= 0,
              b_happy_5_w= 0,
              b_happy_5_b= 0,
              b_happy_5_p= 0
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
  
  V[['walk']]    = asc_walk    +    b_happy_2_w * (happiness==2) +  b_happy_3_w * (happiness==3) +  b_happy_4_w * (happiness==4) +  b_happy_5_w * (happiness==5)
  V[['bicycle']] = asc_bicycle +    b_happy_2_b * (happiness==2) +  b_happy_3_b * (happiness==3) +  b_happy_4_b * (happiness==4) +  b_happy_5_b * (happiness==5)
  V[['pt']]      = asc_pt      +    b_happy_2_p * (happiness==2) +  b_happy_3_p * (happiness==3) +  b_happy_4_p * (happiness==4) +  b_happy_5_p * (happiness==5)
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