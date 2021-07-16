# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(dplyr)
library(readxl)
library(fastDummies)

### Initialise code
apollo_initialise()

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database <- read_delim("Rotterdam.csv", 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
database <- database[c("respid","tripid","tripnr","country_lifestyle", "mode1","hhincome","insun","sunshine_perception",
                       "precipitation_perception","wind_perception","tfeel","summer_person","dest_duration","trip_duration",
                       "motive4","weekend","age", "male", "dist","public_transport_card","Bicycle_availa","car_availa",
                       "Ta_max","Ws_avg18h","Psum_avg18h","Fog_dum18h","Snow_dum18h","Thunder_dum18h","nonwestern","happiness",
                       "fear","irritation","tiredness")]
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

### Set core controls
apollo_control = list(
  modelName  = "ICLV Model for Latent Variable-Happiness",
  modelDescr = "ICLV model on Rotterdam data, using continuous measurement model for indicators",
  indivID    = "respid",
  mixing     = TRUE,
  panelData = TRUE,
  nCores     = 3
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_walk              = 0, 
                b_bicycle           = 0, 
                b_public_trans      = 0, 
                b_car               = 0, 
                b_dist_walk         = 0,
                b_dist_bicycle      = 0,
                b_dist_publictrans  = 0,
                b_dist_car          = 0,
                b_time_w            = 0,
                b_time_b            = 0,
                b_time_p            = 0,
                b_time_c            = 0,
                lambda_w            = 0,
                lambda_b            = 0,
                lambda_p            = 0,
                gamma_age_50        = 0,
                gamma_male          = 0,
                gamma_nonwestern    = 0,
                zeta_happiness      = 1,
                sigma_happiness     = 1
               
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_car", "b_dist_car", "b_time_c")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=600,          
  interUnifDraws=c(),      
  interNormDraws=c("eta"), 
  
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_male*male + gamma_nonwestern*nonwestern + gamma_age_50*age_50 + eta
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of indicators
   normalDensity_settings1 = list(outcomeNormal = happiness, 
                                 xNormal       = zeta_happiness*LV, 
                                  mu            = 0, 
                                  sigma         = sigma_happiness, 
                                 # rows          = (task==1),
                                  componentName = "indic_happiness")
  
  
   P[["indic_happiness"]]     = apollo_normalDensity(normalDensity_settings1, functionality)
 
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['walk']] = (b_walk + b_dist_walk * log(dist) + b_time_w*log(trip_duration) + lambda_w*LV )
  V[['bicycle']] = ( b_bicycle + b_dist_bicycle * log(dist)+ b_time_b*log(trip_duration) + lambda_b*LV )
  V[['publictrans']] = (b_public_trans +  b_dist_publictrans * log(dist)+ b_time_p*log(trip_duration) + lambda_p*LV)
  V[['car']] = ( b_car + b_dist_car * log(dist)+ b_time_c*log(trip_duration))
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, bicycle=2, publictrans=3, car=4),
    avail         = list(publictrans= public_transport_card, walk = 1, bicycle=Bicycle_availa, car=car_availa),
    choiceVar     = mode1,
    V             = V,
    componentName = "choice"
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- ---- MODEL PREDICTIONS             ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="indic_happiness")