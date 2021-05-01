# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())
setwd("G:/Mi unidad/2020/Paper Evasion/Modelo")
### Load Apollo library

#install.packages("apollo")
library(apollo)
### Initialise code
apollo_initialise()


#------- MNL ---------

### Set core controls
apollo_control = list(
  modelName  = "ML_Evasion_v11",
  modelDescr = "Modelo ML base" ,
  indivID    = "formato"
)

# ################################################################# #
# LOAD DATA AND APPLY ANY TRANSFORMATIONS                     

database=read.delim("BD_FINAL.txt")

#  DEFINE MODEL PARAMETERS  

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(ASC_SI        = 0, 
                ASC_NO        = 0,
                B_MASCULINO   = 0,
                B_INFORMAL    = 0,
                B_MULTA       = 0, 
                B_POSIBILIDAD = 0, 
                B_RETENCION   = 0, 
                B_CAMARAS     = 0, 
                B_ALEATORIA   = 0,
                gamma_EDAD2940 = 0,
                gamma_EDAD4150 = 0,
                gamma_EDAD50 = 0,
                gamma_EDAD1420 = 0,
                gamma_10mas = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("ASC_NO", "B_MASCULINO", "B_INFORMAL")

# GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()

# DEFINE MODEL AND LIKELIHOOD FUNCTION

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['VNO']] = ( ASC_NO )
  V[['VSI']] = ( ASC_SI 
                 + B_MULTA*multa + B_POSIBILIDAD*prob
                 + B_RETENCION*retencion + B_CAMARAS*camara + B_ALEATORIA*aleatoria 
                 + gamma_EDAD2940*ed_29_40 + gamma_EDAD4150*ed_41_50 + gamma_EDAD50*ed_50 + gamma_EDAD1420*ed_14_20 + gamma_10mas*colarse_10mas )
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(VNO=0, VSI=1),
    avail        = list(VNO=1, VSI=1),
    choiceVar    = evade,
    V            = V
  )
  
  ### Compute probabilities for MNL model component
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# MODEL ESTIMATION

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#  MODEL OUTPUTS

apollo_modelOutput(model)





#------- ML ---------

### Set core controls
apollo_control = list(
  modelName  = "ML_Evasion_v11",
  modelDescr = "Modelo ML base" ,
  indivID    = "formato",
  mixing     = TRUE,
  nCores     = 4
)

# ################################################################# #
# LOAD DATA AND APPLY ANY TRANSFORMATIONS                     

database=read.delim("BD_FINAL.txt")

#  DEFINE MODEL PARAMETERS  

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(ASC_SI        = 0, 
                ASC_NO        = 0,
                B_MASCULINO   = 0,
                B_INFORMAL    = 0,
                B_MULTA       = 0, 
                B_POSIBILIDAD = 0, 
                B_RETENCION   = 0, 
                B_CAMARAS     = 0, 
                B_ALEATORIA   = 0,
                PANEL         = 0,
                gamma_EDAD2940 = 0,
                gamma_EDAD4150 = 0,
                gamma_EDAD50 = 0,
                gamma_EDAD1420 = 0,
                gamma_10mas = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("ASC_NO", "B_MASCULINO", "B_INFORMAL")

# DEFINE RANDOM COMPONENTS

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=500,          
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
  
  randcoeff[["panel"]] = eta
  
  return(randcoeff)
}

# GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()

# DEFINE MODEL AND LIKELIHOOD FUNCTION

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['VNO']] = ( ASC_NO )
  V[['VSI']] = ( ASC_SI 
                 + B_MULTA*multa + B_POSIBILIDAD*prob
                 + B_RETENCION*retencion + B_CAMARAS*camara + B_ALEATORIA*aleatoria 
                 + gamma_EDAD2940*ed_29_40 + gamma_EDAD4150*ed_41_50 + gamma_EDAD50*ed_50 + gamma_EDAD1420*ed_14_20 + gamma_10mas*colarse_10mas
                 + PANEL * panel )
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(VNO=0, VSI=1),
    avail        = list(VNO=1, VSI=1),
    choiceVar    = evade,
    V            = V
  )
  
  ### Compute probabilities for MNL model component
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# MODEL ESTIMATION

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#  MODEL OUTPUTS

apollo_modelOutput(model)
