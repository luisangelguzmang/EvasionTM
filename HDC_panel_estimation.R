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

### Set core controls
apollo_control = list(
  modelName  = "Hibrido_Evasion_v11",
  modelDescr = "Modelo Hibrido Evasion, usando logit ordinal" ,
  indivID    = "formato",
  mixing     = TRUE,
  nCores     = 4
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database=read.delim("BD_FINAL.txt")
# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

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
                ETA_PERSON    = 1,
                ETA_SATISF    = 1,
                gamma_EDAD2940  = 0, 
                gamma_EDAD4150  = 0, 
                gamma_EDAD50    = 0,
                gamma_EDAD1420  = 0,
                gamma_10mas     = 0,
                zeta_SATCOM     = 1, 
                zeta_SATCOS     = 1, 
                zeta_SATGRAL    = 1, 
                zeta_SATSEG     = 1,
                zeta_FINJUS     = 1, 
                zeta_GANAR      = 1, 
                zeta_NOREG      = 1, 
                tau_SATCOM_1    =-2, 
                tau_SATCOM_2    =-1, 
                tau_SATCOM_3    = 1, 
                tau_SATCOM_4    = 2, 
                tau_SATCOS_1    =-2, 
                tau_SATCOS_2    =-1, 
                tau_SATCOS_3    = 1, 
                tau_SATCOS_4    = 2, 
                tau_SATGRAL_1   =-2, 
                tau_SATGRAL_2   =-1, 
                tau_SATGRAL_3   = 1, 
                tau_SATGRAL_4   = 2, 
                tau_SATSEG_1    =-2, 
                tau_SATSEG_2    =-1, 
                tau_SATSEG_3    = 1,
                tau_SATSEG_4    = 2,
                tau_FINJUS_1    =-2, 
                tau_FINJUS_2    =-1, 
                tau_FINJUS_3    = 1, 
                tau_FINJUS_4    = 2, 
                tau_GANAR_1     =-2, 
                tau_GANAR_2     =-1, 
                tau_GANAR_3     = 1, 
                tau_GANAR_4     = 2, 
                tau_NOREG_1     =-2, 
                tau_NOREG_2     =-1, 
                tau_NOREG_3     = 1, 
                tau_NOREG_4     = 2,
                PANEL           = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("ASC_NO", "B_MASCULINO", "B_INFORMAL")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=500,          
  interUnifDraws=c(),      
  interNormDraws=c("etas","etap","eta"), 
  
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["SATISF"]] = gamma_EDAD2940*ed_29_40 + gamma_EDAD4150*ed_41_50 + gamma_EDAD50*ed_50  + etas
  randcoeff[["PERSON"]] = gamma_EDAD1420*ed_14_20 + gamma_10mas*colarse_10mas+ etap
  randcoeff[["panel"]] = eta
  
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
  ol_settings1 = list(outcomeOrdered=satis_comodidad, 
                      V=zeta_SATCOM*SATISF, 
                      tau=c(tau_SATCOM_1, tau_SATCOM_2, tau_SATCOM_3, tau_SATCOM_4),
                      rows=(escenario==1))
  ol_settings2 = list(outcomeOrdered=satis_costo, 
                      V=zeta_SATCOS*SATISF, 
                      tau=c(tau_SATCOS_1, tau_SATCOS_2, tau_SATCOS_3, tau_SATCOS_4), 
                      rows=(escenario==1))
  ol_settings3 = list(outcomeOrdered=satis_general, 
                      V=zeta_SATGRAL*SATISF, 
                      tau=c(tau_SATGRAL_1, tau_SATGRAL_2, tau_SATGRAL_3, tau_SATGRAL_4), 
                      rows=(escenario==1))
  ol_settings4 = list(outcomeOrdered=satis_seguridad, 
                      V=zeta_SATSEG*SATISF, 
                      tau=c(tau_SATSEG_1, tau_SATSEG_2, tau_SATSEG_3, tau_SATSEG_4), 
                      rows=(escenario==1))
  ol_settings5 = list(outcomeOrdered=fin_justifica, 
                      V=zeta_FINJUS*PERSON, 
                      tau=c(tau_FINJUS_1, tau_FINJUS_2, tau_FINJUS_3, tau_FINJUS_4),
                      rows=(escenario==1))
  ol_settings6 = list(outcomeOrdered=importa_ganar, 
                      V=zeta_GANAR*PERSON, 
                      tau=c(tau_GANAR_1, tau_GANAR_2, tau_GANAR_3, tau_GANAR_4), 
                      rows=(escenario==1))
  ol_settings7 = list(outcomeOrdered=no_reglas, 
                      V=zeta_NOREG*PERSON, 
                      tau=c(tau_NOREG_1, tau_NOREG_2, tau_NOREG_3, tau_NOREG_4), 
                      rows=(escenario==1))
  
  P[["indic_satis_comodidad"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_satis_costo"]]         = apollo_ol(ol_settings2, functionality)
  P[["indic_satis_general"]]       = apollo_ol(ol_settings3, functionality)
  P[["indic_satis_seguridad"]]     = apollo_ol(ol_settings4, functionality)
  P[["indic_fin_justifica"]]       = apollo_ol(ol_settings5, functionality)
  P[["indic_importa_ganar"]]       = apollo_ol(ol_settings6, functionality)
  P[["indic_no_reglas"]]           = apollo_ol(ol_settings7, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['VNO']] = ( ASC_NO )
  V[['VSI']] = ( ASC_SI 
                 + B_MULTA*multa + B_POSIBILIDAD*prob
                 + B_RETENCION*retencion + B_CAMARAS*camara + B_ALEATORIA*aleatoria 
                 + ((ETA_PERSON*(PERSON)+ETA_SATISF)*SATISF) + PANEL * panel )
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(VNO=0, VSI=1),
    avail        = list(VNO=1, VSI=1),
    choiceVar    = evade,
    V            = V
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

### Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)


# ----------------------------------------------------------------- #
#---- PREDICCIONES               ----
# ----------------------------------------------------------------- #

#BASE
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
base_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                modelComponent="choice")
#######POLITICAPUBLICA######################################################
#CAMARAS
database$policia_perros=0
database$camara=1
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
camara_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                  modelComponent="choice")
#ALEATORIO
database$policia_perros=0
database$camara=0
database$aleatoria=1
database$multa=214
database$retencion=0
database$prob=0.29
aleatorio_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                     modelComponent="choice")
#######HORASDETENIDO######################################################
#HORA2
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=2
database$prob=0.29
h2_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#HORA4
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=4
database$prob=0.29
h4_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#HORA6
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=6
database$prob=0.29
h6_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#HORA8
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=8
database$prob=0.29
h8_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#HORA10
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=10
database$prob=0.29
h10_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA12
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=12
database$prob=0.29
h12_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA14
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=14
database$prob=0.29
h14_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA16
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=16
database$prob=0.29
h16_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA18
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=18
database$prob=0.29
h18_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA20
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=20
database$prob=0.29
h20_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA22
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=22
database$prob=0.29
h22_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA24
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=24
database$prob=0.29
h24_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA26
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=26
database$prob=0.29
h26_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA28
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=28
database$prob=0.29
h28_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA30
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=30
database$prob=0.29
h30_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA32
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=32
database$prob=0.29
h32_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA34
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=34
database$prob=0.29
h34_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#HORA36
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=36
database$prob=0.29
h36_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#######MULTA######################################################
#M00
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*0
database$retencion=0
database$prob=0.29
M00_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M05
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*0.5
database$retencion=0
database$prob=0.29
M05_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M15
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*1.5
database$retencion=0
database$prob=0.29
M15_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M20
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*2
database$retencion=0
database$prob=0.29
M20_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M25
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*2.5
database$retencion=0
database$prob=0.29
M25_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M30
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*3
database$retencion=0
database$prob=0.29
M30_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M35
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*3.5
database$retencion=0
database$prob=0.29
M35_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M40
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*4
database$retencion=0
database$prob=0.29
M40_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M45
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*4.5
database$retencion=0
database$prob=0.29
M45_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M50
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*5
database$retencion=0
database$prob=0.29
M50_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M55
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*5.5
database$retencion=0
database$prob=0.29
M55_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")

#M60
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*6
database$retencion=0
database$prob=0.29
M60_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M65
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*6.5
database$retencion=0
database$prob=0.29
M65_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")

#M70
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*7
database$retencion=0
database$prob=0.29
M70_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#M75
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*7.5
database$retencion=0
database$prob=0.29
M75_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")

#M80
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214*8
database$retencion=0
database$prob=0.29
M80_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")

#######PROBABILIDAD######################################################
#P0
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0
P0_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P1
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.1
P1_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P2
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.2
P2_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P3
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.3
P3_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P4
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.4
P4_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P5
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.5
P5_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P6
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.6
P6_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P7
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.7
P7_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P8
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.8
P8_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P9
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.9
P9_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="choice")
#P10
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=1
P10_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                               modelComponent="choice")
#######SATISFACCION######################################################
BetaEstimado=model$estimate
#SATIS11
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.1*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.1*BetaEstimado['ETA_PERSON']
SATIS11_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS12
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.2*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.2*BetaEstimado['ETA_PERSON']
SATIS12_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS13
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.3*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.3*BetaEstimado['ETA_PERSON']
SATIS13_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS14
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.4*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.4*BetaEstimado['ETA_PERSON']
SATIS14_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS15
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.5*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.5*BetaEstimado['ETA_PERSON']
SATIS15_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS16
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.6*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.6*BetaEstimado['ETA_PERSON']
SATIS16_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS17
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.7*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.7*BetaEstimado['ETA_PERSON']
SATIS17_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS18
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.8*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.8*BetaEstimado['ETA_PERSON']
SATIS18_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS19
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.9*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.9*BetaEstimado['ETA_PERSON']
SATIS19_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS20
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=2*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=2*BetaEstimado['ETA_PERSON']
SATIS20_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#######PERSON######################################################

#PERSON09
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.9*BetaEstimado['ETA_PERSON']
PERSON09_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON08
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.8*BetaEstimado['ETA_PERSON']
PERSON08_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON07
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.7*BetaEstimado['ETA_PERSON']
PERSON07_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON06
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.6*BetaEstimado['ETA_PERSON']
PERSON06_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON05
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.5*BetaEstimado['ETA_PERSON']
PERSON05_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON04
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.4*BetaEstimado['ETA_PERSON']
PERSON04_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON03
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.3*BetaEstimado['ETA_PERSON']
PERSON03_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON02
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.2*BetaEstimado['ETA_PERSON']
PERSON02_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON01
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0.1*BetaEstimado['ETA_PERSON']
PERSON01_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#PERSON00
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_PERSON']=0
PERSON00_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                    modelComponent="choice")
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#######SATISFACCIONyPERSON######################################################

#SATIS11PERSON09
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.1*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.1*0.9*BetaEstimado['ETA_PERSON']
S11P09_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS12PERSON08
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.2*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.2*0.8*BetaEstimado['ETA_PERSON']
S12P08_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS13PERSON07
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.3*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.3*0.7*BetaEstimado['ETA_PERSON']
S13P07_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS14PERSON06
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.4*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.4*0.6*BetaEstimado['ETA_PERSON']
S14P06_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS15PERSON05
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.5*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.5*0.5*BetaEstimado['ETA_PERSON']
S15P05_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS16PERSON04
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.6*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.6*0.4*BetaEstimado['ETA_PERSON']
S16P04_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS17PERSON03
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.7*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.7*0.3*BetaEstimado['ETA_PERSON']
S17P03_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS18PERSON02
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.8*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.8*0.2*BetaEstimado['ETA_PERSON']
S18P02_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS19PERSON01
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=1.9*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=1.9*0.1*BetaEstimado['ETA_PERSON']
S19P01_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#SATIS20PERSON00
database$policia_perros=1
database$camara=0
database$aleatoria=0
database$multa=214
database$retencion=0
database$prob=0.29
model$estimate['ETA_SATISF']=2*BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=2*0.000001*BetaEstimado['ETA_PERSON']
S20P00_forecast=apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")
model$estimate['ETA_SATISF']=BetaEstimado['ETA_SATISF']
model$estimate['ETA_PERSON']=BetaEstimado['ETA_PERSON']

#######RESUMEN######################################################
summary(base_forecast)
summary(camara_forecast)
summary(aleatorio_forecast)
summary(h2_forecast)
summary(h4_forecast)
summary(h6_forecast)
summary(h8_forecast)
summary(h10_forecast)
summary(h12_forecast)
summary(h14_forecast)
summary(h16_forecast)
summary(h18_forecast)
summary(h20_forecast)
summary(h22_forecast)
summary(h24_forecast)
summary(h26_forecast)
summary(h28_forecast)
summary(h30_forecast)
summary(h32_forecast)
summary(h34_forecast)
summary(h36_forecast)
summary(M00_forecast)
summary(M05_forecast)
summary(M15_forecast)
summary(M20_forecast)
summary(M25_forecast)
summary(M30_forecast)
summary(M35_forecast)
summary(M40_forecast)
summary(M45_forecast)
summary(M50_forecast)
summary(M55_forecast)
summary(M60_forecast)
summary(M65_forecast)
summary(M70_forecast)
summary(M75_forecast)
summary(M80_forecast)
summary(P0_forecast)
summary(P1_forecast)
summary(P2_forecast)
summary(P3_forecast)
summary(P4_forecast)
summary(P5_forecast)
summary(P6_forecast)
summary(P7_forecast)
summary(P8_forecast)
summary(P9_forecast)
summary(P10_forecast)
summary(PERSON00_forecast)
summary(PERSON01_forecast)
summary(PERSON02_forecast)
summary(PERSON03_forecast)
summary(PERSON04_forecast)
summary(PERSON05_forecast)
summary(PERSON06_forecast)
summary(PERSON07_forecast)
summary(PERSON08_forecast)
summary(PERSON09_forecast)
summary(SATIS20_forecast)
summary(SATIS19_forecast)
summary(SATIS18_forecast)
summary(SATIS17_forecast)
summary(SATIS16_forecast)
summary(SATIS15_forecast)
summary(SATIS14_forecast)
summary(SATIS13_forecast)
summary(SATIS12_forecast)
summary(SATIS11_forecast)
summary(S11P09_forecast)
summary(S12P08_forecast)
summary(S13P07_forecast)
summary(S14P06_forecast)
summary(S15P05_forecast)
summary(S16P04_forecast)
summary(S17P03_forecast)
summary(S18P02_forecast)
summary(S19P01_forecast)
summary(S20P00_forecast)
