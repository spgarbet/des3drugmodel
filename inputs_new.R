####
## 
# Define Simluation Inputs
##
####

# Function to Convert Nominal to Real Dollars
# Get CPI-U from FRED
 library(quantmod)
 getSymbols("CPIAUCSL", src='FRED')
 avg.cpi <- apply.yearly(CPIAUCSL, mean) 
 #save(avg.cpi,file="./main/cpi.Rdata")

realdol = function(nominal,year=2016,base=2012)
{
  cf <- avg.cpi/as.numeric(avg.cpi[paste(base)])
  return(as.numeric(nominal*cf[paste(year)]))
}

epsilon <- 0.000000000001



clopidogrel = list(
    vPREDICTsens = 0.23,
    vPREDICTspec = 0.93,
    vProbabilityRead = 1.00, # probability of physician using test results
    vProbabilityReactive = 1.00, # Under reactive, probability of ordering test

    vDAPT.SecondLine = "Ticagrelor",

    # Prognostic Model
    #vSensitivityPrDAPT = .9,#0.74,
    #vSpecificityPrDAPT = .9,#0.61,

    # Population-Level Allele Frequency Distribution
    vCYP2C19.Poor    = 0.21, # (0.15-0.40)
    vCYP2C19.Rapid   = 0.33, # (0.10-.40)
    vCYP2C19.Unknown = 0.07, # (0.05-0.09)

    # Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
    vDAPTShape = 0.59,
    vDAPTScale = 60475.53,

    # This parameter governs whether repeat DAPT therapy is more or less likely after having one.
    vRRRepeat.DAPT = epsilon,

    # This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
    # set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
    vMaxDAPT = 4,

    vDAPT.Tx.Duration = 365, # (12mo-48mo)

    vProbabilityDAPTSwitch = 0.55, # Source: VUMC PREDICT DATA
    
    # Stent Thrombosis: Event Rates and Relative Risks
    
    # Relative Risk of ST for patients with loss of function allele who are treated with 
    # Clopidogrel.
    vRR.ST.LOF = 1.75, #(1.50-2.03) High Discrimination Scenario =  2.81 (1.81-4.37)
    
    # The Stent Thrombosis Risks are drawn from a piecewise exponential with the following
    # durations and rates. 
    vRiskST30    = 0.0150, # (0.010-0.020)
    vRiskST365   = 0.0060, # (0.003-0.009)
    vRiskSTgt365 = 0.0022, # (0.001-0.003)

    vRR.ST.Ticagrelor = 0.75, # (0.59-0.95)
    vRR.ST.Prasugrel  = 0.48, # (0.36-0.64)
    vRR.ST.Aspirin    = 1.29, # (1.12-1.48)
    vSt.Case.Fatality = 0.20, #(15-30)
    vPrCABG.ST = 0.10,  # WHAT IS SOURCE?  CAN'T FIND IN ANNALS PAPER...

    # Myocardial Infarction: Event Rates and Relative Risks
    vRiskMI = 0.035, #(0.013-0.097)
    vRR.MI.Ticagrelor =0.84, # (0.75-0.95)
    vRR.MI.Prasugrel = 0.76, # (0.67-0.85)
    vRR.MI.Aspirin = 1.29, # (1.12-1.48)
    vPrCABG.MI = 0.08, # (4-12)
    vPrPCI.MI = 0.55, # (45-65)

    # Revascularization
    vRiskRV365 = 0.10, # (0.05-0.15)
    vRiskRVgt365 = 0.03, # (0.02-0.04)
    vPrCABG.RV = .25, # (15-35)

    # Bleeding
    vRiskExtBleed = 0.0230, # (0.015-0.070)
    vRiskIntBleed = 0.0015, # (0.001-0.002)
    vRiskTIMIMinor = 0.0200, # (0.010-0.060)
    vRiskFatalBleed = 0.0015, # (0.001-0.003)
    vRR.ExtBleed.Ticagrelor = 1.30, # (1.05-1.61)
    vRR.ExtBleed.Prasugrel = 1.22, #(0.93-1.6)
    vRR.ExtBleed.Aspirin =  0.72, #(0.60-1.00)

    vRR.IntBleed.Ticagrelor = 1.15, # (.55-2.41)
    vRR.IntBleed.Prasugrel =  0.83, # (0.36-1.92)
    vRR.IntBleed.Aspirin =  0.71, # (0.23-2.23)

    vRR.TIMIMinor.Ticagrelor = 1.07, # ( .91 - 1.26)
    vRR.TIMIMinor.Prasugrel =  1.16, # (.91-1.49)
    vRR.TIMIMinor.Aspirin =  0.47, #(.39-.57)

    vRR.FatalBleed.Ticagrelor = 0.87, # (0.48-1.59)
    vRR.FatalBleed.Prasugrel =  4.19, # (1.58-11.11)
    vRR.FatalBleed.Aspirin =  1.35, #(0.62-0.95)
    
    vRiskCABGTIMImajor = 0.022, # (0.013-0.031) 
    vRR.RiskCABGTIMImajor.Ticagrelor = 1.08, # (0.85-1.36)
    vRR.RiskCABGTIMImajor.Prasugrel =  4.73,# (1.90-11.82)
    vRR.RiskCABGTIMImajor.Aspirin =  1.08, # (0.61-1.91)

    vRR.MI.LOF = 1.48, #(1.05-2.07) High Discrimination Scenario = 1.45 (1.09-1.92)
    vRR.Mort.LOF = 1.28, #(0.95-1.73) #Not sure how to use this one
    vRR.Bleed.LOF = 0.84, # (0.75-1.00)
    #vRR.Thrombotic.GOF = 0.75, # (0.66-1.00)
    #vRR.Bleed.GOF = 1.26  # (1.00-1.50)
    
    vRiskStroke = epsilon
    
)

# or converted to rr
or_rr <- function(or,prev) {
        or/((1-prev)+prev*or)
}

simvastatin <- list(
    vPREDICTsens = 0.23,
    vPREDICTspec = 0.93,
    vProbabilityRead = 1.00, # probability of physician using test results
    vProbabilityReactive = 1.00, # Under reactive, probability of ordering test
    
    # Weibull for statin prescription
    vScale = 13989.02,
    vShape = 0.54,
    
    #
    vMedMetabolizer  = 0.249,   # Prevalence of medium metabolizers
    vPoorMetabolizer = 0.021,   # Prevalence of poor metabolizers
  
    vProbSimvastatinAlt = 1.00,  # Prob. of Alt | Variant
    vProbSimStopMild = 0.23,  # Prob. of Stop | Mild Myo
    vProbSimStopMod  = 0.23,  # Prob. of Stop | Mod Myo
    vProbSimStopSev  = 1.00,  # Prob. of Stop | Sev Myo
 
    # 5-year Mild Myopathy Risks
    vMildMyoBaseNoVar=1e-7, # No Drug Risk of mild myopathy
    vMildMyoSimNoVar=0.05,  # Simvastatin Mild Myopathy Baseline Risk
    vMildMyoSimMedVar=2.727,     # Rel Risk|Medium metabolizer
    vMildMyoSimPoorVar=6.429,    # Rel Risk|Poor metabolizer
    vMildMyoAltNoVar=0.05,  # Alternate Drug Mild Myopathy Baseline Risk
    vMildMyoAltMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoAltPoorVar=1,    # Rel Risk|Poor metabolizer

    # 5-year Moderate Myopathy Risks
    vModMyoBaseNoVar=1e-10, # No Drug Risk of mild myopathy
    vModMyoSimNoVar=0.00011,  # Simvastatin Mild Myopathy Baseline Risk
    vModMyoSimMedVar=2.999,    # Rel Risk|Medium metabolizer
    vModMyoSimPoorVar=8.992,   # Rel Risk|Poor metabolizer
    vModMyoAltNoVar=0.00011,  # Alternate Drug Mild Myopathy Baseline Risk
    vModMyoAltMedVar=1,    # Rel Risk|Medium metabolizer
    vModMyoAltPoorVar=1,    # Rel Risk|Poor metabolizer

    # 5-year Severe Myopathy Risks
    vSevMyoBaseNoVar=1e-16,   # No Drug Risk of mild myopathy
    vSevMyoSimNoVar=0.000034, # Simvastatin Mild Myopathy Baseline Risk
    vSevMyoSimMedVar=3,   # Rel Risk|Medium metabolizer
    vSevMyoSimPoorVar=9,   # Rel Risk|Poor metabolizer
    vSevMyoAltNoVar=0.000034, # Alternate Drug Mild Myopathy Baseline Risk
    vSevMyoAltMedVar=1,    # Rel Risk|Medium metabolizer
    vSevMyoAltPoorVar=1,   # Rel Risk|Poor metabolizer
    
    vProbRahbdoDeath = 0.1, # Case fatality for severe myopathy
    vProbcvdDeath = 0.117 # Case fatality for CVD

)

warfarin = list(
  vPREDICTsens = 0.23,
  vPREDICTspec = 0.93,

  vProbabilityRead = 1.00, # probability of physician using test results
  vProbabilityReactive = 1.00, # Under reactive, probability of ordering test
  
  # start warfarin
  vpct_afib = 0.09, # add last observed % w/ a.fib among those on warfarin
  #aLP_Warfarin = 1,
  vshape_timetowarfarin = 0.66,
  vscale_timetowarfarin = 33471.45,
  
  # INR: initial & time to get in range
  vMedianTimetoINR	= 0.0239, 
  vMedianTimetoINR_PGx	= 0.033,
  vMedianTimetoINR_PGx_delay = 0.02888, 
  vINRfreq = (read.csv("./warfarin/warfarin_inputs_INR.csv"))$INR_freq,
  vINRvalue = (read.csv("./warfarin/warfarin_inputs_INR.csv"))$INR_value,
  
  # adverse events: bleed
  vAF_Risk_Major_Bleed_3 = 0.01497, # risk of bleeding events for INR < 3 & AF indication
  vAF_Risk_Major_Bleed_3to4 = 0.06224,
  vAF_Risk_Major_Bleed_Over4 = 0.39118,
  vRRMajorBleed_AF = 1,
  vNonAF_Risk_Major_Bleed_3 = 0.01497,
  vNonAF_Risk_Major_Bleed_3to4 = 0.06224,
  vNonAF_Risk_Major_Bleed_Over4 = 0.39118, 
  vRRMajorBleed_NonAF = 1,
  
  vAF_Risk_Minor_Bleed_3 = 0.0936, # risk of bleeding events for INR < 3 & AF indication (minor fix)
  vAF_Risk_Minor_Bleed_3to4 = 0.3890,
  vAF_Risk_Minor_Bleed_Over4 = 0.9999,
  vRRMinorBleed_AF = 1,
  vNonAF_Risk_Minor_Bleed_3 = 0.0936,
  vNonAF_Risk_Minor_Bleed_3to4 = 0.3890,
  vNonAF_Risk_Minor_Bleed_Over4 =	0.9999,
  vRRMinorBleed_NonAF = 1,
  
  vTimeDurBleed = 365,

  #adjusted estimates:
  # V1:    90d risk is 1.95 * other time
  # major: 0.01497 0.06224 0.39118 
  # minor: 0.0973 0.4046 0.9999
  # V2:    90d risk is 1.95 * average 1 yr
  # major: 0.01852 0.07702 0.48409 
  # minor: 0.1204 0.5007 0.9999
  
  ##prob distribution of six types of major bleeding events
  vR_Bleed_ICH	= 0.144, 
  vR_Bleed_ICH_Fatal = 0.156,
  vR_Bleed_GI	= 0.557,
  vR_Bleed_GI_Fatal	= 0.043,
  vR_Bleed_Other = 0.098,
  vR_Bleed_Other_Fatal = 0.002,
  
  # adverse event: stroke
  vAF_Risk_Stroke_1.5 = 0.077,
  vAF_Risk_Stroke_1.5to2 =	0.019,
  vAF_Risk_Stroke_Over2 =	0.006,
  vRRStroke_AF = 1,
  vNonAF_Risk_Stroke_3 =	0.00001,
  vNonAF_Risk_Stroke_Over3 =	0.006,
  vRRStroke_NonAF = 1,
  vTimeDurStroke = 365,
  
  ## prob distribution of three types of stroke events, among which major deficit stops warfarin and goes back to main model
  vR_Stroke_MinorDeficit_2 = 0.4116,
  vR_Stroke_MajorDeficit_2 = 0.4284,
  vR_Stroke_Fatal_2 = 0.16,
  vR_Stroke_MinorDeficit_Over2 = 0.5358,
  vR_Stroke_MajorDeficit_Over2 = 0.4042,
  vR_Stroke_Fatal_Over2 = 0.06,
  
  # adverse event: DVTPE, only for NonAF patients
  vNonAF_Risk_DVTPE_2 = 0.019,
  vNonAF_Risk_DVTPE_Over2 =	0.007,
  vRRDVTPE_NonAF = 1,
  vTimeDurDVTPE = 365,
  
  ## prob distribution of three types of DVTPE events
  vR_DVT = 0.4,
  vR_PE = 0.1,
  vR_DVTPE_Fatal = 0.5
)


inputs <- list(
  # Population Parameters
  vN           = 1000,   # Patients to simulate
  vNIter       = 4,      # Number of Iterations (parallel processing)
  #vLowerAge    = 40,      # Lower age to simulate coming in (uniform distribution)
  #vUpperAge    = 85,      # Upper age to simulate
  vHorizon     = 10,      # Length of simulation upon a patient entering
  #vPctFemale   = 0.5,     # Percent Female
  
  # Strategies
  vPreemptive  = "None",  # Can be one of following: "None", "Panel", "PREDICT", or "Age >= 50"
  vReactive    = "None", # Can be one of following: "None", "Single", "Panel"

# Control Which Drugs Are Run in the Model 
  vDrugs       = list(vSimvastatin = TRUE, 
                      vWarfarin = FALSE,
                      vClopidogrel = FALSE),

# CURRENTLY PANEL IS FOR ALL DRUGS ???
  vPanel       = list(vSimvastatin = TRUE, 
                    vWarfarin=TRUE, 
                    vClopidogrel = TRUE),

  # Drug specific model parameters
  clopidogrel = clopidogrel,
  simvastatin = simvastatin,
  warfarin    = warfarin,
  
  # If these names match the event names from the simmer model, then computation can be generalized!
  # These must be DAILY costs
  costs = list(
    panel_test      =   realdol(250,year=2012),
    single_test     =   realdol(100,year=2012),
    mild_myopathy   =   realdol(129/30,year=2012),
    mod_myopathy    =  realdol(2255/60,year=2012), # Note this divided by duration
    sev_myopathy    = realdol(12811/60,year=2012),
    rahbdo_death    = realdol(12811,year=2012), #pass on total sev_myopathy cost
    cvd             = realdol(20347/30,year=2012),
    cvd_death       = realdol(20347,year=2012), #pass on total cvd cost
    simvastatin     =   realdol(147/365,year=2012),
    alt_simvastatin = realdol(173.1/365,year=2012),
    
    aspirin         = realdol(4/30,year=2011),
    clopidogrel     = realdol(30/30,year=2011),
    ticagrelor      = realdol(220/30,year=2011),
    prasugrel       = realdol(261/30,year=2011),
    bleed_ext_maj_nonfatal = realdol(10120/14,2011),
    bleed_int_maj_nonfatal = realdol(20740,2011),
    bleed_min_nonfatal = realdol(79/2,2011),
    bleed_fatal     = realdol(17920,2011),
    st_fatal        = realdol(24540,2011),
    st_cabg         = realdol(67720,2011),
    st_pci     = realdol(27840,2011), 
    mi_cabg           = realdol(67720,2011),
    mi_med_manage   = realdol(17200,2011),
    mi_pci     = realdol(27840,2011), 
    revasc_cabg     = realdol(50560/14,2011),
    revasc_pci      = realdol(20670/7,2011),
    cabg_bleed      = realdol(35570/7,2011),
    
    warfarin        = realdol(71/90,year=2007),
    #bleeding events: sharing w/ chlopidigrel model??
    MajorBleed_ICH	= realdol(20740,year=2011),
    MajorBleed_GI	  = realdol(2328/14,year=2011),
    MajorBleed_Other = realdol(6154/14,year=2011),
    MajorBleed_ICH_Fatal =	realdol(17920,year=2011),
    MajorBleed_GI_Fatal =	realdol(17920,year=2011),
    MajorBleed_Other_Fatal =	realdol(17920,year=2011),
    MinorBleed = realdol(79/2,year=2011),
    Stroke_MajorDeficit = realdol(21537,year=2007),
    Stroke_MinorDeficit = realdol(15499,year=2007),
    Stroke_Fatal    = realdol(10396,year=2007),
    DVT	= realdol(7594,year=2004), 
    PE =	realdol(13018,year=2004),
    DVTPE_Fatal =	realdol(7000,year=2004),
    out_of_range = realdol(29/3,year=2007),
    in_range = realdol(29/7,year=2007)

  ),
  # Each listed duration will be corrected in the final data frame (temp disutility)
  durations = list(
    mild_myopathy =  30,
    mod_myopathy  = 60,
    sev_myopathy  = 60,
    cvd           = 30,
   
    bleed_ext_maj_nonfatal = 14,
    bleed_min_nonfatal = 2,
    revasc_cabg     = 14,
    revasc_pci      = 7,
    cabg_bleed      = 7,
    
    MajorBleed_GI	= 14,
    MajorBleed_Other =	14,
    MinorBleed = 2
    
  ),
  disutilities = list(
    mild_myopathy = 0.0100,
    mod_myopathy  = 0.0500,
    sev_myopathy  = 0.5300,
    cvd           = 0.2445,
    cvd_death     = 1,
    rahbdo_death  = 1,
    
    
    bleed_ext_maj_nonfatal = .2,
    bleed_int_maj_nonfatal = .61,
    bleed_min_nonfatal = .2,
    bleed_fatal     = 1,
    st_fatal        = 1,
    st_cabg         = .12,
    st_pci     = .12,    
    mi_cabg         = .12,
    mi_med_manage   = .12,
    mi_pci     = .12,
    revasc_cabg     = .5,
    revasc_pci      = .5,
    cabg_bleed      = .5,
    
    MajorBleed_ICH	= 0.61,
    MajorBleed_GI	  = 0.1511,
    MajorBleed_Other = 0.1511,
    MajorBleed_ICH_Fatal = 1, 
    MajorBleed_GI_Fatal =	1, 
    MajorBleed_Other_Fatal = 1,
    MinorBleed      = 0.2,
    Stroke_MajorDeficit = 0.64,
    Stroke_MinorDeficit = 0.24,
    Stroke_Fatal   = 1, 
    DVT         	 = 0.16,
    PE             = 0.37,
    DVTPE_Fatal    = 1,
    out_of_range = 0.012/3,
    in_range = 0.012/7,
    
    secular_death = 1
  
  ),
# Each shows whether the event permanently decreases utility (type==0) vs temporarily(type==1)
type= list(
  mild_myopathy = 1,
  mod_myopathy  = 1,
  sev_myopathy  = 1,
  cvd           = 1,
  cvd_death     = 0,
  rahbdo_death  = 0,
  
  
  bleed_ext_maj_nonfatal = 1,
  bleed_int_maj_nonfatal = 0,
  bleed_min_nonfatal = 1,
  bleed_fatal     = 0,
  st_fatal        = 0,
  st_cabg         = 0,
  st_pci     = 0,    
  mi_cabg         = 0,
  mi_med_manage   = 0,
  mi_pci     = 0,
  revasc_cabg     = 1,
  revasc_pci      = 1,
  cabg_bleed      = 1,
  
  MajorBleed_ICH	= 0,
  MajorBleed_GI	  = 1,
  MajorBleed_Other = 1,
  MajorBleed_ICH_Fatal = 0, 
  MajorBleed_GI_Fatal =	0, 
  MajorBleed_Other_Fatal = 0,
  MinorBleed      = 1,
  Stroke_MajorDeficit = 0,
  Stroke_MinorDeficit = 0,
  Stroke_Fatal   = 0, 
  DVT         	 = 0,
  PE             = 0,
  DVTPE_Fatal    = 0,
  out_of_range = 1,
  in_range = 1,
  
  secular_death = 0
  
)   
)






