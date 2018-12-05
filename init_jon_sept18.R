

####
## 
# Define Simulation Environment.
#
# NOTE: This must be done at a global level for the simmer now() function to be available
#       inside trajectories. Without this at a global level, the simulation won't work.
####
#env  <- simmer("RIGHT-v1.1")

####
## 
# Define Simulation Scenario
##
####
source("./inputs_new.R")

####
## Secular Death
source('./main/event_secular_death.R')

# Define Panel Test attributes, functions
all_genotyped <- function(attrs)
{
        attrs[['aGenotyped_CVD']]     == 1 &&  # Simvastatin
                attrs[['aGenotyped_CYP2C19']] == 1 &&  # Clopidogrel
                attrs[['aGenotyped_Warfarin']] == 1    # Warfarin  
}

any_genotyped <- function(attrs)
{
        attrs[['aGenotyped_CVD']]     == 1 ||
                attrs[['aGenotyped_CYP2C19']] == 1 ||
                attrs[['aGenotyped_Warfarin']] == 1 
}

panel_test <- function(traj, inputs)
{
        traj %>% 
                set_attribute('aGenotyped_CYP2C19', 1)  %>%
                set_attribute('aGenotyped_CVD',     1)  %>%
                set_attribute('aGenotyped_Warfarin', 1) %>%
                mark("panel_test")
}

#####
## Clopidogrel
source("./clopidogrel/counters.R")
source("./clopidogrel/initial-patient-attributes_split.R")
source("./clopidogrel/cleanup.R")
source("./clopidogrel/PGx-attributes.r")
source("./clopidogrel/dapt-events_split.r")

####
## Simvastatin
source('./simvastatin/counters.R')
source('./simvastatin/initial-patient-attributes.R')
source('./simvastatin/PGx-attributes.R')
source('./simvastatin/cleanup.R')
source('./simvastatin/cvd_framingham.R')
source('./simvastatin/event_cvd.R')
source('./simvastatin/event_myopathy.R')
source('./simvastatin/event_statin.R')

## Warfarin
source('./warfarin/counters.R')
source('./warfarin/initial-patient-attributes.R')
source('./warfarin/PGx-attributes.R')
source('./warfarin/cleanup.R')
source('./warfarin/event_warfarin.R')
source('./warfarin/event_in_range.R')
source('./warfarin/event_90days.R')
source('./warfarin/event_bleed.R')
source('./warfarin/event_stroke.R')
source('./warfarin/event_DVTPE.R')
source('./warfarin/event_6m_NonAF.R')

#load('./main/NHANES_pop_11_14.rda')
study_pop <- read.csv("./main/Age_Sex.csv")
initialize_patient <- function(traj, inputs)
{
        traj %>%
                seize("time_in_model")       %>%
                
                # Empirical
                set_attribute("aNo", function(attrs) sample(1:nrow(study_pop), 1, prob=rep(1/nrow(study_pop),nrow(study_pop)))) %>%
                set_attribute("aGender",    function(attrs) study_pop$gender[attrs[['aNo']]]) %>% 
                set_attribute("aAge",       function(attrs) study_pop$age.mh[attrs[['aNo']]]) %>% 
                
                # NHANES version
                #set_attribute("aNo", function(attrs) sample(1:nrow(NHANES_pop), 1, prob=1/NHANES_pop$wt)) %>%
                #set_attribute("aGender",    function(attrs) NHANES_pop$gender[attrs[['aNo']]]) %>% 
                #set_attribute("aAge",       function(attrs) NHANES_pop$age[attrs[['aNo']]]) %>% 
                
                # set_attribute("aGender",    function(attrs) sample(1:2,1,prob=c(0.5,0.5))) %>% 
                # set_attribute("aAge",       function(attrs) runif(1,18,90)) %>%
                
                set_attribute("aAgeInitial",function(attrs) attrs[['aAge']])  %>%
                assign_clopidogrel_attributes(inputs) %>%
                assign_simvastatin_attributes(inputs) %>%
                assign_warfarin_attributes(inputs)
}

predict_draw <- function(traj, inputs)
{
        traj %>%
                predict_clopidogrel_draw(inputs) %>%
                predict_simvastatin_draw(inputs) %>%
                predict_warfarin_draw(inputs)
}

predict_test <- function(traj, inputs)
{
        traj %>%
                predict_clopidogrel(inputs) %>%
                predict_simvastatin(inputs) %>%
                predict_warfarin(inputs)
}

# Must Be Run After The Initial Event Times Have Been Determined 
# For predict to work

# No modification required for adding more drug models
preemptive_strategy <- function(traj, inputs)
{
        traj <- predict_draw(traj, inputs) # Always execute predict random draw to keep seeded random number
        # states the same
        
        # Note this doesn't have to use branch, because it's a global that every trajectory gets
        if        (inputs$vPreemptive == "None"     )
        {
                traj # Do nothing
        } else if (inputs$vPreemptive == "Panel"    )
        {
                traj %>% panel_test(inputs) 
        } else if (inputs$vPreemptive == "PREDICT")
        {
                traj %>%
                        predict_test(inputs) %>%
                        branch(
                                function(attrs) ifelse(any_genotyped(attrs),2,1),
                                continue=rep(TRUE,2),
                                trajectory() %>% timeout(0), # Nothing genotyped, do nothing
                                trajectory() %>% panel_test(inputs) # Something was genotyped via PREDICT, do panel
                        )
        } else if (inputs$vPreemptive == "Age >= 50")
        {
                traj %>%
                        branch(
                                function(attrs) if(attrs[['aAge']] >= 50) 1 else 2,
                                continue = c(TRUE, TRUE),
                                trajectory() %>% panel_test(inputs), 
                                trajectory() %>% timeout(0)  # Do nothing
                        )
        } else stop("Unhandled Preemptive Strategy")
}

####
## Cleanup 
cleanup_on_termination <- function(traj)
{
        traj %>% 
                #print_attrs() %>%
                release("time_in_model") %>%
                cleanup_clopidogrel() %>%
                cleanup_aspirin() %>% 
                cleanup_simvastatin() %>%
                cleanup_warfarin()
}

terminate_simulation <- function(traj, inputs)
{
        traj %>%
                branch(
                        function() 1, 
                        continue=FALSE,
                        trajectory() %>% cleanup_on_termination()
                )
}

####
## Event Registry
event_registry <- list(
        
        #### Global Events
        list(name          = "Secular Death",
             attr          = "aSecularDeathTime",
             time_to_event = days_till_death,
             func          = secular_death,
             reactive      = FALSE),
        list(name          = "Terminate at 10 years",
             attr          = "aTerminate",
             time_to_event = function(attrs,inputs) 365.0*(inputs$vHorizon),
             func          = terminate_simulation,
             reactive      = FALSE),
        
        #### Simvastatin Events
        list(name          = "Start of Statin",
             attr          = "aStartStatin",
             time_to_event = days_till_statin,
             func          = prescribe_statin,
             reactive      = FALSE),
        list(name          = "Mild Myopathy",
             attr          = "aMildMyoTime",
             time_to_event = days_till_mild_myopathy,
             func          = mild_myopathy,
             reactive      = FALSE),
        list(name          = "Moderate Myopathy",
             attr          = "aModMyoTime",
             time_to_event = days_till_mod_myopathy,
             func          = mod_myopathy,
             reactive      = FALSE),
        list(name          = "Severe Myopathy",
             attr          = "aSevMyoTime",
             time_to_event = days_till_sev_myopathy,
             func          = sev_myopathy,
             reactive      = FALSE),
        list(name          = "Cardiovascular Disease",
             attr          = "aCVDTime",
             time_to_event = days_till_cvd,
             func          = cvd,
             reactive      = TRUE),
        list(name          = "Reassess CVD Risk",
             attr          = "aCVDReassess",
             time_to_event = days_till_reassess_cvd,
             func          = reassess_cvd,
             reactive      = FALSE),
        
        #### Clopidogrel Events
        list(name          = "DAPT Initialized",
             attr          = "aTimeDAPTInitialized",
             time_to_event = days_till_dapt,
             func          = dapt,
             reactive      = FALSE) ,
        list(name          = "DAPT Ended",
             attr          = "aDAPTEnded",
             time_to_event = dapt_end_time,
             func          = dapt_end,
             reactive      = FALSE),
        list(name          = "Stent Thromb",
             attr          = "aST",
             time_to_event = time_to_ST,
             func          = ST_event,
             reactive      = FALSE),
        list(name          = "Myocardial Infarction",
             attr          = "aMI",
             time_to_event = time_to_MI,
             func          = MI_event,
             reactive      = FALSE) , 
        list(name          = "Revascularization",
             attr          = "aRV",
             time_to_event = time_to_RV,
             func          = RV_event,
             reactive      = FALSE) ,
        list(name          = "Extracranial TIMI Non-Fatal",
             attr          = "aExtBleed",
             time_to_event = time_to_ExtBleed,
             func          = ExtBleed_event,
             reactive      = FALSE) ,
        list(name          = "Intracranial TIMI Major Nonfatal",
             attr          = "aIntBleed",
             time_to_event = time_to_IntBleed,
             func          = IntBleed_event,
             reactive      = FALSE),
        list(name          = "TIMI Minor",
             attr          = "aTIMIMinor",
             time_to_event = time_to_TIMIMinor,
             func          = TIMIMinor_event,
             reactive      = FALSE) ,
        list(name          = "Fatal Bleed",
             attr          = "aFatalBleed",
             time_to_event = time_to_FatalBleed,
             func          = FatalBleed_event,
             reactive      = FALSE),
        list(name          = "CABG-related bleed",
             attr          = "aCABGBleed",
             time_to_event = time_to_CABGBleed,
             func          = CABGBleed_event,
             reactive      = FALSE),
        
        #### Warfarin Events
        list(name          = "Start Warfarin",
             attr          = "aTimeToStartWarfarin",
             time_to_event = days_till_warfarin,
             func          = prescribe_warfarin,
             reactive      = FALSE),
        list(name          = "Get in range",
             attr          = "aTimeToInRange",
             time_to_event = days_till_in_range,
             func          = get_in_range,
             reactive      = FALSE),
        list(name          = "Pass 90 days",
             attr          = "aTimeTo90d",
             time_to_event = days_till_90d,
             func          = reach_90d,
             reactive      = FALSE),
        list(name          = "Pass 6 months",
             attr          = "aTimeTo6m",
             time_to_event = days_till_6m,
             func          = reach_6m_NonAF,
             reactive      = FALSE),
        list(name          = "Major Bleed",
             attr          = "aTimeToMajorBleed",
             time_to_event = days_till_major_bleed,
             func          = major_bleed_event,
             reactive      = FALSE),
        list(name          = "Minor Bleed",
             attr          = "aTimeToMinorBleed",
             time_to_event = days_till_minor_bleed,
             func          = minor_bleed_event,
             reactive      = FALSE),
        list(name          = "Stroke",
             attr          = "aTimeToStroke",
             time_to_event = days_till_stroke,
             func          = stroke_event,
             reactive      = FALSE),
        list(name          = "DVTPE",
             attr          = "aTimeToDVTPE",
             time_to_event = days_till_DVTPE,
             func          = DVTPE_event,
             reactive      = FALSE)
)

#####
## Counters
source("./main/counters.R")
counters <- c(counters.gen, counters.dapt, counters.simvastatin, counters.warfarin)


#####################################################################
####
##
# Setup and Run the Simulation.
##
####
source('./main/event_main_loop.R')

