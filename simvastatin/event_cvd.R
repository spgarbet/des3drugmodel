  ###########################################################################
 ##
##  Assign and Track Cardiovascular Events
##
## Risk is a composite based on age and gener
## Source: http://chd.bestsciencemedicine.com/calc2.html
## Risks of heart attacks, angina/coronary insufficiency, heart failure, 
## stroke and intermittent claudication based on data from Framingham
## Cohort Study.
##
## Trying to look at replacing the CVD event generator

mean_us_chol <- function(age, gender)
{
  if(age <= 44.0)
  {
    if(gender == 1) 191 else 187
  } else if(age <= 64.0)
  {
    if(gender == 1) 200 else 212
  } else if(age <= 74.0)
  {
    if(gender == 1) 184 else 206
  } else 
  {
    if(gender == 1) 173 else 201
  }
}

mean_us_sys_bp <- function(age, gender) 
{
  if(gender == 1)
  {
    if(age <= 59.0) 124 else 133
  } else
  {
    if(age <= 59.0) 122 else 139
  }
}

us_prevalence_diabetes <- function(age)
{
  if(age <= 44.0)
  {
    0.037
  } else if (age <= 64.0)
  {
    0.162
  } else
  {
    0.268
  }
}

# This causes a reassessment of CVD risk based on age
days_till_reassess_cvd <- function(inputs) { 365 }

reassess_cvd <- function(traj,inputs)
{
  traj # Does nothing, but triggers reassessment, via reactive events
}

days_till_cvd <- function(inputs)
{
  # Are they at risk of being prescribed Statins => increased CVD risk
  # QUESTION: Should this be dictated by end of life, not just end of simulation?
  # if(get_attribute(env, 'aStartStatin') > inputs$vHorizon*365)
  # {
  #   return(inputs$vHorizon*365+ 1) # No CVD present above background rate
  # }
  hx         <- get_attribute(env, 'aStatinRxHx')
  drug       <- get_attribute(env, 'aCVDdrug')
  gender     <- get_attribute(env, 'aGender')
  age        <- get_attribute(env, 'aAge')
  
  tot_chol      <- mean_us_chol(age, gender)
  sys_bp        <- mean_us_sys_bp(age, gender)
  diabetes_prob <- us_prevalence_diabetes(age)
  
  time_frame <- 3650 # Rates are over 10 years
  
  rr <- if(drug==0) {1} else {0.65}
  
  prob <- if(gender == 1)
  {
    cvd_prob_10_year_male_framingham(
      age,
      tot_chol=tot_chol,
      hdl_chol=47,  # http://jama.jamanetwork.com/article.aspx?articleid=1383233
      systolic_bp=sys_bp, 
      smoker=0.205,  # http://www.cdc.gov/nchs/data/hus/hus14.pdf#061
      diabetic=diabetes_prob)
  } else
  {
    cvd_prob_10_year_female_framingham(
      age,
      tot_chol=tot_chol,
      hdl_chol=57,  # http://jama.jamanetwork.com/article.aspx?articleid=1383233
      systolic_bp=sys_bp, 
      smoker=0.153,  # http://www.cdc.gov/nchs/data/hus/hus14.pdf#061
      diabetic=diabetes_prob)
  }
  
  rate <- -log(1-prob)*rr/time_frame
  
  # Only include CVD (and thus tracking) if they are in Simvastatin study, i.e. taking a statin
  if(hx>=1)
  {
    rexp(1, rate)
  } else {
    inputs$vHorizon*365+1
  }
}

cvd <- function(traj,inputs)
{
  traj %>%
  branch(
    function() sample(1:2, 1, prob=c(inputs$simvastatin$vProbcvdDeath, 1-inputs$simvastatin$vProbcvdDeath)),
    continue=c(FALSE, TRUE),
    trajectory("CVD w/ Death") %>% mark("cvd_death") %>% cleanup_on_termination(),
    trajectory("CVD Event")    %>% mark("cvd") %>% timeout(0)
  ) %>%
  assign_statin(inputs)
}