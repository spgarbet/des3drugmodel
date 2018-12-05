
#' Female regression coefficients the Framingham data set.
framingham_beta_f <- c(2.32888, 1.20904, -0.70833, 2.76157, 2.82263, 0.52873, 0.69154)

#' Computes the 10 year cvd event risk for a female using
#' a cox-regression of the Framingham data set.
#'
#' Parameters taken from R D'Agostino, Vasan, R, Pencina, M, Wolf, P, Cobain,
#' M, Massaro, J, Kannel, W. _General Cardiovascular Risk Profile for Use in
#' Primay Care: The Framingham Heart Study_, Circulation, 2008 117:743-753.
#' doi: 10.1161/CIRCULATIONAHA.107.699579
#' 
#' This function is vectorized. Defaults for all parameters but Age
#' center those parameters (i.e. correctly treat as unknown).
#' 
#' @param age          Age in years
#' @param tot_chol     Total Cholesterol, mg/dL
#' @param hdl_chol     HDL Cholesterol, mg/dL
#' @param systolic_bp  Systolic BP, mm Hg
#' @param bp_treatment Boolean, TRUE is subject is receiving BP treatment
#' @param smoker       Boolean, TRUE if subject is a smoker
#' @param diabetic     Boolean, TRUE if subject is diabetic
#' 
#' @return Vector of probabilities for 10 year CVD event
#' @export
cvd_prob_10_year_female_framingham <- Vectorize(function(
  age,
  tot_chol=215.1,
  hdl_chol=57.6,
  systolic_bp=125.8, 
  bp_treatment=532/4522,
  smoker=1548/4522,
  diabetic=170/4522)
{
  data <- c(log(age), log(tot_chol), log(hdl_chol),
            (1-bp_treatment)*log(systolic_bp),  bp_treatment*log(systolic_bp),
            smoker, diabetic)
  
  lh <- sum(data * framingham_beta_f)
  
  #1 - 0.95012^exp(lh - 26.1931) # Center from Appendix
  
  1 - 0.95012^exp(lh - 26.28405)  # Center computed from table
})

#' Female regression coefficients the Framingham data set.
framingham_beta_m <- c(3.06117, 1.12370, -0.93263, 1.93303, 1.99881, 0.65451, 0.57367)

#' Computes the 10 year cvd event risk for a male using
#' a cox-regression of the Framingham data set.
#'
#' Parameters taken from R D'Agostino, Vasan, R, Pencina, M, Wolf, P, Cobain,
#' M, Massaro, J, Kannel, W. _General Cardiovascular Risk Profile for Use in
#' Primay Care: The Framingham Heart Study_, Circulation, 2008 117:743-753.
#' doi: 10.1161/CIRCULATIONAHA.107.699579
#' 
#' This function is vectorized. Defaults for all parameters but Age
#' center those parameters (i.e. correctly treat as unknown).
#' 
#' @param age          Age in years
#' @param tot_chol     Total Cholesterol, mg/dL
#' @param hdl_chol     HDL Cholesterol, mg/dL
#' @param systolic_bp  Systolic BP, mm Hg
#' @param bp_treatment Boolean, TRUE is subject is receiving BP treatment
#' @param smoker       Boolean, TRUE if subject is a smoker
#' @param diabetic     Boolean, TRUE if subject is diabetic
#' 
#' @return Vector of probabilities for 10 year CVD event
#' @export
cvd_prob_10_year_male_framingham <- Vectorize(function(
  age,
  tot_chol=212.5,
  hdl_chol=44.9,
  systolic_bp=129.7, 
  bp_treatment=402/3969,
  smoker=1398/3969,
  diabetic=258/3969)
{
  data <- c(log(age), log(tot_chol), log(hdl_chol),
           (1-bp_treatment)*log(systolic_bp),  bp_treatment*log(systolic_bp),
           smoker, diabetic)

  lh <- sum(data * framingham_beta_m)

  #1 - 0.88936^exp(lh - 23.9802) # Center from Appendix
  
  1 - 0.88936^exp(lh - 24.0607)  # Center computed from table
})
