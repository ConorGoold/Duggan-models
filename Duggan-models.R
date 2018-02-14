## Systems modelling in R (Duggan)

library(deSolve)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)

#--------------------------------------------------------------------------
#### Model on page 53
#--------------------------------------------------------------------------

start <- 0; finish <- 100; step <- 0.25
simtime <- seq(start, finish, by = step)
stocks <- c(sStock = 100)
auxs <- c(aCapacity = 1e4, 
          aRef_availability = 1, 
          aRef_growth_rate = 0.10
          )

model <- function(time, stocks, auxs)
{
  with( as.list( c(stocks, auxs) ),{
    aAvailability <- 1 - (sStock / aCapacity)
    aEffect <- aAvailability / aRef_availability
    aGrowth_rate <- aRef_growth_rate * aEffect 
    fNet_flow <- sStock * aGrowth_rate
    
    # differential equation
    dS_dt <- fNet_flow
    
    return( list( c(dS_dt),    # stocks, may be multiple
                  NetFlow = fNet_flow, 
                  Growth_rate = aGrowth_rate, 
                  Effect = aEffect,
                  Availability = aAvailability
                  )
            )
  })
}


fit <- data.frame(ode(y = stocks, times = simtime, func = model, 
                      parms = auxs, method = "euler"))

ggplot(fit, aes(time, sStock)) + 
  geom_line()

#--------------------------------------------------------------------------
#### Model on page 56
#--------------------------------------------------------------------------

start <- 0; finish <- 100; step <- 0.25
simtime <- seq(start, finish, step)
stocks <- c(sMachines = 100)
auxs <- c(aReinvest_frac = 0.2,
          aDeprec_frac = 0.1,
          aLabour = 100
          )

model <- function(time, stocks, auxs)
{
  with(as.list( c(stocks, auxs)),{
    aEcon_output <- aLabour * sqrt(sMachines)
    fInvest <- aEcon_output * aReinvest_frac
    fDiscards <- sMachines * aDeprec_frac
    
    # differential equation
    dM_dt <- fInvest - fDiscards
    
    return(list(c(dM_dt), # stocks, may be multiple
                Investment = fInvest, 
                Discards = fDiscards,
                Economic_output = aEcon_output))
  }
  )
}


fit <- data.frame(
  ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler")
)

ggplot(fit, aes(time, sMachines)) + 
  geom_line() + 
  geom_line(aes(x = time, y = max(sMachines)),
            linetype="dashed")

#--------------------------------------------------------------------------
#### Model on page 60
#--------------------------------------------------------------------------

start <- 0; finish = 200; step = 0.25
simtime <- seq(start, finish, step)
stocks <- c(sCapital = 5, sResource = 1000)
auxs <- c(aDeprec_rate = 0.05, 
          aDesired_growth = 0.07, 
          aRev_per_extract = 3, 
          aFrac_reinvest = 0.12, 
          aCost_per_invest = 2)


## non-linear relationship between resource and extraction efficiency (less efficient extraction
## with less resource avaiable)
x.resource <- seq(0,1000,by=100)
y.efficiency <- c(0,0.25,0.45,0.63,0.75,0.85,0.92,0.96,0.98,0.99,1.0)
func.efficiency <- approxfun(x = x.resource,
                             y = y.efficiency,
                             method = "linear",
                             yleft=0, yright = 1.0)

# define the model

model <- function(time, stocks, auxs)
{
  with(as.list(c(stocks, auxs)), {
    
    fDeprec <- sCapital * aDeprec_rate
    aDesired_invest <- aDesired_growth * sCapital
    
    aExtract_effic <- func.efficiency(sResource)
    fExtract <- sCapital * aExtract_effic
    
    aTotal_rev <- aRev_per_extract * fExtract
    aCapital_costs <- sCapital * 0.10
    aProfit <- aTotal_rev - aCapital_costs
    
    aCapital_funds <- aProfit * aFrac_reinvest
    aMax_invest <- aCapital_funds / aCost_per_invest
    
    fInvest <- min(aDesired_invest, aMax_invest)
    
    # differential equations
    dC_dt <- fInvest - fDeprec
    dR_dt <- -fExtract
    
    return(
      list(
        c(dC_dt, dR_dt),
        Desired_investment = aDesired_invest,
        Maximum_investment = aMax_invest,
        Investment = fInvest, 
        Depreciation = fDeprec,
        Extraction = fExtract
      )
    )
  })
}

fit <- data.frame(
  ode(
    y = stocks, times = simtime, func = model, parms = auxs, method = "euler"
  )
)

ggplot(fit, aes(time, sCapital)) + 
  geom_line(col = "blue") 

ggplot(fit, aes(time, sResource)) + 
  geom_line(col = "blue") 

ggplot(fit, aes(time, Investment - Depreciation)) + 
  geom_line(col="blue") + 
  geom_line(aes(time, Depreciation), col = "darkgreen", linetype="dashed") + 
  geom_line(aes(time, Investment),col = "black", linetype="dashed") 

#--------------------------------------------------------------------------
#### Model on page 74
#--------------------------------------------------------------------------

start <- 0; finish <- 20; step <- 0.125
simtime <- seq(start, finish, step)
stocks <- c(sRework = 600)
auxs <- c(aAvg_fix_time = 6)

model <- function(time, stocks, auxs)
{
  with(as.list(c(stocks, auxs)),
       {
         fErrors_fixed <- sRework / aAvg_fix_time
         fErrors_found <- if(time == 0) 100 else 150
         if(fErrors_fixed - fErrors_found < 0 ){
           dR_dt <- 600
         }
         else { 
         dR_dt <- fErrors_fixed - fErrors_found
         }
         
    return(list(c(dR_dt),
                Errors_fixed = fErrors_fixed,
                Errors_found = fErrors_found
    ))
  })
}

fit <- data.frame(
  ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler")
)

head(fit)

fit %>% 
  ggplot(aes(x = time, y = sRework)) + 
  geom_line() 


#--------------------------------------------------------------------------
#### Model on page 77
#--------------------------------------------------------------------------

start <- 0; finish <- 50; step = 0.25
simtime <- seq(start, finish, step)
stocks <- c(sEmployees = 100, 
            sExp_quit_rate = 10)
auxs <- c(aQuit_frac = 0.10,
          aED = 2, 
          aAT = 4
          )

model <- function(time, stocks, auxs)
{
  with(as.list(c(stocks,auxs)),{
    
    fQuit_rate <- sEmployees * aQuit_frac
    aDiscrep <- fQuit_rate - sExp_quit_rate
    fCEQR <- aDiscrep / aED
    
    # step function
    aTarget_employ <- if(time <= 6) 100 else 150
    
    aAdjust_employ <- (aTarget_employ - sEmployees)/aAT
    fHire_rate <- max(0, sExp_quit_rate + aAdjust_employ)
    
    dE <- fHire_rate - fQuit_rate
    dEQR <- fCEQR
    
    return(list(c(dE, dEQR),
                Quit_rate = fQuit_rate,
                Discrepancy = aDiscrep, 
                CEQR = fCEQR,
                Adjustment = aAdjust_employ,
                Target = aTarget_employ,
                Hire_rate = fHire_rate))
    
  })
}

fit <- data.frame(
  ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler")
)


fit %>%
  ggplot(aes(x=time, y=Target)) + 
  geom_line() + 
  geom_line(aes(x=time, y=sEmployees), col = "blue")

fit %>%
  ggplot(aes(x=time, y=Adjustment)) + 
  geom_line() + 
  geom_line(aes(x=time, y=Quit_rate), col = "blue")

#--------------------------------------------------------------------------
#### Health care model
#--------------------------------------------------------------------------

start <- 2014; finish <- 2050; step <- 0.25
simtime <- seq(start, finish, step)
stocks <- c(
  # demographic model
  sP_0.14 = 1e6,
  sP_15.39 = 1.5e6,
  sP_40.64 = 2e6,
  sP_65 = 5e5,
  
  # delivery sector
  sPBT = 24e6,
  
  # supply sector
  sGP = 4000,
  sExp_retire = 100
)

auxs <- c(
  # demographic model
  aBirth_frac = 20/1000,
  aDeath_frac = 7/1000,
  aD1 = 15, aD2 = 25,
  aGPV_0.14 = 3, aGPV_15.39 = 4, aGPV_40.64 = 5, aGPV_65 = 10,
  
  # delivery sector
  aTarget_completion = 1,
  aStd_wky = 250, 
  aStd_GP_prod = 24,
  
  #supply sector
  aAvg_career_dur = 40,
  aDC = 3,
  aDesired_GP_per_thou = 0.8/1000,
  aAdjustment_time = 5,
  
  wky_flag = 1,
  prod_flag = 1
)

func.Sys_press_year <- approxfun(x = seq(0,2.5,0.25), 
                                 y = c(0.75,0.79,0.84,0.90,1.0,1.09,
                                       1.17,1.23,1.25,1.25,1.25))

func.Sys_press_prod <- approxfun(x = seq(0,2,0.2), 
                                 y = c(0.62,0.65,0.84,0.79,0.89,1.0,
                                       1.14,1.24,1.32,1.37,1.4))


health_care_model <- function(time, stocks, auxs)
{
  with(as.list( c(stocks, auxs)),{
    
    # demographic model
    fC1.C2 <- sP_0.14/aD1
    fC2.C3 <- sP_15.39/aD2
    fC3.C4 <- sP_40.64/aD2
    aTotal_pop <- sP_0.14 + sP_15.39 + sP_40.64 + sP_65
    fBirths <- aTotal_pop * aBirth_frac
    fDeaths <- aTotal_pop * aDeath_frac
    
    aTGPV_0.14 <- aGPV_0.14 * sP_0.14
    aTGPV_15.39<- aGPV_15.39 * sP_15.39
    aTGPV_40.64 <- aGPV_40.64 * sP_40.64
    aTGPV_65 <- aGPV_65 * sP_65
    aTotal_GPD <- aTGPV_0.14 + aTGPV_15.39 + aTGPV_40.64 + aTGPV_65
    
    dP_0.14 <- fBirths - fC1.C2
    dP_15.39 <- fC1.C2 - fC2.C3
    dP_40.64 <- fC2.C3 - fC3.C4
    dP_65 <- fC3.C4 - fDeaths
    
    # delivery sector
    fPatient_visits <- aTotal_GPD
    aDesired_visits <- sPBT / aTarget_completion
    aStd_annual <- sGP * aStd_wky * aStd_GP_prod
    aSys_press <- aDesired_visits / aStd_annual
    aEffect_sp_wky <- func.Sys_press_year(aSys_press)
    aWky <- ifelse(wky_flag == 1, 
                   aEffect_sp_wky * aStd_wky,
                   aStd_wky)
    aEffect_sp_prod <- func.Sys_press_prod(aSys_press)
    aProd <- ifelse(prod_flag == 1, 
                    aEffect_sp_prod * aStd_GP_prod,
                    aStd_GP_prod)
    aPoten_visits <- sGP * aProd * aWky
    fCompl_visits <- min(aDesired_visits, aPoten_visits)
    
    d_PBT = fPatient_visits - fCompl_visits
    
    # supply sector
    
    fRetire <- sGP / aAvg_career_dur
    aDiscrepancy <- fRetire - sExp_retire
    fCERR <- aDiscrepancy/aDC
    aDesired_GPs <- aTotal_pop * aDesired_GP_per_thou
    aAdjustment_GPs <- (aDesired_GPs - sGP) / aAdjustment_time
    fRecruit <- max(0, sExp_retire + aAdjustment_GPs)
    d_GPs <- fRecruit - fRetire
    d_Exp_rr <- fCERR
    
    return(list(c(
      
      # demographic model
      dP_0.14, dP_15.39, dP_40.64, dP_65,
      
      #delivery model
      d_PBT,
      
      # supply model
      d_GPs, d_Exp_rr
    ),
    
    # demographic model
    C1.to.C2 = fC1.C2, C2.to.C3 = fC2.C3, C3.to.C4 = fC3.C4,
    Births = fBirths, Deaths = fDeaths,
    TGPV_0.14 = aTGPV_0.14, TGPV_15.39 = aTGPV_15.39, 
    TGPV_40.64 = aTGPV_40.64, TGPV_65 = aTGPV_65,
    Total_pop = aTotal_pop, 
    Total_GPD = aTotal_GPD,
    
    # delivery model
    Patient_visits = fPatient_visits,
    Desired_visits = aDesired_visits,
    Standard_annual_complete = aStd_annual,
    System_pressure = aSys_press,
    Effect_pressure_wky = aEffect_sp_wky,
    Work_year = aWky,
    Effect_pressure_prod = aEffect_sp_prod,
    Prod = aProd,
    Potential_completed_visits = aPoten_visits,
    Completed_visits = fCompl_visits,
    
    # supply model
    Retirement_rate = fRetire,
    Recruitment_rate = fRecruit,
    Discrepancy = aDiscrepancy,
    CERR = fCERR,
    Desired_GPs = aDesired_GPs,
    Adjustment_GPs = aAdjustment_GPs
    ))
  })
}


fit <- data.frame(
  ode(y = stocks, times = simtime, func = health_care_model, parms = auxs, method = "euler")
)

fit_BAU <- data.frame(
  ode(y = stocks, times = simtime, func = health_care_model, 
      parms = c(auxs[-16], "wky_flag" = 0), method = "euler")
)

fit_Flex <- data.frame(
  ode(y = stocks, times = simtime, func = health_care_model, 
      parms = c(auxs[-16], "wky_flag" = 1), method = "euler")
)

head(fit)

fit %>%
  ggplot() + 
  geom_line(aes(time, Total_pop)) 
  # geom_line(aes(time, sP_0.14), col = "blue", linetype="dashed") + 
  # geom_line(aes(time, sP_15.39), col = "blue", linetype="dashed") + 
  # geom_line(aes(time, sP_40.64), col = "blue", linetype="dashed") + 
  # geom_line(aes(time, sP_65), col = "blue", linetype="dashed") 

fit %>%
  ggplot() + 
  geom_line(aes(time, Desired_GPs), linetype="dashed") + 
  geom_line(aes(time, sGP))


ggplot() + 
  geom_line(data = fit_BAU, aes(time, Desired_visits)) + 
  geom_line(data = fit_Flex, aes(time, Desired_visits), linetype="dashed") + 
  geom_line(data = fit_BAU, aes(time, Potential_completed_visits), col = "blue") + 
  geom_line(data = fit_Flex, aes(time, Potential_completed_visits), col = "blue", linetype="dashed")

ggplot() + 
  geom_line(data = fit_Flex, aes(time, Effect_pressure_wky) ) + 
  geom_line(data = fit_Flex, aes(time, Effect_pressure_prod), linetype="dashed" )

#--------------------------------------------------------------------------
#### SIR diffusion model (page 98)
#--------------------------------------------------------------------------

start <- 0; finish <- 30; step <- 0.125
simtime <- seq(0, 30, step)
stocks <- c(sSusceptible = 99999, 
            sInfected = 1, 
            sRecovered = 0)
auxs <- c(aCE = 2, 
          aDelay = 2)

SIR <- function(time, stocks, auxs)
{
  with( as.list(c(stocks, auxs)),{
    aTotal_pop <- sSusceptible + sInfected + sRecovered
    aBeta <- aCE / aTotal_pop
    aLambda <- aBeta * sInfected
    fIR <- sSusceptible * aLambda
    fRR <- sInfected / aDelay
    
    dS <- -fIR
    dI <- fIR - fRR
    dR <- fRR
    return(list(c(dS, dI, dR), 
                Total_popv = aTotal_pop, 
                Beta = aBeta, 
                Lambda = aLambda, 
                IR = fIR, 
                RR = fRR))
  })
}


fit_SIR <- data.frame(
  ode(y = stocks, times = simtime, func = SIR, parms = auxs, method = "euler")
)

head(fit_SIR)

fit_SIR %>%
  ggplot() + 
  geom_line(aes(time, IR), col = "blue") + 
  geom_line(aes(time, RR), col = "red")


fit_SIR %>%
  ggplot() + 
  geom_line(aes(time, sSusceptible), col = "blue") + 
  geom_line(aes(time, sInfected), col = "red") + 
  geom_line(aes(time, sRecovered), col = "green")

fit_SIR %>%
  ggplot() + 
  geom_line(aes(time, Lambda), col = "blue")


#--------------------------------------------------------------------------
#### SIR diffusion model for policy analysis (page 103)
#--------------------------------------------------------------------------

start <- 0; finish <- 20; step <- 0.125
simtime <- seq(start, finish, step)
stocks <- c(sSusceptible = 99999, 
            sInfected = 1,
            sQuarantine = 0, 
            sRecovered = 0)
auxs <- c(aDelay = 2, aCE = 2, aVF = 0.05, aQF = 0.05)

SIR_policy <- function(time, stocks, auxs)
{
  with(as.list( c(stocks, auxs)), {
    aTotal_pop <- sSusceptible + sInfected + sRecovered + sQuarantine
    aBeta <- aCE / aTotal_pop
    aLambda <- aBeta * sInfected
    fIR <- sSusceptible * aLambda
    fRR <- sInfected / aDelay
    fQRR <- sQuarantine / aDelay ## this is wrongly presented in the book!
    fVR <- sSusceptible * aVF
    fQR <- sInfected * aQF
    
    # differential equations
    dS <- -fIR - fVR
    dI <- fIR - fRR - fQR
    dQ <- fQR - fQRR
    dR <- fRR + fQRR + fVR
    
    return(list(c(dS, dI, dQ, dR), 
                Total_popv = aTotal_pop, 
                Beta = aBeta, 
                Lambda = aLambda, 
                IR = fIR, 
                RR = fRR, 
                VR = fVR, 
                QR = fQR, 
                QRR = fQRR))
  })
}

fit_SIR_policy <- data.frame(
  ode(y = stocks, times = simtime, func = SIR_policy, parms = auxs, method = "euler")
)

head(fit_SIR_policy,20)

fit_SIR_policy %>%
  ggplot() + 
  geom_line(aes(time, sInfected))

fit_SIR_policy %>%
  ggplot() + 
  geom_line(aes(time, sQuarantine)) + 
  geom_line(aes(time, sInfected), linetype = "dashed") + 
  geom_line(aes(time, sSusceptible), col = "blue" , linetype = "dashed")
 

#--------------------------------------------------------------------------
#### SIR disaggregate model (page 108)
#--------------------------------------------------------------------------

start <- 0; finish <- 20; step <- 0.125
simtime <- seq(start, finish, step)
stocks <- c(sSusceptibleY = 24999, sSusceptibleA = 50000, sSusceptibleE = 25000, 
               sInfectedY = 1    ,    sInfectedA = 0    ,    sInfectedE = 0    , 
              sRecoveredY = 0    ,   sRecoveredA = 0    ,   sRecoveredE = 0    
            )

N_cohorts <- 3; N_states <- 3

CE <- matrix(c(3.0, 2.0, 1.0, 
               2.0, 2.0, 1.0, 
               1.0, 1.0, 2.0),
             nrow = N_cohorts, ncol = N_states, byrow = 3)


N_people <- c(Young = 25000, Adult = 50000, Elderly = 25000)

beta <- CE / N_people

delays <- c(DY = 2, DA = 2, DE = 2)

auxs <- NULL

SIR_dis <- function(time, stocks, auxs)
{
  with( as.list(c( stocks, auxs)),{
    
    states <- matrix(stocks, nrow = N_cohorts, ncol = N_states)
    Susceptible <- states[,1]
    Infected <- states[,2]
    Recovered <- states[,3]
    
    Lambda <- beta %*% Infected
    IR <- Lambda * Susceptible
    RR <- Infected / delays
    
    dS <- -IR
    dI <- IR - RR
    dR <- RR
    
    return(list(c(dS, dI, dR), 
                IR = IR, 
                RR = RR))
    
  })
}


fit_SIR_dis <- data.frame(
  ode(y = stocks, times = simtime, func = SIR_dis, parms = auxs, method = "euler")
)

fit_SIR_dis %>%
  ggplot() + 
  geom_line(aes(time, sInfectedY), col = "blue") + 
  geom_line(aes(time, sInfectedA), col = "red") + 
  geom_line(aes(time, sInfectedE), col = "green") + 
  geom_line(aes(time, sInfectedY+sInfectedA+sInfectedE))

