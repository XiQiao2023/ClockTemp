library(MCMCglmm)
library(openxlsx)
library(dplyr)
set.seed(2022)


## Data Loading
ClockMaster = read.csv("AccAgeMaster.csv")

## Prior settings
prior = list(
  G = list(G1 = list(V = diag(rep(1,10)), nu = 10)),
  R = list(R1 = list(V = diag(rep(1,10)), nu = 10))
)

## Binary Temperatures
## Cut off: 36
ClockMaster$FirstMonthTemp_36Bi = ifelse(ClockMaster$X1.month.pre.gestation.Mean.LST.Daytime.Temperature >=36,1,0)
ClockMaster$FirstTrimTemp_36Bi = ifelse(ClockMaster$X1st.Trimester.Gestation.Mean.LST.Daytime.Temperature>=36,1,0)
ClockMaster$SecondTrimTemp_36Bi = ifelse(ClockMaster$X2nd.Trimester.Gestation.Mean.LST.Daytime.Temperature>=36,1,0)
ClockMaster$ThirdTrimTemp_36Bi = ifelse(ClockMaster$X3rd.Trimester.Mean.LST.Daytime.Temperature>=36,1,0)
ClockMaster$BirthToInterviewTempZ_36Bi = ifelse(ClockMaster$Birth.Month.to.Interview.Month.Mean.LST.Daytime.Temperature>=36,1,0)

## Cut off: 36.5
ClockMaster$FirstMonthTemp_365Bi = ifelse(ClockMaster$X1.month.pre.gestation.Mean.LST.Daytime.Temperature >=36.5,1,0)
ClockMaster$FirstTrimTemp_365Bi = ifelse(ClockMaster$X1st.Trimester.Gestation.Mean.LST.Daytime.Temperature>=36.5,1,0)
ClockMaster$SecondTrimTemp_365Bi = ifelse(ClockMaster$X2nd.Trimester.Gestation.Mean.LST.Daytime.Temperature>=36.5,1,0)
ClockMaster$ThirdTrimTemp_365Bi = ifelse(ClockMaster$X3rd.Trimester.Mean.LST.Daytime.Temperature>=36.5,1,0)
ClockMaster$BirthToInterviewTempZ_365Bi = ifelse(ClockMaster$Birth.Month.to.Interview.Month.Mean.LST.Daytime.Temperature>=36.5,1,0)

## Cut off: 37
ClockMaster$FirstMonthTemp_37Bi = ifelse(ClockMaster$X1.month.pre.gestation.Mean.LST.Daytime.Temperature >=37,1,0)
ClockMaster$FirstTrimTemp_37Bi = ifelse(ClockMaster$X1st.Trimester.Gestation.Mean.LST.Daytime.Temperature>=37,1,0)
ClockMaster$SecondTrimTemp_37Bi = ifelse(ClockMaster$X2nd.Trimester.Gestation.Mean.LST.Daytime.Temperature>=37,1,0)
ClockMaster$ThirdTrimTemp_37Bi = ifelse(ClockMaster$X3rd.Trimester.Mean.LST.Daytime.Temperature>=37,1,0)
ClockMaster$BirthToInterviewTempZ_37Bi = ifelse(ClockMaster$Birth.Month.to.Interview.Month.Mean.LST.Daytime.Temperature>=37,1,0)

fit = MCMCglmm(cbind(AccAgePedBE,AccAgeWu,AccAgeHorvath, AccAgeskinHorvath,
                     AccAgeHannum,AccAgePhenoAge,AccAgeGrimAge2,AccAgeDNAmTL,
                     DunedinPACE,DunedinPoAm38) ~ 
                 trait + trait : (drought + Female + Epi + Fib + gravida+ Birth_Season1) - 1, 
              random = ~us(trait):mother, rcov = ~ us(trait):units,
              family = rep("gaussian",10), data = ClockMaster,prior = prior,
              verbose = FALSE,nitt = 20000,burnin = 5000,thin = 1)

summary(fit) 




