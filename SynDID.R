did <- read.csv("C:/Users/Nick Zhang/Desktop/NUS 2021-2022/Econ/GP2/Data/data.csv")

setup = synthdid::panel.matrices(did,"Bank","Date","TAR","Treated")

tem.X = did[,c("Bank","Date","Profitability","Leverage_ratio","Total_assets","Non_performing_loan_ratio",
               "Cost_income_ratio", "Deposit_ratio","Real_estate_loan_ratio","Liquidity_ratio", "CPP_recipient")]
temp.X = tem.X[order("Bank","Date")]

X = array(matrix(unlist(tem.X[,c("Profitability","Leverage_ratio","Total_assets","Non_performing_loan_ratio",
                                 "Cost_income_ratio", "Deposit_ratio","Real_estate_loan_ratio","Liquidity_ratio",
                                 "CPP_recipient")]), nrow = nrow(setup$Y), byrow =TRUE),dim=c(dim(setup$Y),1))

tau.hat.X = synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0, X)
print(summary(tau.hat.X))
se.X = sqrt(vcov(tau.hat.X, method='placebo'))
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat.X-1.96*se.X, tau.hat.X+1.96+se.X)
plot(tau.hat.X, se.method='placebo')
