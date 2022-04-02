# (1)
rctdata <- read.csv("Q5/rctdata.csv") # data import
ATE_rct <- mean(rctdata$outcome[rctdata$treatment == 1]) - mean(rctdata$outcome[rctdata$treatment == 0])

## plot
plot(rctdata$treatment, rctdata$outcom)

## regression
reg_rct <- lm(outcome ~ treatment, data = rctdata)
summary(reg_rct)

# (2)
obsdata <- read.csv("Q5/obsdata.csv") # data import
# 線形回帰モデルを用いないやり方(=E[Yi|Ti = 1] − E[Yi|Ti = 0])
ATE_obs <- mean(obsdata$outcome[obsdata$treatment == 1]) - mean(obsdata$outcome[obsdata$treatment == 0])
# 線形回帰モデルを用いるやり方
reg_obs <- lm(outcome ~ treatment, data = obsdata)

# (3)
# 親の年収のみをコントロールした場合の条件付き期待値の差E[Yi|Ti = 1, X1i] − E[Yi|Ti = 0, X1i]
control_p <- outcome ~ (treatment + parential_income)
reg_control_p <- lm(control_p, data = obsdata)
summary(reg_control_p)

# 親の年収と SPI スコアの両方をコントロールした場合の条件付き期待値の差E[Yi|Ti = 1, X1i, X2i] − E[Yi|Ti = 0, X1i, X2i]
control_ps <- outcome ~ (treatment + parential_income + score)
reg_control_ps <- lm(control_ps, data = obsdata)
summary(reg_control_ps)

# 変数同士の相関確認
cor(obsdata$treatment,obsdata$score)
cor(obsdata$treatment,obsdata$parential_income)
cor(obsdata$score,obsdata$parential_income)

# 多条件が一致しているか確認
mean(obsdata$parential_income[obsdata$treatment==1])
mean(obsdata$parential_income[obsdata$treatment==0])
mean(obsdata$score[obsdata$treatment==1])
mean(obsdata$score[obsdata$treatment==0])

