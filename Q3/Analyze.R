## 1. データを読み込む
dataset <- read.csv("train_data_2015.csv")

#対数をとらない場合の賃金の分布
hist(dataset$wage, xlab= "hourly wage", main="Wage distribution of US", breaks= 35)
#対数をとった場合の賃金の分布
hist(dataset$lwage, xlab= "hourly wage", main="Wage distribution of US", breaks= 35)
# ========== Comment beginning ==========
# 対数をとったグラフの形状ほうが正規分布に近づいている
# ========== Comment end ==========

#momentsというパッケージをインストール
# install.packages("moments")
#momentsパッケージを読み込む
library("moments")

#対数をとらない場合の賃金の基本統計量を計算する
mean(dataset$wage)
median(dataset$wage)
skewness(dataset$wage)
#対数をとった場合の賃金の基本統計量を計算する
mean(dataset$lwage)
median(dataset$lwage)
skewness(dataset$lwage)
# ========== Comment beginning ==========
# 対数をとらないよりも対数をとった場合のほうが
# 1.平均値と中央値の差が小さい
# 2.尖度の値が0に近い
# ========== Comment end ==========



## 3. モデルの訓練を行う
#モデル１：simpleモデル
simple <- lwage~ (MALE + MW + SO + WE)
reg_simple <- lm(simple, data=dataset)
reg_simple 
cat( "simpleモデルにおける入力変数の数:",length(reg_simple$coef), '\n')

#モデル２：benchmarkモデル
benchmark <- lwage~ (MALE + MW + SO + WE + hsg+ scl + clg + adv + PEXP)
reg_benchmark <- lm(benchmark, data=dataset)
reg_benchmark 
cat( "benchmarkモデルにおける入力変数の数:",length(reg_benchmark$coef), '\n')

#モデル３：complexモデル
complex <- lwage~ (MALE + MW + SO + WE + hsg+ scl + clg + adv + PEXP + PEXP2 + PEXP3 + PEXP4 
                   + (PEXP + PEXP2 + PEXP3 + PEXP4)*(MALE + MW + SO + WE + hsg+ scl + clg + adv))
reg_complex <- lm(complex, data=dataset)
reg_complex 
cat( "complexモデルにおける入力変数の数:",length(reg_complex$coef), '\n')

#super complexモデルの訓練
scomplex <- lwage~ (MALE + MW + SO + WE + hsg+ scl + clg + adv + PEXP + PEXP2 + PEXP3 + PEXP4 + PEXP5 + PEXP6 + PEXP7 + PEXP8 + PEXP9
                    + (PEXP + PEXP2 + PEXP3 + PEXP4 + PEXP5 + PEXP6 + PEXP7 + PEXP8 + PEXP9)
                    *(MALE + MW + SO + WE + hsg+ scl + clg + adv))
reg_scomplex <- lm(scomplex, data=dataset)
reg_scomplex 

## 4. モデルの評価を行う（テストデータがある場合）
#推定結果のsummaryを用意しておく
sumsimple <- summary(reg_simple)
sumbenchmark <- summary(reg_benchmark)
sumcomplex <- summary(reg_complex)
sumscomplex <- summary(reg_scomplex)

#モデル1のtraining-MSE
MSEi_simple <- mean(sumsimple$res^2)
cat("simpleモデルのtraining MSE: ", MSEi_simple, "\n")

#モデル2のtraining-MSE
MSEi_benchmark <- mean(sumbenchmark$res^2)
cat("benchmarkモデルのtraining MSE: ", MSEi_benchmark, "\n")

#モデル3のtraining-MSE
MSEi_complex <- mean(sumcomplex$res^2)
cat("complexモデルのtraining MSE: ", MSEi_complex, "\n")

#super complexモデルのtraining-MSE
MSEi_scomplex <- mean(sumscomplex$res^2)
cat("super complexモデルのtraining MSE: ", MSEi_scomplex, "\n")

# ========== Comment beginning ==========
# 2015年のデータを用いた各モデルのtraining MSEの大きさ以下の順番である
# super complex(0.1603555) < complex(0.1610503) < benchmark(0.1678191) < simple(0.2157036)
# やはりsuper complex modelのMSEが最も小さい値となったが、これは訓練したモデルが訓練データを非常にうまく再現できていることを意味する。
# しかし、我々が行いたいのは、未知データに対する予測であるので、次にテストデータで評価を行っていく。
# ========== Comment end ==========

## 4.2. テスト誤差（test error）での評価
#テストデータを読み込む
testdataset <- read.csv("test_data_2017.csv")

#MSEを計算する際の予測対象（＝テストデータの賃金）を用意する
y.test <- log(testdataset$wage)

#モデル1のout-of-sample-MSE
predict_simple <- predict(reg_simple, newdata=testdataset)
MSE.test_simple <- sum((y.test-predict_simple)^2)/length(y.test)
R2.test_simple<- 1- MSE.test_simple/var(y.test)

cat("Test MSE for the simple model: ", MSE.test_simple, " ")
cat("Test R2 for the simple model: ", R2.test_simple)

#モデル2のout-of-sample-MSE
predict_benchmark <- predict(reg_benchmark, newdata=testdataset)
MSE.test_benchmark <- sum((y.test-predict_benchmark)^2)/length(y.test)
R2.test_benchmark<- 1- MSE.test_benchmark/var(y.test)

cat("Test MSE for the benchmark model: ", MSE.test_benchmark, " ")
cat("Test R2 for the benchmark model: ", R2.test_benchmark)

#モデル3のout-of-sample-MSE
predict_complex <- predict(reg_complex, newdata=testdataset)
MSE.test_complex <- sum((y.test-predict_complex)^2)/length(y.test)
R2.test_complex<- 1- MSE.test_complex/var(y.test)

cat("Test MSE for the complex model: ", MSE.test_complex, " ")
cat("Test R2 for the complex model: ", R2.test_complex)

#super complexモデルのin-sample-MSE
predict_scomplex <- predict(reg_scomplex, newdata=testdataset)
MSE.test_scomplex <- sum((y.test-predict_scomplex)^2)/length(y.test)
R2.test_scomplex<- 1- MSE.test_scomplex/var(y.test)

cat("Test MSE for the super complex model: ", MSE.test_scomplex, " ")
cat("Test R2 for the super complex model: ", R2.test_scomplex)

# ========== Comment beginning ==========
# 2017年のデータを用いた各モデルのtest MSEの大きさ以下の順番である
# complex(0.1582725) < super complex(0.1570772) < benchmark(0.1630934) < simple(0.2105963)
# 各モデルのR2の大きさは以下のとおりである
# simple(-0.01497636) < benchmark(0.2139656) < super complex(0.2371999) < complex(0.242961)  
# training data による train MES は super complex model が最も小さな値を示した。
# しかし、test MSE はcomplex model が最も小さな値であった。
# そのためMSEを最小化するという意味でもっともよい未知データの予測モデルはcomplex model と考えられる。
# ここで注意をしなければいけないのは、2015年のデータで訓練したモデルを2017年のデータでテストしているという点である。
# 2015年の状況と2017年の状況が同質であるとの仮定があれば問題ないが、そうでない場合はtest MSEによる評価に対して慎重になるべきである。
# そのため次に2015年のデータを使って訓練したモデルを別の2015年のデータによってテストしていく。
# ========== Comment end ==========


# ## 5. モデルの評価を行う（テストデータがない場合）
# #caretパッケージのインストール
# install.packages("caret")
# #caretパッケージを読み込む
library(caret)

# ## 5.1. ホールドアウト法
# #データを訓練データ（80%）と検証データ（20%）に分割する
# training.index <- createDataPartition(dataset$lwage, p = .8, list = FALSE, times = 1) 
# training.data  <- dataset[training.index, ]
# validation.data <- dataset[-training.index, ]
# 
# #きちんと分割されているか念のため確かめよう
# training.data
# 
# #モデル1のホールドアウト検証
# reg_simple.ho <- lm(simple, data=training.data)
# predictions.ho.simple <- predict(reg_simple.ho, newdata=validation.data)
# RMSE.ho.simple <- RMSE(predictions.ho.simple, validation.data$lwage)
# cat("Hold-out MSE for the simple model: ", RMSE.ho.simple, " ")
# 
# #モデル2のホールドアウト検証
# reg_benchmark.ho <- lm(benchmark, data=training.data)
# predictions.ho.benchmark <- predict(reg_benchmark.ho, newdata=validation.data)
# RMSE.ho.benchmark <- RMSE(predictions.ho.benchmark, validation.data$lwage)
# cat("Hold-out MSE for the simple model: ", RMSE.ho.benchmark, " ")
# 
# #モデル3のホールドアウト検証
# reg_complex.ho <- lm(complex, data=training.data)
# predictions.ho.complex <- predict(reg_complex.ho, newdata=validation.data)
# RMSE.ho.complex <- RMSE(predictions.ho.complex, validation.data$lwage)
# cat("Hold-out MSE for the simple model: ", RMSE.ho.complex, " ")
# 
# #Super complexモデルのホールドアウト検証
# reg_scomplex.ho <- lm(scomplex, data=training.data)
# predictions.ho.scomplex <- predict(reg_scomplex.ho, newdata=validation.data)
# RMSE.ho.scomplex <- RMSE(predictions.ho.scomplex, validation.data$lwage)
# cat("Hold-out MSE for the simple model: ", RMSE.ho.scomplex, " ")

## 5.2. LOOCV（リーブワンアウト）法
# #trainControl関数で交差検証法をLOOCVに指定
# loocv <- trainControl(method = "LOOCV")
# #モデル1のLOOCV検証
# simple.loocv <- train(simple, data = dataset, method = "lm",trControl = loocv)
# print(simple.loocv)
# #モデル2のLOOCV検証
# benchmark.loocv <- train(benchmark, data = dataset, method = "lm",trControl = loocv)
# print(benchmark.loocv)
# #モデル3のLOOCV検証
# complex.loocv <- train(complex, data = dataset, method = "lm",trControl = loocv)
# print(complex.loocv)
# #Super complexモデルのLOOCV検証
# scomplex.loocv <- train(scomplex, data = dataset, method = "lm",trControl = loocv)
# print(scomplex.loocv)

## 5.3. k分割交差検証法
#trainControl関数で交差検証法を5分割交差検証に指定
fivefold <- trainControl(method = "cv", number = 5)
#モデル1の5分割交差検証
simple.fivefold <- train(simple, data = dataset, method = "lm",trControl = fivefold)
print(simple.fivefold)
#モデル2の5分割交差検証
benchmark.fivefold <- train(benchmark, data = dataset, method = "lm",trControl = fivefold)
print(benchmark.fivefold)
#モデル3の5分割交差検証
complex.fivefold <- train(complex, data = dataset, method = "lm",trControl = fivefold)
print(complex.fivefold)
#Super complexモデルの5分割交差検証
scomplex.fivefold <- train(scomplex, data = dataset, method = "lm",trControl = fivefold)
print(scomplex.fivefold)

# ========== Comment beginning ==========
# simple RMSE:0.464467 , Rsquared:0.01914325 , MAE:0.3430981
# benchmark RMSE:0.4099286 , Rsquared:0.2361784 , MAE:0.2869021
# complex RMSE:0.4024196 , Rsquared:0.2644563 , MAE:0.2786168
# super complex RMSE:0.4077243 , Rsquared:0.2483688 , MAE:0.2793598
# MESはcomplex modelが最も小さい。またR2もcomplex modelが最も大きい
# ========== Comment end ==========
