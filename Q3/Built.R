## ==========
## 2. Rでrawデータセットを読み込む

# ipumsrパッケージをインストールして使える状態にする
# install.packages('ipumsr')
library('ipumsr')

## 2.1 IPUMSのデータをRのデータフレームに変換する
#データを読み込む
ddi <- read_ipums_ddi("Q3/cps_00001.xml")
data <- read_ipums_micro(ddi)

## ==========
## 3 訓練データ・テストデータを作成する
## 3.1 対象外のデータを除外する
# 今回は、「白人」かつ「未婚」で「フルタイムで働いている」個体のみを残すことにする。

Built_data <- function(year){
  subdata <- subset(data, data$YEAR == year)
  #賃金データが欠損している個体を落とす
  subdata <- subset(subdata, subdata$HOURWAGE != 999.99)
  #白人かつ未婚でフルタイムで働いている個体だけ残す
  subdata <- subset(subdata, subdata$RACE == 100 & MARST==6 & EMPSTAT == 10)
  
  # 3.2 学歴ダミーをつくる
  #中卒未満（educ < 12）
  subdata$shs <- ifelse(subdata$EDUC == 002| subdata$EDUC == 010 |subdata$EDUC == 020| subdata$EDUC == 030| subdata$EDUC == 040| subdata$EDUC == 050| subdata$EDUC == 060, 1,0)
  #中卒～高卒（12 <= educ < 16）
  subdata$hsg <- ifelse(subdata$EDUC == 071 |subdata$EDUC == 073, 1,0)
  #大学中退者
  subdata$scl <- ifelse(subdata$EDUC == 081, 1,0)
  #学部卒業
  subdata$clg <- ifelse(subdata$EDUC == 111, 1,0)
  #大学院以降
  subdata$adv <- ifelse(subdata$EDUC == 123|subdata$EDUC == 124|subdata$EDUC == 125, 1,0)
  #どれにも当てはまらない個体を落とす
  subdata <- subset(subdata, subdata$shs == 1| subdata$hsg == 1| subdata$scl == 1| subdata$clg == 1| subdata$adv == 1)
  
  # 3.3 潜在就業経験年数をつくる
  ##Jaeger (2003)およびHersch et al. (2020)に倣って最終学歴年数(highest grade completed=hgc)をつくる
  subdata$hgc <- numeric(nrow(subdata))
  
  subdata$hgc[subdata$EDUC == 010] <- 2.5
  subdata$hgc[subdata$EDUC == 020] <- 5.5
  subdata$hgc[subdata$EDUC == 030] <- 7.5
  subdata$hgc[subdata$EDUC == 040] <- 9.0
  subdata$hgc[subdata$EDUC == 050] <- 10.0
  subdata$hgc[subdata$EDUC == 060] <- 11.0
  subdata$hgc[subdata$EDUC == 071] <- 12.0
  
  
  #Highest Grade/Degree Completed is 
  # GED/HS Diploma AND
  #    ...Received GED or Equivalent AND Highest Grade Completed before GED was
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 01] <- 0  #<1st grade
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 02] <- 2.5  #1st, 2nd, 3rd, or 4th grade
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 03] <- 5.5  #5th or 6th grade
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 04] <- 7.5 #7th or 8th grade
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 05] <- 9 #9th grade
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 06] <- 10 #10th grade
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 07] <- 11 #11th grade 
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 02 & subdata$EDHGCGED == 08] <- 12 #12th grade, no diploma
  #    ...Received HS Diploma
  subdata$hgc[subdata$EDUC == 073 & subdata$EDDIPGED == 01] <- 12
  
  #Some College OR Occupational or Academic Associate's Degree AND
  #    ...years of college credit completed is
  subdata$hgc[(subdata$EDUC == 081 | subdata$EDUC == 091 | subdata$EDUC == 092) & subdata$EDCYC == 01] <- 12  #<1 year (includes 0)
  subdata$hgc[(subdata$EDUC == 081 | subdata$EDUC == 091 | subdata$EDUC == 092) & subdata$EDCYC == 02] <- 13  #First, or Freshman year
  subdata$hgc[(subdata$EDUC == 081 | subdata$EDUC == 091 | subdata$EDUC == 092) & subdata$EDCYC == 03] <- 14  #Second, or Sophomore year
  subdata$hgc[(subdata$EDUC == 081 | subdata$EDUC == 091 | subdata$EDUC == 092) & subdata$EDCYC == 04] <- 15  #Third, or Junior year 
  subdata$hgc[(subdata$EDUC == 081 | subdata$EDUC == 091 | subdata$EDUC == 092) & subdata$EDCYC == 05] <- 16  #Four or more years 
  
  #Completed a Batchelor’s degree AND 
  subdata$hgc[subdata$EDUC == 111] <- 16  #None
  subdata$hgc[subdata$EDUC == 123] <- 17  #Completed Master's degree
  subdata$hgc[subdata$EDUC == 124 | subdata$EDUC == 125] <- 18  #Completed Professional school degree or Doctorate degree
  
  #取りこぼしている個体がいないか確認
  sum(subdata$hgc == 0 & subdata$EDUC != 02 & subdata$EDHGCGED != 01)
  
  ##潜在就業経験年数（＝max(min(subdata$AGE-subdata$hgc-7, subdata$AGE-17),0)）をつくる
  flag1 <- subdata$AGE-subdata$hgc-7 < subdata$AGE-17
  subdata$PEXP <- (subdata$AGE-subdata$hgc-7)*flag1 + (subdata$AGE-17)*(1-flag1)
  subdata$PEXP[subdata$PEXP < 0] = 0 
  
  
  #潜在就業経験年数の累乗変数をつくる
  subdata$PEXP2 <- (subdata$PEXP)^2/100
  subdata$PEXP3 <- (subdata$PEXP)^3/1000
  subdata$PEXP4 <- (subdata$PEXP)^4/10000
  subdata$PEXP5 <- (subdata$PEXP)^5/100000
  subdata$PEXP6 <- (subdata$PEXP)^6/1000000
  subdata$PEXP7 <- (subdata$PEXP)^7/10000000
  subdata$PEXP8 <- (subdata$PEXP)^8/100000000
  subdata$PEXP9 <- (subdata$PEXP)^9/1000000000
  
  ## 3.4 性別ダミーをつくる
  ##男性ダミー（男なら1, 女なら0）をつくる
  subdata$MALE <- ifelse(subdata$SEX == 1, 1,0)
  
  ## 3.5 地域ダミーをつくる
  ##地域ダミーをつくる
  subdata$NE <- ifelse(subdata$REGION == 11|subdata$REGION == 21, 1,0)
  subdata$MW <- ifelse(subdata$REGION == 21|subdata$REGION == 22, 1,0)
  subdata$SO <- ifelse(subdata$REGION == 31|subdata$REGION == 32, 1,0)
  subdata$WE <- ifelse(subdata$REGION == 41|subdata$REGION == 42, 1,0)
  
  ## 3.6 最終処理
  ##出力変数の名前を変える
  subdata$wage <- subdata$HOURWAGE
  subdata$lwage <- log(subdata$wage)
  
  
  ##必要な変数だけ残す
  dataset <- subdata[which(colnames(subdata) %in% c("wage","lwage","MALE","NE","MW","SO","WE","shs","hsg","scl","clg","adv","PEXP","PEXP2","PEXP3","PEXP4","PEXP5","PEXP6","PEXP7","PEXP8","PEXP9"))]
 
  return(dataset) 
}

# 2015年の訓練データを作成する
data_2015 <- Built_data(2015)
# 2015年のテストデータを作成する
data_2017 <- Built_data(2017)

# データフレームとしてほぞんする
write.table(data_2015, "train_data_2015.csv", append = F,sep = ",", row.names = F, quote = F)
write.table(data_2017, "test_data_2017.csv", append = F,sep = ",", row.names = F, quote = F)
