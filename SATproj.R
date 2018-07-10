#This code is to investigate the relationship between SAR scores and acceptance rate
#acceptance rate
setwd("C:\Users\Smera\Documents\Dir, R\SATproject")
acceptance<-read.csv("Acceptance.csv",
                   stringsAsFactors = FALSE)
satscore<-read.csv("SAT.csv",
                   stringsAsFactors = FALSE)
#examine data frame
head(satscore)
head(acceptance)
#summary of data frame
summary(satscore)
summary(acceptance)
#change the data type of desired column
satscore$university<-as.integer(satscore$university)
satscore$sat_math_25<-as.numeric(satscore$sat_math_25)
satscore$sat_math_75<-as.numeric(satscore$sat_math_75)
satscore$sat_cr_25<-as.integer(satscore$sat_cr_25)
satscore$sat_cr_75<-as.integer(satscore$sat_cr_75)
acceptance$university<-as.numeric(acceptance$university)
acceptance$admissions_total<-as.numeric(acceptance$admissions_total)
acceptance$applicants_total<-as.numeric(acceptance$applicants_total)
#merge data frames
maindata<-cbind(satscore, acceptance)
head(maindata)
#remove repeats
maindata<-maindata[, -c(8:10)]
head(maindata)
#add total SAT score column and acceptance rate column
maindata$totalscore<-maindata$sat_math_75+ maindata$sat_cr_75
maindata$arate<-maindata$admissions_total/maindata$applicants_total
head(maindata)
#subset the main data frame according to years
df2014<-subset(maindata, maindata$year==2014)
df2015<-subset(maindata, maindata$year==2015)
df2016<-subset(maindata, maindata$year==2016)
#visualize SAT scores vs acceptance rate
par(mfrow = c(1,3))
plot (df2014$totalscore, df2014$arate, xlab="total SAT", ylab="acceptance",
      main="2014")
plot (df2015$totalscore, df2015$arate, xlab="total SAT", ylab="acceptance",
      main="2015")
plot (df2016$totalscore, df2016$arate, xlab="total SAT", ylab="acceptance",
      main="2016")
#observe associatin for above 1300
#analize
sub2014<-subset(df2014, df2014$totalscore >=1300)
sub2015<-subset(df2015, df2014$totalscore >=1300)
sub2016<-subset(df2016, df2014$totalscore >=1300)

#find correlation
cor2014<-cor(sub2014$totalscore, sub2014$arate)
cor2015<-cor(sub2015$totalscore, sub2015$arate)
cor2016<-cor(sub2016$totalscore, sub2016$arate)


par(mfrow = c(1,3))
plot (sub2014$totalscore, sub2014$arate, xlab="SAT above 1300", ylab="acceptance",
      main="2014 above 1300")
text(1550,0.85,round(cor2014, digits = 2))
abline(lm(sub2014$arate~sub2014$totalscore),col="blue")
plot (sub2015$totalscore, sub2015$arate, xlab="SAT above 1300", ylab="acceptance",
      main="2015 above 1300")
text(1550,0.85,round(cor2015, digits = 2))
abline(lm(sub2015$arate~sub2015$totalscore),col="green")
plot (sub2016$totalscore, sub2016$arate, xlab="SAT above 1300", ylab="acceptance",
      main="2016 above 1300")
text(1550,0.85,round(cor2016, digits = 2))
abline(mdl2016<-lm(sub2016$arate~sub2016$totalscore),col="red")