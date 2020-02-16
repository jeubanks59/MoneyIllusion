# Script: DataAndModel.R
# Author: Joshua Eubanks
# Date: 28 April 2018

library("reshape2")
library("TSA")

# Bringing in Data Sets
Consumption <- read.csv("Consumption.csv", header = TRUE)
Expenditures <- read.csv("Expenditures.csv", header = TRUE)

# Interested in only series 1000
Consumption <- Consumption[Consumption$Series == 1000,]
Consumption$Series <- NULL
Consumption$Concat <- paste(Consumption$Year,Consumption$Country.or.Area, sep = "")
Expenditures$Concat <- paste(Expenditures$Year,Expenditures$Country.or.Area, sep = "")
BigData <- merge(x = Consumption, y = Expenditures, by = "Concat", all.x = TRUE)
BigData$Concat <- NULL
BigData$Year.y <- NULL
BigData$Country.or.Area.y <- NULL

colnames(BigData) <- c("Country", "Category","Year","Consumption","Expenditures")
BigData$Ratio <- BigData$Consumption/BigData$Expenditures
BigData$Consumption <- log(BigData$Consumption)
BigData$Expenditures <- log(BigData$Expenditures)

# Binning category and country
BigData$Key <- seq(1:nrow(BigData))
BinCountry <- dcast(BigData, Key ~ Country, length)
BinCate <- dcast(BigData, Key ~ Category, length)

m1 <- merge(BinCountry,BinCate,by.x = "Key", by.y = "Key")
BinBigData <- merge(m1,BigData,by.x = "Key", by.y = "Key")

Factors <- colnames(BinBigData)


# Stuff for Ireland
pdf("Ireland.pdf")
Austria <- BinBigData[BinBigData$Country == Factors[8],]
AusFood <- Austria[Austria$Category == Factors[17],]


TrimmedAus <- as.data.frame(AusFood$Consumption)


test <- ts(TrimmedAus)


par(mfrow = c(2,2))

covariates <- c("Year","Expenditures")
air.m1 = arimax(test
	, order = c(0,0,1)
	, xtransf = data.frame(I911 = 1*(seq(test) == 8), I911 = 1*(seq(test) == 8))
	, xreg = AusFood[,covariates]
	, transfer = list(c(0,0),c(1,0))
	, method = 'ML'
	)

 omega <- air.m1$coef[3]
 delta <- air.m1$coef[4]
 delta2 <- air.m1$coef[5]

plot(test
	,ylab = 'log(Food Expenditures)'
	,xlab = 'Year'
	)
points(fitted(air.m1))

air.m1
acf(air.m1$residuals, main = "")
pacf(air.m1$residuals, main = "")

plot(test
	,ylab = 'log(Food Expenditures)'
	,xlab = 'Year'
	)
points(fitted(air.m1))

Nine11p <- 1*(seq(test)==8)
plot(ts(Nine11p*(omega) + 
	filter(Nine11p,filter = delta, method = 'recursive', side = 1)*(delta2), frequency = 1, start = 1995)
	, xlab = 'Year'
	, ylab = 'Euro Introduction log(Effects)'
	, main = ' '
	, type = 'h'); abline(h=0)

invisible(dev.off())


# Stuff for Italy

pdf("Italy.pdf")
Austria <- BinBigData[BinBigData$Country == Factors[9],]
AusFood <- Austria[Austria$Category == Factors[17],]


TrimmedAus <- as.data.frame(AusFood$Consumption)


test <- ts(TrimmedAus)

par(mfrow = c(2,2))

covariates <- c("Year","Expenditures")

air.m1 = arimax(test
	, order = c(0,0,1)
	, xtransf = data.frame(I911 = 1*(seq(test) == 8), I911 = 1*(seq(test) == 8))
	, xreg = AusFood[,covariates]
	, transfer = list(c(0,0),c(1,0))
	, method = 'ML'
	)

 omega <- air.m1$coef[3]
 delta <- air.m1$coef[4]
 delta2 <- air.m1$coef[5]

air.m1
acf(air.m1$residuals, main = "")
pacf(air.m1$residuals, main = "")

plot(test
	,ylab = 'log(Food Expenditures)'
	,xlab = 'Year'
	)
points(fitted(air.m1))

Nine11p <- 1*(seq(test)==8)
plot(ts(Nine11p*(omega) + 
	filter(Nine11p,filter = delta, method = 'recursive', side = 1)*(delta2), frequency = 1, start = 1995)
	, xlab = 'Year'
	, ylab = 'Euro Introduction log(Effects)'
	, main = ' '
	, type = 'h'); abline(h=0)

invisible(dev.off())
