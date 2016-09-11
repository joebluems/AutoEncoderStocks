# AutoEncoderStocks
RMarkdown notebook for training an autoencoder with H2O to find stock price anomalies

## Setup & Customizing Ideas
* Install RStudio version with notebook: https://www.rstudio.com/products/rstudio/download/preview/ <BR>
* Install h2o package within R <BR>
* You can also run these commands from the R prompt 
* Replace the symbols & names to use your own training stocks
* Replace the test symbols and names to use your own test stocks
* Experiment with the number of hidden nodes and training epochs

## R commands (or use the attached rmd file)

\### create training symbols (DJIA) ### <BR>
symbols <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DD", "XOM", "GE", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT") <BR>
names <- c("3M", "American Express", "Apple", "Boeing", "Caterpillar", "Chevron", "Cisco", "Coca-Cola", "Disney", "du Pont", "Exxon Mobil", "General Electric", "Goldman Sachs", "Home Depot", "IBM", "Intel", "Johnson & Johnson", "JPMorgan Chase", "McDonald's", "Merck", "Microsoft", "Nike", "Pfizer", "Procter & Gamble", "Travelers Companies Inc", "United Technologies", "UnitedHealth", "Verizon", "Visa", "Wal-Mart") <BR>
<BR>
\### GENERATE RECENT CLOSING PRICES ON STOCKS ### <BR>
size <- 50 <BR>
df <- data.frame(matrix(vector(),size,0)) <BR>
for (i in 1:length(symbols)) {<BR>
 URL <- paste ("http://ichart.finance.yahoo.com/table.csv?s=",symbols[i],sep="")<BR>
 dat <- read.csv(URL)<BR>
 df <- cbind(df,dat$Close[1:size])<BR>
}<BR>
colnames(df) <- symbols<BR>
<BR>
\### quick plot of the first few of the symbols in the DJIA ###<BR>
par(mfrow=c(2,3))<BR>
for (j in 1:30) {<BR>
  plot(as.Date(dat$Date[1:size]),t(df[j]),xlab="Date",ylab="Close",main=names[j],type='l')<BR>
}<BR>
<BR>
\### TEST POPULATION - Missouri Stocks ###<BR>
testSymbols <- c("S", "ESRX", "EMR", "MON", "RGA", "ORLY", "AEE", "CERN", "CNC", "KSU", "HRB", "DOX")<BR>
testNames <- c("Sprint", "Express Scripts", "Emerson Electric", "Monsanto", "Reinsurance Group of America", "O’Reilly Automotive",
 "Ameren", "Cerner", "Centene", "Kansas City Southern", "H&R Block", "Amdocs")<BR>
<BR>
\### GENERATE RECENT CLOSING PRICES ON STOCKS ###<BR>
size <- 50<BR>
dfTest <- data.frame(matrix(vector(),size,0))<BR>
for (i in 1:length(testSymbols)) {<BR>
 URL <- paste ("http://ichart.finance.yahoo.com/table.csv?s=",symbols[i],sep="")<BR>
 dat <- read.csv(URL)<BR>
 dfTest <- cbind(dfTest,dat$Close[1:size])<BR>
}<BR>
<BR>
\### PLOT THE FIRST FEW TEST STOCKS ###<BR>
par(mfrow=c(2,3))<BR>
for (j in 1:11) {<BR>
  plot(as.Date(dat$Date[1:size]),t(dfTest[j]),xlab="Date",ylab="Close",main=testNames[j],type='l')<BR>
}<BR>
<BR>
\### prepare env & train model on DJIA training data ### <BR>
library(h2o) <BR>
localH2O = h2o.init(nthreads=-1) <BR>
Z <- t(scale(df,center=TRUE,scale=TRUE)) <BR>
Z_h2o <- as.h2o(Z) <BR>
autoEncoder <- h2o.deeplearning(x=c(1:size), training_frame=Z_h2o, activation="Tanh", autoencoder=T, hidden=c(8), ignore_const_cols=F, epochs=20) <BR>
<BR>
\### create standardized test matrix & calculate errors ### <BR>
colnames(dfTest) <- testSymbols <BR>
ZTest <- t(scale(dfTest,center=TRUE,scale=TRUE)) <BR>
ZTest_h2o <- as.h2o(ZTest) <BR>
recon <- h2o.predict(autoEncoder, ZTest_h2o) <BR>
mse <- h2o.anomaly(autoEncoder, ZTest_h2o) <BR>
error <- cbind(as.data.frame(mse),as.data.frame(testSymbols),as.data.frame(testNames)) <BR>
error$ID <- seq.int(row(error)[,1]) <BR>
head(error[with(error,order(-Reconstruction.MSE)), ]) <BR>
<BR>
\### individual reconstruction graphs ### <BR>
n <- 4 <BR>
plot(as.Date(dat$Date[1:size]),as.data.frame(ZTest_h2o[n,]),xlab="Date",ylab="Close <BR> (normalized)",main=testNames[n],type='l',col="blue",lwd=2) <BR>
lines(as.Date(dat$Date[1:size]),as.data.frame(recon[n,]),type='l',lty=2,col="red",lwd=3)
