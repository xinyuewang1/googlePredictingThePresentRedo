# Google data
google = read.csv('google.csv');
google$Week = as.Date(google$Week, "%d/%m/%Y");

# CarSalesBase data
dat = read.csv("actual.csv");
# library(zoo);
# dat$Month = as.yearmon(dat$Month, "%Y-%m");
dat$Month = as.Date(paste(dat$Month,"-01",sep=""));

dat = rbind(dat, dat[nrow(dat), ]);
dat[nrow(dat), 'Month'] = as.Date('2018-09-01');
dat[nrow(dat), -1] = rep(NA, ncol(dat)-1);

# Add time lags
dat$s1 = c(NA, dat$CarSalesBase[1:(nrow(dat)-1)]);
dat$s12 = c(rep(NA, 12), dat$CarSalesBase[1:(nrow(dat)-12)]);

# Plot
par(mfrow=c(2,1));
plot(CarSalesBase ~ Month, data= dat, lwd=2, type='l', main='Ford Sales', ylab='Sales', xlab='Time');
plot(trend ~ Week, data= google, lwd=2, type='l', main='Google Trends: Ford', ylab='Percentage Change', xlab='Time');

# Merge data
google$Month = as.Date(paste(substr(google$Week, 1, 7), '01', sep='-'))
dat = merge(dat, google);

#  t.lag = 1 if you want to include 1st-3rd week of the corresponding month
t.lag = 1;
# Pick out the last weeks in months.
id = which(dat$Month[-1] != dat$Month[-nrow(dat)]);
mdat = dat[id + 1, c('Month', 'CarSalesBase', 's1', 's12')];
mdat$trends1 = dat$trend[id + t.lag];
mdat$trends2 = dat$trend[id + t.lag + 1];
mdat$trends3 = dat$trend[id + t.lag + 2];

# train & test split ( only 1 test set)
dat1 = mdat[1:(nrow(mdat)-1), ];
dat2 = mdat[nrow(mdat), ];

# Plot
acf(log(dat1$CarSalesBase));
Box.test(log(dat1$CarSalesBase), type="Ljung-Box");

plot(y = log(dat1$CarSalesBase), x = dat1$trends1, main='', pch=19, ylab='log(Sales)', xlab='Google Trends - 1st Week')
abline(lm(log(dat1$CarSalesBase) ~ dat1$trends1), lwd=2, col=2)
cor.test(y = log(dat1$CarSalesBase), x = dat1$trends1)
cor.test(y = log(dat1$CarSalesBase), x = dat1$trends2)
cor.test(y = log(dat1$CarSalesBase), x = dat1$trends3)

fit = lm(log(CarSalesBase) ~ log(s1) + log(s12) + trends1, data=dat1);
summary(fit)

par(mfrow=c(2,2));
plot(fit)

predict.fit = predict(fit, newdata=dat2, se.fit=TRUE);




