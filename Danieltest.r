# Read in the data
Walmart = read.csv(file.choose(),header = TRUE)
# Load the Data
Stor9Item50 = Walmart %>% filter(item == 50 & store == 9)#Look at and Visualize the data
librahead(Stor9Item50)
plotts.wge(Stor9Item50$sales)
#Break into month day and year.
Stor9Item50 = separate(Stor9Item50,col = date,into = c("month","day","year"), sep = "/")
#Change to dataframe
Stor9Item50 = data.frame(Stor9Item50)
#Look at Spectral density... evidence of yearly, monthly and weekly trend?  
#Yearly and Monthly are going to be tough with daily data. 
parzen.wge(Stor9Item50$sales, trunc= 500)
# Change to integers for easier sorting later.... could use other package to handle dates. 
Stor9Item50$month = as.integer(Stor9Item50$month)
Stor9Item50$year = as.integer(Stor9Item50$year)
# Aggregate to get monthly sales
Stor9Item50_grouped = Stor9Item50 %>% group_by(year,month) %>% summarise(mean_sales = mean(sales))
#Note data is out of order and that is a big deal.
head(Stor9Item50_grouped)
#Order by year and month
Stor9Item50_grouped = Stor8Item1_grouped[order(Stor9Item50_grouped$year,Stor9Item50_grouped$month),]
# Evidence of yearly trend?  Montly trend is still tough since there are differnt number of days in a month.
parzen.wge(Stor9Item50_grouped$mean_sales)
# to more clearly see the annual trend
parzen.wge(Stor9Item50_grouped$mean_sales,trunc = 30) 
# Shows combo of pseudo cyclic and wandering behavior.
acf(Stor9Item50_grouped$mean_sales,lag = 30) 


#Original Series
acf(Stor9Item50_grouped$mean_sales,lag = 30) 
plotts.wge(Stor9Item50_grouped$sales)

factor.wge(phi=c(.967))
plotts.true.wge(phi=c(-.967))

factor.wge(phi=      c(1.452,-.453,-.294,.175,.237,-.154))
plotts.true.wge(phi= c(1.452,-.453,-.294,.175,.237,-.154))

factor.wge(phi=      c(1.445,-.411,-.038,.170,.362,-.245,-.177,.213))
plotts.true.wge(phi=c(1.445,-.411,-.038,.170,.362,-.245,-.177,.213))

factor.wge(phi=c(1.384,-.359,-.309,.063,.317,-.140,-.0587,-.199,.2877));
plotts.true.wge(phi=c(1.384,-.359,-.309,.063,.317,-.140,-.0587,-.199,.2877));


findFrequencyAR2 <- function(phi1,phi2) {
  acos(phi1/(2*sqrt(phi2*-1)))/(2*pi)
}
findFrequencyAR2(.2,-.8)
findFrequencyAR2(1.6,-.8)
findFrequencyAR2(-.5,-.6)

plotts.true.wge(phi=);


