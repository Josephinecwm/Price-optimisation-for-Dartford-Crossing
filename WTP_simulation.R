# load llibraries
library(dplyr)
# load data
vehicle_data = read.csv("dft_aadf_count_point_id_27923.csv")
# drop necessary data
vehicle_data = select(vehicle_data, c(2, 19:24, 30))
# 4 types of vehicles
# 1. motorbikes
vehicle_data$motorbike = vehicle_data$pedal_cycles + vehicle_data$two_wheeled_motor_vehicles
# 2. private cars
vehicle_data$private_cars = vehicle_data$cars_and_taxis
# 3. vans
vehicle_data$vans = vehicle_data$buses_and_coaches + vehicle_data$lgvs + vehicle_data$hgvs_2_rigid_axle
# 4. goods
vehicle_data$multi_goods = vehicle_data$all_hgvs - vehicle_data$hgvs_2_rigid_axle
vehicle_data = select(vehicle_data, -c("pedal_cycles", "two_wheeled_motor_vehicles", 
                                       "cars_and_taxis", "buses_and_coaches", "lgvs", 
                                       "hgvs_2_rigid_axle", "all_hgvs"))
vehicle_data$total = vehicle_data$motorbike+vehicle_data$private_cars+vehicle_data$vans+vehicle_data$multi_goods
vehicle_data$motorbike = vehicle_data$motorbike/vehicle_data$total
vehicle_data$private_cars = vehicle_data$private_cars/vehicle_data$total
vehicle_data$vans = vehicle_data$vans/vehicle_data$total
vehicle_data$multi_goods = vehicle_data$multi_goods/vehicle_data$total
# taking 2019
vehicle_data = vehicle_data[vehicle_data$year == 2019, ]

# size of the simulated dataset of 10000 drivers
# of drivers for each vehicle type
N = 10000
vehicle_data = select(vehicle_data, c(2:5))
vehicle_data = round(vehicle_data*N)
WTP = data.frame(matrix(ncol = 25, nrow = N))
column_names = c(rep(0, 24), "vehicle_type")
for (i in 1:24){
  name = gsub(" ", "", paste("hour", as.character(i)))
  column_names[i] = name
}
colnames(WTP) = column_names
vehicle_type = c(rep("motorbike", vehicle_data$motorbike))
vehicle_type = append(vehicle_type, c(rep("private_cars", vehicle_data$private_cars)))
vehicle_type = append(vehicle_type, c(rep("vans", vehicle_data$vans)))
vehicle_type = append(vehicle_type, c(rep("multi_goods", vehicle_data$multi_goods)))
WTP$vehicle_type = vehicle_type

# simulate WTP for motorbikes
# £1
# 20%
# peak hours: 8am-10am; 5-7pm
for (i in c(8:10, 17:19)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'motorbike', i] = rnorm(vehicle_data$motorbike, 1*1.2, 0.2)
}
for (i in c(1:7, 11:16, 20:24)){ #non-peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'motorbike', i] = rnorm(vehicle_data$motorbike, 1, 0.2)
}

# simulate WTP for private cars
# peaks hours: 8am-10am; 5-7pm
# £2.5 current pricing structure
# assume they are willing to pay 50% more during peak hours
for (i in c(8:10, 17:19)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'private_cars', i] = rnorm(vehicle_data$private_cars, 2.5*1.5, 0.2)
}
for (i in c(1:7, 11:16, 20:24)){ #non-peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'private_cars', i] = rnorm(vehicle_data$private_cars, 2.5, 0.2)
}

# simulate WTP for vans
# peak hours: 6am-10pm
# £3
# assume they are willing to pay 25% more during peak hours
for (i in c(6:22)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'vans', i] = rnorm(vehicle_data$vans, 3*1.25, 0.2)
}
for (i in c(1:5, 23:24)){ #non-peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'vans', i] = rnorm(vehicle_data$vans, 3, 0.2)
}

# simulate WTP for multi-goods vehicles
# £6*10% range
for (i in c(1:24)){ #peak
  set.seed(i)
  WTP[WTP$vehicle_type == 'multi_goods', i] = rnorm(vehicle_data$multi_goods, 6, 0.2)
}

#average of WTP for each vehicle type
WTP$vehicle_type = factor(WTP$vehicle_type)
summary(WTP$vehicle_type)
# get the subset for each vehicle type 
Motorbike=subset(WTP, vehicle_type=="motorbike")[1:24]
multi.goods=subset(WTP, vehicle_type=="multi_goods")[1:24]
private.cars=subset(WTP, vehicle_type=="private_cars")[1:24]
Vans=subset(WTP, vehicle_type=="vans")[1:24]

# get the mean WTP for each hour
mean_motorbike_WTP = colMeans(Motorbike)
mean_multi_goods_WTP = colMeans(multi.goods)
mean_private_cars_WTP = colMeans(private.cars)
mean_vans_WTP = colMeans(Vans)

#plot the mean WTP for all vehicles across all hours
par(mfrow=c(1,1))
plot(mean_vans_WTP, 
     type = "b", 
     col="green", 
     xlab = "hour", 
     ylab = "Mean WTP",
     xlim = c(0,24),
     ylim = c(0, 7),
     main = "Mean WTP for different types of vehicles across all hours")
lines(mean_multi_goods_WTP, type = "b", col = "blue")
lines(mean_motorbike_WTP, type = "b", col = "red")
lines(mean_private_cars_WTP, type = "b", col = "yellow")
xticks = seq(0, 24, by=1)
par(xpd = TRUE)
axis(1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24))
legend("topright", inset = c(-0.1,1.1), legend=c("motorbike", "private cars","two-axle goods vehicle", "multiple-axle goods vehicle"),
      col=c("red", "yellow", "green", "blue"), lty=1, cex=0.7, box.col = "white")


###### HOT lanes ------
# simulate HOT WTP for all types of vehicles 
WTP$HOT = 0
WTP$Normal = 0

WTP[WTP$vehicle_type == 'motorbike', "Normal"] = round(rnorm(vehicle_data$motorbike, 1, 0.2), 2)
WTP[WTP$vehicle_type == 'motorbike', "HOT"] = round(rnorm(vehicle_data$motorbike, 1*0.7, 0.2), 2)
WTP[WTP$vehicle_type == 'private_cars', "Normal"] = round(rnorm(vehicle_data$private_cars, 2.5, 0.2), 2)
WTP[WTP$vehicle_type == 'private_cars', "HOT"] = round(rnorm(vehicle_data$private_cars, 2.5*0.7, 0.2), 2)
WTP[WTP$vehicle_type == 'multi_goods', "Normal"] = round(rnorm(vehicle_data$multi_goods, 6, 0.2), 2)
WTP[WTP$vehicle_type == 'multi_goods', "HOT"] = round(rnorm(vehicle_data$multi_goods, 6*0.7, 0.2), 2)
WTP[WTP$vehicle_type == 'vans', "Normal"] = round(rnorm(vehicle_data$vans, 3, 0.2), 2)
WTP[WTP$vehicle_type == 'vans', "HOT"] = round(rnorm(vehicle_data$vans, 3*0.7, 0.2), 2)

plot(WTP[WTP$vehicle_type == 'motorbike', "Normal"], type = "b", col = "blue")
lines(WTP[WTP$vehicle_type == 'motorbike', "HOT"], type = "b", col = "yellow")

#subset the data
WTP_HOT = WTP[,25:27]

#calculate the max price and demand 
max_price_motorbike = max(WTP[WTP$vehicle_type == 'motorbike', "Normal"])
max_price_cars = max(WTP[WTP$vehicle_type == 'private_cars', "Normal"])
max_price_vans = max(WTP[WTP$vehicle_type == 'vans', "Normal"])
max_price_multigoods = max(WTP[WTP$vehicle_type == 'multi_goods', "Normal"])

demand_motorbikes = rep(NA, max_price_motorbike)
demand_cars= rep(NA, max_price_cars)
demand_vans= rep(NA, max_price_vans)
demand_multigoods = rep(NA, max_price_multigoods)

# setting HOT and normal lane for motorbike simutinuously
N = nrow(WTP_HOT)
surplusHOT_motorbike<-rep(0,N)
surplusNormal_motorbike<-rep(0,N)

demandHOT_motorbike = rep(0, 261)
demandNormal_motorbike = rep(0,261)
speedHOT_motorbike = rep(0,max_price_motorbike)
speedNormal_motorbike = rep(0,max_price_motorbike)
emission_HOT_motorbike = rep(0,max_price_motorbike)
emission_Normal_motorbike = rep(0,max_price_motorbike)
total_emission_motorbike = rep(0,max_price_motorbike)
total_demand_motorbike  = rep(0,max_price_motorbike)


index=1
for (HOTPrice in seq(from = 0, to = max_price_motorbike, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_motorbike, by = 0.2)){
    for (i in 1:N){
      surplusHOT_motorbike[i]=max(WTP_HOT[i,2]-HOTPrice)
      surplusNormal_motorbike[i]=WTP_HOT[i,3]-NormalPrice
     }
    demandHOT_motorbike[index]=sum((surplusHOT_motorbike>surplusNormal_motorbike)*(surplusHOT_motorbike>=0))
    demandNormal_motorbike[index]=sum((surplusNormal_motorbike>=surplusHOT_motorbike)*(surplusNormal_motorbike>=0))
    speedHOT_motorbike[index] =  30 - 0.0625 * (demandHOT_motorbike[index]/1000)
    speedNormal_motorbike[index] =  30 - 0.0625 * (demandNormal_motorbike[index]/1000)
    if (speedHOT_motorbike[index] < 25){
      emission_HOT_motorbike[index] = 2200*2.872/(0.354006*(1.48*speedHOT_motorbike[index]))
    } 
    if (speedHOT_motorbike[index] >= 25){
      emission_HOT_motorbike[index]= 2000*2.872/(0.354006*(45.33 - 0.33*speedHOT_motorbike[index]))
    } 
    if (speedNormal_motorbike[index] < 25){
      emission_Normal_motorbike[index]= 2200*2.872/(0.354006*(1.48*speedNormal_motorbike[index]))
    } 
    if (speedNormal_motorbike[index]>= 25){
      emission_Normal_motorbike[index]= 2000*2.872/(0.354006*(45.33 - 0.33*speedNormal_motorbike[index]))
    } 
    total_emission_motorbike[index] = emission_Normal_motorbike[index]+emission_HOT_motorbike[index]
    total_demand_motorbike[index] = demandNormal_motorbike[index] + demandHOT_motorbike[index]
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata<-data.frame(matrix(nrow=261,ncol = 5))
colnames(newdata)=c("index","HOTPrice_motorbike","NormalPrice_motorbike","HOTDemand_motorbike", "NormalDemand_motorbike")
index=1
for (HOTPrice in seq(from = 0, to = max_price_motorbike, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_motorbike, by = 0.2)){
    newdata[index,1]=index
    newdata[index,2]=HOTPrice
    newdata[index,3]=NormalPrice
    newdata[index,4]=demandHOT_motorbike[index]
    newdata[index,5]=demandNormal_motorbike[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata$revenue=newdata$HOTPrice_motorbike*newdata$HOTDemand_motorbike+newdata$NormalPrice_motorbike*newdata$NormalDemand_motorbike

# Regression for the dependent variable HOTDemand
fit2HOT <-lm(HOTDemand_motorbike ~ HOTPrice_motorbike+NormalPrice_motorbike, data=newdata)
summary(fit2HOT)

a1=coef(fit2HOT)[1]
b11=coef(fit2HOT)[2]
b12=coef(fit2HOT)[3]

# Regression for the dependent variable NonPeakDemand
library(stargazer)
fit2Normal <-lm(NormalDemand_motorbike ~ HOTPrice_motorbike+NormalPrice_motorbike, data=newdata)
a2=coef(fit2Normal)[1]
b21=coef(fit2Normal)[2]
b22=coef(fit2Normal)[3]

stargazer(fit2HOT,fit2Normal, type="text")





# Finding optimal revenue by optimization
library("nloptr")

# Differentiated Prices

eval_f <- function(x){
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  revenue=HOTPrice*HOTDemand+NormalPrice*NormalDemand
  objfunction=-revenue
  return(objfunction)
}

eval_g_ineq <- function(x) {
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  totaldemand_motorbike = HOTDemand + NormalDemand
  speedHOT_motorbike=  30 - 0.0625 * (HOTDemand/1000)
  speedNormal_motorbike=  30 - 0.0625 * (NormalDemand/1000)
  if (speedHOT_motorbike < 25){
    emission_HOT_motorbike = 2200*2.872/(0.354006*(1.48*speedHOT_motorbike))
  } 
  if (speedHOT_motorbike >= 25){
    emission_HOT_motorbike= 2000*2.872/(0.354006*(45.33 - 0.33*speedHOT_motorbike))
  } 
  if (speedNormal_motorbike < 25){
    emission_Normal_motorbike= 2200*2.872/(0.354006*(1.48*speedNormal_motorbike))
  } 
  if (speedNormal_motorbike>= 25){
    emission_Normal_motorbike= 2000*2.872/(0.354006*(45.33 - 0.33*speedNormal_motorbike))
  } 
  totalemission_motorbike = emission_Normal_motorbike + emission_HOT_motorbike
  constraint <- c(-HOTDemand,
                  -NormalDemand,
                  x[1]-x[2],
                  speedHOT_motorbike - 48.3,
                  speedNormal_motorbike - 48.3,
                  totalemission_motorbike - (totaldemand_motorbike*1286.59)
                  )
  return(constraint)
}

# initial values
x0 <- c(0,0)
# lower and upper bounds of control
lb <- c(0,0)
ub <- c(1.3,1.3)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)
# print(result)

priceOpt<-result$solution
RevenueOpt<- -result$objective

print(paste("Optimal Base Price:",priceOpt[1]))

##### cars----
# setting HOT and normal lane for cars simutinuously
N = nrow(WTP_HOT)
surplusHOT_car<-rep(0,N)
surplusNormal_car<-rep(0,N)

demandHOT_car = rep(0, 261)
demandNormal_car = rep(0,261)
speedHOT_car = rep(0,max_price_cars)
speedNormal_car = rep(0,max_price_cars)
emission_HOT_car = rep(0,max_price_cars)
emission_Normal_car = rep(0,max_price_cars)
total_emission_car = rep(0,max_price_cars)
total_demand_car  = rep(0,max_price_cars)


index=1
for (HOTPrice in seq(from = 0, to = max_price_cars, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_cars, by = 0.2)){
    for (i in 1:N){
      surplusHOT_car[i]=max(WTP_HOT[i,2]-HOTPrice)
      surplusNormal_car[i]=WTP_HOT[i,3]-NormalPrice
    }
    demandHOT_car[index]=sum((surplusHOT_car>surplusNormal_car)*(surplusHOT_car>=0))
    demandNormal_car[index]=sum((surplusNormal_car>=surplusHOT_car)*(surplusNormal_car>=0))
    speedHOT_car[index] =  30 - 0.0625 * (demandHOT_car[index]/1000)
    speedNormal_car[index] =  30 - 0.0625 * (demandNormal_car[index]/1000)
    if (speedHOT_car[index] < 25){
      emission_HOT_car[index] = 2392*2.872/(0.354*(1.48*speedHOT_car[index]))
    } 
    if (speedHOT_car[index] >= 25){
      emission_HOT_car[index]= 2392*2.872/(0.354006*(45.33 - 0.33*speedHOT_car[index]))
    } 
    if (speedNormal_car[index] < 25){
      emission_Normal_car[index]= 2392*2.872/(0.354*(1.48*speedNormal_car[index]))
    } 
    if (speedNormal_car[index]>= 25){
      emission_Normal_car[index]= 2392*2.872/(0.354006*(45.33 - 0.33*speedNormal_car[index]))
    } 
    total_emission_car[index] = emission_Normal_car[index]+emission_HOT_car[index]
    total_demand_car[index] = demandNormal_car[index] + demandHOT_car[index]
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata_car<-data.frame(matrix(nrow=261,ncol = 5))
colnames(newdata_car)=c("index","HOTPrice_car","NormalPrice_car","HOTDemand_car", "NormalDemand_car")
index=1
for (HOTPrice in seq(from = 0, to = max_price_cars, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_cars, by = 0.2)){
    newdata_car[index,1]=index
    newdata_car[index,2]=HOTPrice
    newdata_car[index,3]=NormalPrice
    newdata_car[index,4]=demandHOT_car[index]
    newdata_car[index,5]=demandNormal_car[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata_car$revenue=newdata_car$HOTPrice_car*newdata_car$HOTDemand_car+newdata_car$NormalPrice_car*newdata_car$NormalDemand_car

# Regression for the dependent variable HOTDemand
fit2HOT <-lm(HOTDemand_car ~ HOTPrice_car+NormalPrice_car, data=newdata_car)
summary(fit2HOT)

a1=coef(fit2HOT)[1]
b11=coef(fit2HOT)[2]
b12=coef(fit2HOT)[3]

# Regression for the dependent variable NonPeakDemand
library(stargazer)
fit2Normal <-lm(NormalDemand_car ~ HOTPrice_car+NormalPrice_car, data=newdata_car)
a2=coef(fit2Normal)[1]
b21=coef(fit2Normal)[2]
b22=coef(fit2Normal)[3]

stargazer(fit2HOT,fit2Normal, type="text")

# Finding optimal revenue by optimization
library("nloptr")

# Differentiated Prices

eval_f <- function(x){
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  revenue=HOTPrice*HOTDemand+NormalPrice*NormalDemand
  objfunction=-revenue
  return(objfunction)
}

eval_g_ineq <- function(x) {
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  totaldemand = HOTDemand + NormalDemand
  speedHOT=  30 - 0.0625 * (HOTDemand/1000)
  speedNormal=  30 - 0.0625 * (NormalDemand/1000)
  if (speedHOT < 25){
    emission_HOT= 2200*2.872/(0.354006*(1.48*speedHOT))
  } 
  if (speedHOT >= 25){
    emission_HOT= 2000*2.872/(0.354006*(45.33 - 0.33*speedHOT))
  } 
  if (speedNormal < 25){
    emission_Normal= 2200*2.872/(0.354006*(1.48*speedNormal))
  } 
  if (speedNormal>= 25){
    emission_Normal= 2000*2.872/(0.354006*(45.33 - 0.33*speedNormal))
  } 
  totalemission = emission_Normal+ emission_HOT
  constraint <- c(-HOTDemand,
                  -NormalDemand,
                  x[1]-x[2],
                  speedHOT - 48.3,
                  speedNormal- 48.3,
                  totalemission - (totaldemand*1286.59)
  )
  return(constraint)
}

# initial values
x0 <- c(0,1)
# lower and upper bounds of control
lb <- c(0,0)
ub <- c(3.3,3.3)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)
# print(result)

priceOpt<-result$solution
RevenueOpt<- -result$objective

print(paste("Optimal Base Price:",priceOpt[1]))

##### vans----
# setting HOT and normal lane for vans simutinuously
N = nrow(WTP_HOT)
surplusHOT_van<-rep(0,N)
surplusNormal_van <-rep(0,N)

demandHOT_van = rep(0, 261)
demandNormal_van = rep(0,261)
speedHOT_van = rep(0,max_price_vans)
speedNormal_van = rep(0,max_price_vans)
emission_HOT_van = rep(0,max_price_vans)
emission_Normal_van = rep(0,max_price_vans)
total_emission_van = rep(0,max_price_vans)
total_demand_van  = rep(0,max_price_vans)

index=1
for (HOTPrice in seq(from = 0, to = max_price_vans, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_vans, by = 0.2)){
    for (i in 1:N){
      surplusHOT_van[i]=max(WTP_HOT[i,2]-HOTPrice)
      surplusNormal_van[i]=WTP_HOT[i,3]-NormalPrice
    }
    demandHOT_van[index]=sum((surplusHOT_van>surplusNormal_van)*(surplusHOT_van>=0))
    demandNormal_van[index]=sum((surplusNormal_van>=surplusHOT_van)*(surplusNormal_van>=0))
    speedHOT_van[index] =  30 - 0.0625 * (demandHOT_van[index]/1000)
    speedNormal_van[index] =  30 - 0.0625 * (demandNormal_van[index]/1000)
    if (speedHOT_van[index] < 25){
      emission_HOT_van[index] = 2500*2.872/(0.354006*(1.48*speedHOT_van[index]))
    } 
    if (speedHOT_van[index] >= 25){
      emission_HOT_van[index]= 2600*2.872/(0.354006*(45.33 - 0.33*speedHOT_van[index]))
    } 
    if (speedNormal_van[index] < 25){
      emission_Normal_van[index]= 2500*2.872/(0.354006*(1.48*speedNormal_van[index]))
    } 
    if (speedNormal_van[index]>= 25){
      emission_Normal_van[index]= 2600*2.872/(0.354*(45.33 - 0.33*speedNormal_van[index]))
    } 
    total_emission_van[index] = emission_Normal_van[index]+emission_HOT_van[index]
    total_demand_van[index] = demandNormal_van[index] + demandHOT_van[index]
    index=index+1
  }
}

# Create a data table which we will use to run the two regressions:
newdata<-data.frame(matrix(nrow=261,ncol = 5))
colnames(newdata)=c("index","HOTPrice_van","NormalPrice_van","HOTDemand_van", "NormalDemand_van")
index=1
for (HOTPrice in seq(from = 0, to = max_price_vans, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_vans, by = 0.2)){
    newdata[index,1]=index
    newdata[index,2]=HOTPrice
    newdata[index,3]=NormalPrice
    newdata[index,4]=demandHOT_van[index]
    newdata[index,5]=demandNormal_van[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata$revenue=newdata$HOTPrice_van*newdata$HOTDemand_van+newdata$NormalPrice_van*newdata$NormalDemand_van

# Regression for the dependent variable HOTDemand
fit2HOT <-lm(HOTDemand_van ~ HOTPrice_van+NormalPrice_van, data=newdata)
summary(fit2HOT)

a1=coef(fit2HOT)[1]
b11=coef(fit2HOT)[2]
b12=coef(fit2HOT)[3]

# Regression for the dependent variable NonPeakDemand
library(stargazer)
fit2Normal <-lm(NormalDemand_van ~ HOTPrice_van+NormalPrice_van, data=newdata)
a2=coef(fit2Normal)[1]
b21=coef(fit2Normal)[2]
b22=coef(fit2Normal)[3]

stargazer(fit2HOT,fit2Normal, type="text")

# Finding optimal revenue by optimization
library("nloptr")

# Differentiated Prices

eval_f <- function(x){
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  revenue=HOTPrice*HOTDemand+NormalPrice*NormalDemand
  objfunction=-revenue
  return(objfunction)
}

eval_g_ineq <- function(x) {
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  totaldemand_van = HOTDemand + NormalDemand
  speedHOT_van =  30 - 0.0625 * (HOTDemand/1000)
  speedNormal_van =  30 - 0.0625 * (NormalDemand/1000)
  if (speedHOT_van < 25){
    emission_HOT_van = 2200*2.872/(0.354006*(1.48*speedHOT_van))
  } 
  if (speedHOT_van >= 25){
    emission_HOT_van= 2000*2.872/(0.354006*(45.33 - 0.33*speedHOT_van))
  } 
  if (speedNormal_van < 25){
    emission_Normal_van= 2200*2.872/(0.354006*(1.48*speedNormal_van))
  } 
  if (speedNormal_van>= 25){
    emission_Normal_van= 2000*2.872/(0.354006*(45.33 - 0.33*speedNormal_van))
  } 
  totalemission_van = emission_Normal_van + emission_HOT_van
  constraint <- c(-HOTDemand,
                  -NormalDemand,
                  x[1]-x[2],
                  speedHOT_van - 48.3,
                  speedNormal_van - 48.3,
                  totalemission_van - (totaldemand_van*1286.59)
  )
  return(constraint)
}

# initial values
x0 <- c(3.5,3.5)
# lower and upper bounds of control
lb <- c(3.3,3.3)
ub <- c(3.72,3.72)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)
# print(result)

priceOpt<-result$solution
RevenueOpt<- -result$objective

print(paste("Optimal Base Price of van:",priceOpt[1]))

##### multigoods----
# setting HOT and normal lane for multigoods simutinuously
N = nrow(WTP_HOT)
surplusHOT_multigoods<-rep(0,N)
surplusNormal_multigoods<-rep(0,N)

demandHOT_multigoods = rep(0, 261)
demandNormal_multigoods = rep(0,261)
speedHOT_multigoods = rep(0,max_price_multigoods)
speedNormal_multigoods = rep(0,max_price_multigoods)
emission_HOT_multigoods = rep(0,max_price_multigoods)
emission_Normal_multigoods = rep(0,max_price_multigoods)
total_emission_multigoods = rep(0,max_price_multigoods)
total_demand_multigoods  = rep(0,max_price_multigoods)

index=1
for (HOTPrice in seq(from = 0, to = max_price_multigoods, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_multigoods, by = 0.2)){
    for (i in 1:N){
      surplusHOT_multigoods[i]=max(WTP_HOT[i,2]-HOTPrice)
      surplusNormal_multigoods[i]=WTP_HOT[i,3]-NormalPrice
    }
    demandHOT_multigoods[index]=sum((surplusHOT_multigoods>surplusNormal_multigoods)*(surplusHOT_multigoods>=0))
    demandNormal_multigoods[index]=sum((surplusNormal_multigoods>=surplusHOT_multigoods)*(surplusNormal_multigoods>=0))
    speedHOT_multigoods[index] =  30 - 0.0625 * (demandHOT_multigoods[index]/1000)
    speedNormal_multigoods[index] =  30 - 0.0625 * (demandNormal_multigoods[index]/1000)
    if (speedHOT_multigoods[index] < 25){
      emission_HOT_multigoods[index] = 2600*2.872/(0.354*(1.48*speedHOT_multigoods[index]))
    } 
    if (speedHOT_multigoods[index] >= 25){
      emission_HOT_multigoods[index]= 2600*2.872/(0.354*(45.33 - 0.33*speedHOT_multigoods[index]))
    } 
    if (speedNormal_multigoods[index] < 25){
      emission_Normal_multigoods[index]= 2600*2.872/(0.354*(1.48*speedNormal_multigoods[index]))
    } 
    if (speedNormal_multigoods[index]>= 25){
      emission_Normal_multigoods[index]= 2600*2.872/(0.354*(45.33 - 0.33*speedNormal_multigoods[index]))
    } 
    total_emission_multigoods[index] = emission_Normal_multigoods[index]+emission_HOT_multigoods[index]
    total_demand_multigoods[index] = demandNormal_multigoods[index] + demandHOT_multigoods[index]
    index=index+1
  }
}


# Create a data table which we will use to run the two regressions:
newdata<-data.frame(matrix(nrow=261,ncol = 5))
colnames(newdata)=c("index","HOTPrice_multigoods","NormalPrice_multigoods","HOTDemand_multigoods", "NormalDemand_multigoods")
index=1
for (HOTPrice in seq(from = 0, to = max_price_multigoods, by = 0.2)){
  for (NormalPrice in seq(from = 0, to = max_price_multigoods, by = 0.2)){
    newdata[index,1]=index
    newdata[index,2]=HOTPrice
    newdata[index,3]=NormalPrice
    newdata[index,4]=demandHOT_multigoods[index]
    newdata[index,5]=demandNormal_multigoods[index]
    index=index+1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata$revenue=newdata$HOTPrice_multigoods*newdata$HOTDemand_multigoods+newdata$NormalPrice_multigoods*newdata$NormalDemand_multigoods

# Regression for the dependent variable HOTDemand
fit2HOT <-lm(HOTDemand_multigoods ~ HOTPrice_multigoods+NormalPrice_multigoods, data=newdata)
summary(fit2HOT)

a1=coef(fit2HOT)[1]
b11=coef(fit2HOT)[2]
b12=coef(fit2HOT)[3]

# Regression for the dependent variable NonPeakDemand
library(stargazer)
fit2Normal <-lm(NormalDemand_multigoods ~ HOTPrice_multigoods+NormalPrice_multigoods, data=newdata)
a2=coef(fit2Normal)[1]
b21=coef(fit2Normal)[2]
b22=coef(fit2Normal)[3]

stargazer(fit2HOT,fit2Normal, type="text")

# Finding optimal revenue by optimization
library("nloptr")

# Differentiated Prices

eval_f <- function(x){
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  revenue=HOTPrice*HOTDemand+NormalPrice*NormalDemand
  objfunction=-revenue
  return(objfunction)
}
length_of_bridge = 

eval_g_ineq <- function(x) {
  HOTPrice=x[1]
  NormalPrice=x[2]
  HOTDemand=max(0,a1+b11*HOTPrice+b12*NormalPrice)
  NormalDemand=max(0,a2+b21*HOTPrice+b22*NormalPrice)
  totaldemand_multigoods = HOTDemand + NormalDemand
  speedHOT_multigoods =  30 - 0.0625 * (HOTDemand/1000)
  speedNormal_multigoods =  30 - 0.0625 * (NormalDemand/1000)
  if (speedHOT_multigoods < 25){
    emission_HOT_multigoods = 2200*2.872/(0.354006*(1.48*speedHOT_multigoods))
  } 
  if (speedHOT_multigoods >= 25){
    emission_HOT_multigoods= 2000*2.872/(0.354006*(45.33 - 0.33*speedHOT_multigoods))
  } 
  if (speedNormal_multigoods < 25){
    emission_Normal_multigoods = 2200*2.872/(0.354006*(1.48*speedNormal_multigoods))
  } 
  if (speedNormal_multigoods >= 25){
    emission_Normal_multigoods = 2000*2.872/(0.354006*(45.33 - 0.33*speedNormal_multigoods))
  } 
  totalemission_multigoods = emission_Normal_multigoods + emission_HOT_multigoods
  constraint <- c(-HOTDemand,
                  -NormalDemand,
                  x[1]-x[2],
                  speedHOT_multigoods - 48.3,
                  speedNormal_multigoods - 48.3,
                  totalemission_multigoods - (totaldemand_multigoods*1286.59)
  )
  return(constraint)
}

# initial values
x0 <- c(4,5)
# lower and upper bounds of control
lb <- c(3.3,)
ub <- c(6.57,6.57)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)
# print(result)

priceOpt<-result$solution
RevenueOpt<- -result$objective

print(paste("Optimal Base Price of multigoods:",priceOpt[1]))
