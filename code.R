attach(MedicareData2)


##Graphic
plot(Ave_Income, MSPB, xlab='Ave_Income', ylab='MSPB')
plot(Ave_Hospital, MSPB, xlab='Ave_Hospital', ylab='MSPB')
plot(Life_Expectancy, MSPB, xlab='Life_Expectancy', ylab='MSPB')


##Model
model=lm(MSPB~.-State, data=MedicareData2)
summary(model)
library(car)
vif(model)
Medicaredata3=subset(MedicareData2,select=-State)
str(Medicaredata3)
cor(Medicaredata3)

model2=lm(MSPB~Unemployment_Rate+Life_Expectancy+Ave_Income+Ave_Hospital) #improved model
summary(model2)

model3=lm(MSPB~Unemployment_Rate+Life_Expectancy+Ave_Income+Ave_Hospital+I(1/Ave_Hospital))
summary(model3)
vif(model3)
anova(model2,model3) #nested F-test p-value: 0.0044, which means model3 is more useful

model4=lm(MSPB~Life_Expectancy+Ave_Income+Ave_Hospital+I(1/Ave_Hospital)) #advanced model
summary(model4)
anova(model4,model3) #nested F-test p-value: 0.1765, which means model4 is more useful


##Residual
library(car)
crPlots(model4)
qqnorm(resid(model4))
dwt(model4)


##Influence Plot
influencePlot(model4) #Alaska (#2) and Hawaii (#12)
predict(model2,data.frame(Unemployment_Rate=2.2,Life_Expectancy=82, Ave_Income=78084, Ave_Hospital=1.623856e-05)) #6701
