summary(lm(SalePrice~factor(Neighborhood), train))


##Neighborhood accounts for .5456 in the model


#train$SalePrice_100s <- train$SalePrice/100

ggplot(train, aes(factor(Neighborhood), SalePrice)) + 
  geom_boxplot(alpha=.5) + stat_smooth(method="lm", se=F) +
  labs(title = "Sale Price by Neighborhood")

summary(lm(SalePrice~LotArea+factor(Neighborhood),train))
summary(lm(SalePrice~LotArea*factor(Neighborhood),train))
summary(lm(SalePrice~factor(Foundation),train))
summary(lm(SalePrice~TotRmsAbvGrd,train))
summary(lm(SalePrice~factor(Neighborhood)+factor(MSZoning),train))
summary(lm(SalePrice~Alley,train))
summary(lm(SalePrice~factor(Neighborhood),train))

summary(lm(SalePrice~factor(Neighborhood)+factor(FullBath),train))
summary(lm(SalePrice~factor(Neighborhood)+factor(KitchenQual),train))
summary(lm(SalePrice~factor(Neighborhood)+factor(KitchenQual)+factor(FullBath),train))
summary(lm(SalePrice~TotRmsAbvGrd,train))
summary(lm(SalePrice~factor(Neighborhood)+factor(KitchenQual)+factor(FullBath)+TotRmsAbvGrd,train))
#The best model we could find
summary(lm(SalePrice~factor(Neighborhood)+totalsqft+factor(FullBath)+OverallQual+factor(YearBuilt),train))
Model<- (lm(SalePrice~factor(Neighborhood)+totalsqft+factor(FullBath)+OverallQual+factor(YearBuilt),train))
summary(lm(SalePrice~factor(Neighborhood)+SaleType+factor(FullBath)+OverallQual+factor(YearBuilt),train))
summary(lm(SalePrice~factor(Neighborhood)+totalsqft+MSSubClass+OverallQual+factor(YearBuilt),train))

summary(Model)

table(train$Neighborhood)
summary(subset(train,Neighborhood=="NAmes")$SalePrice)


library(leaps)

plot(regsubsets(SalePrice ~ ., data = train_1, method = "exhaustive", nbest = 1))

display(step(lm(SalePrice~factor(MSSubClass)+LotFrontage+LotArea+factor(Street)+factor(Alley)+factor(LotShape)+factor(LandContour)+factor(Utilities)+factor(LotConfig)+factor(LandSlope)+factor(Neighborhood)+factor(Condition1)+factor(Condition2)+factor(BldgType)+factor(HouseStyle)+factor(OverallQual)+factor(OverallCond)+factor(YearBuilt)+factor(YearRemodAdd)+factor(RoofStyle)+fa
                ctor(RoofMatl)+factor(Exterior1st)+factor(Exterior2nd)+factor(MasVnrType)+MasVnrArea+factor(ExterQual)+factor(ExterCond)+factor(Foundation)+factor(BsmtQual)+
                  factor(BsmtCond)+factor(BsmtExposure)+BsmtFinTypeSF1+factor(BsmtFinType2)+BsmtFinSF2,train)))

train_1$totalsqft <- train_1$`1stFlrSF` + train_1$`2ndFlrSF` + train_1$TotalBsmtSF 
train$totalsqft <- train$X1stFlrSF + train$X2ndFlrSF + train$TotalBsmtSF


ggplot(train, aes(factor(Neighborhood), SalePrice)) + 
  geom_boxplot(alpha=.5) + stat_smooth(method="lm", se=F) +
  labs(title = "Sale Price by Neighborhood")


ggplot(train, aes(YearBuilt,SalePrice))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(x = 'Year Built', y = 'Sale Price', title = 'Sale Price by Year Built')+
  scale_x_continuous(limits = c(1872,2010))


ggplot(train, aes(OverallQual,SalePrice))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Overall Quality", y = "Sale Price", title = "Sale Price by Overall Quality")+
  scale_x_continuous(breaks = c(1:10)) 

ggplot(train, aes(totalsqft,SalePrice))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Total Square Feet", y = "Sale Price", title = "Sale Price by Square Feet")
 
rmse <- function(actual, fitted){
  sqrt(mean((fitted - actual)^2))
}
round(rmse(train$SalePrice, fitted(Model)))

library(caret)
Model_real <-train(SalePrice~factor(Neighborhood)+totalsqft+factor(FullBath)+OverallQual+factor(YearBuilt),
      method = "lm",
      data = train)
Model_real

Model_real1 <-lm(SalePrice~factor(Neighborhood)+totalsqft+FullBath+OverallQual+factor(YearBuilt),train)
                   
test$SalePrice <- NULL
myprediction <- fitted(Model_real1, newdata = test, type = "class")
test$SalePrice <- myprediction




test$totalsqft <- test$'1stFlrSF'+ test$'2ndFlrSF' + test$TotalBsmtSF
test$SalePrice <- predict(Model_real1, newdata = test)

my_solution <- data.frame(Id = test$Id, SalePrice = myprediction[1:1459])
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
