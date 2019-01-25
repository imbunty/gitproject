#https://stackoverflow.com/questions/11291845/plot-the-results-of-a-multivariate-logistic-regression-model-in-r
#http://www.cookbook-r.com/Statistical_analysis/Logistic_regression/

library(InformationValue)
#install.packages("smbinning")
#install.packages("InformationValue")
#install.packages("VIF")
library(smbinning)
library("dummies")
library("car")
library("VIF")
my_data<-read.csv("austismdata.csv",header=T,sep=",")
#Remove NA values from data
my_data[my_data=="?"]<-NA
my_data<-na.omit(my_data)

my_data <- data.frame(lapply(my_data, function(x) {gsub("'", "", x)}))
my_data$age<-as.numeric(my_data$age)
#my_data$result<-as.numeric(my_data$result)

write.csv(my_data, "austismdataout.csv")

attach(my_data)
my_data$agecat[age > 30] <- "31 and Above"
my_data$agecat[age > 20 & age <= 30] <- "21 to 30"
my_data$agecat[age <= 20] <- "20 or less"
detach(my_data)

my_data=filter(my_data, age<150)

#my_data <- my_data[, c("A1_Score","A2_Score","A3_Score","A4_Score","A5_Score","A6_Score","A7_Score","A8_Score","A9_Score","A10_Score","age","gender","ethnicity","jundice","austim","contry_of_res","used_app_before","result","age_desc","relation","agecat","Class.ASD")]


head(my_data,2)

#my_data$age<-NULL
my_data$age_desc<-NULL
my_data$ethnicity<-NULL
my_data$contry_of_res<-NULL


my_data <- dummy.data.frame(my_data, names = c("A1_Score","A2_Score","A3_Score","A4_Score","A5_Score","A6_Score","A7_Score","A8_Score","A9_Score","A10_Score","gender","jundice","austim","used_app_before","result","relation"))

set.seed(123)
smp_size <- floor(0.75 * nrow(my_data))
train_ind <- sample(seq_len(nrow(my_data)), size = smp_size)
##sample size 75%

new_train_data <- my_data[train_ind, ]
new_test_data <- my_data[-train_ind, ]

head(new_train_data[ , -which(names(new_train_data) %in% c("Class.ASD"))])

new_train_cpy<-new_train_data
identical(new_train_cpy,new_train_data)
#new_train_data$Class.ASD<-NULL

head(new_train_data,1)


pca.mtrx <- prcomp((new_train_data[ , -which(names(new_train_data) %in% c("Class.ASD"))]),center=TRUE  , scale. = T)

#pca.mtrx <- prcomp((new_train_data[ , -which(names(new_train_data) %in% c("Class.ASD"))]),center=TRUE  )

head(pca.mtrx$rotation)

names(pca.mtrx)
pca.mtrx$center

dim(pca.mtrx$x)
pca.mtrx$rotation[,1:5]

biplot(pca.mtrx, scale = 0)
pca.stdev <- pca.mtrx$sdev
pca.var <- pca.stdev^2
pca.screedata <- pca.var/sum(pca.var)

pca.screedata

#scree plot
plot(pca.screedata, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(pca.screedata), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
library(psych)
pairs.panels(pca.mtrx$x,gap=0,bg=c("red","black")[new_train_cpy$Class.ASD],pch=21)

dim(pca.mtrx$rotation[,3:5])
pca.mtrx$rotation[,1:5]
library("devtools")
#install.packages("rlang")
#install_github('fawda123/ggord')
#install_github('ggbiplot','vqv')
library(ggbiplot)
ggpl<-ggbiplot(pca.mtrx,
               obs.scale=1,
               var.scale=1,
               groups=new_train_data$Class.ASD,
               ellipse = TRUE,
               circle=TRUE,
               ellipse.prob = 0.95)
ggpl<-ggpl + scale_color_discrete(name='')
ggpl<-ggpl+theme(legend.direction = 'horizontal',
                 legend.position='top')
print(ggpl)

dim(pca.mtrx$x)
#add a training set with principal components
new_train_data.data <- data.frame(pca.mtrx$x,Class.ASD = my_data$Class.ASD[1:456])

str(new_train_data.data)
dim(new_train_data.data)

new_train_data.data_t<-new_train_data.data[1:20]



logr_vm <-
  glm(Class.ASD ~ PC1+PC2+PC3+PC4+PC5,
      family=binomial(link="logit"),
      data = new_train_data.data)
#transform test into PCA


summary(logr_vm)

-summary(logr_vm)$coef[1,1]/summary(logr_vm)$coef[2,1]
abline(v= 6.393355,lty=2, new=TRUE)

lines(new_train_data.data$PC1,logr_vm$fitted,type="l",col="red")
plot(logr_vm,which=1)

vif(logr_vm)

head(train.data)


train.data <- predict(pca.mtrx, new_train_data)
train.data <- data.frame(train.data,Class.ASD=new_train_data$Class.ASD)
test.data <- predict(pca.mtrx, newdata = new_test_data)
test.data <- data.frame(test.data,Class.ASD=new_test_data$Class.ASD)

identical(train.data,test.data)
summary(test.data)
summary(train.data)
model <-   glm(Class.ASD ~ PC1+PC2+PC3+PC4+PC5+PC7+PC24+PC31,
               family = binomial(link = 'logit'),
               data = train.data)
summary(model)

pp <- predict(model, type = 'response')
tabl<-table(train.data$Class.ASD, pp > 0.5)
tabl
sum(diag(tabl))/sum(tabl)
1-sum(diag(tabl))/sum(tabl)


pp <- predict(model, type = 'response')
tabl<-table(test.data$Class.ASD, pp > 0.5)
tabl
sum(diag(tabl))/sum(tabl)
1-sum(diag(tabl))/sum(tabl)


#logr_vm <- glm(Class.ASD ~ PC1, data=dat, family=binomial(link="logit"))
#make prediction on test data
test.data<-new_test_data
test.data <- predict(pca.mtrx, newdata = test.data)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:20]

test.data$Class.ASD <- predict(logr_vm, newdata=test.data,type="response")
str(test.data$Class.ASD)
test.data$Class.ASD 

library(ggplot2)
ggplot(train.data, aes(x=PC5, y=Class.ASD)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

par(mar = c(4, 4, 1, 1)) # Reduce some of the margins so that the plot fits better


ggplot(test.data, aes(x = PC1, y = Class.ASD, colour = PC2, group = PC2)) + 
  geom_line()


quartz(title="TotalCharges vs. Class.ASD") # creates a quartz window with title

plot(train.data$PC7,train.data$Class.ASD,xlab="PC1",ylab="Probability of customer Class.ASD") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(Class.ASD~PC1,family=binomial,train.data) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
f1 <- function(x) 0.044*x+1.638
f2 <- function(x) 0.3438*x+0.5155
x0 <- seq(0, 1, by = 0.1)
ylim <- range(c(f1(x0), f2(x0)))
curve(f1, ylim = ylim)
curve(f2, add=TRUE, col = "red")
par(mar = c(4, 4, 1, 1)) # Reduce some of the margins so that the plot fits better
curve(predict(g,data.frame(PC1=x),type="resp"),add=TRUE,from=100,to=-100) # draws a curve based on prediction from logistic regression model

points(train.data$PC5,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.


train.data <- predict(pca.mtrx, new_train_data)
train.data <- data.frame(train.data,Class.ASD=new_train_data$Class.ASD)

plot(train.data$PC1,train.data$PC2,col=train.data$Class.ASD,main="PCA")

install.packages("FactoMineR")
install.packages("factoextra")

library("factoextra")
library("FactoMineR")

fviz_cos2(pca.mtrx,choice="var",axes=1:6)

