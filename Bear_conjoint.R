library(ggplot2)
library(ggplot2)
library(ggthemes)
library(knitr)
library(broom)
library(GA)

cData <- read.csv(file ="E://backup2//Beer_conjoint_data//Beer_fullData.csv", header=TRUE)
cData$Price<-as.factor(cData$Price)
kable(cData,caption = "Conjoint data for 10 respondents")
xData <- cData[,-10:-1]
Ratings <- cData[,1:10]
Ratings[,3]
attach(cData)
fit <- lm(Ratings[,1]~ Price + Brand + Packaging + Aftertaste)
summary(fit)

get_importance <- function(xData, fit){
  +     get_range<-function(attr, df, fit){
    +         coeff <- c(0)
    +         for(coef in names(fit$coefficients)){
      +             if(grepl(attr, coef)){coeff <- c(coeff, as.numeric(fit$coefficients[coef]))}
              }
    +         
      +         range <- max(coeff) - min(coeff)
      +         return(range = range)
            }
  +     
    +     
    +     Ranges <- c()
    +     for(attr in names(xData)){
      +         range <- get_range(attr = attr,df = cData, fit = fit)
      +         Ranges <- c(Ranges, range)
         }
    +     
      +     imp <- round(100 * Ranges / sum(Ranges),2)
      +     Res <- data.frame(names(xData), Ranges,  imp)
      +     names(Res) <- c('Attribute', 'Range', 'Importance')
      +     return(Res)
          }
library(knitr)
impTable <- get_importance(xData, fit)
kable(impTable, caption = 'Attributes relative importance', align="lcc", digits=2)


g <- ggplot(data = impTable) + geom_bar(mapping = aes(x = Attribute, y = Importance),stat = 'identity', fill="goldenrod1", col = "goldenrod4") + theme_few()
show(g)
newData <- read.csv(file = "E://backup2//Machine_Learning//Beer_conjoint_data//Beer_Products.csv", 
                      +                     header=TRUE)
newData <- read.csv(file = "E://backup2//Beer_conjoint_data//Beer_Products.csv", 
                        +                     header=TRUE)
newData$Price <- as.factor(newData$Price)
kable(newData,
        +       caption = "Products in the market and potential new products")


newData_resp1 <- newData
newData_resp1$Expected_utility <- predict(fit, newData_resp1)
kable(newData_resp1,caption = "Expected utility for product 1")

I <- ncol(Ratings)
I
importances <- matrix(0,nrow = ncol(xData),ncol = I+1)
importances
rownames(importances) <- colnames(xData)
rownames(importances)
colnames(importances) <- c(paste("Respondent",1:I,sep = "_"),"Average")
colnames(importances)

for(i in 1:I){
  +     fit <- lm(Ratings[,i]~ Price + Brand + Packaging + Aftertaste)
  +     #print(summary(fit_list[[i]]))
    +     importances[,i] <- get_importance(xData, fit)[,3]
    +     importances[,i]
    +     newData <- cbind(newData,predict(fit, newData))
    +     newData
    +     colnames(newData)[ncol(newData)] <- paste("Exp_Ut",i,sep = "_")
     }
 importances[,I+1] <- rowMeans(importances)

 kable(newData, 
          +       caption = "Expected utility for all respondents for all products")


kable(importances, 
          +       caption = "Attribute importance for all respondents and average importance")


Exp_ut <- newData[,6:15]
Exp_ut
newData$Scenario_1 <- newData$Available + (1:6==4)
newData$Scenario_1

newData$Scenario_2 <- newData$Available + (1:6==5)
newData$Scenario_2
newData$Scenario_3 <- newData$Available + (1:6==6)
newData$Scenario_3
matrix(apply(Exp_ut[which(newData$Available==1),],2,max),
         +        nrow = 6,ncol = I,byrow = TRUE)
choice_available <- Exp_ut == matrix(apply(Exp_ut[which(newData$Available==1),],2,max),
                                         +                                      nrow = 6,ncol = I,byrow = TRUE)
newData$current_mkt_share<-rowSums(choice_available)/I
choice_1 <- Exp_ut == matrix(apply(Exp_ut[which(newData$Scenario_1==1),],2,max),
                               +                              nrow = 6,ncol = I,byrow = TRUE)
newData$sce_1_mkt_share <- rowSums(choice_1)/I
choice_2 <- Exp_ut == matrix(apply(Exp_ut[which(newData$Scenario_2==1),],2,max),
                               +                              nrow = 6,ncol = I,byrow = TRUE)
newData$sce_2_mkt_share <- rowSums(choice_2)/I
choice_3 <- Exp_ut == matrix(apply(Exp_ut[which(newData$Scenario_3==1),],2,max),
                               +                              nrow = 6,ncol = I,byrow = TRUE)
newData$sce_3_mkt_share <- rowSums(choice_3)/I
kable(newData[,-6:-15],
        +       caption = "Market share for current, and all potential scenarios")


plData <- read.csv(file = "E://backup2//Beer_conjoint_data//Assignment_project//Beer_ProductLine.csv", header=TRUE)
plData <- read.csv(file = "E://backup2//Beer_conjoint_data//Beer_ProductLine.csv", header=TRUE)
plData$Price <- as.factor(plData$Price)
margin <- plData$Margin
kable(plData)
status_quo <-apply(Exp_ut[which(newData$Available==1),],2,max)
P <- nrow(plData)
utils <- matrix(0, nrow = I, ncol = P+1)
colnames(utils) <- c("status_quo",paste("Product",1:P,sep = "_"))
rownames(utils) <- paste("Respondent",1:I,sep = "_")
utils[,1] <- status_quo
for(i in 1:I){
  +     fit <- lm(Ratings[,i]~ Price + Brand + Packaging + Aftertaste)
  +     #print(summary(fit_list[[i]]))
    +     utils[i,2:(P+1)] <- predict(fit, plData)
     }
kable(utils)
profit<-function(offered, utils, margin){
    +     
      +     offered1<-c(1, offered)
      +     
        +     uOffered <- t(apply(utils, 1, function(x)(x * offered1)))
        +     maxUtil <- apply(uOffered, 1, max)
        +     
          +     prodHighest<-matrix(0, nrow(utils), ncol(utils)-1)
          +     
            +     for(i in 1:nrow(utils)){
              +         for(j in 2:ncol(utils)){
                +             if(uOffered[i, j] == maxUtil[i]) {prodHighest[i,j-1]=1;break}
                      }
                  }
          +     profitVec<-apply(prodHighest, 1, function(x){x %*% margin})
          +     sum(profitVec)
           }

obj<-function(offered, utils, margin, numProd){
    +     
      +     pr<-profit(offered, utils, margin)
      +     penalty<-10*max(margin)*abs(sum(offered)-numProd)
      +     
        +     pr-penalty
       }
set.seed(0)
gaOpt<-ga("binary", fitness=obj, utils=utils, margin=margin, numProd=3, nBits=length(margin), maxiter=100)
summary(gaOpt)

sol <- gaOpt@solution
sol
print(paste("Number of solutions:",nrow(sol)))
kable(plData[which(c(sol[1,])==1),])
plot(gaOpt)
t(apply(utils, 1, function(x)(x * c(1,sol[1,]))))
profit(sol[1,], utils, margin)


