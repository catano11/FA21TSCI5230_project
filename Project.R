

##' 3. There is a data set called swiss data in R, standardized fertility measure
##' and socio-economic indicators for each of 47 French-speaking provinces of 
##' Switzerland at about 1888.

data(swiss) #importing the data 
##help(swiss) 
head(swiss) 


##'  Perform a principal component analysis with the data.

##'   (i) What proportion of variance is explained by the first and second 
##'   component, respectively? 
##' 
##'   The first principal component explains 53.3% of the variance in the data 
##'   and the second principal component explains 19.8% of the variance  

apply(swiss , 2, mean)  #examining means differences among the variables

apply(swiss , 2, var)  #examining variance differences among the variables

pr.out <- prcomp(swiss , scale = TRUE)

names(pr.out)

pr.out$center     # means of variables
pr.out$scale      # standard deviations of variables
pr.out$rotation   #principal component loading vectors


dim(pr.out$x)

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out , scale = 0)

pr.out$sdev   # standard deviation of each principal component
pr.var <- pr.out$sdev^2
pr.var   # variance explained by each principal component 


pve <- pr.var / sum(pr.var)
pve   # Proportion of variance explained by each principal component
   
##'   (ii) Determine the number of principal components with a scree plot.
##'   
par(mfrow = c(1, 2))
plot(pve , xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


##'     
##'   (iii) Create a biplot with the PCA results. What do you see from the plot?
##'
  
biplot(pr.out , scale = 0)



##'   The biplot represents the principal component scores and principal 
##'   component loadings.  Factors such Agriculture, Catholic, Fertility and
##'   Infant Mortality score positive on the first component , while 
##'   Agriculture has the larger positive association with the second component. 
##'   Examination and Education loadings are very close to each other since 
##'   they are correlated. Catholic and Fertility are also close, meaning they are 
##'   correlated as well.
##'   
##'   
##'   (iv) Plot the first two principal components score vectors, with each 
##'   scatter point colored according to its cluster assignment generated from 
##'   a K-means clustering with K=2
   
km.out <- kmeans(pr.out$rotation[,1:2], 2, nstart = 50)

km.out$cluster


plot(pr.out$rotation[,1:2], col = (km.out$cluster + 1),
       main = "K-Means Clustering Results with K = 2",
       xlab = "", ylab = "", pch = 20, cex = 2)

#' The K-means clustering perfectly separated the observations into two clusters.
#' The cluster 1 (red color) is shown too the left, and the cluster 2 (green color)
#' is shown to the right



## 4. Textbook page 552, Chapter 12, Exercise 13
#  13. On the book website, www.statlearning.com, there is a gene expression
#  data set (Ch12Ex13.csv) that consists of 40 tissue samples with
#  measurements on 1,000 genes. The first 20 samples are from healthy
#  patients, while the second 20 are from a diseased group.

##  (a) Load in the data using read.csv(). You will need to select header = F.

dat0 <- read.csv('C:/Users/GABE/Desktop/CBDS/Ch12Ex13.csv', header = F)  

data.tr <- t(as.matrix(dat0))
sd.data <- scale(data.tr)


#    (b) Apply hierarchical clustering to the samples using correlation based
#    distance, and plot the dendrogram. Do the genes separate
#    the samples into the two groups? Do your results depend on the
#    type of linkage used?

par(mfrow = c(1, 1))

data.dist <- as.dist(1 - cor(t(sd.data)))

plot(hclust(data.dist, method = "complete"), 
     xlab = "", sub = "", ylab = "", 
     main = "Complete Linkage \n with Correlation-Based Distance")


plot(hclust(data.dist, method = "average"), 
     xlab = "", sub = "", ylab = "", 
     main = "Average Linkage \n with Correlation-Based Distance")


plot(hclust(data.dist, method = "single"), 
     xlab = "", sub = "", ylab = "", 
     main = "Single Linkage \n with Correlation-Based Distance")

##' ANSWER: The gene expression patterns clearly separated the samples into 
##' two groups (Group 1= samples 1 to 20; Group 2= samples 21 to 40).
##' The separation can be seen when other types of clustering where used, 
##' such as average linkage or single linkage.

#   (c) Your collaborator wants to know which genes differ the most
#   across the two groups. Suggest a way to answer this question,
#   and apply it here.

##' Testing the mean difference in gene expression between the two groups. I 
##' performed a set of t tests and selected the lowest p-values from those 
##' comparisons as shown below.

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out , 2)
hc.clusters   # creating clusters by tissue origin

table(hc.clusters)
library(magrittr)
library(dplyr)
library(glmnet)
library(caret)
#install.packages("vip")
library(vip)

dat0.cluster <- as.data.frame(data.tr) %>% mutate(cluster=hc.clusters)


x<- as.matrix(dat0.cluster[, 1:1000])
y<- as.matrix(dat0.cluster[,1001])

cv.out <- cv.glmnet(x,y, family="binomial")
cv.out

vi1 <- vip::vi_model(
cv.out,
  s = lambda.min
)
vip::vip(vi1)

p.values <- rep(0,1000)
for(i in 1:1000){ 
  p.values[i] <- t.test(dat0.cluster[,i], dat0.cluster[,1001])$p.value
  }
gene_significant <- which(p.values<0.05/1000) #organizing genes by importance 
## after using Bonferroni correction (1000 comparisons)
gene_significant


gene <- which(p.values<0.05/10^15)
gene


##' ANSWER: The vector "gene" contains the 9 most significant genes differing 
##' between the two groups, this is, those at the positions 141, 148, 203, 277, 
##' 359, 447, 665, 710 and 997. These genes had the lowest p-values after a 
##' cut point of 0.05/10^15 for alpha. Other cut points provide either too many
##' or too few genes to be of any utility to the collaborator.  Please note that 
##' this calculation is too stringent even when compared with Bonferroni correction.  




## 5. Textbook page 594, Chapter 13, Exercise 7
## This problem makes use of the Carseats dataset in the ISLR2 package.

##install.packages("ISRL2")
library(ISLR2)

attach(Carseats)  #importing the dataset


#' (a) For each quantitative variable in the dataset besides Sales, fit a linear
#' model to predict Sales using that quantitative variable. Report the p-values
#' associated with the coefficients for the variables. That is, for each
#' model of the form Y = B0 + B1X + e, report the p-value associated with the 
#' coefficient B1. Here, Y represents Sales and X represents one of the other 
#' quantitative variables.
#' 
head(Carseats)

#individual linear models for quantitative variables
model_CompPrice <- lm(Sales~CompPrice, data=Carseats)
model_Income <- lm(Sales~Income, data=Carseats)
model_Advertising <- lm(Sales~Advertising, data=Carseats)
model_Population <- lm(Sales~Population, data=Carseats)
model_Price <- lm(Sales~Price, data=Carseats)
model_Age <- lm(Sales~Age, data=Carseats)
model_Education <- lm(Sales~Education, data=Carseats)

#results of individual linear models
summary(model_CompPrice)
summary(model_Income)
summary(model_Advertising)
summary(model_Population)
summary(model_Price)
summary(model_Age)
summary(model_Education)

#summary of individual linear models
variable<- c("CompPrice", "Income", "Advertising", "Population", "Price", "Age",
             "Education")
Coefficients <- c(model_CompPrice$coefficients[2], model_Income$coefficients[2]
                  ,model_Advertising$coefficients[2],model_Population$coefficients[2],
                  model_Price$coefficients[2], model_Age$coefficients[2], 
                  model_Education$coefficients[2])
pvalues <- c(summary(model_CompPrice)$coefficients[2,4],summary(model_Income)$coefficients[2,4],
              summary(model_Advertising)$coefficients[2,4],summary(model_Population)$coefficients[2,4],
              summary(model_Price)$coefficients[2,4], summary(model_Age)$coefficients[2,4],
              summary(model_Education)$coefficients[2,4])
data.frame(variable, Coefficients, pvalues)



#' (b) Suppose we control the Type I error at level alpha = 0.05 for the p-values 
#' obtained in (a). Which null hypotheses do we reject?
#' 
#' Control of error rate by Bonferroni method
sales.df <- Carseats[, c(1:6,8,9)] #removing non-quantitative variables

sales.pvalue0 <- rep(0, 8)
  for (i in 2:8)
    sales.pvalue0[i] <- summary(lm(Sales~., data=sales.df[ ,c(1,i)]))$coefficients[,4][2]


sales.pvalue <- sales.pvalue0[2:8] # vector of p values
sales.pvalue

p.adjust(sales.pvalue , method = "bonferroni")
pmin(sales.pvalue * 7, 1)    #correction by Bonferroni for 7 hypotheses


#' ANSWER: we reject the null hypotheses H02: coeff =0 (ComPrice); HO3: coeff =0 (Income);
#' H05: coeff=0 (Price) and H06: coeff=0 (Education).This means, the price 
#' charged bu a competitor at each location (ComPrice), the community income 
#' level (Income), the population size of the region(Population) and the price 
#' the company charges for car seats at each site (Price) have a significant 
#' effect in Sales after Bonferroni correction.
#' 
#' 
#' (c) Now suppose we control the FWER at level 0.05 for the p-values.
#' Which null hypotheses do we reject?
#' 
p.adjust(sales.pvalue , method = "holm")
p.adjust(sales.pvalue , method = "holm")<=0.05

#' ANSWER: Similarly to the Bonferroni correction, the FWER using the Holm method
#' reject the same null hypotheses, this is, H02: coeff =0 (ComPrice); 
#' HO3: coeff =0 (Income); H05: coeff=0 (Price) and H06: coeff=0 (Education).

#' 
#' (d) Finally, suppose we control the FDR at level 0.2 for the p-values.
#' Which null hypotheses do we reject?

q.values.BH <- p.adjust(sales.pvalue , method = "BH")

q.values.BH
q.values.BH<=0.2

sum(q.values.BH <= .2)

#' ANSWER: The FDR at the level of 0.2 reject the same null hypotheses as the 
#' previous methods, this is, H02: coeff =0 (ComPrice); HO3: coeff =0 (Income);
#' H05: coeff=0 (Price) and H06: coeff=0 (Education), when the Benjamini-Hochberg 
#' Procedure was used.




