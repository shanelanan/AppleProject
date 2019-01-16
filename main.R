library(DBI)
library(dplyr)
library(broom)
library(ROCR)
library(extraTrees)

rm(list = ls()) # clear all data
try(dev.off(),silent=TRUE) # clear all plots

# system("python etl.py")  # call python ETL script
pdf(file='./docs/Rplots.pdf',width=10,height=7.5)  # begin pdf writer

# helper function to print huge dataframe
printWideDataFrame <- function(df, n){
  head(df[c(1:n,(ncol(df)-n):ncol(df))])
}

# connect to db, fetch table into RAM, disconnect from db
con <- dbConnect(RSQLite::SQLite(), "warehouse.db")
df_orig <- dbGetQuery(con, "select * from sample") %>%
  mutate(INSERTED_ON = as.POSIXct(INSERTED_ON), MFG_DATE = as.POSIXct(MFG_DATE))
dbDisconnect(con)

# preview dataframe
printWideDataFrame(df_orig, 20)

# massage for statistics
df <- df_orig %>% 
  mutate(PASS_FAIL = ifelse(PASS_FAIL==1,0,1)) %>%  # 1 = pass, 0 = fail
  fastDummies::dummy_cols() %>%  # add dummy variables for all string columns
  select(-c(ID, INSERTED_ON, MFG_DATE, MAT_VENDOR, PART_VENDOR, SIL_VENDOR, ADHS_VENDOR, SOP_VENDOR))  # drop columns

# preview dataframe
printWideDataFrame(df, 20)


##########################################################
# Model 1: Logistic Regression
# Starting with a simple approach
##########################################################

# create copy
df1 <- df

# impute NAs in dataframe with column medians
for(col in names(df1)) {
  # feature columns only (they start with "F" and the other digits are numeric)
  if((substring(col,1,1) == "F") && !is.na(as.numeric(substring(col,2)))) {
    df1[is.na(df1[,col]), col] <- median(df1[,col], na.rm = TRUE)
  }
}

# preview dataframe
printWideDataFrame(df1, 20)

# # initialize models
# m1_full = glm(PASS_FAIL ~ ., data=df1, family=binomial(), control = list(maxit = 50))
# m1_null = glm(PASS_FAIL ~ 1, data=df1, family=binomial(), control = list(maxit = 50))
# 
# # down-select variables
# # m1_bwd = step(m1_full, direction="backward"), backward not a good choice for high dimensionality problem
# m1_fwd = step(m1_null, scope=list(lower=m1_null, upper=m1_full), direction="forward")
# m1_both = step(m1_null, scope = list(upper=m1_full), direction="both")
# 
# # compare methods
# if(m1_fwd$aic<m1_both$aic){
#   print("Forward selection chosen")
#   m1_varModel = m1_fwd
# }else{
#   print("Both selection chosen")
#   m1_varModel = m1_both
# }
# m1_formula <- m1_varModel$formula
# m1_formula

# # BOTH SELECTED, TODO
m1_formula <- as.formula(
"PASS_FAIL ~ SIL_VENDOR_eee + F103 + F59 + F21 + F73 + F428 +
F569 + F64 + F75 + F129 + F433 + F365 + F9 + F443 + F473 +
F500 + F368 + F488 + SOP_VENDOR_ggg + F411 + F476 + F38 +
F87 + F104 + F484 + F349 + F84 + F72 + F56 + F554 + F131 +
F511 + F545 + F470 + F410 + F419 + F418 + F32 + SIL_VENDOR_ccc +
SOP_VENDOR_aaa + F320 + F66 + F321 + F94 + F132 + F575"
)

m1_base = glm(m1_formula, data=df1, family=binomial(), control = list(maxit = 50))
summary(m1_base)


##########################################################
# Model 2: Extremely Random Trees
# Handles NAs gracefully
# Handles noisey data and high dimensionality
##########################################################

# create copy
df2 <- df

# preview dataframe
printWideDataFrame(df2, 20)

# run model and summarize
m2_base = extraTrees(df2 %>% select(-PASS_FAIL),df2$PASS_FAIL,numRandomCuts=1,na.action="fuse")
m2_base


##########################################################
# Cross Validation
##########################################################


n = 5  # number of folds
df = df[sample(nrow(df)),]  # Randomly shuffle the data
folds = cut(seq(1,nrow(df)),breaks=n,labels=FALSE)  # Create 5 equally size folds

# create empty matrix for accuracy and precision
accuracy = matrix(data=NA,nrow=n,ncol=2)
precision = matrix(data=NA,nrow=n,ncol=2)
cutoff = 0.50

# Perform 5 fold cross validation
for(i in 1:n){
  # Segment the data by fold using the which() function 
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = df[testIndexes, ]
  trainData = df[-testIndexes, ]
  
  # model 1: logistic regression  
  m1 = glm(m1_formula,data=trainData,family='binomial',control=list(maxit=50))
  p1 = predict(m1,newdata=testData,type='response')
  pr1 = prediction(p1,testData$PASS_FAIL)
  prf1 = performance(pr1,measure="tpr",x.measure="fpr")
  prec1 = performance(pr1,measure="prec")
  acc1 = performance(pr1,measure="acc")
  auc1 = performance(pr1,measure="auc")
  
  # model 2: extremely random forest
  m2 = extraTrees(trainData %>% select(-PASS_FAIL),trainData$PASS_FAIL,numRandomCuts=1,na.action="fuse")
  p2 = predict(m2,testData %>% select(-PASS_FAIL))
  pr2 = prediction(p2,testData$PASS_FAIL)
  prf2 = performance(pr2,measure="tpr",x.measure="fpr")
  prec2 = performance(pr2,measure="prec")
  acc2 = performance(pr2,measure="acc")
  auc2 = performance(pr2,measure="auc")
  
  # graph results
  par(pty="s")
  plot(prf1,main=paste('ROC: Fold ',i,sep=''),xaxs='i',yaxs='i',asp=1)
  lines(prf2@x.values[[1]],prf2@y.values[[1]],col='red')
  abline(a=0,b=1,lty=2)
  legend('bottomright',
         c(paste('Model 1 | AUC=',format(round(auc1@y.values[[1]],3),3),sep=''),
           paste('Model 2 | AUC=',format(round(auc2@y.values[[1]],3),3),sep='')),
         col=c('black','red'),lty=c(1,1))

  par(pty="m")
  plot(prec1,main=paste('Precision: Fold ',i,sep=''),ylim=c(0.4,1))
  lines(prec2@x.values[[1]],prec2@y.values[[1]],col='red')
  abline(v=0.5,lty=2)
  legend('topleft',c('Model 1','Model 2'),col=c('black','red'),lty=c(1,1))

  plot(acc1,main=paste('Accuracy: Fold ',i,sep=''),ylim=c(0.4,1))
  lines(acc2@x.values[[1]],acc2@y.values[[1]],col='red')
  abline(v=0.5,lty=2)
  legend('topleft',c('Model 1','Model 2'),col=c('black','red'),lty=c(1,1))

  accuracy[i,1] = acc1@y.values[[1]][max(which(acc1@x.values[[1]]>=cutoff))]
  accuracy[i,2] = acc2@y.values[[1]][max(which(acc2@x.values[[1]]>=cutoff))]

  precision[i,1] = prec1@y.values[[1]][max(which(prec1@x.values[[1]]>=cutoff))]
  precision[i,2] = prec2@y.values[[1]][max(which(prec2@x.values[[1]]>=cutoff))]
  
}

dev.off()  # close pdf writer

## TODO, INFER ALL CONCLUSIONS

# defined as null hypothesis: m1-m2=0
accuracy_test = t.test(accuracy[,1],accuracy[,2],conf.level=0.95,paired=T)
precision_test = t.test(precision[,1],precision[,2],conf.level=0.95,paired=T)

accuracy
accuracy_test
# Model 1 & 2 are not significantly more or less accurate than one another.

precision
precision_test
# Model 2 is more precise than Model 1.





