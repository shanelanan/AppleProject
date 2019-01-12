library(DBI)
library(dplyr)
library(broom)
rm(list = ls())

# system("python etl.py")

# helper function to print huge dataframe
printWideDataFrame <- function(df, n){
  head(df[c(1:n,(ncol(df)-n):ncol(df))])
}

# connect to db, fetch table into RAM, disconnect from db
con <- dbConnect(RSQLite::SQLite(), "warehouse.db")
dfRaw = dbGetQuery(con, "select * from sample")
dbDisconnect(con)

# preview dataframe
printWideDataFrame(dfRaw, 20)

# massage dataframe for statistical calculations
df = dfRaw %>% 
  mutate(
    INSERTED_ON = as.POSIXct(INSERTED_ON),
    MFG_DATE = as.POSIXct(MFG_DATE),
    PASS_FAIL = ifelse(PASS_FAIL==1,0,1)) %>%  # 1 = pass, 0 = fail
  fastDummies::dummy_cols() %>%  # add dummy variables for all string columns
  select(-c(ID, MFG_DATE, MAT_VENDOR, PART_VENDOR, SIL_VENDOR, ADHS_VENDOR, SOP_VENDOR))

# preview dataframe
printWideDataFrame(df, 20)

# impute NAs in dataframe with column medians
for(col in names(df)) {
  # feature columns only (they start with "F")
  if(substr(col,1,1) == "F"){
    df[is.na(df[,col]), col] <- median(df[,col], na.rm = TRUE)
  }
}

# preview dataframe
printWideDataFrame(df, 20)


# mean pass rate
mean(df$PASS_FAIL)

# logistic regression due to binary response variable
# 1 = pass, 0 = fail
fullModel = glm(PASS_FAIL ~ ., data=df, family=binomial())
nullModel = glm(PASS_FAIL ~ 1, data=df, family=binomial())

fwd = step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
# bwd = step(fullModel, direction="backward"), backward not a good choice for high dimensionality problem
both = step(nullModel, scope = list(upper=fullModel), direction="both") 

# show results
fwd$aic
both$aic

# compare methods
if(fwd$aic<both$aic){
  print("Forward selection chosen")
  fwd$formula
  varModel = fwd
}else{
  print("Both selection chosen")
  both$formula
  varModel = both
}

selectedModel = glm(varModel$formula, data=df, family=binomial())

m2 = glm(both$formula, data = df, family = binomial())


  # filter columns for signficant ones only
validColumns = tidy(m) %>%
  filter(p.value <= 0.05)

df = df %>%
  select(c(PASS_FAIL, validColumns$term))

# basicl OLS model
m = lm(PASS_FAIL ~ ., data=df)
summary(m)




