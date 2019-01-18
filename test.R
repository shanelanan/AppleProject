
library(DBI)
library(dplyr)
library(broom)
library(ROCR)
library(extraTrees)
library(lubridate)
library(ggplot2)
library(plotly)

rm(list = ls()) # clear all data
try(invisible(dev.off()),silent=TRUE) # clear all plots

# helper function to print huge dataframe
printWideDataFrame <- function(df, n){
  head(df[c(1:n,(ncol(df)-n):ncol(df))])
}
# connect to db, fetch table into RAM, disconnect from db
con <- dbConnect(RSQLite::SQLite(), "warehouse.db")
df_orig <- dbGetQuery(con, "select * from sample") %>%
  mutate(
    INSERTED_ON = as.POSIXct(INSERTED_ON),
    MFG_DATE = as.POSIXct(MFG_DATE),
    PASS_FAIL = ifelse(PASS_FAIL==1,0,1))
dbDisconnect(con)

# preview dataframe
printWideDataFrame(df_orig, 15)

 


# massage for statistics
df_stats <- df_orig %>% 
  fastDummies::dummy_cols() %>%  # add dummy variables for all string columns
  select(-c(ID, INSERTED_ON, MFG_DATE, MAT_VENDOR,
            PART_VENDOR, SIL_VENDOR, ADHS_VENDOR, SOP_VENDOR))  # drop columns

# preview dataframe
printWideDataFrame(df_stats, 15)


# mfg volume vs time
df_orig %>%
  group_by(MFG_DATE = floor_date(MFG_DATE, "day")) %>%
  summarize(
    QTY_PASS = sum(PASS_FAIL==1),
    QTY_FAIL = sum(PASS_FAIL==0)
  ) %>%
  plot_ly(x = ~MFG_DATE, y = ~QTY_PASS, name = "Pass", type = "bar") %>%
  add_trace(y = ~QTY_FAIL, name = 'Fail') %>%
  layout(
    xaxis=list(title='Manufacturing Date'),
    yaxis=list(title='Volume'),
    barmode="stack",
    title='Semi-Conductor Production Volume vs. Time')

# mfg volume vs time (cumulative)
df_orig %>%
  group_by(MFG_DATE = floor_date(MFG_DATE, "day")) %>%
  summarize(
    QTY = n(),
    QTY_PASS = sum(PASS_FAIL==1),
    QTY_FAIL = sum(PASS_FAIL==0)
  ) %>%
  mutate(
    QTY = cumsum(QTY),
    QTY_PASS = cumsum(QTY_PASS),
    QTY_FAIL = cumsum(QTY_FAIL),
    PERC_PASS = QTY_PASS/QTY
  ) %>%
  plot_ly() %>%
  add_trace(x=~MFG_DATE, y=~QTY_PASS, name="Pass", type="bar", yaxis='y1') %>%
  add_trace(x=~MFG_DATE, y=~QTY_FAIL, name='Fail', type="bar", yaxis='y1') %>%
  add_trace(x=~MFG_DATE, y=~PERC_PASS, type='scatter', mode='lines', name='Yield %', yaxis='y2') %>%
  layout(
    xaxis=list(title='Manufacturing Date'),
    yaxis=list(side='left',title='Volume (Cumulative)',
               showgrid=FALSE,zeroline=FALSE),
    yaxis2=list(side='right',overlaying="y",title='Yield %',
                showgrid=FALSE,zeroline=FALSE,tickformat="%"),
    barmode="stack",
    title='Semi-Conductor Production Volume vs. Time (Cumulative)'
  )


# create copy
df1 <- df_stats

# impute NAs in dataframe with column medians
for(col in names(df1)) {
  # feature columns only (they start with "F" and the other digits are numeric)
  if((substring(col,1,1) == "F") && !is.na(as.numeric(substring(col,2)))) {
    df1[is.na(df1[,col]), col] <- median(df1[,col], na.rm = TRUE)
  }
}

# preview dataframe
printWideDataFrame(df1, 15)

# initialize models
m1_full <- glm(PASS_FAIL ~ ., data=df1, family=binomial(), control = list(maxit = 50))
m1_null <- glm(PASS_FAIL ~ 1, data=df1, family=binomial(), control = list(maxit = 50))

# down-select variables
# m1_bwd <- step(m1_full, direction="backward", trace=0)  # not good for high dimensionality problem
m1_fwd <- step(m1_null, scope=list(lower=m1_null, upper=m1_full), direction="forward", trace=0)  # both has lower AIC
m1_both <- step(m1_null, scope = list(upper=m1_full), direction="both", trace=0) # best choice

if(m1_fwd$aic<m1_both$aic){
  print("Forward selection used")
  m1_base_formula = m1_fwd$formula
}else{
  print("Both selection used")
  m1_base_formula = m1_both$formula
}

print(m1_base_formula)

# temp hard code
# m1_base_formula <- as.formula(
#   "PASS_FAIL ~ SIL_VENDOR_eee + F103 + F59 + F21 + F73 + F428 + 
#      F569 + F64 + F75 + F129 + F433 + F365 + F9 + F443 + F473 + 
#  F500 + F368 + F488 + SOP_VENDOR_ggg + F411 + F476 + F38 + 
#  F87 + F104 + F484 + F349 + F84 + F72 + F56 + F554 + F131 + 
#  F511 + F545 + F470 + F410 + F419 + F418 + F32 + SIL_VENDOR_ccc + 
#  SOP_VENDOR_aaa + F320 + F66 + F321 + F94 + F132 + F575"
# 
# )
m1_base <- glm(m1_base_formula, data=df1, family = binomial(), control = list(maxit=50))
