rm(list = ls())

library(haven)
library(rstudioapi)
library(ggplot2)
library(doBy)
library(lfe)



                              #### Define variables and network structures ####


# Find connected sets
cs <- compfactor(list(factor(hp_installed$store_id), factor(df$manager_id)))
df$connected_set <- as.numeric(as.character(cs))

keep <- c("store_id", "manager_id", "connected_set")
df_cs <- df[,(names(df) %in% keep)]
#df_cs %>% distinct()
df_cs <- unique(df_cs)

# rank connected sets by size (# manager-store pairs)
cs_list <- list(df_cs$connected_set)
t <- as.data.frame(table(cs_list), responseName = "freq")
t <- t[order(-t$freq),]
t$cs_rank <- seq.int(nrow(t))
t <- t[, names(t) %in% c("cs_list", "cs_rank")]

df_cs <- merge(df_cs, t, by.x="connected_set", by.y="cs_list", all=TRUE)
df <- merge(df, t, by.x="connected_set", by.y="cs_list", all=TRUE)

# Save results to Stata
write_dta(df_cs, "A_connected.dta")


                              #### Estimate fixed effects ####

reg <- felm(log_prod ~ 1 | manager_id + store_id + time, data=df, cmethod='reghdfe')
print(summary(reg))

# Recover estimates
mng_store_fe <- getfe(reg, se=TRUE) #, robust=TRUE)

store_fe <- subset(mng_store_fe, fe == 'store_id', select=c('effect','se','comp','idx'))
names(store_fe) <- c('fe', 'fe_se', 'connected_set', 'store_id')

manager_fe <- subset(mng_store_fe, fe == 'manager_id', select=c('effect','se','comp','idx'))
names(manager_fe) <- c('fe', 'fe_se', 'connected_set', 'manager_id')

# set zeros to missing
store_fe[store_fe['fe'] == 0, c('fe', 'fe_se')] <- NA
manager_fe[manager_fe['fe'] == 0, c('fe', 'fe_se')] <- NA

# Save results to Stata
indx <- sapply(store_fe, is.factor)
store_fe[indx] <- lapply(store_fe[indx], function(x) as.numeric(as.character(x)))
write_dta(store_fe, "A_store_fe.dta")

indx <- sapply(manager_fe, is.factor)
manager_fe[indx] <- lapply(manager_fe[indx], function(x) as.numeric(as.character(x)))
write_dta(manager_fe, "A_manager_fe.dta")


                              #### Variance-covariance decomposition ####

# Subtract time FE from log_prod
time_fe <- subset(mng_store_fe, fe == 'time', select=c('effect','idx'))
names(time_fe) <- c('time_fe', 'time')

df2 <- merge(df, time_fe, by = 'time')
df2$log_prod_time <- df2$log_prod - df2$time_fe

# Subset data into connected components
for (j in 1:8) {
  
  cat("\nComputing covariance in set rank =", j)
  cat("\n")
  
  # subset to largest connected set (rank = j)
  lcs <- j
  df_sub <- subset(df2, cs_rank == lcs)
 
  # Fixed effects inside LCS
  reg_sub <- felm(log_prod_time ~ 1 | manager_id + store_id, data=df_sub, cmethod='reghdfe')
  fe_sub <- getfe(reg_sub, se=TRUE) #, robust=TRUE)
  
  # save printed output to file
  setwd(tables_dir)
  file_name <- paste(j, "cs_covar.txt", sep = "-")
  sink(file_name)
  
  print(summary(reg_sub))
  
  # Bias corrected var-covar matrix
  covar <- fevcov(reg_sub, fe_sub) #, robust=TRUE, tol=0.01)
  
  covar_biased <- covar + attr(covar, 'bias')
  attr(covar_biased, 'bias') <- NULL
  
  # correlations
  corr <- cov2cor(covar)
  attr(corr, 'bias') <- NULL

  corr_biased <- cov2cor(covar_biased)
  
  # Total variance
  total_var <- var(df_sub$log_prod_time)
  
  #### check var(log_prod_hat)... why is var(m) + var(s) + 2cov(m,s) > var(prod)?
  
  # Shares
  var_share <- covar / total_var
  attr(var_share, 'bias') <- NULL
  
  
  # save output
  cat("\n===========================")
  cat("=### Total Variance ###=")
  cat("===========================\n")
  print(total_var) 
  
  cat("\n===========================")
  cat("=### Covariance Matrix ###=")
  cat("===========================\n")
  print(covar)
  
  cat("\n=========================")
  cat("### Covariance - Biased ###")
  cat("=========================\n")
  print(covar_biased)

  cat("\n============================")
  cat("====### Correlation ###=====")
  cat("============================\n")
  print(corr)

  cat("\n============================")
  cat("### Correlation - Biased ###")
  cat("============================\n")
  print(corr_biased)
  
  cat("\n============================")
  cat("### Variance Share ###")
  cat("============================\n")
  print(var_share)
  
  sink()
}


# #### Plots ####
# 
# setwd(figures_dir)
# 
# # Number of connected sets and stores/managers per set
# keep <- c("store_id", "connected_set")
# df_cs_s <- df_cs[,(names(df_cs) %in% keep)]
# df_cs_s <- unique(df_cs_s)
# df_cs_s$num_stores <- 1
# df_cs_s <- summaryBy(num_stores ~ connected_set, FUN=sum, data=df_cs_s)
# 
# keep <- c("manager_id", "connected_set")
# df_cs_m <- df_cs[,(names(df_cs) %in% keep)]
# df_cs_m <- unique(df_cs_m)
# df_cs_m$num_managers <- 1
# df_cs_m <- summaryBy(num_managers ~ connected_set, FUN=sum, data=df_cs_m)
# 
# df_cs_sm <- merge(df_cs_s, df_cs_m, by='connected_set', all=TRUE)
# df_cs_sm <- df_cs_sm[order(df_cs_sm$num_managers.sum),]
# df_cs_sm$ordered_id <- length(df_cs_sm$num_managers.sum):1
# 
# 
# # plot
# p <- ggplot(df_cs_sm) + geom_point(aes(x=ordered_id, y=num_stores.sum, color='num_stores'), shape=16) +
#   geom_point(aes(x=ordered_id, y=num_managers.sum, color='num_managers'), shape=16) +
#   labs(x="Connected Set", y="Number of Managers/Stores") + theme_bw() +
#   scale_color_manual(name="", labels=c("Managers", "Stores"), 
#                      values=c('num_managers'='blue', 'num_stores'='red')) +
#   theme(legend.position="bottom", legend.key = element_blank(), 
#         legend.background = element_rect(fill="white", size=0.5, linetype="solid", 
#                                          colour ="black")) + 
#   scale_y_continuous(breaks=c(0,5,10,15,20))
# 
# ggsave("num_stores_mng_cs.png", plot=p, device="png", dpi=300, width = 18, 
#        height = 12, units = "cm")
# print(p)
