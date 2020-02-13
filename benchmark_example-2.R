library(rbenchmark)

element1_base <- function(df, target){
  tryCatch({
    
    # 1)baisc descritpive statistics----
    df <- as.data.frame(df)
    str(df)
    cat('\nThe basic summary as follow:')
    print(summary(df))
    
    # 2)Missing value checking----
    cat('\nThere are', sum(is.na(df)),'missing values in this dataset\n')
    print(md.pattern(df))
    #using aggr() to findout more about missing value
    #b/c difficult to use base plot only
    aggr(df,
         numbers = TRUE,
         prop = FALSE,
         sortVars = TRUE,
         cex.axis = .5,
         gap = 2,
         ylab = c("Number of misisngs", "Pattern"))
    count <- nrow(df)
    cat('\nIf we omit all observation with NA, we retain ',
        (count - sum(!complete.cases(df)))/count*100, '% of original data\n')
    par(mfrow=c(1,1))
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



element1_tidy <- function(df, target){
  
  # 1)baisc descritpive statistics----
  df <- as.data.frame(df)
  glimpse(df)
  cat('\nThe basic summary as follow:\n')
  count_num <- df %>% keep(is.numeric) %>% ncol
  cat('\nThere are ',count_num,'numeric columns\n')
  # for numeric
  df_sum <- df %>% na.omit %>%
    summarise_if(is.numeric,funs(min = min,
                                 q25 = quantile(., 0.25),
                                 median = median,
                                 q75 = quantile(., 0.75),
                                 max = max,
                                 mean = mean,
                                 sd = sd))
  
  # reshape it using tidyr functions if there are more than 1
  if(count_num > 1){
    df_stats_tidy <- df_sum %>% gather(stat, val) %>%
      separate(stat, into = c("column", "stat"), sep = "_") %>%
      spread(stat, val)
    print(df_stats_tidy)}
  else {print(df_sum)}
  
}

x<-benchmark('base'= element1_base(iris,'Species'),
             'tidy' = element1_tidy(iris,'Species'))  
