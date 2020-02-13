library(rbenchmark)
library(tidyverse)

element1_base <- function(df, target){
  tryCatch({
    
    # 1)baisc descritpive statistics----
    df <- as.data.frame(df)
    str(df)
    cat('\nThe basic summary as follow:')
    print(summary(df))
    
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

element1_tidy <- function(df){
  
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
  # for factor
  count_factor <- df %>% as.data.frame %>% keep(is.factor) %>% ncol
  cat('\nThere are ',count_factor,'factor columns\n')
  if(count_factor >0){print(df %>% keep(is.factor) %>% table)
}}

(compare_mean1 <- benchmark("base"= {m1 <- element1_base(iris)},
                           "tidy"= {m2 <- element1_tidy(iris)},
                           columns = c("test", "replications", "elapsed", "relative"),
                           order = "relative",
                           replications = 50))


element2_base <- function(df, target){
  tryCatch({
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
  par(mfrow=c(1,1))}
  ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


element2_tidy <- function(df){
  #2) Missing value checking-----
  cat('\nThere are', sum(is.na(df)),'missing values in this dataset\n')
  # graph missing value
  missing_plot <-df  %>%
    is.na %>%
    melt %>%  #melting into multiple columns simultaneously. 
    ggplot(data = .,
           aes(x = X2,
               y = X1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Non-missing","Missing")) +
    theme_dark() +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    labs(x = "Columns in Dataset",
         y = "Rows in Dataset")
  print(missing_plot)
  count <- df %>% nrow()
  cat('\nIf we omit all observation with NA, we retain ',
      (count - sum(!complete.cases(df)))/count*100, '% of original data\n')}


(compare_mean2 <- benchmark("base"= {m1 <- element2_base(iris)},
                           "tidy"= {m2 <- element2_tidy(iris)},
                           columns = c("test", "replications", "elapsed", "relative"),
                           order = "relative",
                           replications = 50))


element3_base <- function(df){
  tryCatch({
    par(mar=c(2,2,2,2))
    #for numeric
    par(mfrow=c(2,2))
    nums <- unlist(lapply(df, is.numeric))
    if (sum(nums)>0){
      num_df <- as.data.frame(df[ , nums])
      for (i in seq(1, length(num_df),1)){
        hist(num_df[,i], main= names(num_df)[i])
      }
    }
    par(mfrow=c(1,1))
    cat('\nThere are',sum(nums),'numeric columns\n')
    
    #for factor
    par(mfrow=c(2,2))
    fac <- unlist(lapply(df, is.factor))
    if (sum(fac)>0){
      fac_df <- as.data.frame(df[ , fac])
      for (i in seq(1, length(fac_df),1)){
        barplot(table(fac_df[,i]), main= names(fac_df)[i])
      }
    }
    par(mfrow=c(1,1))
    cat('\nThere are',sum(fac),'factor columns\n')}
    
    ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    }
  )
}

element3_tidy <- function(df){
  num_df <- df %>% keep(is.numeric) %>% gather()
  if ((num_df %>% ncol())==0) {
    print('there is no Numeric column')
  } else if ((num_df %>% select(key) %>% n_distinct())==1){
    graph<-num_df %>% ggplot( aes(value)) + geom_histogram() + ggtitle('Numeric column')
    print(graph)
  }else {
    graph<- num_df %>%
      ggplot( aes(value)) + geom_histogram() + ggtitle('Numeric columns') +
      facet_wrap(~ key, scales = "free",  nrow = 3) +  #wraps a 1d sequence of panels into 2d, better use of screen space
      labs(caption = paste0(num_df %>% dplyr::select(key) %>% n_distinct(),' columns'))  #caption to provide information about the data source
    print(graph)
  }
  
  # for factor
  fac_df <- df %>% keep(is.factor) %>% gather()
  if ((fac_df %>% ncol())==0) {
    print('there is no Factor column ')
  } else if ((fac_df %>% select(key) %>% n_distinct())==1){
    graph<- fac_df %>% ggplot( aes(value)) + geom_bar() + ggtitle('Factor column')
    print(graph)
  }else {
    graph<-fac_df %>%
      ggplot( aes(value)) + geom_bar() + ggtitle('Factor columns') +
      facet_wrap(~ key, scales = "free", nrow = 3) +
      labs(caption = paste0(fac_df %>% dplyr::select(key) %>% n_distinct(),' columns'))
    print(graph)
  }
}
element33_tidy <- safely(element3_tidy)

element3_tidy(iris)

(compare_mean3 <- benchmark("base"= {m1 <- element3_base(iris)},
                           "tidy"= {m2 <- element33_tidy(iris)},
                           columns = c("test", "replications", "elapsed", "relative"),
                           order = "relative",
                           replications = 10))
