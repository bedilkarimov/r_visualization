#library----
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
if(!require(nnet)){install.packages("nnet")}
library(nnet)
if(!require(reshape)){install.packages("reshape")}
library(reshape)


#function to do all in one with tidyverse (mainly) and reshape library
tidyverse_unstable <- function(df,target=NULL){
  start <- Sys.time()

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
    if(count_factor >0){print(df %>% keep(is.factor) %>% table) }

    #2) Missing value checking-----
    cat('\nThere are', sum(is.na(df)),'missing values in this dataset\n')
    # graph missing value
    missing_plot <-df  %>%
      is.na %>%
      melt %>%
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
        (count - sum(!complete.cases(df)))/count*100, '% of original data\n')


    #3)basic visualization for data----
    #for numeric
    num_df <- df %>% keep(is.numeric) %>% gather()
    if ((num_df %>% ncol())==0) {
      print('there is no Numeric column')
    } else if ((num_df %>% select(key) %>% n_distinct())==1){
      graph<-num_df %>% ggplot( aes(value)) + geom_histogram() + ggtitle('Numeric column')
      print(graph)
    }else {
      graph<- num_df %>%
        ggplot( aes(value)) + geom_histogram() + ggtitle('Numeric columns') +
        facet_wrap(~ key, scales = "free",  nrow = 3) +
        labs(caption = paste0(num_df %>% select(key) %>% n_distinct(),' columns'))
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
        labs(caption = paste0(fac_df %>% select(key) %>% n_distinct(),' columns'))
      print(graph)
    }


    #4) basic mode----
    print("---------------------------------------------")
    message('\nIf THERE IS an NA in dataset, the model may not run correctly at all\n')

    if('factor' %in% class(df[[target]])){
      message('\nThe dependent variable is factor, multiple logistic regression is used')
      fit<-multinom(as.formula(paste(target,"~.")), data= df, maxit=200)
      print(summary(fit))
      prediction <- predict(fit,type='class')
      real_value <- df[[target]]%>%na.omit
      t<-table(real_value, prediction)
      print(t)
    }
    else if('numeric' %in% class(df[[target]]) || 'integer' %in% class(df[[target]])){
      message('\nThe dependent variable is numeric/integer, linear regression is used')
      fit<-lm(as.formula(paste(target,"~.")), data= df)
      print(summary(fit))
      y_pred <-  predict(fit)
      real_value <- df[[target]]%>%na.omit
      new_df <- bind_cols(real=real_value, as.data.frame(y_pred))
      g<-ggplot(data = new_df, aes(x=real, y=y_pred)) +
        geom_point() +geom_smooth()
      print(g)

    } else {print('the target column class is neither simple factor nor numeric/integer')}
    end = Sys.time()
    message('\nThe whole process took ', end - start,'seconds\n')
}

tidyverse_explorer <- safely(tidyverse_unstable)

tidyverse_explorer(iris,"Species")  
tidyverse_explorer(Titanic,'Class')
tidyverse_explorer(cars, 'dist') 
tidyverse_explorer(mtcars,'mpg') 
tidyverse_explorer(diamonds,'color')
tidyverse_explorer(airquality,'Ozone') #with NA then no table or graph summary

