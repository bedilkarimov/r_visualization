#library
if(!require(nnet)){install.packages("nnet")}
library(nnet)
if(!require(mice)){install.packages("mice")}
library(mice)
if(!require(VIM)){install.packages("VIM")}
library(VIM)



#base function
base_explorer <- function(df, target){
  start <- Sys.time()
  tryCatch({

    # 1)baisc descritpive statistics----
    df <- as.data.frame(df)
    str(df)
    cat('\nThe basic summary as follow:\n')
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
   
  #3) Visualization----
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


  #4) basic mode----
  tryCatch({
    print("---------------------------------------------")
    message('\nIf THERE IS any NA in dataset, the model may not run correctly at all\n')

    if('factor' %in% class(df[[target]])){
      message('\nThe dependent variable is factor, multiple logistic regression is used')
      fit<-multinom(as.formula(paste(target,"~.")), data= df, maxit=200)
      print(summary(fit))
      prediction <- predict(fit,type='class')
      real_value <- na.omit(df[[target]])
      t<-table(real_value, prediction)
      print(t)
    }
    else if('numeric' %in% class(df[[target]]) || 'integer' %in% class(df[[target]])){
      message('\nThe dependent variable is numeric/integer, linear regression is used')
      fit<-lm(as.formula(paste(target,"~.")), data= df)
      print(summary(fit))
      y_pred <-  predict(fit)
      real_value <- na.omit(df[[target]])
      new_df <- cbind(real=real_value, as.data.frame(y_pred))
      g <- plot(real_value, y_pred)
      abline(lm(real_value ~ y_pred))
      print(g)

    } else {print('the target column class is neither simple factor nor numeric/integer')}
  }
  ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  end = Sys.time()
  cat('\nThe whole process took ', end - start,'seconds\n')
}

base_explorer(iris,'Species')
base_explorer(Titanic,'Class')
base_explorer(cars, 'dist') 
base_explorer(mtcars,'mpg') 
base_explorer(diamonds,'color')
base_explorer(airquality,'Ozone') #with NA then no table or graph summary
