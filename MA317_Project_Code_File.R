

                  #Packages required
                    install.packages('gapminder')
                    install.packages('finalfit')
                    install.packages('Hmisc')
                    install.packages('ggpubr')
                    install.packages('psych')
                    install.packages("mice")
                    install.packages('faraway')
                    install.packages('corrplot')
                    install.packages('mlbench')
                    install.packages('caret')
                   
                    
                  
                  #loading libraries
                    library(ggplot2)
                    library(finalfit)  #package for finishing tabulation %  reference: https://finalfit.org/
                    library(gapminder) #package for finding missing values %  reference: https://cran.r-project.org/web/packages/gapminder/README.html
                    library(Hmisc)  
                    library("ggpubr") # package must be installed first
                    library(psych)
                    library(mice)    #reference https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4701517/
                    library(VIM)     #reference https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4701517/
                    library(faraway)
                    library(corrplot)
                    library(mlbench)
                    library(caret)
                    
                  #load in data
                  life_expectancy <- read.csv("Life_Expectancy_Data1 _1.csv")
                  
                  ## List characteristics of the dataframe
                  head(life_expectancy)
                  str(life_expectancy)
                  colnames(life_expectancy) 
                  
                 
                  #1) we will remove country name and country code columns    1) Country Code    2) Country Name
                  
                  #saving continent in another variable for reusing in question5
                  var_continent <- life_expectancy$Continent;
                  
                  #Three columns have been eliminated as country and country codes are unique 
                  #Removed EG.FEC.RNEW.ZS as all the values are null/NA's
                  life_expectancy <- life_expectancy[, -c(1,2,3, 25)]  
                  
                  missing_glimpse(life_expectancy) # missing data for each variable, we will remove any variable in which missing values are more than 60%
                  
                 
                  # Following columns are to be removed as they contain more than 80% missing data points
                  # "SE.PRM.CUAT.ZS" 83.4
                  # "SE.TER.CUAT.BA.ZS" 82.5
                  # "SE.ADT.LITR.ZS" 88.5
                  # "SI.POV.LMIC"89.9 
                  
                  life_expectancy <- life_expectancy[, !(colnames(life_expectancy) %in% c("SE.PRM.CUAT.ZS","SE.TER.CUAT.BA.ZS","SE.ADT.LITR.ZS", "SI.POV.LMIC"))]
                 
               
                 
                  #histogram
                  #Histograms are the  exploratory plots 
                  #because they show densities of data and can assist in providing better distributional information. 
               
                  hist.data.frame(life_expectancy)
                  
                  #Normal distribution : SP.DYN.LE00.IN , SP.POP.GROW , NY.ADJ.NNTY.KD.ZG ,FR.INR.RINR ,SH.XPD.CHEX.GD.ZS , 
                  
                  #Positively skewed :   SH.HIV.INCD.14 , SE.PRM.UNER, SP.DYN.IMRT.IN, EN.POP.DNST , SP.POP.TOTL, SH.XPD.CHEX.PC.CD, SL.UEM.TOTL.NE.ZS ,
                                         #NY.GDP.MKTP.KD.ZG, NY.GDP.PCAP.CD, SP.DYN.CBRT.IN, SH.HIV.INCD
                  
                  #Negatively skewed : EG.ELC.ACCS.ZS ,NY.ADJ.NNTY.PC.KD.ZG , SE.PRM.CMPT.ZS , SH.H2O.SMDW.ZS,
                  
                  # After analyzing features, following could be converted to normal distribution by either applying log or taking sqrt(square root)
                 
                  
                  #examining distribution of dependent variable i.e. SP.DYN.LE00.IN
                  ggplot(life_expectancy, aes(SP.DYN.LE00.IN)) + geom_density(fill="blue")  # dependent variable distribution
                  ggplot(life_expectancy, aes(log(SP.DYN.LE00.IN))) + geom_density(fill="blue")
                  ggplot(life_expectancy, aes(sqrt(SP.DYN.LE00.IN))) + geom_density(fill="blue")
                  
                  
                  # Following lines of code is used to check the distribution of features
                  ggplot(life_expectancy, aes(SH.H2O.SMDW.ZS)) + geom_density(fill="green")  # dependent variable distribution
                  ggplot(life_expectancy, aes(log(SH.H2O.SMDW.ZS))) + geom_density(fill="green")
                  ggplot(life_expectancy, aes(sqrt(SH.H2O.SMDW.ZS))) + geom_density(fill="green")
                  
                  
                  #Following features have been transformed by applying either log or square root (sqrt)
                  life_expectancy$SH.XPD.CHEX.GD.ZS <- log(life_expectancy$SH.XPD.CHEX.GD.ZS)   # for normal distribution
                  life_expectancy$SP.DYN.IMRT.IN <- log(life_expectancy$SP.DYN.IMRT.IN)  
                  life_expectancy$EN.POP.DNST <- log(life_expectancy$EN.POP.DNST)
                  life_expectancy$SP.POP.TOTL <- log(life_expectancy$SP.POP.TOTL)
                  life_expectancy$SH.XPD.CHEX.PC.CD <- log(life_expectancy$SH.XPD.CHEX.PC.CD)
                  life_expectancy$SL.UEM.TOTL.NE.ZS <- sqrt(life_expectancy$SL.UEM.TOTL.NE.ZS)
                  life_expectancy$NY.GDP.PCAP.CD <- log(life_expectancy$NY.GDP.PCAP.CD)
                  life_expectancy$SP.DYN.CBRT.IN <- log(life_expectancy$SP.DYN.CBRT.IN)
                  
                  #Now we are checking distribution again, we can see now that most of the data is normally distributed
                  hist.data.frame(life_expectancy)
                  
                  
                  #density plot  allow to analyze the spread and the shape of the distribution
                  plot(density(life_expectancy$SH.XPD.CHEX.PC.CD, na.rm = TRUE))
                  plot(density(life_expectancy$SP.DYN.IMRT.IN, na.rm = TRUE))
                  plot(density(life_expectancy$SL.UEM.TOTL.NE.ZS, na.rm = TRUE))
                  plot(density(life_expectancy$NY.GDP.PCAP.CD, na.rm = TRUE))
                  plot(density(life_expectancy$SP.DYN.CBRT.IN, na.rm = TRUE))
                  
                  
                  ggdensity(life_expectancy$NY.GDP.PCAP.CD,
                            main = "Density plot of POP.Grow",
                            xlab = "POP.Grow"
                  )
                  ggdensity(life_expectancy$SL.UEM.TOTL.NE.ZS,
                            main = "Density plot of POP.Grow",
                            xlab = "POP.Grow"
                  )
                  
                  #Description of qq plots
                  ggqqplot(life_expectancy$SH.XPD.CHEX.PC.CD)
                  qqPlot(life_expectancy$SL.UEM.TOTL.NE.ZS)
                  
                  qqnorm(life_expectancy$SP.DYN.CBRT.IN, pch = 1, frame = FALSE)
                  qqline(life_expectancy$SP.DYN.CBRT.IN, col = "red", lwd = 2)
                  
                  qqnorm(life_expectancy$SH.XPD.CHEX.PC.CD, pch = 1, frame = FALSE)
                  qqline(life_expectancy$SH.XPD.CHEX.PC.CD, col = "red", lwd = 2)
                  
                  #scatterplot 
                  #Often you will want to see how to numeric variables relate to each other, and scatterplot (simply plot())
                  #From scatter analyzes we can see that most of the variables show positive or negative correlation
                  
                  #examining correlation between indendepndt and dependent variables
                  
                  plot(life_expectancy$SP.DYN.LE00.IN ~ life_expectancy$SH.XPD.CHEX.GD.ZS)
                  
                  plot(life_expectancy$SP.DYN.LE00.IN ~ life_expectancy$SP.POP.GROW)
                  
                  plot(life_expectancy$SP.DYN.LE00.IN ~ life_expectancy$SP.DYN.IMRT.IN)
                  
                  ggplot(data = life_expectancy) +
                    geom_point(mapping = aes(x = EG.ELC.ACCS.ZS, y = SP.DYN.LE00.IN))
                  
                  ggplot(data = life_expectancy) +
                    geom_point(mapping = aes(x = EN.POP.DNST, y = SP.DYN.LE00.IN))
                  
                  ggplot(data = life_expectancy) +
                    geom_point(mapping = aes(x = NY.GDP.PCAP.CD, y = SP.DYN.LE00.IN))
                  
                  
                  #Detecting outliers
                 
                   df1 <- life_expectancy[,1:7]
                   df2 <- life_expectancy[,8:14]
                   df3 <- life_expectancy[,15:21]
                   
                   par(mar=c(1,1,1,1))
                  
                  # From below box plot, it is clear depicting outliers in different variables, 
                  # SH.HIV.INCD.14, Fr.INR.RINR and some other variables are showing maximum outliers along with few others. 
                  boxplot(df1, col = rainbow(ncol(df1)))
                  boxplot(df2, col = rainbow(ncol(df2)))
                  boxplot(df3, col = rainbow(ncol(df3)))
                  
                  
                  
                  #Descriptive statistics
               
                  #summary: Results of summary will be in the report section, summary is showing the null values,  mean, median
                  summary(life_expectancy)  # done with r summary function
                  
                  #Descriptive statistics with describeBy()   reference: https://statsandr.com/blog/descriptive-statistics-in-r/
                  
                  
                  #The describeBy() function from the {psych} package allows to report several 
                  #summary statistics (i.e., number of valid cases, mean, standard deviation, median, trimmed mean, mad: 
                  #median absolute deviation (from the median), minimum, maximum, range, skewness and kurtosis) by a grouping variable.
                  
                  describeBy(
                    life_expectancy
                  )
                  
                  
                  #------------------------------------------------------------------------------------#
                  
                  
                  #--------------------------Question_2------------------------------------------------#
                  
                  #life expectancy in another variable
                  
                  life_exp_q1 <- life_expectancy
                  
                  str(life_expectancy)
                 
                  dim(life_expectancy)
                  
                  missing_glimpse(life_expectancy)  # percentage of missing values 
                  
                  # Below is the table showing missing values % in each column
                  
                  #  Features                                               Missing count     Missing values percentage
                  
                  #SP.DYN.LE00.IN             SP.DYN.LE00.IN    <dbl> 198        19             8.8
                  #EG.ELC.ACCS.ZS             EG.ELC.ACCS.ZS    <dbl> 216         1             0.5
                  #NY.ADJ.NNTY.KD.ZG       NY.ADJ.NNTY.KD.ZG    <dbl> 138        79            36.4
                  #NY.ADJ.NNTY.PC.KD.ZG NY.ADJ.NNTY.PC.KD.ZG    <dbl> 138        79            36.4
                  #SH.HIV.INCD.14             SH.HIV.INCD.14    <int>  90       127            58.5
                  #SE.PRM.UNER                   SE.PRM.UNER    <dbl> 118        99            45.6
                  #SP.DYN.IMRT.IN             SP.DYN.IMRT.IN    <dbl> 193        24            11.1
                  #SE.PRM.CMPT.ZS             SE.PRM.CMPT.ZS    <dbl> 128        89            41.0
                  #FR.INR.RINR                   FR.INR.RINR    <dbl> 113       104            47.9
                  #SH.XPD.CHEX.PC.CD       SH.XPD.CHEX.PC.CD    <dbl> 186        31            14.3
                  #SH.XPD.CHEX.GD.ZS       SH.XPD.CHEX.GD.ZS    <dbl> 186        31            14.3
                  #SL.UEM.TOTL.NE.ZS       SL.UEM.TOTL.NE.ZS    <dbl> 121        96            44.2
                  #SH.HIV.INCD                   SH.HIV.INCD    <int> 129        88            40.6
                  #SH.H2O.SMDW.ZS             SH.H2O.SMDW.ZS    <dbl> 128        89            41.0
                  
                  
                  # The md.pattern() function along with Multivariate Imputation by Chained Equations (MICE) package
                  # helps in producing a table displaying the missing pattern
                  md.pattern(life_expectancy)
                  
                  # The below pattern is displaying that there are 42 rows with no missing values, 54 rows in which there is one column
                  # data missing
                  md.pattern(life_expectancy[,c(1:7)],rotate.names = TRUE)
                  md.pattern(life_expectancy[,c(8:14)])
                  md.pattern(life_expectancy[,c(15:21)])
                  
                  md.pairs(life_expectancy)
                  
                  
                  par(mar=c(1,1,1,1))
                  marginplot(life_expectancy[,c('SP.DYN.LE00.IN', 'EG.ELC.ACCS.ZS')])
                  
                  #Nonmissing values are displayed in blue color and missing values are in red color. There are 19 missing values on SP.DYN.LE00.IN
                  marginplot(life_expectancy[,c('EG.ELC.ACCS.ZS', 'SP.DYN.LE00.IN')])
                  
                  #Nonmissing values are displayed in blue color and missing values are in red color. There are 99 missing values on SP.DYN.LE00.IN
                  # and 19 missing values in another column SE.PRM.UNER
                  marginplot(life_expectancy[,c('SE.COM.DURS', 'SE.PRM.UNER')])
                  
                  life_expect_impute <- life_expectancy #store data in another variable to preserve life expectancy variable
                  
                  
                  #More than 59% values in the data set with no missing value. 
                  #There are 36% missing values in NY.ADJ.NNTY.PC.KD.ZG, 14% missing values in SH.XPD.CHEX.PC.CD and SH.XPD.CHEX.GD.ZS and so on. 
                  mice_plot <- aggr(life_expect_impute, col=c('navyblue','yellow'),
                                    numbers=TRUE, sortVars=TRUE,
                                    labels=names(life_expect_impute), cex.axis=.7,
                                    gap=3, ylab=c("Missing data","Pattern"))
                 
                
                  colnames(life_expect_impute)
                  
                  #method Applying multiple imputation
                  
                  # storing dependent variable life expectancy and will scale others -- applying normallization, scaling as some values are bigger
                  sp.leoo.in <- life_expect_impute$SP.DYN.LE00.IN
                  subset_life_expect = life_expect_impute
                  subset_life_expect <- subset_life_expect[, !(colnames(subset_life_expect) %in% c("SP.DYN.LE00.IN"))]
                  
                  subset_life_expect.scaled = scale(subset_life_expect, center= TRUE, scale=TRUE)
                
                  imputed_Data <- mice(subset_life_expect.scaled, m=5, maxit = 50, method = 'pmm', seed = 500)
                  summary(imputed_Data)
                  
                  #check imputed values
                  imputed_Data$imp$EG.ELC.ACCS.ZS
                  
                  # we are applying two different methods to impute data i.e m = 1,2
                  
                  #Question 2 part 2, imputed dependent variable
                  # https://bookdown.org/mwheymans/bookmi/single-missing-data-imputation.html  #reference code has been done here
                  #using regression to impute missing values in dependent variable
                  #The life expectancy variables are used to predict the missing dependent variable values
                  
                  # The method “norm.predict” in the mice package fits a linear regression model in the dataset and generates the imputed values
                  # for the variable by using the regression coefficients of the linear regression model. 
                  # The completed dataset can be extracted by using the complete function in the mice package.
                  # Complete data
                 
                
                  imputed_Data <- mice(subset_life_expect.scaled, m=5, maxit = 50, method = 'pmm', seed = 500)
                  summary(imputed_Data)
                  
                  dataset1 <- complete(imputed_Data,1)
                  dataset2 <- complete(imputed_Data,2)
                  dataset3 <- complete(imputed_Data,3)
                  dataset4 <- complete(imputed_Data,4)
                  dataset5 <- complete(imputed_Data,5)
                  
                  dataset1$SP.DYN.LE00.IN <- sp.leoo.in
                  dataset2$SP.DYN.LE00.IN <- sp.leoo.in
                  dataset3$SP.DYN.LE00.IN <- sp.leoo.in
                  dataset4$SP.DYN.LE00.IN <- sp.leoo.in
                  dataset5$SP.DYN.LE00.IN <- sp.leoo.in
                  
                  imp.regress1 <- mice(dataset1, method="norm.predict", m=1, maxit=1)
                  imp.regress2 <- mice(dataset2, method="norm.predict", m=1, maxit=1)
                  imp.regress3 <- mice(dataset3, method="norm.predict", m=1, maxit=1)
                  imp.regress4 <- mice(dataset4, method="norm.predict", m=1, maxit=1)
                  imp.regress5 <- mice(dataset5, method="norm.predict", m=1, maxit=1)
                  
                  completeData_life_expectancy1 <- complete(imp.regress1,1)
                  completeData_life_expectancy2 <- complete(imp.regress2,1)
                  completeData_life_expectancy3 <- complete(imp.regress3,1)
                  completeData_life_expectancy4 <- complete(imp.regress4,1)
                  completeData_life_expectancy5 <- complete(imp.regress5,1)
                 
                  
                  fit1 <- lm(SP.DYN.LE00.IN ~ ., data = completeData_life_expectancy1)
                  fit2 <- lm(SP.DYN.LE00.IN ~ ., data = completeData_life_expectancy2)
                  fit3 <- lm(SP.DYN.LE00.IN ~ ., data = completeData_life_expectancy3)
                  fit4 <- lm(SP.DYN.LE00.IN ~ ., data = completeData_life_expectancy4)
                  fit5 <- lm(SP.DYN.LE00.IN ~ ., data = completeData_life_expectancy5)
                  
                  summary(fit1)   # F-statistic: 0.907  , R-Squared :  0.9074  ,  p-value: < 2.2e-16   ,  Residual standard error: 2.335
                  summary(fit2)   # F-statistic: 105.8  , R-Squared :  0.9108  ,  p-value: < 2.2e-16   ,  Residual standard error: 2.309
                  summary(fit3)   # F-statistic: 97.46  , R-Squared :  0.9038  ,  p-value: < 2.2e-16   ,  Residual standard error: 2.391
                  summary(fit4)   # F-statistic: 102.1  , R-Squared :  0.9078  ,  p-value: < 2.2e-16   ,  Residual standard error: 2.325
                  summary(fit5)   # F-statistic: 106.5  , R-Squared :  0.9113  ,  p-value: < 2.2e-16   ,  Residual standard error: 2.289
                  
                  # From the above, we can conclude that Imputation m = 5 is giving the best imputated data values.  
                
                  summary(fit5)   # F-statistic: 106.5  , R-Squared :  0.9113  ,  p-value: < 2.2e-16   ,  Residual standard error: 2.289
                  
                  best_imputed_model <- completeData_life_expectancy5
                  
                  
                  #-------------------------------------------------------------------------------------#
                  
                  #--------------------------Question_3------------------------------------------------#
                  
                  
                  
                  #handling outliers  -> outliers will remove rows, so we will not apply it to our original variable/dataset
                  
                  #method -- IQR
                  life_exp_outliers <- completeData_life_expectancy5
                  
                  colnames(life_exp_outliers)
                  
                  df <- life_exp_outliers
                  
                  #find absolute value of z-score for each value in each column
                  z_scores <- as.data.frame(sapply(df, function(df) (abs(df-mean(df))/sd(df))))
                  
                  #view first six rows of z_scores data frame
                  head(z_scores)
                  
                  #only keep rows in dataframe with all z-scores less than absolute value of 3 
                  no_outliers <- z_scores[!rowSums(z_scores>3), ]
                  
                  #view row and column count of new data frame
                  dim(no_outliers)
                  
                  
                  
                  #Question 3 model collinearity starts from here: 
                  
                  print(best_imputed_model)
                
                  pairs(best_imputed_model, col = "dodgerblue")
                  
                  summary(fit5)
                  
                  #method 1 through VIF factor
                  
                  vif(fit5)
                  
                  #Variation inflation factor for variables
                  
                  # EG.ELC.ACCS.ZS    NY.ADJ.NNTY.KD.ZG NY.ADJ.NNTY.PC.KD.ZG       SH.HIV.INCD.14       SP.DYN.IMRT.IN       SE.PRM.CMPT.ZS          FR.INR.RINR          SP.POP.GROW 
                  #   4.491983          2693.112913          2639.304321             3.278377             6.863412             2.458868             1.418490           114.194711 
                  # EN.POP.DNST          SP.POP.TOTL    SH.XPD.CHEX.PC.CD    SH.XPD.CHEX.GD.ZS    SL.UEM.TOTL.NE.ZS    NY.GDP.MKTP.KD.ZG       NY.GDP.PCAP.CD       SP.DYN.CBRT.IN 
                  #   1.333766             1.932735           204.285173            15.336280             1.975925             1.888550           183.148762            11.227590 
                  # SH.HIV.INCD       SH.H2O.SMDW.ZS          SE.COM.DURS 
                  #   3.002139             6.741018             1.321556
                  
                  
                  # We will drop those columns whose VIF is greater than 10, vif greater than 10 means there variable has high multicollinearity and 
                  # should be removed from the dataset
                  
                  #following columns should be dropped because VIF is greater than 10
                  
                  # NY.ADJ.NNTY.KD.ZG       2693.112913
                  # NY.ADJ.NNTY.PC.KD.ZG    2639.304321
                  # SH.XPD.CHEX.PC.CD  204.285173
                  # SH.XPD.CHEX.GD.ZS  15.336280
                  # NY.GDP.PCAP.CD   183.148762
                  # SP.POP.GROW   114.194711
                  # SP.DYN.CBRT.IN  11.227590
                  
                  
                  #Following columns are dropped  
                  
                  best_imputed_model <- best_imputed_model[, !(colnames(best_imputed_model) %in% c("NY.ADJ.NNTY.KD.ZG","NY.ADJ.NNTY.PC.KD.ZG","SH.XPD.CHEX.PC.CD", "SH.XPD.CHEX.GD.ZS"))]
                  model_VIF_less_than_10 <- best_imputed_model[, !(colnames(best_imputed_model) %in% c("NY.GDP.PCAP.CD","SP.POP.GROW","SP.DYN.CBRT.IN"))]
                  
                  colnames(model_VIF_less_than_10)
                  
                  
                  #Method 2  -> we find correlation of variables and high correlation more than 70% will be removed
                  cor1 = cor(model_VIF_less_than_10)
                  corrplot.mixed(cor1, lower.col = 'black', number.cex = .7)
                  
                  # reference :  https://stackoverflow.com/questions/35095638/caret-package-findcorrelation-function
                  set.seed(7)
            
                  correlationMatrix <- cor(model_VIF_less_than_10)
                  # summarize the correlation matrix
                  print(correlationMatrix)
                  # find attributes that are highly corrected (ideally >0.70)
                  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.70)
                  # print indexes of highly correlated attributes
                  print(highlyCorrelated)
                  
                  
                  # from the correlation it is clear that these features/variables have high correlation 
                  # SH.HIV.INCD.14  SH.H2O.SMDW.ZS         
                  
                  model_VIF_less_than_10 <- model_VIF_less_than_10[, !(colnames(model_VIF_less_than_10) %in% c("SH.HIV.INCD.14","SH.H2O.SMDW.ZS"))]
                  
                  #Final model after collinearity variables/columns
                  
                  colnames(model_VIF_less_than_10)
                  
                  # "EG.ELC.ACCS.ZS"    "SP.DYN.IMRT.IN"    "SE.PRM.CMPT.ZS"   
                  # "FR.INR.RINR"   "EN.POP.DNST"  "SP.POP.TOTL"   "SL.UEM.TOTL.NE.ZS" 
                  # "NY.GDP.MKTP.KD.ZG" "SH.HIV.INCD" "SE.COM.DURS"      
                  # "SP.DYN.LE00.IN"
                  
                  #-------------------------------------------------------------#
                  
                  #--------------------------Question4------------------------------------------------#
                  
                  
                  #suggest the best model
                  # 1) Remove Redundant Features
                  # 2) Rank Features By Importance
                  # 3) Feature Selection -- Recursive Feature Elimination or RFE
                  
                  
                  lifeexpectancy_model1 = lm(SP.DYN.LE00.IN ~ ., data = model_VIF_less_than_10)
                  summary(lifeexpectancy_model1)
                  
                  
                  
                  lifeexpectancy_model2 = lm(SP.DYN.LE00.IN ~ -SE.PRM.CMPT.ZS - SL.UEM.TOTL.NE.ZS - SE.COM.DURS, data = model_VIF_less_than_10)
                  summary(lifeexpectancy_model2)
                  
                  
                  lifeexpectancy_model3 = lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS  + SP.DYN.IMRT.IN  + FR.INR.RINR + EN.POP.DNST + SP.POP.TOTL  + NY.GDP.MKTP.KD.ZG + NY.GDP.MKTP.KD.ZG +  SH.HIV.INCD, data = model_VIF_less_than_10)
                  summary(lifeexpectancy_model3)
                  #Residual standard error: 2.435 on 209 degrees of freedom
                  #Multiple R-squared:  0.8935,	Adjusted R-squared:  0.8899 
                  #F-statistic: 250.5 on 7 and 209 DF,  p-value: < 2.2e-16
                  
                  
                  
                  
                  #reference: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
                  # Recursive feature elimination 
                  #-> from the graph it is clear that with 2,6 features, we can get a model with root mean squared error less than 2.65
                  # A random forest algorithm is used to evaulate features
                  # ensure the results are repeatable
                  set.seed(7)
                  # define the control using a random forest selection function
                  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
                  # run the RFE algorithm
                  results <- rfe(model_VIF_less_than_10[,1:10], model_VIF_less_than_10[,11], sizes=c(1:12), rfeControl=control)
                  # summarize the results
                  print(results)
                  # list the chosen features
                  predictors(results)
                  # plot the results
                  plot(results, type=c("g", "o"))
                  
                
                  
                  
                  
                  # AIC BIC and Adjusted  R squared   criteria for model selection
                  
                  
                  set.seed(1234)
                  
                  #Selection Procedures
                  
                  #Backward Search
                  
                  life_expect_mod1 = lm(SP.DYN.LE00.IN ~ ., data = model_VIF_less_than_10[,1:11])
                  coef(life_expect_mod1)
                  extractAIC(life_expect_mod1) # returns both p and AIC  ->  11.0000 399.3832
                  
                  
                  life_expect_mod1_back_aic = step(life_expect_mod1, direction = "backward")
                  # least AIC=394.16
                  
                  n = length(resid(life_expect_mod1))
                  (p = length(coef(life_expect_mod1)))
                  
                  aic_factor <- n * log(mean(resid(life_expect_mod1) ^ 2)) + 2 * p              
                  aic_factor  # 399.3832
                  
                  coef(life_expect_mod1_back_aic)
                  
                  
                  #using BIC
                  
                  n = length(resid(life_expect_mod1))
                  life_expect_mod1_back_bic = step(life_expect_mod1, direction = "backward", k = log(n))
                  
                  coef(life_expect_mod1_back_bic)
                  
                  #From the below it can be observed that adjusted r squared is almost same for model 1, backward aic and backward bic with k = log(n)
                  #AIC R squared
                  summary(life_expect_mod1)$adj.r.squared     # 0.8887421
                  
                  summary(life_expect_mod1_back_aic)$adj.r.squared   # 0.8899456   
                  
                  #BIC R squared
                  summary(life_expect_mod1_back_bic)$adj.r.squared   # 0.8880772
                  
                  
                  #functions
                  
                  # From the text: http://daviddalpiaz.github.io/appliedstats/variable-selection-and-model-building.html
                  calc_loocv_rmse = function(model) {
                    sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
                  }
                  calc_rmse = function(actual, predicted) {
                    sqrt(sum((actual - predicted)^2) / length(actual)) 
                  }
                  calc_avg_per_error = function(actual, predicted) {
                    inter_abs = abs(predicted - actual)
                    100 * (sum(inter_abs / actual)) / length(actual)
                  }
                  
                  
                  calc_loocv_rmse(life_expect_mod1)            # 2.542271
                  
                  calc_loocv_rmse(life_expect_mod1_back_aic)   # 2.510068
                  calc_loocv_rmse(life_expect_mod1_back_bic)   # 2.513588
                  
                  #We see that we would prefer the model chosen via AIC if using LOOCV RMSE as our metric.
                  
                  
                  #Forward Search
                  
                  colnames(model_VIF_less_than_10)
                  #"EG.ELC.ACCS.ZS"    "SP.DYN.IMRT.IN"    "SE.PRM.CMPT.ZS"    "FR.INR.RINR"       "EN.POP.DNST"       "SP.POP.TOTL"       "SL.UEM.TOTL.NE.ZS" "NY.GDP.MKTP.KD.ZG" "SH.HIV.INCD"       "SE.COM.DURS"       "SP.DYN.LE00.IN"
                  
                  life_expect_mod1_start = lm(SP.DYN.LE00.IN ~ 1, data = model_VIF_less_than_10[,1:11])
                  life_expect_mod1_forw_aic = step(life_expect_mod1_start, scope = SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS  + SP.DYN.IMRT.IN  + 
                                                     EN.POP.DNST + SP.POP.TOTL  + NY.GDP.MKTP.KD.ZG +  SE.COM.DURS + SH.HIV.INCD + SL.UEM.TOTL.NE.ZS + FR.INR.RINR + SE.PRM.CMPT.ZS,  direction = "forward")
                  
                  #Step:  AIC=394.16
                  #SP.DYN.LE00.IN ~ SP.DYN.IMRT.IN + EG.ELC.ACCS.ZS + EN.POP.DNST + 
                  #  FR.INR.RINR + NY.GDP.MKTP.KD.ZG + SH.HIV.INCD + SP.POP.TOTL
                  
                  # Df Sum of Sq    RSS    AIC
                  # <none>                           1239.6 394.16
                  # + SE.PRM.CMPT.ZS     1    3.4876 1236.2 395.55
                  # + SE.COM.DURS        1    1.2270 1238.4 395.95
                  # + SL.UEM.TOTL.NE.ZS  1    0.0012 1239.6 396.16   least AIC value
                  
                  
                  life_expect_mod1_forw_bic = step(
                    life_expect_mod1_start, 
                    scope = SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS  + SP.DYN.IMRT.IN  + 
                      EN.POP.DNST + SP.POP.TOTL  + NY.GDP.MKTP.KD.ZG +  SE.COM.DURS + SH.HIV.INCD + SL.UEM.TOTL.NE.ZS + FR.INR.RINR + SE.PRM.CMPT.ZS,  direction = "forward", k = log(n))
                  # Step:  AIC=416.16
                  # SP.DYN.LE00.IN ~ SP.DYN.IMRT.IN + EG.ELC.ACCS.ZS + EN.POP.DNST + FR.INR.RINR + NY.GDP.MKTP.KD.ZG   This is the best model against forward AIc 416.16
                  
                  
                  summary(life_expect_mod1)$adj.r.squared              # 0.8887421
                   
                  summary(life_expect_mod1_forw_aic)$adj.r.squared     # 0.8899456
                  
                  summary(life_expect_mod1_forw_bic)$adj.r.squared     # 0.8880772
                  
                  
                  calc_loocv_rmse(life_expect_mod1)            # 2.542271
                   
                  calc_loocv_rmse(life_expect_mod1_forw_aic)   # 2.510068
                  
                  calc_loocv_rmse(life_expect_mod1_forw_bic)   # 2.513588
                  
                  
                  #We can compare the two selected models' Adjusted R2 as well as their LOOCV  RMSE
                  #The results are very similar to those using backwards selection, although the models are not exactly the same.
                  
                  
                  #Stepwise Search
                  
                  life_expect_mod1_both_aic = step(
                    life_expect_mod1_start, 
                    scope =  SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS  + SP.DYN.IMRT.IN  + 
                      EN.POP.DNST + SP.POP.TOTL  + NY.GDP.MKTP.KD.ZG +  SE.COM.DURS + SH.HIV.INCD + SL.UEM.TOTL.NE.ZS + FR.INR.RINR + SE.PRM.CMPT.ZS, 
                    direction = "both")
                  
                  
                  life_expect_mod1_both_bic = step(
                    life_expect_mod1_start, 
                    scope =  SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS  + SP.DYN.IMRT.IN  + 
                      EN.POP.DNST + SP.POP.TOTL  + NY.GDP.MKTP.KD.ZG +  SE.COM.DURS + SH.HIV.INCD + SL.UEM.TOTL.NE.ZS + FR.INR.RINR + SE.PRM.CMPT.ZS, 
                    direction = "both", k = log(n))
                  
                  
                  summary(life_expect_mod1)$adj.r.squared            # 0.8887421
                  
                  summary(life_expect_mod1_both_aic)$adj.r.squared   # 0.8899456
                  
                  summary(life_expect_mod1_both_bic)$adj.r.squared   # 0.8880772
                  
                  calc_loocv_rmse(life_expect_mod1)           # 2.542271
                  
                  calc_loocv_rmse(life_expect_mod1_both_aic)  # 2.510068
                  
                  calc_loocv_rmse(life_expect_mod1_both_bic)  # 2.513588
                  
                  
                  
                  #Exhaustive Search
                  
                  install.packages('leaps')
                  library(leaps)
                  
                  all_life_expectancy_mod = summary(regsubsets(SP.DYN.LE00.IN ~ ., data = model_VIF_less_than_10[,1:11]))
                  
                  all_life_expectancy_mod$which
                  
                  
                  all_life_expectancy_mod$rss
                  
                  all_life_expectancy_mod$adjr2
                  
                  #  find which model has the highest Adjusted R2 we can use the which.max() function.
                  
                  (best_r2_ind = which.max(all_life_expectancy_mod$adjr2))
                  
                  
                  all_life_expectancy_mod$which[best_r2_ind, ]
                  
                  
                  p = length(coef(life_expect_mod1))
                  n = length(resid(life_expect_mod1))
                  
                  life_expect_model1_aic = n * log(all_life_expectancy_mod$rss / n) + 2 * (2:p)
                  
                  best_aic_ind = which.min(life_expect_model1_aic)
                  all_life_expectancy_mod$which[best_aic_ind,]
                  
                  life_expect_mod1_best_aic = lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + SP.DYN.IMRT.IN + FR.INR.RINR + EN.POP.DNST + SP.POP.TOTL + NY.GDP.MKTP.KD.ZG + SH.HIV.INCD  , data = model_VIF_less_than_10[,1:11])
                  
                  extractAIC(life_expect_mod1_best_aic)
                  
                  extractAIC(life_expect_mod1_back_aic)
                  
                  extractAIC(life_expect_mod1_forw_aic)
                  
                  extractAIC(life_expect_mod1_both_aic)
                  
                  
                  plot(life_expect_model1_aic ~ I(2:p), ylab = "AIC", xlab = "p, number of parameters", 
                       pch = 20, col = "dodgerblue", type = "b", cex = 2,
                       main = "AIC vs Model Complexity")
                  
                  # We could easily repeat this process for   BIC
                  
                  
                  life_expect_mod1_bic = n * log(all_life_expectancy_mod$rss / n) + log(n) * (2:p)
                  
                  which.min(life_expect_mod1_bic)
                  
                  all_life_expectancy_mod$which[5,]
                  
                  life_expectancy_mod1_best_bic = lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + SP.DYN.IMRT.IN + FR.INR.RINR + EN.POP.DNST + SP.POP.TOTL + NY.GDP.MKTP.KD.ZG + SH.HIV.INCD  , data = model_VIF_less_than_10[,1:11])
                  
                  extractAIC(life_expectancy_mod1_best_bic, k = log(n))
                  
                  extractAIC(life_expect_mod1_back_bic, k = log(n))
                  
                  extractAIC(life_expect_mod1_forw_bic, k = log(n))
                  
                  extractAIC(life_expect_mod1_both_bic, k = log(n))
                  
                  # best models
                  summary(life_expect_mod1_best_aic)
                  
                  summary(life_expectancy_mod1_best_bic)
                  
                  #  Coefficients:
                  #   Estimate Std. Error t value Pr(>|t|)    
                  #  (Intercept)        72.6242     0.1695 428.466  < 2e-16 ***
                  #    EG.ELC.ACCS.ZS      2.2125     0.2306   9.594  < 2e-16 ***
                  #    SP.DYN.IMRT.IN     -4.9795     0.2326 -21.404  < 2e-16 ***
                  #    FR.INR.RINR         0.8455     0.1926   4.390 1.80e-05 ***
                  #    EN.POP.DNST         0.7647     0.1723   4.437 1.47e-05 ***
                  #    SP.POP.TOTL         0.2949     0.1790   1.647  0.10097    
                  #    NY.GDP.MKTP.KD.ZG  -0.4814     0.1697  -2.836  0.00502 ** 
                  #    SH.HIV.INCD        -0.4335     0.2133  -2.032  0.04337 *  
                  
                  #Residual standard error: 2.435 on 209 degrees of freedom
                  #Multiple R-squared:  0.8935,	Adjusted R-squared:  0.8899 
                  #F-statistic: 250.5 on 7 and 209 DF,  p-value: < 2.2e-16
                  
                  #-------------------------------------------------------------------------#
                  
                  
                  
                  #--------------------------Question 5 ------------------------------------#
                  
                  # var_continent -> variable we store continents in question 1
                  
                  #dataset from question 4
                  summary(model_VIF_less_than_10)
                 
                  factor_Continent <- var_continent
                  
                  model_VIF_less_than_10$Continent <- factor_Continent
                  
                  
                  one_way_Anova_model <- model_VIF_less_than_10[,c(11,12)]
                  
                  factor_level <- as.factor(one_way_Anova_model$Continent)
                  factor_level  
                  
                  table(one_way_Anova_model$Continent)
                  
                  
                  group_mean <- group_by(one_way_Anova_model, Continent) %>%
                    summarise(
                      mean = mean(SP.DYN.LE00.IN, na.rm = TRUE),
                      sd = sd(SP.DYN.LE00.IN, na.rm = TRUE)
                    )
                  
                  group_mean
                  
                  #A tibble: 6 x 3
                  #Continent          mean    sd
                  #<chr>             <dbl> <dbl>
                  #  1 Africa             64.1  5.93
                  #2 Asia               74.6  5.07
                  #3 Australia/Oceania  73.3  5.25
                  #4 Europe             79.5  3.54
                  #5 North America      76.1  3.90
                  #6 South America      75.1  3.15
                  
                  one_way_Anova_model %>%  
                    ggplot(aes(x = factor(Continent), y = SP.DYN.LE00.IN)) +
                    geom_boxplot(aes(fill = Continent)) +     # add colour to boxplots
                    geom_jitter(alpha = 0.4) +                # alpha = transparency
                    facet_wrap(~ Continent, ncol = 5) +       # spread by continent
                    theme(legend.position = "none") +         # remove legend
                    xlab("") +                            # label x-axis
                    ylab("Life expectancy (years)")        # label y-axis
                  
                  
                  
                  # Compute the analysis of variance
                  aov.model = aov(SP.DYN.LE00.IN ~ Continent,data=one_way_Anova_model)  #do the analysis of variance
                  
                  # Summary of the analysis
                  summary(aov.model) #show the summary table
                  
                  #The output includes the columns F value (59.68) and Pr(>F) -> <2e-16 corresponding to the p-value of the test.
                  # P value is less than the threshold value 0.05, so there is a statistical difference between the groups/continents.
                  
                  #The above Anova test has a significant F-test score (59.68) and a small P-value, 
                  #we can conclude that there is a strong association between a life expectancy and continent variables.
                  
                  #Null Hypothesis
                  # The null hypothesis says the mean is same for all groups mean1=mean2=mean3=mean4=mean5=mean6
                  # The alternate hypthosis means the mean is not same or at least one of the mean is different
                  
                  
                  #The null hypothesis is rejected as the alternative hypothesis has a p-value <0.05 which is <2e-16
                  # The details are below
                  
                  # Df Sum Sq Mean Sq F value Pr(>F)    
                  # Continent     5   6819  1363.9   59.68 <2e-16 ***
                  #  Residuals   211   4822    22.9  
                
                  
                  #reference: http://www.sthda.com/english/wiki/one-way-anova-test-in-r
                  # reference: https://www.guru99.com/r-anova-tutorial.html
                  
                  # Multiple pairwise-comparison between the means of groups
                  #In one-way ANOVA test, a significant p-value indicates that some means value of group is different, but we don’t know which pairs of groups are different.
                  #It’s possible to perform multiple pairwise-comparison, to determine if the mean difference between specific pairs of group are statistically significant.
                  
                  #Tukey multiple pairwise-comparisons
                  # Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) used for performing pairwise comparison between means of different groups
                  TukeyHSD(aov.model)
                  
                  
                  #It can be seen from the output, that only the difference between 
                  # Europe-Asia, Europe-Australia/Oceania, North America-Europe,  are significant with an 
                  #adjusted p-value of 0.0000119, 0.0000425, 0.0210795 respectively. 
                  
                  print(model.tables(aov.model,"means"),digits=3)       #report the means and the number of subjects/cell
                  boxplot(SP.DYN.LE00.IN~Continent,data=one_way_Anova_model)        #graphical summary
                  
                  
                  
                  #--------------------------------------------------------------------------#
                  
                  
                  
                  
                  
                 
                  