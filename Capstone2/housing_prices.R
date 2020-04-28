# Housing regression using Ames housing data


###############################################################################
# PART 1: LOAD PACKAGES/LIBRARIES AND INGEST DATA
###############################################################################
# Install necessary packages, as well as some 'unnecessary' packages that are 
# still useful for data display purposes. Select 'yes' if prompted.
if(!require(car)) install.packages("car")
if(!require(caret)) install.packages("caret")
if(!require(corrplot)) install.packages("corrplot")
if(!require(DataExplorer)) install.packages("DataExplorer")
if(!require(data.table)) install.packages("data.table")
if(!require(e1071)) install.packages("e1071")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(glmnet)) install.packages("glmnet")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(Hmisc)) install.packages("Hmisc")
if(!require(Metrics)) install.packages("Metrics")
if(!require(mice)) install.packages("mice")
if(!require(moments)) install.packages("moments")
if(!require(psych)) install.packages("psych")
if(!require(psychometric)) install.packages("psychometric")
if(!require(randomForest)) install.packages("randomForest")
if(!require(rcompanion)) install.packages("rcompanion")
if(!require(repr)) install.packages("repr")
if(!require(Rmisc)) install.packages("Rmisc")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(xgboost)) install.packages("xgboost")


# Load libraries
library(car)             # Companion to Applied Regression
library(caret)           # Classification And REgression Training
library(DataExplorer)    # Handy for plotting 'NA' and missing data
library(corrplot)        # Graphically display correlation matrices
library(dplyr)           # Working with dataframe-like objects
library(e1071)           # Latent class analysis, etc.
library(ggrepel)         # Positioning non-overlapping text in figures
library(glmnet)          # For creating generalized linear model
library(gridExtra)       # Misc. functions to work with grid graphics package
library(Hmisc)           # For pretty correlation matrix plots
library(knitr)           # General-purpose programming and tablemaking
library(lattice)         # Trellis graphics
library(MASS)            # Functions from 'Modern Applied Stats with S', 2004
library(Metrics)         # Evaluation metrics for supervised machine learning
library(mice)            # Diagnostic plots
library(moments)         # Cumulants, skewness, kurtosis, and related tests
library(psych)           # Really just used for graphics capabilities
library(psychometric)    # See above
library(randomForest)    # Classification and regression trees
library(rcompanion)      # Really just used for additional stats tools
library(repr)            # String and binary representations of objects
library(Rmisc)           # Functions for data analysis and utility operations
library(tidyverse)       # Pretty much always needed...
library(xgboost)         # Gradient boosting framework


# Load training and test data. The file.choose() function will allow you to 
# point to wherever you've saved the CSV files.
train_data <- file.choose()                                   # Choose training data filepath interactively
test_data  <- file.choose()                                   # Choose test data filepath interactively
train_set  <- read.csv(train_data, stringsAsFactors = FALSE)  # Load training data into variable
test_set   <- read.csv(test_data, stringsAsFactors = FALSE)   # Load test data into variable
rm(train_data)                                                # Clean up workspace
rm(test_data)                                                 # Clean up workspace








###############################################################################
# PART 2: DATA OVERVIEW & EXPLORATORY DATA ANALYSIS
###############################################################################
options(scipen=999)     # Disable scientific notation for plots
head(train_set)         # View header of original file
summary(train_set)      # Summary statistics
# Sale price summary as follows
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 34900  129975  163000  180921  214000  755000 
dim(train_set)                 # 1,460 observations, 81 variables
dim(test_set)                  # 1,459 observations, 80 variables
sum(is.na(train_set$col))      # Sale price = 0 NA, rest must be converted to vars

# For visualizaiton purposes, the sale price in $k is neater. Not going to be
# used for the model or in the test data.
train_set_graph <- train_set

# Create separate data frame for graphing
train_set_graph  <-  mutate(train_set_graph, sale_k = SalePrice / 1000) 



# Use the DataExplorer library to see how many NA entries are in the dataset
DataExplorer::plot_missing(train_set_graph, geom_label_args = list('size'=2, 
     'label.padding' = unit(0.1, 'lines'))) + 
     ggtitle('Missing Data') + theme_bw() +
     theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))



# Histogram of sale prices
train_set_graph %>% ggplot(aes(sale_k, na.rm = TRUE)) + 
     geom_histogram(fill='black', binwidth = 5, na.rm = TRUE) +
     ggtitle('Sale Prices') + xlab('Sale price, $k') + ylab('Count') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))
# Sale prices are strongly right-skewed; intuitively makes sense since fewer 
# customers will be able to afford larger houses.
 







# Histogram of sale prices by house type
# From data dictionary,
# 20	 1-STORY 1946 & NEWER ALL STYLES
# 30	 1-STORY 1945 & OLDER
# 40	 1-STORY W/FINISHED ATTIC ALL AGES
# 45	 1-1/2 STORY - UNFINISHED ALL AGES
# 50	 1-1/2 STORY FINISHED ALL AGES
# 60	 2-STORY 1946 & NEWER
# 70	 2-STORY 1945 & OLDER
# 75	 2-1/2 STORY ALL AGES
# 80	 SPLIT OR MULTI-LEVEL
# 85	 SPLIT FOYER
# 90	 DUPLEX - ALL STYLES AND AGES
# 120 1-STORY PUD (Planned Unit Development) - 1946 & NEWER
# 150 1-1/2 STORY PUD - ALL AGES
# 160 2-STORY PUD - 1946 & NEWER
# 180 PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
# 190 2 FAMILY CONVERSION - ALL STYLES AND AGES
# Can visualize cleanly with box and violin plots by house typee:
groups <- train_set_graph %>% group_by(MSSubClass)
groups <- groups$MSSubClass
train_set_graph %>% ggplot(aes(x=factor(groups), y=sale_k)) + geom_boxplot() + 
     ggtitle('Sale Prices by House Type') + 
     xlab('House type') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(groups), y=sale_k)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Sale Prices by House Type') + 
     xlab('House type') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))
# Violin plots (mean = blue squares; median = red circles); similar to boxplots,
# but shows full distribution of data by house type
# Majority of sales = houses under $400k regardless of house type

 


# Plots of lot area and lot frontage by quality vs. sell price
train_set_graph %>% ggplot(aes(LotArea, sale_k, color = OverallQual)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Prices by Lot Area') + 
     xlab('Lot area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(LotFrontage, sale_k, color = OverallQual)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Prices by Lot Frontage') + 
     xlab('Lot frontage, ft.') + ylab('Lot frontage, ft.') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Plots of lot area and frontage by house type
train_set_graph %>% ggplot(aes(x=factor(groups), y=LotArea)) + geom_boxplot() + 
     scale_y_log10() +
     ggtitle('Lot Area by House Type') + 
     xlab('House type') + ylab('Lot area, sq.ft.') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(groups), y=LotArea)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Lot Area by House Type') + 
     xlab('House type') + ylab('Lot area, sq.ft.') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Plots of lot frontage by house type
train_set_graph %>% ggplot(aes(x=factor(groups), y=LotFrontage)) + geom_boxplot() + 
     scale_y_log10() +
     ggtitle('Lot Frontage by House Type') + 
     xlab('House type') + ylab('Lot frontage, ft.') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(groups), y=LotFrontage)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Lot Frontage by House Type') + 
     xlab('House type') + ylab('Lot frontage, ft.') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Plots of lot configuration versus sale price
train_set_graph %>% ggplot(aes(LotArea, sale_k, color = LotConfig)) + geom_point() +
     ggtitle('Sale Price by Lot Size & Configuration') + 
     xlab('Lot area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Plots of sale price vs. quality and condition
train_set_graph %>% ggplot(aes(x=factor(OverallQual), y=sale_k)) + geom_boxplot() + 
     ggtitle('Sale Price by Overall Quality') + 
     xlab('Overall quality') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(OverallQual), y=LotFrontage)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Sale Price by Overall Quality') + 
     xlab('Overall quality') + ylab('Sale price, $k') +
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(OverallCond), y=sale_k)) + geom_boxplot() + 
     scale_y_log10() +
     ggtitle('Sale Price by Overall Condition') + 
     xlab('Overall condition') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(OverallCond), y=LotFrontage)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Sale Price by Overall Condition') + 
     xlab('Overall condition') + ylab('Sale price, $k') +
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Sell price vs land slope
train_set_graph %>% ggplot(aes(x=factor(LandSlope), y=sale_k)) + geom_boxplot() + 
     scale_y_log10() +
     ggtitle('Sale Price by Land Slope') + 
     xlab('Land slope') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(LandSlope), y=LotFrontage)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Sale Price by Land Slope') + 
     xlab('Land slope') + ylab('Sale price, $k') +
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Sell price vs land contour
train_set_graph %>% ggplot(aes(x=factor(LandContour), y=sale_k)) + geom_boxplot() + 
     scale_y_log10() +
     ggtitle('Sale Price by Land Contour') + 
     xlab('Land contour') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

train_set_graph %>% ggplot(aes(x=factor(LandContour), y=LotFrontage)) + geom_violin() + 
     scale_y_log10() +
     stat_summary(fun='mean', geom='point', shape=15, size=3, color='blue') +
     stat_summary(fun='median', geom='point', shape=16, size=3, color='red') +
     ggtitle('Sale Price by Land Contour') + 
     xlab('Land contour') + ylab('Sale price, $k') +
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Sell price vs year built
train_set_graph %>% ggplot(aes(YearBuilt, sale_k)) + 
     geom_point(alpha=0.5) +
     ggtitle('Sale Price by Year Built') + 
     xlab('Year built') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))


# Plot of above-grade living area (sq.ft.) vs price for several variables
# Building type
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = BldgType)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Building Type') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# House style
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = HouseStyle)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & House Style') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Neighborhood
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = Neighborhood)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Neighborhood') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Proximity to conditions
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = Condition1)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Proximities') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Year built
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = YearBuilt)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Year Built') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Year remod
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = YearRemodAdd)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Year Remodeled') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Roof style
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = RoofStyle)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Roof Style') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))

# Roof style
train_set_graph %>% ggplot(aes(GrLivArea, sale_k, color = RoofMatl)) + 
     geom_point(alpha = 0.5) +
     ggtitle('Sale Price by Above-Grade Living Area & Roof Material') + 
     xlab('Above-grade living area, sq.ft.') + ylab('Sale price, $k') + 
     theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(text=element_text(family='Times New Roman', size=10))
# None of the point distributions change much regardless of how they're grouped.
# Therefore, the above-grade living space is the driving variable, not any of
# the other factors.








###############################################################################
# PART 3: DATA PREPARATION & MODEL CREATION
###############################################################################
# There are NAs in ALL of the data, so all of the data should be combined first,
# and then cleaned. After cleaning, data can be partitioned into training data
# and test data
# Combine training and test sets for pre-processing
train_set2 <- within(train_set,rm(SalePrice)) # Remove last column
all_data <- rbind(train_set2, test_set)
set.seed(1, sample.kind = 'Rounding')


# Linear models require normality from predictors, so high skew and/or high
# kurtosis is not acceptable.
# Separate vector for sale prices
sale_prices <- rbind(data.frame(version='log(SalePrice+1)',
                x = log(train_set$SalePrice + 1)),
                data.frame(version='Price',x = train_set$SalePrice))

ggplot(data=sale_prices) + facet_wrap(~version, ncol = 2, 
                scales = 'free_x') + geom_histogram(aes(x=x), fill = 'black') + 
        ggtitle('Comparison of Sale Price') + 
        xlab('log(SalePrice), left; sale price, right') + ylab('Count') + 
        theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
        theme(text=element_text(family='Times New Roman', size=10))

sale_price_qq <- as.data.frame(train_set$SalePrice)
sale_price_qq %>% ggplot(aes(sample = train_set$SalePrice)) + 
        stat_qq() + stat_qq_line() +
        ggtitle('Q-Q Plot of Sale Price') + 
        xlab('Theoretical quantiles') + ylab('Sale price') + 
        theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
        theme(text=element_text(family='Times New Roman', size=10))

sale_price_qq <- mutate(sale_price_qq, logSalePrice = log(train_set$SalePrice))
sale_price_qq %>% ggplot(aes(sample = logSalePrice)) + 
        stat_qq() + stat_qq_line() +
        ggtitle('Q-Q Plot of log Sale Price') + 
        xlab('Theoretical quantiles') + ylab('Sale price') + 
        theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
        theme(text=element_text(family='Times New Roman', size=10))
# Much more better!




# Adjust the training set to have the log of the sale price 
train_set$SalePrice <- log(train_set$SalePrice + 1)

# Log transform must be done to all continuous variables with excessive skew
# Sort variables by type
class_vars <- sapply(names(all_data),function(x){class(all_data[[x]])})
num_vars <-names(class_vars[class_vars != "character"])

# Calculate skew for each feature. Use type 2 method as per SPSS
# Source: https://cran.r-project.org/web/packages/e1071/e1071.pdf
skew_numeric <- sapply(num_vars,function(x){e1071::skewness(all_data[[x]],
                na.rm=TRUE, type = 2)})

# keep only features that exceed a threshold for skewness
skew_numeric <- skew_numeric[skew_numeric > 0.7]

# log transform of features with excessive skew
for(x in names(skew_numeric)) {
        all_data[[x]] <- log(all_data[[x]] + 1)
}


# Create vector with categorical variables, then transform them into dummy
# variables. Set any 'NA' level to zero
cat_vars <- names(class_vars[class_vars == 'character'])
dummy_vars <- dummyVars(~.,all_data[cat_vars])
cat_vars2 <- predict(dummy_vars,all_data[cat_vars])
cat_vars2[is.na(cat_vars2)] <- 0



# For missing values in the numeric variables vector, substitute the median of 
# that feature. Avoid using the mean to reduce the effect of outliers.
num_vars2 <- all_data[num_vars]
for (x in num_vars) {
        median_value <- median(train_set[[x]],na.rm = TRUE)
        all_data[[x]][is.na(all_data[[x]])] <- median_value
}


# Reconstruct the all_data vector with processed data
all_data <- cbind(all_data[num_vars],cat_vars2)


# Create training and test datasets. Store all sale prices in y for predictions.
train_set2 <- all_data[1:nrow(train_set),]
test_set2  <- all_data[(nrow(train_set)+1):nrow(all_data),]
y <- train_set$SalePrice                                






############################# CREATE MODEL #####################################
# Set up model training parameters for caret function. Five-fold cross-validation
# is given as an example. Can be tuned based on preferences and computing power.
train_control <- trainControl(method = 'repeatedcv', number = 5, 
                              repeats = 25, verboseIter = FALSE)

# First attempt: ridge regression model (alpha = 0)
lambdas <- seq(10,0,-0.001)              # Regularization parameter
ridge_model <- train(x = train_set2, y = y, method='glmnet', metric='RMSE',
                     maximize = FALSE, trControl=train_control,
                     tuneGrid=expand.grid(alpha=0,  lambda=lambdas))


# View results - lamdba can certainly be less than 0.2
ggplot(data=filter(ridge_model$result, lambda<0.2),aes(x = lambda, y = RMSE)) + 
        geom_line() + 
        ggtitle('RMSE vs Lambda') + 
        xlab('Lambda') + ylab('RMSE') + 
        theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
        theme(text=element_text(family='Times New Roman', size=10))

mean(ridge_model$resample$RMSE) # 0.1310



# Second attempt: LASSO model (alpha = 1) with narrowed lambda range
lasso_model <- train(x = train_set2, y = y, method='glmnet', metric='RMSE',
                maximize = FALSE, trControl = train_control,
                tuneGrid = expand.grid(alpha = 1, 
                lambda = c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                           0.00075,0.0005,0.0001)))

mean(lasso_model$resample$RMSE)   # 0.1257




# Extract regression coefficients for the LASSO model
regression_coefs <- data.frame(coefficient_name = dimnames(coef(lasso_model$finalModel,
                        s = lasso_model$bestTune$lambda))[[1]], 
                        coefficient_value = matrix(coef(lasso_model$finalModel,
                        s = lasso_model$bestTune$lambda)))

# exclude the (Intercept) term
# coef <- coef[-1,]


# Model results
betas    <- filter(regression_coefs,coefficient_value != 0)
unpicked <- nrow(filter(regression_coefs,coefficient_value == 0))

# Reorder regression coefficients in ascending value
betas <- arrange(betas, -coefficient_value)
cat('Number of variables selected by LASSO model:',nrow(betas),
        '\nNumber of variables not selected by LASSO model:',unpicked)

ggplot(data = betas, aes(x = coefficient_name, y = coefficient_value)) + 
                geom_bar(stat = 'identity') + 
                #ylim(-1.5,0.6) +
                coord_flip() +
                ggtitle('LASSO Model Coefficients') +
                xlab('Lambda') + ylab('RMSE') + 
                theme_bw() + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(text=element_text(family='Times New Roman', size=10))
                


# Obviously, 108 variables is going to be a bit much for a linear regression model. 
# Break the betas into positive and negative sets, then replot.
beta_positives <- betas %>% filter(coefficient_value > 0)
intercept      <- beta_positives[1,]                    # Separate intercept
beta_positives <- beta_positives[-1,]
beta_negatives <- betas %>% filter(coefficient_value < 0)

beta_positives <- rbind(head(beta_positives,10))
beta_negatives <- rbind(tail(beta_negatives,10))
beta_final     <- rbind(beta_positives, beta_negatives)


ggplot(data = beta_final, aes(x = reorder(coefficient_name, coefficient_value), 
       y = coefficient_value)) + 
        geom_bar(stat = 'identity') + 
        #ylim(-1.5,0.6) +
        coord_flip() +
        ggtitle('LASSO Model Coefficients') +
        xlab('Lambda') + ylab('RMSE') + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        theme(text=element_text(family='Times New Roman', size=10))


# Final betas
# coefficient_name      coefficient_value
# GrLivArea             0.38121749
# NeighborhoodCrawfor   0.10185455
# NeighborhoodStoneBr   0.09894868
# Condition2PosA        0.08847122
# RoofMatlWdShngl       0.08540229
# X1stFlrSF             0.08043133
# NeighborhoodNoRidge   0.07757189
# LotArea               0.07556226
# KitchenQualEx         0.06687422
# FunctionalTyp         0.06569215
# Condition1RRAe       -0.04371492
# SaleConditionAbnorml -0.05273959
# HeatingGrav          -0.08864734
# Exterior1stBrkComm   -0.09449847
# KitchenAbvGr         -0.12065531
# FunctionalMaj2       -0.14471320
# FunctionalSev        -0.18976152
# MSZoningC (all)      -0.33592821
# Condition2PosN       -0.52708545
# RoofMatlClyTile      -1.40936404




############################# FINAL PREDICTION #################################
prediction <- exp(predict(lasso_model, newdata = test_set2, se.fit = FALSE))-1
solution   <- data.frame(Id = as.integer(rownames(test_set2)), SalePrice = prediction)
verification_set <- train_set[1:1459,'SalePrice']
verification_set <- exp(verification_set) -1
sol_check  <- cbind(solution,verification_set)
sol_check  <- mutate(sol_check, diff = (verification_set - SalePrice)/verification_set * 100)
median(sol_check$diff)
