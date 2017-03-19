
###YOU MUST RUN THIS FUNCTION TO GET THE PLOTS!
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#read in data and inspect it
housingdat = read.csv("train.csv")
names(housingdat)

#get your data (you need to adjust for whichever variables you were assigned. I had 40 thru 60)
housing = housingdat[, 40:60]
names(housing)
str(housing)


#how many NA values do we have?
#fireplace, Garage, GarageYrBlt are problematic
NaData = apply(housing, 2, function(x) sum(is.na(x)))
NaData

#get logical list (and then convert to vector) to get names of integer and factor valued variables
library(purrr)
library(dplyr)
# housing %>% 

m = housing %>% 
  map(is.factor)
um = unlist(m)

yesfactor = um[um == TRUE]
yesinteger = um[um == FALSE]


#how many factor variables are there?
# sum(unlist(m))
# 57-33

########################################################
#function to get a list of integer and factor variables
########################################################
my_integers = list(NULL)
my_factors = list(NULL)
for (i in seq_along(names(housing))){
  if (class(housing[,i]) == "integer"){
    my_integers[[i]] = housing[,i]
  }
  else if (class(housing[,i]) == "factor"){
    my_factors[[i]] = housing[, i]
  }
}


# myints = my_integers[-(which(sapply(my_integers, is.null), arr.ind = TRUE))]
# myfacts = my_factors[-(which(sapply(my_factors, is.null), arr.ind = TRUE))] 

# Now we have to get rid of null entries
m = lapply(my_integers, function(x) is.null(x))
n = lapply(my_factors, function(x) is.null(x))
onlyints = my_integers[m == FALSE]
onlyfactors = my_factors[n == FALSE]

#make the lists into a dataframe
intsdf = as.data.frame(onlyints)
factsdf = as.data.frame(onlyfactors)
intnames = names(yesinteger)
factnames = names(yesfactor)

#assign proper names to variables!
names(intsdf) = intnames
names(factsdf) = factnames

#################################################################################################
# now we can create a function to output boxplots and histograms of integer and factor variables.
#################################################################################################

#integer valued variable plots (boxplots)
myplots_ints = function(data){
  allvars=names(data)
  varcols = ncol(data)
  varnames = allvars
  listofplots=list(NULL)
  for (i in seq_along(varnames)){
    listofplots[[i]]=
      ggplot(data, aes_string(x = factor(1), y = varnames[i])) + 
      geom_boxplot(width = .8) + 
      theme(axis.title.x = element_blank(),
            plot.margin = unit(c(1,1,1,1), "cm"), 
            axis.title.y = element_text(size=12))
  }
  return(listofplots)
}

#run funciton on intsdf and get lots of boxplots
try1 = myplots_ints(intsdf)
multiplot(plotlist = try1[1:6], cols = 3)
multiplot(plotlist = try1[7:12], cols = 3)
multiplot(plotlist = try1[13:18], cols = 3)
multiplot(plotlist = try1[19:24], cols = 3)

#histogram function for factor valued variables
myplots_facts = function(data){
  allvars=names(data)
  varcols = ncol(data)
  varnames = allvars
  listofplots=list(NULL)
  for (i in seq_along(varnames)){
    listofplots[[i]]=
      ggplot(data, aes_string(varnames[i])) + 
      geom_histogram(stat = "count") + 
      theme(
            plot.margin = unit(c(1,1,1,1), "cm"), 
            axis.title.x = element_text(size=12))
  }
  return(listofplots)
}


# run on the factor dataframe
try2 = myplots_facts(factsdf)
multiplot(plotlist = try2[1:6], cols = 3)
multiplot(plotlist = try2[7:12], cols = 3)
multiplot(plotlist = try2[13:18], cols = 3)
multiplot(plotlist = try2[19:24], cols = 3)
multiplot(plotlist = try2[25:31], cols = 3)
multiplot(plotlist = try2[32:33], cols = 2)


#apply random forests for variable selection
library(randomForest)

housenames = names(housingdat)
paste(housenames,collapse = "+")

x <- sprintf("%5d", 35004:35050) 
paste(sprintf("\"%s\"", x), collapse=" + ") 
housingdat$YearBuilt
myRF = randomForest(SalePrice ~ Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond, data = housingdat, importance = TRUE)
myRF = randomForest(SalePrice ~ MSSubClass+MSZoning+LotFrontage+LotArea+Street+Alley+LotShape+LandContour+Utilities+LotConfig+LandSlope+
                      Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+
                      RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea +ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+
                      BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+BsmtUnfSF  +TotalBsmtSF+Heating+HeatingQC+CentralAir+Electrical+X1stFlrSF +
                      KitchenQual+ TotRmsAbvGrd+Functional+Fireplaces+FireplaceQu+GarageType + 
                      X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr + 
                      +GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual + GarageCond+PavedDrive+WoodDeckSF +OpenPorchSF + EnclosedPorch+
                      X3SsnPorch + ScreenPorch + PoolArea + MoSold,
                      data = housingdat, na.action = na.omit)
                    
                   
                      

  # X3SsnPorch +
  #   ScreenPorch+PoolArea+PoolQC+Fence+MiscFeature
  #                     MiscVal+MoSold+YrSold+SaleType+SaleCondition, data = housingdat, importance=TRUE, na.action = na.omit)

str(housingdat)
imp = myRF$importance
plot(myRF)
varImpPlot(myRF, main = NULL)

impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))

for (i in seq_along(impvar)) {
  partialPlot(rfstream, train, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(15, 19))
}


?paste
which.max(NaData)
?arrayInd
# Make some plots
library(ggplot2)
ggplot(data = housingdat, aes(x = factor(1), y=Fireplaces)) +
  geom_boxplot()

ggplot(data = factsdf, aes(Street)) +
  geom_histogram(stat = 'count')


class(housing$LotShape)

x <- list("a", "b", "c", "d", "e", NULL)
x[x != "b"]

!my_integers[is.null(my_integers)]
#   my_integers
#   my_factors
# }

# intsdf <- data.frame(matrix(unlist(onlyints), nrow=1460, byrow=T))
# factsdf = data.frame(matrix(unlist(onlyfactors), nrow=1460, byrow=T))
# intsdf = as.data.frame(unlist(myints))
# factsdf = as.data.frame(myfacts)