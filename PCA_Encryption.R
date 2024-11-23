## PROGRAM  PCA_Encryption

## SUMMARY  In this program we decode the message that is hidden in the supplied
##          data set. Essentially, this program has three sections.
##          
##          Section 1
##          Read in data and look at the correlation between variables. This
##          highlights that there are two sets of variables within the data.

##          Section 2
##          We Perform PCA on the 1st set of variables to reveal a hidden message
##          with the aid of a shifted alpha-numeric cipher.
##          The end output is exported as a txt file.

##          Section 3
##          We Perform PCA on the 2nd set of variables to reveal a hidden message
##          with the aid of a numeric cipher.
##          The end output is exported as a txt file.





# Section 0
#-------------------------------------------------------------------------------
# Clearing variables and calling required packages

# 0.1a - Clear all variables currently stored in R's memory
rm(list = ls())

# 0.1b - Calling required packages
library('corrr')
library('FactoMineR')
library('tidyverse')
library(psych)




# Section 1
#-------------------------------------------------------------------------------
# Read in data and look at the correlation between variables

# 1.1a - Read in the data set
Dep1a <- read.csv("G:\\zzz Miscellaneous\\PCA\\Data\\Data_Set_1.csv")
str(Dep1a)
Dep1a



# 1.1b - Obtaining the correlation matrix of the data
Dep1b <- subset(Dep1a, select = -c (Row_ID, Y1) )
cor(Dep1b)

    # View as a correlation matrix plot (Will produce error msg)
    # corPlot(Dep1b, cex = 1.2)

    

# 1.1c - Obtain Summary statistics for each variable
Dep1c <- summary(Dep1b)
Dep1c

    
# 1.1d - Obtain the standard deviation for each variable
Dep1d <- sapply(Dep1b, sd)
Dep1d






# Section 2
#-------------------------------------------------------------------------------
# Perform PCA on the first set of variables

# 2.1a - Refine to group1 variables
Dep2a <- subset(Dep1a, select = c(Row_ID, Y1, Var1, Var2, Var3, Var4))
Dep2a

# 2.1b - Refine to numeric group1 variables
Dep2b <- subset(Dep2a, select = -c(Row_ID, Y1))
Dep2b


# 2.1c - Apply principal component analysis to the numeric variables
Dep2c <- prcomp(formula = ~., data=Dep2b, center = TRUE, scale = TRUE)
Dep2c


# 2.1d - Obtain the first principal component score
Dep2d <- Dep2c$x[,1:1]

    # 2.1d1 - Convert string to dataframe
    Dep2d1 <- data.frame(Dep2d)
    
    # 2.1d2 - Give dataframe columns a name
    Dep2d2 <- rownames_to_column(Dep2d1, var = "Row_ID")
    names(Dep2d2)[names(Dep2d2)=='Dep2d'] <- 'PC1'

    Dep2d2
    
    
# 2.1e - Merge the Dep2a and Dep2d2 data sets then sort by principal component score
Dep2e <- merge(Dep2a, Dep2d2, by = "Row_ID")
Dep2e <-Dep2e[order(Dep2e$PC1),]
Dep2e

    

# 2.1f - Read in the Cipher_Shift data set
Dep2f <- read.csv("G:\\zzz Miscellaneous\\PCA\\Data\\Cipher_Shift.csv")
Dep2f


# 2.1g - Merge the previous two data sets together
Dep2g <- merge(Dep2e, Dep2f, by = "Y1")
Dep2g <-Dep2g[order(Dep2g$PC1),]
Dep2g


# 2.1h - Refine to required column
Dep2h <- subset(Dep2g, select = c(Character))
Dep2h


# 2.1i - Convert column to string
Dep2i <- toString(Dep2h)
Dep2i


# 2.1j - Tidy string
Dep2j <- gsub('\"' , ''  , Dep2i)
Dep2j <- gsub(',,' , '!' , Dep2j)
Dep2j <- gsub(','  , ''  , Dep2j)
Dep2j <- gsub('!'  , ',' , Dep2j)
Dep2j



# 2.1k - Export result as a txt file
write.csv(Dep2j, file="G:\\zzz Miscellaneous\\PCA\\Data\\AAA_Section_2_Result.txt")









# Section 3
#-------------------------------------------------------------------------------
# Perform PCA on the second set of variables

# 3.1a - Refine to group1 variables
Dep3a <- subset(Dep1a, select = c(Row_ID, Var0, Var5, Var6, Var7, Var8))
Dep3a

# 3.1b - Refine to numeric group1 variables
Dep3b <- subset(Dep3a, select = -c(Row_ID, Var0))
Dep3b


# 3.1c - Apply principal component analysis to the numeric variables
Dep3c <- prcomp(formula = ~., data=Dep3b, center = TRUE, scale = TRUE)
Dep3c


# 3.1d - Obtain the first principal component score
Dep3d <- Dep3c$x[,1:1]

    # 3.1d1 - Convert string to dataframe
    Dep3d1 <- data.frame(Dep3d)
    
    # 3.1d2 - Give dataframe columns a name
    Dep3d2 <- rownames_to_column(Dep3d1, var = "Row_ID")
    names(Dep3d2)[names(Dep3d2)=='Dep3d'] <- 'PC1'
    
    Dep3d2


# 3.1e - Merge the Dep2a and Dep2d2 data sets then sort by principal component score
Dep3e <- merge(Dep3a, Dep3d2, by = "Row_ID")
Dep3e <-Dep3e[order(Dep3e$PC1),]
Dep3e



# 3.1f - Read in the Cipher_Numeric data set
Dep3f <- read.csv("G:\\zzz Miscellaneous\\PCA\\Data\\Cipher_Numeric.csv")
Dep3f


# 3.1g - Merge the previous two data sets together
Dep3g <- merge(Dep3e, Dep3f, by = "Var0")
Dep3g <-Dep3g[order(Dep3g$PC1),]
Dep3g




# 3.1h - Refine to required column
Dep3h <- subset(Dep3g, select = c(Character))
Dep3h


# 3.1i - Convert column to string
Dep3i <- toString(Dep3h)
Dep3i


# 3.1j - Tidy string
Dep3j <- gsub('\"' , ''  , Dep3i)
Dep3j <- gsub(',,' , '!' , Dep3j)
Dep3j <- gsub(','  , ''  , Dep3j)
Dep3j <- gsub('!'  , ',' , Dep3j)
Dep3j



# 3.1k - Export result as a txt file
write.csv(Dep3j, file="G:\\zzz Miscellaneous\\PCA\\Data\\AAA_Section_3_Result.txt")




