# Clean_f.R
# This file contains the functions to clean the data
# auther: Zongyan Wang
###################################
# hasNA_all(data)
#' @return a vector for which variable has NA data
#'
# replaceNA(var, value = 0.001)
#' @param var: The variable having NA data
#' @param a vector which NA has been replaced with a value
####################################

# Check how many NA in a variable
hasNA_row <- function(var){
  return(sum(is.na(var)))
}

# Check how many NA in variables among whole data
hasNA_all <- function(data){
  k <- sapply(data, function(x) hasNA_row(x))
  return(k[k>0])
}

# replace NA to tiny data 0.001
replaceNA <- function(data, var, value = 0.001){
  test_that("The value is not a numeric value", {
    expect_equal(is.numeric(value), T)
  })
  data[,var][which(is.na(data[,var]))] <- value
  return(data[,var])
}


# replace NA with tiny value 0.001, all data version
replaceNA_all <- function(data, value = 0.001){
  var <- names(hasNA_all(data))
  k <- var %>% sapply(., function(x) replaceNA(data, x, value))
  k <- as.data.frame(k)
  data[,var] <- k
  return(data)
}


# replace NA with the average of the variable within the group
replaceNA_average <- function(var, data){
  test_that("The data should have Group column", {
    expect_equal(length(data$Group) > 0, T)
  })
  miss_index <- which(is.na(data[,var]))
  miss_group <- data$Group[miss_index]
  # replace NA with the average of the variable within the group
  for (i in 1:length(miss_index)){
    data[,var][miss_index[i]] = mean(data[,var][which(data$Group == miss_group[i])], na.rm = T)
  }
  return(data[,var])
}

# replace NA with the average of the variable within the group, all data version
replaceNA_average_all <- function(data){
  var <- names(hasNA_all(data))
  k <- var %>% sapply(., function(x) replaceNA_average(x, data))
  k <- as.data.frame(k)
  data[,var] <- k
  return(data)
}


# number of levels in factor variable
n.factor <- function(var){
  return(length(levels(var)))
}

# number of levels in factor variables among the data
n.factor_all <- function(data){
  x <- data %>% lapply(., function(x) n.factor(x))
  return(as.data.frame(x[x>0]))
}
