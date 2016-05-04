###############################################
# plot_f.R
# This file include the function for plot
#
# "state_latlon.csv" is needed.
#
# lc_heatmap(data, state = state, location = location, title)
#' @param data: A summary data with the variable interested named ratio
#' @param state: state is a data.frame provided by this file, please set it to state
#' @param location: location is provided by this file, please set it to location
#' @param title: title of the plot
#' @return a plot for ratio information over geography
#'
#'
#@ coeff_plot(list.l, status, sig)
#' @param list.l: the list of logistic regressions
#' @param status: a vector constains the status of loan
#' @param sig: true if a p-value plot is requested
#' @return a plot for the parameters' coefficient values or significant values
#'
#'
#' mis_plot <- function(true,pred)
#' @param a vector of true classification
#' @param a vector of prediction classification
#' @return a plot of the classification result
#'
################################################
# heatmap
########################
# Caculate the ratio
lc_ratio <- function(data, status = "Fully_Paid"){
  lc_geoSum <- data %>% group_by(addr_state) %>%  
    dplyr::summarize(ratio =
                       sum(loan_status == status)/length(loan_status))
  lc_geoSum <- as.data.frame(lc_geoSum)
  return(lc_geoSum)
}


# get the abbreviation and full name online
list <- data.frame(full = state.name, abb = state.abb)
state <- list
row.names(state) <- 1 : length(state[,1])




## drop the unnecessary areas 
dropArea <- function(x, state){
  test_that("The data should include variable addr_state", { # should include addr_state
    expect_equal(is.null(x$addr_state), F)
  })
  index_in <- which(x$addr_state %in% state$abb)
  x <- x[index_in,]
  #index_ex <- which(state$abb %in% x$addr_state)
  #state_ex <- state$abb[-index_ex]
  #x_not <- data.frame(addr_state = state_ex, ratio = rep(0,length(state_ex)))
  #x <- rbind(x, x_not)
  row.names(x) <- 1:length(x[,1])
  return(x)
}

## Make every alphabet to lower case and attach the full name of states
low <- function(x, state){
  index <- grep(pattern = as.character(x$addr_state), as.character(state$abb))
  x$region <- state$full[index]
  x$region <- tolower(x$region)
  return(x)
}

Low <- function(data, state){
  test_that("data should include variable addr_state", { # should include addr_state
    expect_equal(is.null(data$addr_state), F)
  })
  test_that("state should include variable abb", { # state should include abb
    expect_equal(is.null(state$abb), F)
  })
  y <- data %>% ddply(~addr_state, 
                      function(x){low(x, state)})
  return(y)
}

# Get Average Latitude and Longitude for US States
location <- read.csv("state_latlon.csv")
names(location)[1] <- c("addr_state")

loc <- function(x, location){
  test_that("data should include variable addr_state", { # should include addr_state
    expect_equal(is.null(x$addr_state), F)
  })
  x <- merge(x, location, by = "addr_state")
  return(x)
}

# map part
heatmap <- function(x, title){
  test_that("data should include variable named addr_state", { # should include addr_state
    expect_equal(is.null(x$addr_state), F)
  })
  test_that("data should include variable named ratio", { # should include ratio
    expect_equal(is.null(x$ratio), F)
  })
  states <- map_data("state")
  map.df <- merge(states,x, by="region", all.x=T)
  map.df <- map.df[order(map.df$order),]
  myPlot <- ggplot(map.df, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill= ratio))+
    geom_path()+ 
    scale_fill_gradientn(colours=rev(heat.colors(20)),na.value="grey90")+
    geom_text(aes(x=longitude, y=latitude, label=addr_state), 
              size=3)+
    ggtitle(title)+
    labs(x = "Longitude", y = "Latitude") +
    coord_map()
  return(myPlot)
}

lc_heatmap <- function(data, state, location, title){
  y <- dropArea(data, state)
  y <- Low(y, state)
  y <- loc(y, location)
  return(heatmap(y, title))  
}


################################################
#Logistic Regression
##################################################

# logistic significant level plot
coeff_plot1 <- function(log.f, sig = F, sig.value = 0.1){
  ## significance value
  plotdata <- summary(log.f)$coefficients[,4]
  # normalized paramter estimation
  plotdata1 <- summary(log.f)$coefficients[,1]/summary(log.f)$coefficients[,2]
  # order the data, select the significance value
  plotdata <- data.frame(var = names(plotdata), value = plotdata) %>%
    mutate(var = reorder(x = var, X = value, min)) %>%
    subset(value < sig.value)
  plotdata1 <- data.frame(var = names(plotdata1), value = plotdata1) %>%
    mutate(var = reorder(x = var, X = value, min)) %>% 
    subset(var %in% plotdata$var)
  if(sig){
    myplot <- ggplot(data=plotdata,
                     aes(x=var, y=value, colour=var)) +
      geom_point() +
      geom_hline(yintercept = 0.05, colour = "red", linetype = 3) +
      labs(y = "p - value", title = "Significant Value Plot")
  }else{
    myplot <- ggplot(data=plotdata1,
                     aes(x=var, y=value, colour=var)) +
      geom_point() +
      geom_hline(yintercept = 0, colour = "red", linetype = 3) +
      labs(y = "Normalized Parameter Estimated value", title = "Normalized Parameter Estimated Value Plot")
  }
  return(myplot)
}

### ROC curve
# Plot the prediction status with particular threshold
plot_pred_type_distribution <- function(df, threshold) {
  test_that("Cannot find \"pred\" or \"true\" in the data frame provided",{
    expect_equal(is.null(df$pred), F)
    expect_equal(is.null(df$true), F)
  })
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$true == 1, "pred=1,true=1", v) # TP
  v <- ifelse(df$pred >= threshold & df$true == 0, "pred=1,true=0", v) #FP
  v <- ifelse(df$pred < threshold & df$true == 1, "pred=0,true=1", v) #FN
  v <- ifelse(df$pred < threshold & df$true == 0, "pred=0,true=0", v) #TN
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=true, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}


# plot roc
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
    norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  myplot <- grid.arrange(p_roc, p_cost, ncol=2)
  return(myplot)
  
}