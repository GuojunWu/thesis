# This part is for selecting interaction effect based on the whole features
# The interaction is measured by Friedman¡¯s H-statistic (square root of the H-squared test statistic)
# and takes on values between 0 (no interaction) to 1.
#
# First, overall interaction is computed and then two-way interaction

###### Overall interaction ######

# 1. Compute H-statistics h and ordered H-statistics "h_ord" ---------------------------------

# use library iml to get interaction importance
mod <-
  Predictor$new(
    model = RF,
    data = trainingdata,
    y = "Y",
    class = 1,
    type = "prob"
  )
ia <- Interaction$new(mod) # plot(ia)

# (1) Compute interaction
h <- as.data.frame(ia$results$.interaction)

# (2) Compute ordered interaction

# Regressor name ordered by interaction (ascending)
x_ord.h <- order(h, decreasing = F)

# Ordered importance (ascending)
h_ord <- h[x_ord.h, 1]



# (3) Importance plot
if (run_plots) {
  rank <- 1:length(h_ord)
  plot(ia)
  # 1
  plot(
    rank,
    h_ord,
    ylab = "overall interaction strength",
    xlab = "features",
    type = "b",
    lty = 3,
    lwd = 2,
    cex.lab = 1.15,
    cex.axis = 1.15
  )
}


# 2. Derivative ---------------------------------


sel_h_ord <- h_ord
sel_x_ord.h <- x_ord.h

backwa <- backward(sel_h_ord)

derivative_first.h <- backwa$first_derivative
derivative_second.h <- backwa$second_derivative

if (run_plots) {
  sel_rank <- 1:length(sel_h_ord)
  plot(
    derivative_first.h,
    xlab = "features",
    ylab = "1st diff of overall interaction strength",
    type = "b",
    lty = 3,
    lwd = 2,
    cex.lab = 1.15,
    cex.axis = 1.15
  )
}


# Summary DATA: descending h
interactions <-
  cbind(
    rev(sel_x_ord.h),
    rev(sel_h_ord),
    rev(derivative_first.h),
    rev(derivative_second.h)
  )
colnames(interactions) <- c("X", "H", "first", "second")


# 3. Threshold ---------------------------------------------------------------------

# rule 1: spline, 2rd derivative = 0, find number before the first negative number (the last point which above the zero line)
# if using directly derivative, the bias is large
# rule 2: directly derivative, maximize 1st derivative, find the number is before largely falling down


# Selected interactions
interactions_h <-
  select(interactions, x_ord.h, threshold.h, rule, peak)$x
# Dropped interactions


# Second, two-way interaction plot is computed and then two-way interaction plot


# 1. Compute h-statistics "h" and ordered-h "h_ord" ---------------------------------
feature_int = vector()

# use library iml to get interaction importance
for (m in 1:length(interactions_h)) {
  ia.1 <-
    Interaction$new(mod, feature = paste("X", interactions_h[m], sep = "")) #"id_interact"
  
  # Whether to delete unimportant interactions
  # Get ia_result
  
  term_inte <- ia.1$results$.feature
  drop_inte <- paste("X", del_x, ":", sep = "")
  position <- vector()
  
  for (i in 1:length(drop_inte)) {
    position[i] <- grep(drop_inte[i], term_inte)
  }
  ia_result <- ia.1$results[-position, ]
  
  
  
  # (1) Compute interaction
  h.1 <- as.data.frame(ia_result$.interaction)
  
  # (2) Compute ordered interaction
  
  # Rank of interaction ordered by h-statistics (ascending)
  x_ord.h.1 <- order(h.1, decreasing = F)
  
  # Ordered interaction (ascending)
  h_ord.1 <- h.1[x_ord.h.1, 1]
  
  # (3) Interaction plot
  if (run_plots) {
    rank <- 1:length(h_ord.1)
    #plot(ia.1)
    plot(
      rank,
      h_ord.1,
      ylab = "2-way interaction strength",
      xlab = "features",
      type = "b",
      lty = 3,
      lwd = 2,
      cex.lab = 1.15,
      cex.axis = 1.15
    )
  }
  
  
  # 2. derivative ---------------------------------
  
  # (1) Delete regressors with extreme high and low imp value
  
  
  sel_h_ord.1 <- h_ord.1
  sel_x_ord.h.1 <- x_ord.h.1
  
  # (2) Compute derivative
  
  
  backwa <- backward(sel_h_ord.1)
  
  derivative_first.h.1 <- backwa$first_derivative
  derivative_second.h.1 <- backwa$second_derivative
  
  if (run_plots) {
    sel_rank <- 1:length(sel_h_ord.1)
    
    # plot(derivative_first,col = "darkgreen",type="b")
    plot(
      derivative_first.h.1,
      ylab = "1st diff of 2-way interaction strength",
      xlab = "interactions",
      type = "b",
      lty = 3,
      lwd = 2,
      cex.lab = 1.15,
      cex.axis = 1.15
    )
  }
  
  
  # Summary DATA: descending h
  interactions.1 <-
    cbind(
      rev(sel_x_ord.h.1),
      rev(sel_h_ord.1),
      rev(derivative_first.h.1),
      rev(derivative_second.h.1)
    )
  colnames(interactions.1) <- c("X", "H", "first", "second")
  
  
  # 3. Threshold ---------------------------------------------------------------------
  
  # rule 1: spline, 2rd derivative = 0, find number before the first negative number (the last point which above the zero line)
  # if using directly derivative, the bias is large
  # rule 2: directly derivative, maximize 1st derivative, find the number is before largely falling down
  
  
  interactions_h.1 <-
    select(interactions.1, x_ord.h.1, threshold.h.1, rule, peak = 1)$x
  
  # 4. Formulation -----------------------------------------------------------------------
  
  # Transfer form interaction number to Interaction term "
  term_inte <- ia_result$.feature
  
  # Selected interaction term
  features1_int = vector()
  for (i in 1:length(interactions_h.1)) {
    sel_term_inte <- term_inte[interactions_h.1[i]]
    features1_int <-
      c(gsub(":", ":", sel_term_inte, fixed = T), features1_int)
    
  }
  feature_int <- c(feature_int, features1_int)
  # including important features
}


