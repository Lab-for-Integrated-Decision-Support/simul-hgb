# Extracted functions from `03_Clinical_Accuracy.Rmd`
# Note: do NOT include `createPairedDataset` here


#'
#' @title Calculate Error Grid
#'
#' @description Calculates points within each area of Error Grid and plots
#'
#' @param df The paired samples data frame, calculated above
#' @param to.plot If TRUE [Default], displays the Error Grid plot
#' @param to.return If TRUE [Default], returns the plot
#'
calculateErrorGrid <- function (df, to.plot = T, to.return = T) {

  #'
  #' Sub-function to define the underlying grid pts
  #'
  makeBaseGrid <- function () {

    # Define the points which comprise the Error Grid
    A <- data.frame(
      X = c(0, 6, 6, 10, 25, 25, 9, 9, 5.4, 0),
      Y = c(0, 0, 5.4, 9, 9, 25, 25, 10, 6, 6))

    B <- data.frame(
      X = c(0, 25, 25, 0),
      Y = c(0, 0, 25, 25))

    C <- data.frame(
      X = c(0, 6, 6, 0),
      Y = c(10, 10, 25, 25))

    D <- data.frame(
      X = c(10, 10, 25, 25),
      Y = c(0, 6, 6, 0))

    # Generate grid
    p <-
      ggplot() +
      geom_abline(mapping = NULL, data = NULL,
                  slope = 1, intercept = 0, na.rm = FALSE,
                  show.legend = NA, size = 1) +
      geom_polygon(aes(x = X, y = Y), size = 1.5, color = 'green',
                   fill = 'green', alpha = 0.2, data = A) +
      geom_polygon(aes(x = X, y = Y), size = 1.5, color = 'yellow',
                   fill = 'yellow', alpha = 0.1, data = B) +
      geom_polygon(aes(x = X, y = Y), size = 1.5, color = 'red',
                   fill = 'red', alpha = 0.2, data = C) +
      geom_polygon(aes(x = X, y = Y), size = 1.5, color = 'red',
                   fill = 'red', alpha = 0.2, data = D) +
      coord_cartesian(ylim = c(4, 20), xlim = c(4, 20)) +
      scale_fill_distiller(palette = 4, direction = 1)

    return(list(
      A = A, B = B, C = C, D = D, p = p
    ))
  } # End of sub-function

  g <- makeBaseGrid()

  Queries <- as.matrix(df %>% dplyr::select(NUM_VAL.x, NUM_VAL.y))

  A.res <- ptinpoly::pip2d(Vertices = as.matrix(g$A), Queries = Queries)
  B.res <- ptinpoly::pip2d(Vertices = as.matrix(g$B), Queries = Queries)
  C.res <- ptinpoly::pip2d(Vertices = as.matrix(g$C), Queries = Queries)
  D.res <- ptinpoly::pip2d(Vertices = as.matrix(g$D), Queries = Queries)

  # First display raw "Box" output
  cat(sprintf(paste0(
    'Counts by Box:\n',
    '\tBox A: %d (%0.1f %%)\n',
    '\tBox B: %d (%0.1f %%)\n',
    '\tBox C: %d (%0.1f %%)\n',
    '\tBox D: %d (%0.1f %%)\n'),
    sum(A.res >= 0), sum(A.res >= 0) / length(A.res) * 100.,
    sum(B.res >= 0), sum(B.res >= 0) / length(A.res) * 100.,
    sum(C.res >= 0), sum(C.res >= 0) / length(A.res) * 100.,
    sum(D.res >= 0), sum(D.res >= 0) / length(A.res) * 100.))

  # Now display by Green, Yellow, Red
  #   Note that Green = A, Yellow = B - A - C - D, Red = C + D
  cat(sprintf(paste0(
    'Counts by Area:\n',
    '\tGreen Area: %d (%0.2f %%)\n',
    '\tYellow Area: %d (%0.2f %%)\n',
    '\tRed Area: %d (%0.2f %%)\n'),
    sum(A.res >= 0), sum(A.res >= 0) / length(A.res) * 100.,
    sum(B.res >= 0) - sum(A.res >= 0) - sum(C.res >= 0) - sum(D.res >= 0),
    ( sum(B.res >= 0) - sum(A.res >= 0) - sum(C.res >= 0) - sum(D.res >= 0) ) /
      length(A.res) * 100.,
    sum(C.res >= 0) + sum(D.res >= 0),
    (sum(C.res >= 0) + sum(D.res >= 0) ) / length(A.res) * 100.
  ))

  # Plot and return (pending parameters)
  if (to.plot & to.return) {
    Error_Grid <-
      g$p +
      geom_jitter(aes(x = NUM_VAL.x, y = NUM_VAL.y), data = df,
                  width = 0.3, height = 0.3, size = 0.3) +
      xlab('Reference Lab Value') +
      ylab('Measured Lab Value') +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))

  }

  if (to.plot) {
    print(Error_Grid)
  }

  if (to.return) {
    return(Error_Grid)

  } else {
    return()

  }
}


#'
#' @title Gather Covariates
#'
#' @description Creates pivoted data frame of covariate labs for each pair
#'
#' @details Requires that the column `ORDER_PROC_KEY.x` is unique in the
#'     paired dataframe. This will be true when the data frame is created
#'     using the above function `createPairedDataset()` which catches duplicates.
#'
#'     To find covariates, instead of matching on the `ORDER_PROC_KEY.x` which
#'     is correct for some of the components, matches on the PN[2] collected time
#'     (`COLLECTED_DT.y`) and the encounter key. These are checked to be unique
#'     in the input joined data frame as well.
#'
#' @param paired.df The paired dataframe containing unique values at the
#'     `ORDER_PROC_KEY.x` column, which is the column for the order key for
#'     PN[1] (typically the CBC). Also must contain (at least) the columns
#'     `COLLECTED_DT.y` and `ENC_KEY`
#' @param labs.df The full labs data frame
#' @param covars A list of covariate names into the `COMP_NAME` column
#'
#' @returns A pivoted data frame of covariates
#'
gatherCovariates <- function (paired.df, labs.df,
                              covars = c('pH', 'Bicarb', 'iCal', 'Gluc', 'Lactate')) {

  # Ensure that these are all unique
  if (length(unique(paired.df$ORDER_PROC_KEY.x)) !=
      length(paired.df$ORDER_PROC_KEY.x))
    stop('PN[1] Order Proc Keys should be unique')

  # Ensure that collected time of PN[2] and ENC_KEY are distinct
  if (nrow(paired.df) !=
      nrow(paired.df %>%
           dplyr::select(COLLECTED_DT.y, ENC_KEY) %>%
           dplyr::distinct()))
    stop('COLLECTED_DT.y and ENC_KEY tuple are not distinct in paired data frame')

  # Filter to remove any cancelled labs or NaNs
  filtered.df <-
    labs.df %>%
    dplyr::filter(!is.na(NUM_VAL) & NUM_VAL != 9999999.)

  # Initialize the result data frame using the unique `ORDER_PROC_KEY.x` values
  result.df <- data.frame(
    ORDER_PROC_KEY.x = paired.df$ORDER_PROC_KEY.x
  )

  cat(sprintf('NUmber of unique PN[1] order procedure keys: %d\n',
              nrow(result.df)))

  # Join the filtered data frame to full lab results and find matches
  # on the ENC_KEY and COLLECTED_DT.y
  joined.df <-
    dplyr::inner_join(
      x = paired.df %>%
        dplyr::select(ORDER_PROC_KEY.x, ENC_KEY, COLLECTED_DT.y),
      y = filtered.df,
      by = c('ENC_KEY', 'COLLECTED_DT.y' = 'COLLECTED_DT')
    )

  # Join each component to the results data frame
  for (CN in covars) {

    result.df <-
      joined.df %>%
      dplyr::filter(COMP_NAME == CN) %>%
      dplyr::select(ORDER_PROC_KEY.x, NUM_VAL, RESULT_DT) %>%
      dplyr::arrange(ORDER_PROC_KEY.x, RESULT_DT) %>%
      dplyr::group_by(ORDER_PROC_KEY.x) %>%
      dplyr::summarize(
        LAST_ADD = first(NUM_VAL)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::right_join(
        y = result.df,
        by = c('ORDER_PROC_KEY.x')
      )

    names(result.df)[which(names(result.df) == 'LAST_ADD')] <- CN
  }

  return(result.df)
}


#'
#' @title Display Covariate Stats
#'
#' @description Displays statistics on covariates in the data frame
#'
#' @param covars.df The Covariates data frame from `gatherCovariates()` function
#'
displayCovariateStats <- function (covars.df) {

  # Display stats on the covariates, including checking for NULL values and
  # displaying distributions
  for (index in 2 : ncol(covars.df)) {

    this.vec <- covars.df[,index]

    print(summary(this.vec))

    cat(sprintf('Count (and %%) of NAs in %s column: %d (%0.2f %%)\n',
                names(covars.df)[index],
                sum(is.na(this.vec)),
                sum(is.na(this.vec)) / nrow(covars.df) * 100.))

    bounds <- quantile(this.vec, probs = c(0.01, 0.99), na.rm = T)

    this.df <-
      covars.df %>%
      dplyr::select(all_of(index))

    names(this.df) <- c('val')

    this.filt.df <-
      this.df %>%
      dplyr::filter(val >= bounds[1] & val <= bounds[2])

    hist.bins <- min(
      length(unique(this.filt.df$val)),
      40)

    p <-
      this.filt.df %>%
      ggplot() +
      geom_histogram(aes(x = val), bins = hist.bins) +
      xlab(paste0(names(covars.df)[index], ' values (1st - 99th percentile)')) +
      ylab('Count') +
      theme_bw()

    print(p)
  }

}


#'
#' @title Join Impute Regress
#'
#' @description Join covars and pairs, impute NA values, regress, and report results
#'
#' @param paired.df A dataframe of paired PN[1] and PN[2] values, created from
#'     the function `createPairedDataset()`
#' @param covars.df A dataframe of covariates, created from the
#'     function `gatherCovariates()`
#' @param thresh.list A list of three-element vectors, where each three-element
#'     vector is of the format c(min, max, thresh). Such that for the list of
#'     pairs, if the mean Hgb (between PN[1] and PN[2]) falls between `min` and
#'     `max` and the difference is less than the threshold `thresh`, then the
#'     pair is considered `WELL_MATCHED` for the purposes of our logistic
#'     regression.
#' @param impute.fx The function for imputing NA values [Default: `median`]
#' @param ci If TRUE, compute the confidence intervals on the regression results
#'     [Default: TRUE]
#'
#' @returns A list of the regression results as well as CIs (if computed)
#'
joinImputeRegress <- function (paired.df, covars.df, thresh.list,
                               impute.fx = median, ci = T) {

  # First we join the paired data frame (as the basis) with the covariates,
  # by the unique ORDER PROC key of PN[1] (should be CBC)
  joined.df <-
    dplyr::left_join(
      x = paired.df,
      y = covars.df,
      by = c('ORDER_PROC_KEY.x')
    ) %>%
    dplyr:: select(NUM_VAL.x, NUM_VAL.y, AGE_PROC, pH, Bicarb, iCal, Gluc, Lactate, DEPT)

  # Impute NA values based on the impute function
  impute.df <-
    joined.df %>%
    dplyr::mutate(
      pH =      ifelse( is.na(pH),      impute.fx(joined.df$pH, na.rm = T),      pH),
      Gluc =    ifelse( is.na(Gluc),    impute.fx(joined.df$Gluc, na.rm = T),    Gluc),
      iCal =    ifelse( is.na(iCal),    impute.fx(joined.df$iCal, na.rm = T),    iCal),
      Lactate = ifelse( is.na(Lactate), impute.fx(joined.df$Lactate, na.rm = T), Lactate),
      Bicarb =  ifelse( is.na(Bicarb),  impute.fx(joined.df$Bicarb, na.rm = T),  Bicarb)
    )

  # Threshold to create "WELL_MATCHED" column
  thresh.df <-
    impute.df %>%
    dplyr::mutate(
      MEAN_HGB = (NUM_VAL.x + NUM_VAL.y) / 2.,
      DIFF_HGB = abs(NUM_VAL.x - NUM_VAL.y),
      WELL_MATCHED = F # Default to FALSE
    )

  # Loop through the threshold list and OR together the WELL_MATCHED values
  for (t in thresh.list) {
    if (length(t) != 3)
      stop('Each vector within the threshold list must be three elements')

    thresh.df$WELL_MATCHED <-
      thresh.df$WELL_MATCHED |
      (thresh.df$MEAN_HGB > t[1] & thresh.df$MEAN_HGB < t[2] & thresh.df$DIFF_HGB < t[3])
  }

  # Remove the MEAN and DIFF variables, as well as PN[1] value
  thresh.df <-
    thresh.df %>%
    dplyr::select(-MEAN_HGB, -DIFF_HGB, -NUM_VAL.x)


  cat(sprintf('Number (%%) of `WELL MATCHED`: %d (%0.2f %%)\n',
              sum(thresh.df$WELL_MATCHED),
              sum(thresh.df$WELL_MATCHED) / nrow(thresh.df) * 100.))

  # Run the logistic regression
  reg.model <- glm(
    WELL_MATCHED ~ NUM_VAL.y +
      pH +
      Gluc +
      Bicarb +
      iCal +
      Lactate +
      AGE_PROC +
      DEPT,
    family = 'binomial',
    data = thresh.df %>%
      dplyr::mutate(pH = pH * 10.)
  )

  print(summary(reg.model))

  print(exp(reg.model$coefficients))

  if (ci) {

    ci.reg <- confint(reg.model)

    print(exp(ci.reg))

    return(list(
      reg.model,
      ci.reg
    ))

  } else {
    return(list(
      reg.model
    ))
  }
}


#'
#' @title Calculate Cohen Kappa
#'
#' @description Calculates the Cohen Kappa statistic for two vectors of CN values
#'
#' @details
#'     Recall that Cohen's Kappa is defined as:
#'
#'       K = (P_observed - P_expected) / (1 - P_expected)
#'
#'       Where:
#'         P_expected = sum(P_pos + P_neg) with
#'           P_pos = P_raterA+ x P_raterB+ and P_neg = P_raterA- x P_raterB-
#'
#'     In our case, Rater A will be positive when value.x (from PN[1]) are less
#'     than the cutoff, suggesting the need for a transfusion. Similarly,
#'     Rater B will be positive when value.y (from PN[2]) are less than the
#'     cutoff.
#'
#' A "Positive" response (meaning we have to transfuse) is when Hgb < cutoff,
#' and a "Negative" response (meaning we do not transfuse) is when Hgb >= cutoff.
#'
#' @param values.x A column vector of Hgb values from PN[1]
#' @param values.y A column vector of Hgb values from PN[2] (with length
#'     same as rater.A.bg)
#' @param cutoff A scalar representing the Hgb cutoff value
#' @param to.print If TRUE, prints results in addition to returning [Default]
#'
#' @return The Cohen Kappa for these two vectors at the cutoff given
#'
calculateCohenKappa <- function (values.x, values.y,
                                 cutoff = 7.0, to.print = T) {

  # Verify that the lengths of the two vectors of PN values are identical
  if (length(values.x) != length(values.y))
    stop('Error: vectors for X and Y must be of equal lengths')

  if (to.print)
    cat(sprintf('Pre-Range Check Length: %d\n', length(values.x)))

  if (to.print)
    cat(sprintf('Cutoff value used: %d\n', cutoff))

  # Consider a 2x2 matrix with two "Raters" (or two vectors):
  #
  #                      values.x
  #                   Yes   |     No
  #                -------------------
  #            Yes |   A    |    B   |
  #  values.y  ----|--------|--------|
  #            No  |   C    |    D   |
  #                -------------------
  #
  #  where: len = A + B + C + D
  #
  P.x.pos <- sum(values.x < cutoff) / length(values.x) # Equiv to 'A + C'  / len
  P.x.neg <- sum(values.x >= cutoff) / length(values.x) # Equiv to 'B + D' / len

  P.y.pos <- sum(values.y < cutoff) / length(values.y) # Equiv to 'A + B' / len
  P.y.neg <- sum(values.y >= cutoff) / length(values.y) # Equiv to ''C + D' / len

  if (to.print) {
    cat(sprintf('X :: Pos: %0.2f\tNeg: %0.2f\n', P.x.pos, P.x.neg))
    cat(sprintf('Y :: Pos: %0.2f\tNeg: %0.2f\n', P.y.pos, P.y.neg))
  }

  P.pos <- P.x.pos * P.y.pos
  P.neg <- P.x.neg * P.y.neg

  if (to.print)
    cat(sprintf('\tP.pos: %0.4f\n\tP.neg: %0.4f\n', P.pos, P.neg))

  P.exp <- P.pos + P.neg

  # Observed are the sum of counts of when both are either > or <=, divided by total
  # Note that we can divide by either length(rater.A.bg) or length(rater.B.cbc)
  # since they are equal
  P.obs <- (
    sum((values.x < cutoff) & (values.y < cutoff)) +
      sum((values.x >= cutoff) & (values.y >= cutoff)) ) / length(values.x)

  if (to.print)
    cat(sprintf('\tP.obs: %0.2f\n\tP.exp: %0.2f\n', P.obs, P.exp))

  kappa <- (P.obs - P.exp) / (1. - P.exp)

  if (to.print)
    cat(sprintf('Kappa: %0.2f\n', kappa))

  return(kappa)
}


#'
#' @title Transfusion Confusion Matrix
#'
#' @description Creates a 2x2 confusion matrix for a pair of cutoffs and direction
#'
#' @param value.x The value of PN[1] elements (typically CBC)
#' @param value.y The value of PN[2] elements (typically BG or iSTAT)
#' @param cutoffs A two-element vector which specifies the cutoffs for PN[1]
#'     (which is typically the gold standard CBC) and for PN[2] (typically the
#'     "test" BG or iStat)
#' @param to.print If TRUE, prints results [Default]
#' @param to.return If TRUE, returns results [Default]
#'
transfusionConfusionMatrix <- function (value.x, value.y,
                                        cutoffs = c(7., 7.),
                                        to.print = T,
                                        to.return = T) {

  # Are the lengths equal
  stopifnot(length(value.x) == length(value.y))

  # Is `cutoffs` a two-element vector?
  stopifnot(length(cutoffs) == 2)

  if (to.print)
    cat(sprintf('Total number of input rows: %d\n',length(value.x)))

  # 2x2 Standard Table:
  #
  #               Gold Standard (PN[1])
  #                      value.x
  #                 Pos     |   Neg
  #               ----------------------
  #           Pos |   TP    |     FP   |
  # Test (PN[2])  |---------|----------|
  #  value.y  Neg |   FN    |     TN   |
  #               ----------------------
  #
  # In the default case, we consider "POS" to reflect the true need for a
  # transfusion, meaning the value was < the cutoff

  if (to.print)
    cat(sprintf('Gold Standard:\n\tPositive: %d (%0.2f %%)\n\tNegative: %d (%0.2f %%)\n',
                sum( value.x < cutoffs[1] ),
                sum( value.x < cutoffs[1] ) / length(value.x) * 100.,
                sum( value.x >= cutoffs[1] ),
                sum( value.x >= cutoffs[1] ) / length(value.x) * 100.))

  # Rater A - yes, Rater B - yes
  TP <- sum( (value.x < cutoffs[1]) & (value.y < cutoffs[2]) )

  # Rater A - yes, Rater B - no
  FP <- sum( (value.x >= cutoffs[1]) & (value.y < cutoffs[2]) )

  # Rater A - no, Rater B - yes
  FN <- sum( (value.x < cutoffs[1]) & (value.y >= cutoffs[2]) )

  # Rater A - no, Rater B - no
  TN <- sum( (value.x >= cutoffs[1]) & (value.y >= cutoffs[2]) )

  # Dummy check - do these all add up to total length
  stopifnot(TP+FP+TN+FN == length(value.x))

  sens <- TP / (TP + FN)
  spec <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (FN + TN)

  if (to.print)
    cat(sprintf(paste0(
      'Cutoffs: PN[1]: %0.1f\tPN[2]: %0.1f\n',
      'TP: %d (%0.4f %%)\t',
      'FP: %d (%0.4f %%)\n',
      'FN: %d (%0.4f %%)\t',
      'TN: %d (%0.4f %%)\n',
      'Sens: %0.4f\n',
      'Spec: %0.4f\n',
      'PPV: %0.4f\n',
      'NPV: %0.4f\n',
      'FOR (1-NPV): %0.4f\n',
      'NNM (1/FOR): %0.4f\n\n'),
      cutoffs[1], cutoffs[2],
      TP, TP / length(value.x) * 100.,
      FP, FP / length(value.x) * 100.,
      FN, FN / length(value.x) * 100.,
      TN, TN / length(value.x) * 100.,
      sens, spec, ppv, npv, (1. - npv), 1. / (1. - npv)
    ))

  if (to.return) {
    return(list(
      cutoffs = cutoffs,
      TP = TP, FP = FP, TN = TN, FN = FN,
      sens = sens, spec = spec, ppv = ppv, npv = npv,
      falseOR = 1. - npv,
      nnm = 1. / (1. - npv)
    ))
  }
}


#'
#' @title Calculate Threshold ROC
#'
#' @description Calculates an ROC and P-R based on Transfusion "Test"
#'
#' @details This function makes use of the above `transfusionConfusionMatrix`
#'     function, which returns a sensitivity and specificity at a given pair
#'     of thresholds - the CBC threshold (PN[1]) and the PN[2] threshold
#'     (either BG or ISTAT).
#'
#'     By generating confusion matrices across a range of test PN[2]
#'     thresholds (default from 0 g/dL to 25 g/dL, the full range of Hgb), at
#'     both "low" and "high" CBC Hgb thresholds (5 g/dL and 7 g/dL), we can
#'     generate ROC curves as well as P-R curves for these two conditions
#'     (low, high). These curves represent the ability of the "test" values
#'     (either BG or iStat) to discriminate the "true" condition of anemia
#'     as defined by a given threshold (low, 5 or high, 7).
#'
#'     We calculate the AUROC using trapezoidal (numeric) integration. We can
#'     also identify the "optimal" threshold to use to maximize sensitivity
#'     and specificity by minimizing the Euclidian distance to the point (0,1)
#'     on the ROC curve.
#'
#'     Similarly, on the P-R curve, we can visualize the tradeoff between
#'     precision (PPV) and recall (sensitivity). In this situation,
#'     precision refers to the % of test values below a threshold which
#'     represent actual anemia (or actual TPs), and is dependent on the
#'     incidence of anemia in the population. Recall (sensitivity) represents
#'     the % of actually anemic patients which are identified by the "test" Hgb.
#'
#' @param paired.df The paired data frame containing `value.x` and `value.y`
#' @param to.print If TRUE, prints results [Default]
#' @param to.return If TRUE, returns results as a list of elements [Default]
#' @param cutoff.minmax A two-element vector of the minimum and maximum Hgb
#'     values used to generate the full cutoff sequence [Default: 0., 25.]
#' @param cutoff.by The difference between successive values in the cutoff seq
#'
calculateThresholdROC <- function (paired.df,
                                   to.print = T, to.return = T,
                                   cutoff.minmax = c(0., 25.),
                                   cutoff.by = 0.1) {

  # Establish the sequence for iterating through the threshold calculation
  cutoff.range <- seq(
    from = cutoff.minmax[1],
    to = cutoff.minmax[2],
    by = cutoff.by
  )

  # Define the empty result data frame
  roc.df <- data.frame()

  # Loop through the cutoff range
  for (index in 1 : length(cutoff.range)) {

    # First calculate using the "high" CBC (or PN[1]) value, 7.0
    res.high <-
      transfusionConfusionMatrix(
        value.x = paired.df$NUM_VAL.x,
        value.y = paired.df$NUM_VAL.y,
        cutoffs = c(7., cutoff.range[index]),
        to.print = F,
        to.return = T
      )

    # Now calculate using the "low" CbC (or PN[1]) value, 5.0
    res.low <-
      transfusionConfusionMatrix(
        value.x = paired.df$NUM_VAL.x,
        value.y = paired.df$NUM_VAL.y,
        cutoffs = c(5., cutoff.range[index]),
        to.print = F,
        to.return = T
      )

    # RBind to the data frame
    roc.df <-
      rbind(
        roc.df,
        data.frame(
          tpr = res.high$sens,
          fpr = 1.0 - res.high$spec,
          precision = res.high$ppv,
          recall = res.high$sens,
          cbc.cutoff = 'High (7.0 g/dL)',
          bg.cutoff = cutoff.range[index]
        ),
        data.frame(
          tpr = res.low$sens,
          fpr = 1.0 - res.low$spec,
          precision = res.low$ppv,
          recall = res.low$sens,
          cbc.cutoff = 'Low (5.0 g/dL)',
          bg.cutoff = cutoff.range[index]
        )
      )
  }

  # Trapezoidal integration to determine AUROC values
  auroc <-
    roc.df %>%
    dplyr::group_by(cbc.cutoff) %>%
    dplyr::mutate(
      diff.fpr = lead(fpr) - fpr
    ) %>%
    dplyr::filter(!is.na(diff.fpr)) %>%
    dplyr::mutate(
      mult = diff.fpr * tpr
    ) %>%
    dplyr::summarize(
      AUROC = sum(mult)
    )

  # Display results of trapezoidal integration
  if (to.print) {
    print(
      knitr::kable(
        auroc,
        col.names = c('CBC Cutoff', 'AUROC'),
        digits = c(0,3)
      ) %>%
        kableExtra::kable_paper("hover")
    )
  }

  # Identify optimum distance to point (0,1) by Euler's distance
  opt.cutoff <-
    roc.df %>%
    dplyr::mutate(
      dist = sqrt( (1. - tpr) ^ 2 + (fpr) ^ 2 )
    ) %>%
    dplyr::arrange(cbc.cutoff, dist) %>%
    dplyr::group_by(cbc.cutoff) %>%
    dplyr::summarize(
      DIST = first (dist),
      CUTOFF = first(bg.cutoff),
      SENS = first(tpr),
      SPEC = 1. - first(fpr)
    )

  if (to.print)
    print(
      knitr::kable(
        opt.cutoff,
        col.names = c('CBC Cutoff', 'Distance', 'Cutoff', 'Sens', 'Spec'),
        digits = c(0, 3, 1, 3, 3)
      ) %>%
        kableExtra::kable_paper("hover")
    )

  # Plot ROC curve
  p.roc <-
    roc.df %>%
    ggplot(aes(x = fpr, y = tpr, color = cbc.cutoff)) +
    geom_point(size = 2) +
    geom_line(size = 1.2) +
    annotate('segment', x = 0, xend = 1, y = 0, yend = 1, color = '#666666', linetype = 'dashed') +
    xlab('False positive rate (1 - spec)') +
    ylab('True positive rate (sens)') +
    labs(color = 'CBC Cutoff') +
    theme_bw() +
    theme(legend.position = c(.6,.3))

  if (to.print)
    print(p.roc)

  # Plot Precision Recall curve
  p.pr <-
    roc.df %>%
    ggplot(aes(x = recall, y = precision, color = cbc.cutoff)) +
    geom_point(size = 2) +
    geom_line(size = 1.2) +
    xlim(0,1) + ylim(0,1) +
    xlab('Recall (sens)') +
    ylab('Precision (ppv)') +
    labs(color = 'CBC Cutoff') +
    theme_bw() +
    theme(legend.position = c(.8,.9))

  if (to.print)
    print(p.pr)

  if (to.return)
    return(list(
      auroc = auroc,
      opt.cutoff = opt.cutoff,
      p.roc = p.roc,
      p.pr = p.pr#,
      #roc.df = roc.df
    ))
}


#'
#' @title Run All Clinical
#'
#' @description Runs through all clinical accuracy tasks, for sensitivity analysis
#'
#' @param labs.df The original labs data frame
#' @param cohort.df The original cohort data frame
#' @param compare.PN The comparison PROC name (e.g. either `BG` or `ISTAT`)
#' @param time.diff The cutoff time difference (in minutes) for determining
#'     whether labs are "simultaneous"
#' @param multi.per.pt If TRUE, allows all results from patients;
#'     If FALSE, only the first (chronological) result from a patient is included
#' @param primary.hgb.cutoff The primary Hgb cutoff to use for Cohen's Kappa
#' @param sens.hgb.cutoffs The secondary Hgb cutoffs for sensitivity analysis
#' @param run.date A string representation of date for saving (format: %Y-%m-%d)
#' @param save.fn The file name (which will be concatenated with SITE and run.date),
#'     or NA [Default] if we do not wish to save any results to a file
#'
runAllClinical <- function (labs.df, cohort.df, compare.PN,
                            time.diff, multi.per.pt, primary.hgb.cutoff,
                            sens.hgb.cutoffs, run.date, save.fn = NA) {

  # Generate the paired dataset
  paired.df <- createPairedDataset(
    labs.df = labs.df,
    cohort.df = cohort.df,
    PN = c('CBC', compare.PN),
    CN = 'Hgb',
    time.diff = time.diff,
    multi.per.pt = multi.per.pt
  )

  # Calculate and display the error grid
  error.grid <- calculateErrorGrid(
    df = paired.df,
    to.plot = T,
    to.return = T
  )

  # Gather covariates for this paired set
  covars.df <- gatherCovariates(paired.df, labs.df)

  # Display summaries of those covars
  displayCovariateStats(covars.df)

  # Threshold lists are a list of three-element vectors, which the three elements
  # corresponding to: min, max, threshold
  # This can be read as, between the min and max, the mean diff must be less than
  # the threshold, otherwise it is not `WELL_MATCHED`
  thresh.list <-
    list(
      c(-100, 6, 1.5),
      c(6, 9, 1.0),
      c(9, 100, 1.5)
    )

  # Join, impute, and run regression on these pairs
  regress.res <-
    joinImputeRegress(
      paired.df = paired.df,
      covars.df = covars.df,
      thresh.list = thresh.list,
      impute.fx = median,
      ci = T
    )


  # Calculate the Cohen's Kappa, both at primary and sensitivity hgb cutoffs
  calculateCohenKappa(
    values.x = paired.df$NUM_VAL.x,
    values.y = paired.df$NUM_VAL.y,
    cutoff = primary.hgb.cutoff,
    to.print = T
  )

  for (thresh in sens.hgb.cutoffs) {

    calculateCohenKappa(
      values.x = paired.df$NUM_VAL.x,
      values.y = paired.df$NUM_VAL.y,
      cutoff = thresh,
      to.print = T
    )

  }

  rm(thresh)

  # Compute Transfusion Confusion Matrix at a range of PN[2] cutoff values
  mat.across.cutoffs <- list()

  for (pn2.cutoff in c(7.0, 7.5, 8.0, 8.5, 9.0)) {

    res <- transfusionConfusionMatrix(
      value.x = paired.df$NUM_VAL.x,
      value.y = paired.df$NUM_VAL.y,
      cutoffs = c(primary.hgb.cutoff, pn2.cutoff),
      to.print = T,
      to.return = T
    )

    mat.across.cutoffs <-
      append(
        mat.across.cutoffs,
        list(list( # Use a double list here so that we enclose each cutoff results in a list
          res = res, cutoff = pn2.cutoff
        ))
      )
  }

  rm(pn2.cutoff)

  # And calculate the ROC and P-R curves
  thresh.roc <- calculateThresholdROC(
    paired.df = paired.df,
    to.print = T,
    to.return = T,
    cutoff.by = 0.01)

  # Save out?
  if (! any(is.na(save.fn)) ) {
    save(
      file = file.path(
        Sys.getenv('PICU_LAB_DATA_PATH'),
        paste0(
          Sys.getenv('PICU_LAB_SITE_NAME'),
          '_', save.fn, '_',
          run.date, '.rData'
        )
      ),
      primary.cutoff,
      #paired.df, covars.df,
      error.grid,
      thresh.list, regress.res, mat.across.cutoffs,
      primary.hgb.cutoff, thresh.roc
    )
  }
}
