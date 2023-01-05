# Extracted functions from `04_Temporal_Analysis.Rmd`

#'
#' @title Describe Repeated Pairs
#'
#' @description Describes counts of repeated pairs and time differences between pairs
#'
#' @param paired.df The data frame of simultaneous (paired) results
#' @param to.print If TRUE, prints results [Default]
#' @param to.return If TRUE, returns results [Default]
#'
describeRepeatedPairs <- function (paired.df, to.print = T, to.return = T) {

  # Identify the counts of simultaneous labs per patient
  counts.per.enc <-
    paired.df %>%
    dplyr::group_by(ENC_KEY) %>%
    dplyr::count()

  if (to.print) {
    cat(sprintf('Summary of counts of pairs per patient:\n'))
    print(summary(counts.per.enc$n))
  }

  # Plot a histogram of counts per patient
  p.counts <-
    counts.per.enc %>%
    dplyr::filter(n > quantile(counts.per.enc$n, probs = c(0.01)) &
                    n < quantile(counts.per.enc$n, probs = c(0.99))) %>%
    ggplot() +
    geom_histogram(aes(x = n), bins = 40) +
    xlab('Number of Simultaneous (Paired) Labs Per Encounter') +
    ylab('Count') +
    theme_bw()

  if (to.print)
    print(p.counts)

  # What is the time difference between results for each patient?
  time.diff.per.enc <-
    paired.df %>%
    dplyr::arrange(ENC_KEY, COLLECTED_DT.x) %>%
    dplyr::group_by(ENC_KEY) %>%
    dplyr::mutate(
      DIFF_ENC = ENC_KEY == lag(ENC_KEY),
      TIME_DIFF = as.numeric(
        COLLECTED_DT.x - lag(COLLECTED_DT.x),
        units = 'mins'
      )
    ) %>%
    dplyr::filter(!is.na(DIFF_ENC)) %>%
    dplyr::ungroup()

  if (to.print) {
    cat(sprintf('Time diff (in Hours):\n'))
    print(summary(time.diff.per.enc$TIME_DIFF / 60.))
  }

  # Plot a histogram of the time difference, between 1st and 99th percentile
  p.time.diff <-
    time.diff.per.enc %>%
    dplyr::filter(TIME_DIFF > quantile(time.diff.per.enc$TIME_DIFF, probs = c(0.01)) &
                    TIME_DIFF < quantile(time.diff.per.enc$TIME_DIFF, probs = c(0.99))) %>%
    ggplot() +
    geom_histogram(aes(x = TIME_DIFF / 60.), bins = 40) +
    xlab('Time Between Successive Simultaneous (Paired) Labs, Per Enc (Hours)') +
    ylab('Count') +
    theme_bw()

  if (to.print)
    print(p.time.diff)

  if (to.return)
    return(list(
      #time.diff = time.diff.per.enc,
      p.time.diff = p.time.diff,
      #counts.per.enc = counts.per.enc,
      p.counts = p.counts
    ))
}


#'
#' @title Calculate Change Comparison
#'
#' @description Generates Delta Error Grid and Percents by Zone
#'
#' @param paired.df The data frame of paired simultaneously acquired labs
#' @param time.diff.max The maximum time difference (in min) between successive
#'     simultaneous labs within the same encounter, to consider for this analysis.
#'     The default [Inf] allows all labs without a restriction.
#' @param pt.size The point size on the Error Grid plot [Default: 0.3]
#' @param pt.alpha The point alpha value on the Error Grid plot [Default: 0.3]
#' @param perturb.amt A two-element list which defines the min and max amount
#'     that each point is perturbed on the PERC_VALUE x-y plot. This is
#'     necessary to avoid points falling on the exact edge of the grids, which
#'     causes double-counting within two error grid polygons. To avoid this, we
#'     perturb the point in both the x and y space by a random value,
#'     uniformilly distributed between these two elements of the list.
#' @param to.print If True, prints results [Default]
#' @param to.return If True, returns results [Default]
#'
calculateChangeComparison <- function (paired.df, time.diff.max = Inf,
                                       pt.size = 0.3, pt.alpha = 0.3,
                                       perturb.amt = c(0.0001, 0.001),
                                       to.print = T, to.return = T) {

  #'
  #' Sub-function to define the underlying grid pts
  #'
  makeBaseGrid <- function (g.size = 0.8, g.alpha = 0.7,
                            g.colors = c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3')) {

    # Center Orange - Zone 1
    A.1 <- data.frame(
      X = c(-5., 5., 5., -5.),
      Y = c(-5., -5., 5., 5.))
    A.2 <- data.frame(
      X = c(5., 15., 15., 5.),
      Y = c(5., 5., 15., 15.))
    A.3 <- data.frame(
      X = c(-15., -5., -5., -15.),
      Y = c(-15., -15., -5., -5.))

    # Middle-outer Orange - Zone 1
    B.1 <- data.frame(
      X = c(15., 40., 40., 15.),
      Y = c(15., 15., 40., 40.))
    B.2 <- data.frame(
      X = c(-40., -15., -15., -40.),
      Y = c(-40., -40., -15., -15.))

    # Outer-most Orange - Zone 1
    C.1 <- data.frame(
      X = c(40., 100., 100., 40.),
      Y = c(40., 40., 100., 100.))
    C.2 <- data.frame(
      X = c(-100., -40., -40., -100.),
      Y = c(-100., -100., -40., -40.))

    # Top Pink - Zone 2
    D.1 <- data.frame(
      X = c(5., 15., 15., 5.),
      Y = c(15., 15., 40., 40.))
    D.2 <- data.frame(
      X = c(15., 40., 40., 15.),
      Y = c(5., 5., 15., 15.))
    D.3 <- data.frame(
      X = c(5., 40., 40., 5.),
      Y = c(40., 40., 100., 100.))
    D.4 <- data.frame(
      X = c(40., 100., 100., 40.),
      Y = c(5., 5., 40., 40.))

    # Bottom Pink - Zone 2
    E.1 <- data.frame(
      X = c(-40., -15., -15., -40.),
      Y = c(-15., -15., -5., -5.))
    E.2 <- data.frame(
      X = c(-15., -5., -5., -15.),
      Y = c(-40., -40., -15., -15.))
    E.3 <- data.frame(
      X = c(-100., -40., -40., -100.),
      Y = c(-40., -40., -5., -5.))
    E.4 <- data.frame(
      X = c(-40., -5., -5., -40.),
      Y = c(-100., -100., -40., -40.))

    # Green - Zone 3
    G.1 <- data.frame(
      X = c(-5., 5., 5., -5.),
      Y = c(5., 5., 100., 100.))
    G.2 <- data.frame(
      X = c(5., 100., 100., 5.),
      Y = c(-5., -5., 5., 5.))
    G.3 <- data.frame(
      X = c(-5., 5., 5., -5.),
      Y = c(-100., -100., -5., -5.))
    G.4 <- data.frame(
      X = c(-100., -5., -5., -100.),
      Y = c(-5., -5., 5., 5.))

    # Blue - Zone 4
    H.1 <- data.frame(
      X = c(-100., -5., -5., -100.),
      Y = c(5., 5., 100., 100.))
    H.2 <- data.frame(
      X = c(5., 100., 100., 5.),
      Y = c(-100., -100., -5., -5.))

    # Generate grid
    p <-
      ggplot() +
      geom_abline(mapping = NULL, data = NULL,
                  slope = 1, intercept = 0, na.rm = FALSE,
                  show.legend = NA, size = .5, color = '#666666', linetype = 'dashed') +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = A.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = A.2) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = A.3) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = B.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = B.2) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = C.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[1], alpha = g.alpha, data = C.2) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = D.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = D.2) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = D.3) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = D.4) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = E.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = E.2) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = E.3) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[2], alpha = g.alpha, data = E.4) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[3], alpha = g.alpha, data = G.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[3], alpha = g.alpha, data = G.2) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[3], alpha = g.alpha, data = G.3) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[3], alpha = g.alpha, data = G.4) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[4], alpha = g.alpha, data = H.1) +
      geom_polygon(aes(x = X, y = Y), size = g.size, color = 'black', linetype = 'dashed',
                   fill = g.colors[4], alpha = g.alpha, data = H.2) +
      xlim(-100, 100) + ylim(-100, 100) +
      coord_cartesian(ylim = c(-100, 100), xlim = c(-100, 100), clip = 'off') #+
    #scale_fill_distiller(palette = 4, direction = 1)
    #scale_fill_brewer(type = 'qual', palette = 'Set2',
    #labels = c('Zone 1', 'Zone 2', 'Zone 3', 'Zone 4'))

    return(list(
      vertices = list(
        A1 = A.1, A2 = A.2, A3 = A.3,
        B1 = B.1, B2 = B.2,
        C1 = C.1, C2 = C.2,
        D1 = D.1, D2 = D.2, D3 = D.3, D4 = D.4,
        E1 = E.1, E2 = E.2, E3 = E.3, E4 = E.4,
        G1 = G.1, G2 = G.2, G3 = G.3, G4 = G.4,
        H1 = H.1, H2 = H.2),
      p = p
    ))
  } # End of sub-function

  g <- makeBaseGrid()

  # Calculate absolute and percent change data frame
  change.df <-
    paired.df %>%
    dplyr::arrange(ENC_KEY, COLLECTED_DT.x) %>%
    dplyr::group_by(ENC_KEY) %>%
    dplyr::mutate(
      DIFF_ENC = ENC_KEY == lag(ENC_KEY),
      TIME_DIFF = as.numeric(
        COLLECTED_DT.x - lag(COLLECTED_DT.x),
        units = 'mins'
      ),
      ABS_CHANGE_VAL.x = NUM_VAL.x - lag(NUM_VAL.x),
      ABS_CHANGE_VAL.y = NUM_VAL.y - lag(NUM_VAL.y),
      PERC_CHANGE_VAL.x = ABS_CHANGE_VAL.x / lag(NUM_VAL.x) * 100.,
      PERC_CHANGE_VAL.y = ABS_CHANGE_VAL.y / lag(NUM_VAL.y) * 100.
    ) %>%
    dplyr::filter(!is.na(DIFF_ENC)) %>%
    dplyr::filter(TIME_DIFF < time.diff.max) %>%
    dplyr::ungroup()

  # How many distinct X,Y percent-change pairs do we have?
  if (to.print) {
    cat(sprintf('Number of rows of `change.df` data frame: %d\n',
                nrow(change.df)))

    cat(sprintf('Number of distinct X,Y pairs of percent changes: %d\n',
                nrow(
                  change.df %>%
                    dplyr::select(PERC_CHANGE_VAL.x, PERC_CHANGE_VAL.y) %>%
                    dplyr::distinct())))
  }

  # Add % Change to Error Grid plot
  Error_Grid <-
    g$p +
    geom_jitter(aes(x = PERC_CHANGE_VAL.x, y = PERC_CHANGE_VAL.y), data = change.df,
                width = 0.3, height = 0.3, size = pt.size, alpha = pt.alpha) +
    xlab('\u0394 Hgb from CBC (%)') +
    ylab('\u0394 Hgb from BG (%)') +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.position = 'bottom', legend.title = element_blank())

  if (to.print)
    print(Error_Grid)

  perc.pts <- as.matrix(
    change.df %>%
      dplyr::select(PERC_CHANGE_VAL.x, PERC_CHANGE_VAL.y) %>%
      dplyr::transmute(
        PERC_CHANGE_VAL.x = PERC_CHANGE_VAL.x +
          runif(nrow(change.df), min = perturb.amt[1], max = perturb.amt[2]),
        PERC_CHANGE_VAL.y = PERC_CHANGE_VAL.y +
          runif(nrow(change.df), min = perturb.amt[1], max = perturb.amt[2])
      ))

  # Declare empty vector for point-in-range counts
  pinp <- vector(length = length(g$vertices))

  for (i in 1 : length(g$vertices)) {

    pinp[i] <-
      sum(
        ptinpoly::pip2d(
          Vertices = as.matrix(g$vertices[[i]]),
          Queries = perc.pts
        ) >= 0
      )
  }

  names(pinp) <- paste0(names(g$vertices), '.n')

  # Calculate the number of points in each "Zone"
  Zone1 <- pinp['A1.n'] + pinp['A2.n'] + pinp['A3.n'] +
    pinp['B1.n'] + pinp['B2.n'] + pinp['C1.n'] + pinp['C2.n']
  Zone2 <- pinp['D1.n'] + pinp['D2.n'] + pinp['D3.n'] + pinp['D4.n'] +
    pinp['E1.n'] + pinp['E2.n'] + pinp['E3.n'] + pinp['E4.n']
  Zone3 <- pinp['G1.n'] + pinp['G2.n'] + pinp['G3.n'] + pinp['G4.n']
  Zone4 <- pinp['H1.n'] + pinp['H2.n']

  if (to.print) {
    cat(sprintf('Sum of all zones: %d\n',
                Zone1 + Zone2 + Zone3 + Zone4))

    cat(sprintf(paste0('Zone 1: %d (%0.2f %%)\n',
                       'Zone 2: %d (%0.2f %%)\n',
                       'Zone 3: %d (%0.2f %%)\n',
                       'Zone 4: %d (%0.2f %%)\n'),
                Zone1, Zone1 / sum(pinp) * 100.,
                Zone2, Zone2 / sum(pinp) * 100.,
                Zone3, Zone3 / sum(pinp) * 100.,
                Zone4, Zone4 / sum(pinp) * 100.))
  }

  # Return
  if (to.return)
    return(list(
      grid = Error_Grid,
      pinp = pinp,
      zones = list(Zone1, Zone2, Zone3, Zone4)
    ))
}


#'
#' @title Run All Temporal
#'
#' @description Runs through all temporal assessments, for sensitivity analysis
#'
#' @param labs.df The original labs data frame
#' @param cohort.df The original cohort data frame
#' @param compare.PN The comparison PROC name (e.g. either `BG` or `ISTAT`)
#' @param time.diff The cutoff time difference (in minutes) for determining
#'     whether labs are "simultaneous"
#' @param multi.per.pt If TRUE, allows all results from patients;
#'     If FALSE, only the first (chronological) result from a patient is included
#' @param time.diff.max The filter for max time between successive paired labs
#' @param run.date A string representation of date for saving (format: %Y-%m-%d)
#' @param save.fn The file name (which will be concatenated with SITE and run.date),
#'     or NA [Default] if we do not wish to save any results to a file
#'
runAllTemporal <- function (labs.df, cohort.df, compare.PN,
                            time.diff, multi.per.pt, time.diff.max,
                            run.date, save.fn = NA) {

  # Create the paired dataset
  paired.df <- createPairedDataset(
    labs.df = labs.df,
    cohort.df = cohort.df,
    PN = c('CBC', compare.PN),
    CN = 'Hgb',
    time.diff = time.diff,
    multi.per.pt = multi.per.pt
  )

  # Describe the repeated pairs
  describe.repeat.pairs <-
    describeRepeatedPairs(
      paired.df = paired.df,
      to.print = T,
      to.return = T
    )

  # First compute Change Error Grid on all time pairs
  compare.all.pairs <-
    calculateChangeComparison(
      paired.df = paired.df,
      time.diff.max = Inf,
      to.print = T,
      to.return = T
    )

  # Then restrict to the time diff specified
  compare.restrict.pairs <-
    calculateChangeComparison(
      paired.df = paired.df,
      time.diff.max = time.diff.max,
      to.print = T,
      to.return = T
    )

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
      time.diff, compare.PN, time.diff.max,
      paired.df,
      describe.repeat.pairs,
      compare.all.pairs, compare.restrict.pairs
    )
  }
}
