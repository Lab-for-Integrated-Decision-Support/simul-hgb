# Extracted functions from `01_Cohort_Description.Rmd`

#'
#' @title Lab Value Descriptions
#'
#' @description Displays graph and summary for lab values by PROC and COMP
#'
#' @param df The lab data frame
#' @param PROC_NAME The Procedure name to display
#' @param COMP_NAME The Component name to display
#'
#' @export
#'
labValueDescriptions <- function (df, PN, CN) {

  # Limit by the PN and CN
  filt.df <-
    df %>%
    dplyr::filter(PROC_NAME == PN & COMP_NAME == CN) %>%
    dplyr::filter(!is.na(NUM_VAL) & NUM_VAL != 9999999.)

  # Display the numerical summary
  print(summary(filt.df$NUM_VAL))

  # Create a density plot and print (display)
  p <-
    filt.df %>%
    ggplot(aes(x = NUM_VAL)) +
    geom_density(fill = 'blue', alpha = 0.5) +
    ggtitle(paste0('Proc: ',PN,', Comp: ', CN)) +
    xlab('Numerical Value') +
    ylab('Density') +
    theme_bw()


  print(p)
}


#'
#' @title Time Threshold Graph
#'
#' @description Joins CN across PNs and plot collected time difference by cutoff
#'
#' @param df The lab data frame
#' @param PN A 2-element list of characters naming the PROC_NAME of interest
#' @param CN A char naming the COMP_NAME of interest
#' @param to.save If TRUE, saves out, otherwise does not save output list
#' @param to.return If TRUE, returns the output list, otherwise returns NA
#'
#' @returns A list of two elements:
#'     cutoffs := The cutoffs data frame containing counts for each minute cutoff
#'     fig := The ggplot figure
#'
#' @export
#'
timeThresholdGraph <- function (df, PN, CN,
                                to.save = T, run.date = NA, to.return = F) {

  if (length(PN) != 2)
    stop('Must pass PN as list of length 2')

  if (any(is.na(run.date)) & to.save)
    stop('If `to.save` is TRUE, must specify a run date')

  # Filter to remove the non-numeric rows
  filter.df <-
    df %>%
    dplyr::filter(!is.na(NUM_VAL) & NUM_VAL != 9999999.) %>%
    dplyr::filter(COMP_NAME == CN)

  cat(sprintf('Number of component numeric rows in input data frame: %d\n',
              nrow(filter.df)))

  # Filter by PN and join to create full data frame
  joined.df <-
    dplyr::inner_join(
      x = filter.df %>%
        dplyr::filter(PROC_NAME == PN[1]) %>%
        dplyr::select(ENC_KEY, COLLECTED_DT, NUM_VAL),
      y = filter.df %>%
        dplyr::filter(PROC_NAME == PN[2]) %>%
        dplyr::select(ENC_KEY, COLLECTED_DT, NUM_VAL),
      by = c('ENC_KEY'),
      suffix = c('.x', '.y')
    )

  # Join using base R, by column number
  #   [[2]] is PN[1] COLLECTED_DT
  #   [[4]] is PN[2] COLLECTED_DT
  joined.df$COLL_TIME_DIFF_MIN <-
    as.numeric(joined.df[[2]] - joined.df[[4]], units = 'mins')

  cat(sprintf('Number of rows in joined data frame: %d\n',
              nrow(joined.df)))

  # Generate cutoffs data frame
  cutoffs.df <-
    data.frame(
      cutoffs = seq(from = 0, to = 90, by = 1)
    )

  # Count the absolute value of time differences less than each cutoff value
  cutoffs.df$in.range.cnt <-
    sapply(
      cutoffs.df$cutoffs,
      function (x) sum(abs(joined.df$COLL_TIME_DIFF_MIN) < x)
    )

  # Generate the figure and display
  p <-
    cutoffs.df %>%
    ggplot(aes(x = cutoffs, y = in.range.cnt)) +
    geom_line() +
    #geom_vline(aes(xintercept = 15), color = 'red') +
    xlab('Cutoff, Minutes Between Collected Time') +
    ylab('Count of included labs') +
    scale_y_continuous(limits = c(0, max(cutoffs.df$in.range.cnt))) +
    theme_bw()

  print(p)

  # Create the output list which will either be saved or returned
  output.list <-
    list(
      cutoffs = cutoffs.df,
      fig = p
    )

  # Save
  if (to.save) {
    save(
      file = file.path(
        Sys.getenv('PICU_LAB_DATA_PATH'),
        paste0(
          Sys.getenv('PICU_LAB_SITE_NAME'),
          '_thresholds_',
          PN[1],'-',PN[2],'_',
          run.date,'.rData'
        )
      ),
      output.list
    )
  }

  # Return
  if (to.return)
    return(output.list)
  else
    return()
}
