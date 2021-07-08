#' @export
aware_class <- \(x,...){
  list(
    original_class = class(x),
    original_class_abbv = vctrs::vec_ptype_abbr(x),
    original_class_group = class_grouping(x)
  )
}

#' @export
aware_size <- \(x,...){
  list(
    length = length(x),
    na_length = sum(is.na(x))
  )
}

#' @export
aware_summary <- \(x,...){
  assertive.properties::assert_is_non_empty(x)
  UseMethod("aware_summary")
}

#' @export
aware_summary.default <- \(x,...){
  summary(x)
}

#' @export
aware_summary.numeric <- \(x, normality.test = TRUE, ...){
  #Summarize base on normality
  assertive.types::assert_is_a_bool(normality.test)
  x <- na.omit(x)
  n <- length(x)
  # --
  norm_p <- NA
  norm_dist <- NULL

  if (normality.test && n >= 3L && n <= 5000L) {
    norm_p <- stats::shapiro.test(x)$p.value
    norm_dist <- norm_p >= 0.05
  }

  if (norm_dist == TRUE) {
    summ <- c(Mean = mean(x), SD = sd(x))
  } else if (norm_dist == TRUE) {
    summ <- stats::setNames(
      stats::quantile(x, probs = c(0.5,0.25,0.75)),
      c("Median","Q1","Q3")
    )
  } else {
    summ <- c(
    Mean = mean(x),
    SD = sd(x),
    setNames(stats::quantile(x, probs = c(0.5,0.25,0.75)),
             c("Median","Q1","Q3")
            )
    )
  }
  list(
    norm_p = norm_p,
    norm_dist = norm_dist,
    summ = summ
  )
}

#' @export
aware_summary.character <- \(x, ...){
  list(summ = count_tab(x))
}

#' @export
aware_summary.factor <- aware_summary.character

#' @export
aware_summary.logical <- aware_summary.character
