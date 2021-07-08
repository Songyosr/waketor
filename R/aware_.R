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


#'
#'
#'
#' @export
aware_summary <- \(x,...){
  UseMethod("aware_summary")
}

aware_summary.default <- \(x,...){
  summary(x)
}

aware_summary.integer <-
aware_summary.numeric <- \(x, normality.test = TRUE, ...){

  #Summarize base on normality
  assertive.types::assert_is_a_bool(normality.test)
  x <- na.omit(x)
  n <- length(x)
  # --
  norm_p <- NA
  norm_dist <- NULL


  if (normality.test && n >= 3L && n <= 5000L) {
    norm_p <- shapiro.test(x)$p.value
    norm_dist <- norm_p >= 0.05
  }

  if (norm_dist == TRUE) {
    summ <- c(Mean = mean(x), SD = sd(x))
  } else if (norm_dist == TRUE) {
    summ <- setNames(
      quantile(x, probs = c(0.5,0.25,0.75)),
      c("Median","Q1","Q3")
    )
  } else {
    summ <- c(
    Mean = mean(x),
    SD = sd(x),
    setNames(quantile(x, probs = c(0.5,0.25,0.75)),
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



# var_name = var_name %||% deparse(substitute(x)),
# var_lab = attr(x, "label") %||% "",
# var_length = length(x),
# var_na = sum(is.na(x)),
# var_class = class(x),
# var_group = class_grouping(x)


#names(airquality)

#getNamespace("iris")
#
# ls()
# ?getNamespace
# k <- parent.frame()
# str(k)
# attributes(k)
# environmentName(iris)
#
# ?environmentName
#
# library(data.table)
# data(iris)
# setDT(iris)
# iris[, ls()]
# BP[,.(ls())]
# ?ls()
# environmentName(sys.frame())
#
# environment(j)
# gsub("*.\\$","","a$545")
#
#
# with(iris, environmentName(environment()))
#
# sys.frame()
#
# caller_env()
#
# BP <- epicalc::BP
# k <- attributes(BP$saltadd)
#
# attributes(Sys.Date())
# class
#
#
# a <- 1:26
# names(a) <- letters
#
# attributes(a)
# sloop::s3_dispatch(print(a))
# 'names<-'
#
# myfunc <- function() {ls()}
#
# labelled::var_label(iris) <- list(Petal.Length = "Length of petal", Petal.Width = "Width of Petal")
# des(iris)
# labelled::look_for(iris)
# attributes(iris$Petal.Length)
