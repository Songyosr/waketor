# OR Null
"%||%" <- function(lhs, rhs) {
  if (!is.null(lhs) & !missing(lhs)) {
    lhs
  } else {
    rhs
  }
}

# Example
# 11 %||% 111
# NULL %||% 44

# Class group
class_grouping <- function(x) {
  x <- last(class(x))
  dplyr::case_when(
    x %in% c("integer", "numeric") ~ "Continuous",
    x %in% c("character", "factor") ~ "Categorical",
    x %in% c("Date", "POSIXt") ~ "Date & Time",
    TRUE ~ x
  )
}
#load_all()
# Example
# class_grouping(Sys.time())
# j <- sapply(BP, class_grouping)
# names(j[j == "Continuous"])


# Print functions for personal class
# print.sum_uni_conti <- \(x, ...){
#
#   # Extract Meta data: Variable names, class, and distribution
#   var_name <- attr(x, "var_name")
#   var_class <- attr(x, "var_class")
#   var_dist_p <- attr(x, "var_dist_p")
#
#   # Printing management
#   ## Header
#   cat("\n", var_name, " (", var_class, ")", " - ", x["obs."], " obs. with ", x["NAs"], " NA value(s)\n", sep = "")
#
#   ## Normal dist ?
#   if (!is.na(var_dist_p)) {
#     if (var_dist_p >= 0.05) {
#       cat("Shapiro-Wilk normality test: p-value = ", summ_pval(var_dist_p), "\n")
#     } else {
#       message("Shapiro-Wilk normality test: p-value = ", summ_pval(var_dist_p), " - Non Normal Distribution")
#     }
#   }
#   # Body - tibblle
#   x <- x[-c(1:2)] |> t() |> as_tibble(.row = 1)
#   print(x)
# }
#
# uni_summ(runif(500), normality.test = T) %>% class()
# tab1x(BP, sbp)

# Improve dnorm
dnorm_date <- \(x, mean = 0, sd = 1){
  x <- as.numeric(x)
  mean <- as.numeric(mean)
  sd <- as.numeric(sd)
  dnorm(x, mean = mean, sd = sd)
}


# Improve p.val presentation
summ_pval <- \(p.value, number = TRUE){
  if (number) {
    p <- round(p.value, 3)
    p[p < 0.001] <- "<0.001"
    return(paste(p, stars.pval(p.value)))
  }
  else {
    return(stars.pval(p.value))
  }
}

# p.val <- c(0.0004, 0.0015, 0.013, 0.044, 0.067, 0.24)
# stars.pval(p.val)
# stars.pval
# summ_pval(p.val)
# rm(p.val)
