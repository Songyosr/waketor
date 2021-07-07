library(vctrs)

# new_vece <- function(x = double()) {
#   assertthat::assert_that(
#     any(
#       class(x) %in% c(
#         "character", "complex", "integer",
#         "logical", "numeric", "Date",
#         "factor", "POSXit"
#       )
#     )
#   )
#   new_vctr(x, class = "vece")
# }

# x <- new_percent(factor(c(0.1, 0.5)))
# x

is.vector(c(seq(0, 1, length.out = 4)))

j[1:30]
vec_restore.ivec <- function(x, to, ..., i = NULL) {
  new_ivec(x, get_info(x))
}



?vector()
# informative vect --------------------------------------------------------
var_info_df <- data.frame(
  var_name = character(),
  var_lab = character(),
  var_length = integer(),
  var_na = integer(),
  var_class = character(),
  var_group = character()
)

# class(vector())
# ivec()
# class(vector()) %in% c(
#   "character", "complex", "integer",
#   "logical", "numeric", "Date",
#   "factor", "POSXit"
# )

new_ivec()
ivec()
new_ivec <- function(x = vector(), var_info = data.frame()) {
  assertthat::assert_that(
      any(
        class(x) %in% c(
          "character", "complex", "integer",
          "logical", "numeric", "Date",
          "factor", "POSXit"
        )
      )
    )

  # assertthat::are_equal(vec_ptype(var_info),vec_ptype(var_info_df))
  new_vctr(x, var_info = var_info, class = c(class(x),"ivec"),
           inherit_base_type = TRUE)
}

new_ivec()
ivec()
get_info <- \(x, var_name = NULL){
  df <- data.frame(
    var_name = var_name %||% deparse(substitute(x)),
    var_lab = attr(x, "label") %||% "",
    var_length = length(x),
    var_na = sum(is.na(x)),
    var_class = class(x),
    var_group = class_grouping(x)
  )
  df
}
# get_info(iris$Sepal.Length)

ivec <- function(x =vector()) {
  assertthat::assert_that(
    any(
      class(x) %in% c(
        "character", "complex", "integer",
        "logical", "numeric", "Date",
        "factor", "POSXit"
      )
    ),
    msg = "'x' should only be a vector"
  )
  var_name <- deparse(substitute(x))
  new_ivec(x, get_info(x, var_name = var_name))
}

ivec()

epicalc::BP$sex |> ivec() |> class()
j <- ivec(iris$Sepal.Length)
attr(j, "var_info")
# test --------------------------------------------------------------------
obj_print_header.ivec <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }
  info <- attr(x, "var_info")
  with(info, {
    cat("\n", var_name, " (", var_class, ":", var_group, ")", " - ",
      var_length, " obs. with ", var_na, " NA value(s)\n",
      if (nchar(var_lab) != 0) paste0("Labels: ", var_lab, "\n"),
      "\n",
      sep = ""
    )
  })
  # cat("# Sum: ", format(attr(x, "sum"), digits = 3), "\n", sep = "")
}

am <- MASS::Animals
vec_cast(1, ivec())


class(am$body)
