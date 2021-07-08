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
purrr <- c("p", "u", "r", "r", "r")
vec_group_id(purrr)
vec_group_rle(purrr)

vec_count(k)
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
new_ivec <- function(x = vector(), levels = NULL,var_info = data.frame()) {
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
  new_vctr(x, levels = levels,var_info = var_info, class = c("ivec",setdiff(class(x),"ivec")))
}


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
  new_ivec(x, levels = attr(x,"levels"),get_info(x, var_name = var_name))
}

ivec()


t.ivec <- \(x,...) t.default(x,...)

t(j) |>sloop::s3_dispatch()
t.default
t(j)
k <- data.frame(j) #|> attributes()
k |> class()

obj_print.ivec <- obj_print

print.ivec <- obj_print.ivec


k
t.d

obj_print_data.ivec <- \(x,...){
  class(x) <- attr(x, "var_info")$var_class
  attr(x, "var_info") <- NULL
  print(x,...)
}


j <- epicalc::BP$sex |> ivec()

levels.ivec <- function(x){
  attr(x, "levels")
}

sloop::s3_dispatch(print(j))
vctrs::obj_print(j)

class(j)
print.default(j)
isS3method("print")
isS3method(print)
isS3stdGeneric("class")
class

?isS3method
j <- ivec(iris$Sepal.Length)
j <- c(1.0,0.5)
vec_ptype_abbr.ivec <- function(x, ...) {
  paste0("a.",abbreviate(attr(x, "var_info")$var_class,3))
}
vec_ptype_full.ivec <- function(x, ...) {
  paste0("a.",attr(x, "var_info")$var_class)

}
str(j)
str(Sys.Date())
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

.S3method(class = "vctrs_vctr")
methods(class = "vctrs_vctr")
vctrs::levels.vctrs_vctr

methods("levels", class = "vctrs_vctr" ) |>print()


k <- new_vctr(1:10, levels = letters[1:10], var_info = "A", class = c("ifactor"))# |> class()
sloop::s3_dispatch(t(k))
tibble::as_tibble(k1 =k, k2 = k)

levels(k)

levels.ifactor <- function(x){
  attr(x, "levels")
}



t(1:10)
t.ifactor <- \(x){

}
p

?t()
levels(k)

t(k)
tibble::as_tibble(k)

tryCatch(k)
print(k)
print.factor
levels.factor(k)
levels.default

vec.name <- list(
  LETTERS,
  1L,
  0.5,
  1+1i,
  TRUE,
  Sys.Date(),
  Sys.time(),
  factor("A"),
  list()
)
sapply(vec.name, vec_ptype_abbr)
sapply(vec.name, vec_ptype_full)

sapply(vec.name, \(x) class(x) |> abbreviate(minlength = 3, ))
abbreviate

?vec_ptype_full

