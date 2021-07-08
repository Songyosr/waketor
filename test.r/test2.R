var_info_df <- data.frame(
  var_name = character(),
  var_lab = character(),
  var_length = integer(),
  var_na = integer(),
  var_class = character(),
  var_group = character()
)

#' @export
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


new_fac <- function (x = integer(), levels = character(), ...,
                     var_info = data.frame(),
                     class = character())
{
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  structure(x, levels = levels, ...,
            var_info = var_info,
            class = c(class, "a_factor","awake"))
}

new_fac(1:26, LETTERS) |> #str()
class()

print.a_factor <- function(x, ...){
  print.factor(x,...)
}
obj_print_footer.a_factor <- function(x, ...) {
  cat("# Sum: asdasdasdasd")
}


vctrs::obj_print(new_fac(1:26, LETTERS)) |> sloop::s3_dispatch()
