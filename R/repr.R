## not exported because repr is only in Suggests
## exported dynamically in .onLoad()
## @exportS3Method repr::repr_html
repr_html.python.builtin.object <- function(obj, ...) {
  # py_has_method() only matches true Python methods; callable attributes are not covered.
  if (!py_has_method(obj, "_repr_html_"))
    return(NULL)

  html <- tryCatch(
    as_r_value(obj$`_repr_html_`()),
    error = function(e) NULL
  )

  if (!is.character(html) || length(html) != 1L || is.na(html) || !nzchar(html))
    return(NULL)

  html
}
