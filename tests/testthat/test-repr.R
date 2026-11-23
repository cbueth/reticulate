context("repr")

test_that("repr_html() is registered for Python objects", {
  skip_if_not_installed("repr")

  method <- utils::getS3method(
    "repr_html",
    "python.builtin.object",
    envir = asNamespace("repr")
  )
  expect_true(is.function(method))
})

test_that("repr_html() uses _repr_html_ for Python objects", {
  skip_if_no_python()
  skip_if_not_installed("repr")

  HTMLRepr <- py_run_string("
class HTMLRepr:
    def _repr_html_(self):
        return '<b>hello</b>'
", convert = FALSE)$HTMLRepr

  obj <- HTMLRepr()
  expect_identical(repr::repr_html(obj), "<b>hello</b>")
})

test_that("repr_html() returns NULL when there is no HTML representation", {
  skip_if_no_python()
  skip_if_not_installed("repr")

  NoneRepr <- py_run_string("
class NoneRepr:
    def _repr_html_(self):
        return None
", convert = FALSE)$NoneRepr

  EmptyRepr <- py_run_string("
class EmptyRepr:
    def _repr_html_(self):
        return ''
", convert = FALSE)$EmptyRepr

  ErrorRepr <- py_run_string("
class ErrorRepr:
    def _repr_html_(self):
        raise RuntimeError('boom')
", convert = FALSE)$ErrorRepr

  plain <- py_eval("object()", convert = FALSE)

  expect_null(repr::repr_html(plain))
  expect_null(repr::repr_html(NoneRepr()))
  expect_null(repr::repr_html(EmptyRepr()))
  expect_null(repr::repr_html(ErrorRepr()))
})
