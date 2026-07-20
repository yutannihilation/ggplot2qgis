test_that("nested elements are indented like the Rust writer", {
  w <- xml_writer(0L)
  xw_start(w, "a")
  xw_attr(w, "x", "1")
  xw_elem(w, "b", "hello")
  xw_empty(w, "c", c(y = "2"))
  xw_end(w)
  expect_equal(
    xw_finish(w),
    "\n<a x=\"1\">\n  <b>hello</b>\n  <c y=\"2\"/>\n</a>"
  )
})

test_that("attributes and text escape asymmetrically", {
  w <- xml_writer(0L)
  xw_start(w, "a")
  xw_attr(w, "k", "v&<>\"")
  xw_text(w, "t&<>")
  xw_end(w)
  expect_equal(
    xw_finish(w),
    "\n<a k=\"v&amp;&lt;>&quot;\">t&amp;&lt;&gt;</a>"
  )
})

test_that("an empty text still closes the element inline", {
  # <projectionacronym></projectionacronym>, not <projectionacronym/>
  w <- xml_writer(0L)
  xw_elem(w, "projectionacronym", "")
  expect_equal(
    xw_finish(w),
    "\n<projectionacronym></projectionacronym>"
  )
})

test_that("base_depth shifts the whole fragment", {
  w <- xml_writer(2L)
  xw_start(w, "a")
  xw_elem(w, "b", "x")
  xw_end(w)
  expect_equal(xw_finish(w), "\n    <a>\n      <b>x</b>\n    </a>")
})

test_that("unbalanced writers are an error", {
  w <- xml_writer(0L)
  xw_start(w, "a")
  expect_error(xw_finish(w), "unclosed XML elements")
  xw_end(w)
  expect_error(xw_end(w), "without xw_start")
})
