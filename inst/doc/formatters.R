## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")
library(formatters)

## -----------------------------------------------------------------------------
list_valid_format_labels()

## -----------------------------------------------------------------------------
format_value(5.1235, format = "xx.xx")

## -----------------------------------------------------------------------------
format_value(c(1.2355, 2.6789), "(xx.xx, xx.xx)")

## -----------------------------------------------------------------------------
## pagdfrow supports a large number of pieces of information regarding
## siblings and what information should be repeated after a pagination.
## we ignore all that here and just give the absolutely crucial info:
## nm (name), lab (label), rnum (absolute row position), pth ("path"),
## extent (how many lines it takes up), rclass ("class of row")
fake_pagdf_row <- function(i, rnms) {
  nm <- rnms[i]
  pagdfrow(
    nm = nm, lab = nm, rnum = i, pth = nm, extent = 1L,
    rclass = "NA"
  )
}

matrix_form.data.frame <- function(df) {
  fmts <- lapply(df, function(x) if (is.null(obj_format(x))) "xx" else obj_format(x))

  bodystrs <- mapply(function(x, fmt) {
    sapply(x, format_value, format = fmt)
  }, x = df, fmt = fmts)

  rnms <- row.names(df)
  if (is.null(rnms)) {
    rnms <- as.character(seq_len(NROW(df)))
  }

  cnms <- names(df)

  strings <- rbind(
    c("", cnms),
    cbind(rnms, bodystrs)
  )

  fnr <- nrow(strings)
  fnc <- ncol(strings)

  ## center alignment for column labels, left alignment for everything else
  aligns <- rbind(
    "center",
    matrix("left", nrow = NROW(df), ncol = fnc)
  )


  ## build up fake pagination df,
  rowdf <- basic_pagdf(row.names(df))
  matrix_print_form(
    strings = strings,
    aligns = aligns,
    spans = matrix(1, nrow = fnr, ncol = fnc),
    formats = matrix("", nrow = fnr, ncol = fnc),
    row_info = rowdf,
    has_topleft = FALSE,
    nlines_header = 1,
    nrow_header = 1
  )
}

cat(toString(matrix_form.data.frame(mtcars)))

