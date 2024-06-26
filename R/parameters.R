#######################################################################
# Code to check parameter/control objects
# Copyright (C) 2021 Michael Hahsler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


## helper to parse parameter lists with defaults
.nodots <- function(...) {
  l <- list(...)
  if (length(l) > 0L) {
    warning("Unknown arguments: ",
      paste(names(l), "=", l, collapse = ", "),
      call. = FALSE
    )
  }
}

.get_parameters <- function(parameter, defaults) {
  if (!is.null(parameter[["help"]])) {
    cat("Available control parameters (with default values):\n")
    cat(rbind(names(defaults), " = ", gsub("\n", " ", as.character(defaults))),
      sep = c("\t", " ", "\n")
    )

    # stop quietly
    opt <- options(show.error.messages = F)
    on.exit(options(opt))
    stop()
  }

  defaults <- as.list(defaults)
  parameter <- as.list(parameter)

  ## add verbose
  if (is.null(defaults$verbose)) {
    defaults$verbose <- FALSE
  }

  if (length(parameter) != 0) {
    o <- pmatch(names(parameter), names(defaults))

    ## unknown parameter
    if (any(is.na(o))) {
      warning(
        sprintf(
          ngettext(
            length(is.na(o)),
            "Unknown control parameter: %s",
            "Unknown control parameters: %s"
          ),
          paste(names(parameter)[is.na(o)],
            collapse = ", "
          )
        ),
        call. = FALSE,
        immediate. = TRUE
      )

      cat("Available control parameters (with default values):\n")
      # print(defaults)
      cat(rbind(names(defaults), " = ", gsub("\n", " ", as.character(defaults))),
        sep = c("\t", " ", "\n")
      )
    }

    defaults[o[!is.na(o)]] <- parameter[!is.na(o)]
  }

  if (defaults$verbose) {
    cat("Used control parameters:\n")
    # print(defaults)
    cat(rbind(names(defaults), " = ", gsub("\n", " ", as.character(defaults))),
      sep = c("\t", " ", "\n")
    )
  }

  defaults
}
