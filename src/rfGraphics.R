## Returns TRUE if test png() call succeeds, FALSE otherwise:
CheckWorkingPNG <- function(png, silent = FALSE)
{
  stopifnot(mode(png) == "function")
  f <- tempfile()
  on.exit(unlink(f))
  x <- try({png(file = f); dev.off()}, silent = silent)
  !inherits(x, "try-error")
}

## Own version of png() that uses bitmap().
## When called, recall with bitmap() and mostly the same arguments.
png_via_bitmap <- function(...)
{
  cl <- match.call()
  cl$type <- "png16m"
  cl$units <- "px"
  cl$res <- 72
  cl$taa <- 4
  cl$gaa <- 4

  cl[[1L]] <- as.name("bitmap")
  eval(cl, parent.frame())
}

## Returns a working png() function, or NULL if it can't find one.
GetWorkingPNG <- function()
{
  ## Does default png() work?
  png <- grDevices::png
  if (CheckWorkingPNG(png, silent = TRUE)) return(png)

  ## Default png() doesn't work.  Try to make it work:
  if (isTRUE(getOption("bitmapType") == "Xlib") && capabilities("cairo")) {
    op <- options(bitmapType = "cairo")
    if (CheckWorkingPNG(png)) {
      warning('set bitmapType option to "cairo" to make png() work')
      return(png)
    }
    options(op)
  }

  ## Unsuccessful; check if own version works:
  if (CheckWorkingPNG(png_via_bitmap)) return(png_via_bitmap)

  NULL
}
