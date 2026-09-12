library(data.table)
cat("data.table:", as.character(packageVersion("data.table")), "\n\n")

# A data.table with truelength 0, i.e. no over-allocated column slots.
# Building the list directly and setting the class produces this, and it is
# also the state that any tidytable verb leaves its result in.
make <- function(ncol_total) {
  cols <- list(tag = c("A", "B"))
  for (i in seq_len(ncol_total - 2L)) cols[[paste0("C", i)]] <- c(i + 0.1, i + 0.2)
  cols$.id <- c(1L, 2L)
  structure(cols, class = c("data.table", "data.frame"), row.names = c(NA, -2L))
}

demo <- function(n) {
  x <- make(n)
  tl <- truelength(x)
  before <- x$.id
  setcolorder(x, ".id")                      # move .id from last to first
  cat(sprintf("ncol=%3d truelength=%d | expected .id = %s | got .id = %-11s %s\n",
              ncol(x), tl, paste(before, collapse = ","),
              paste(x$.id, collapse = ","),
              if (identical(x$.id, before)) "OK" else "<<< WRONG"))
}
for (n in c(10, 32, 63, 64, 72)) demo(n)

cat("\n--- what the corrupted table looks like at ncol=64 ---\n")
x <- make(64)
setcolorder(x, ".id")
print(x[, 1:4])
cat("\n.id should be integer 1,2 and hold no letters; tag should hold A,B\n")

cat("\n--- workaround: copy() restores the over-allocation ---\n")
y <- copy(make(64))
setcolorder(y, ".id")
print(y[, 1:4])
