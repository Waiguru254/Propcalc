#' One Way Tabulation That Includes the Missing values (Columnswise) 
#' @name sam1
#' @param row The factors to be analysed
#' @return  value
#' @export sam1
#' 
sam1 <- function (row, decimal = 1, sort.group = c(FALSE, "decreasing", 
                                                  "increasing"), cum.percent = !any(is.na(row)), graph = FALSE, 
                  missing = TRUE, bar.values = c("frequency", "percent", "none"), 
                  horiz = FALSE, cex = 1, cex.names = 1, main = "auto", xlab = "auto", 
                  ylab = "auto", col = "auto", gen.ind.vars = FALSE, ...) 
{
  if (any(is.na(row))) {
    if (is.factor(row)) {
      output0 <- t(t(as.table(summary(row))))
      output1 <- (t(t(table(row))))
    }
    else {
      output0 <- t(t(table(row, exclude = NULL)))
      output1 <- (t(t(table(row))))
    }
    percent0 <- output0[, 1]/sum(output0) * 100
    percent1 <- output1[, 1]/sum(output1[, 1], na.rm = TRUE) * 
      100
    if (cum.percent) {
      output <- cbind(output0, round(percent0, decimal), 
                      round(cumsum(percent0), decimal), c(round(percent1, 
                                                                decimal), as.integer(0)), round(cumsum(c(percent1, 
                                                                                                         as.integer(0))), decimal))
    }
    else {
      output <- cbind(output0, round(percent0, decimal), 
                      c(round(percent1, decimal), as.integer(0)))
    }
    suppressWarnings(if (sort.group == "decreasing") {
      output <- output[order(output[, 1], decreasing = TRUE), 
                       ]
    })
    suppressWarnings(if (sort.group == "increasing") {
      output <- output[order(output[, 1], decreasing = FALSE), 
                       ]
    })
    if (cum.percent) {
      output <- rbind(output, c(sum(as.integer(output[, 
                                                      1])), 100, 100, 100, 100))
      colnames(output) <- c(.frequency, "Percentage (%)", "cum.%(NA+)", 
                            "  %(NA-)", "cum.%(NA-)")
    }
    else {
      output <- rbind(output, c(sum(as.integer(output[, 
                                                      1])), 100, 100))
      colnames(output) <- c(.frequency, " Percentage (%)", "  %(NA-)")
    }
    rownames(output)[nrow(output)] <- "  Total"
  }
  else {
    output <- (t(t(table(row))))
    suppressWarnings(if (sort.group == "decreasing") {
      output <- output[order(table(row), names(table(row)), 
                             decreasing = TRUE), ]
    })
    suppressWarnings(if (sort.group == "increasing") {
      output <- output[order(table(row), names(table(row)), 
                             decreasing = FALSE), ]
    })
    percent <- output/sum(output) * 100
    if (cum.percent) {
      output <- cbind(output, round(percent, decimal), 
                      round(cumsum(percent), decimal))
      output <- rbind(output, c(sum(output[, 1]), 100, 
                                100))
      colnames(output) <- c(.frequency1, .percent, .cum.percent)
    }
    else {
      output <- cbind(output, round(percent, decimal))
      output <- rbind(output, c(sum(output[, 1]), 100))
      colnames(output) <- c(.frequency1, .percent)
    }
    rownames(output)[length(rownames(output))] <- "  Total"
  }
  if (substring(search()[2], first = 1, last = 8) != "package:") {
    options(warn = -1)
    first.line <- paste(deparse(substitute(row)), ":", attr(get(search()[2]), 
                                                           "var.labels")[attr(get(search()[2]), "names") == 
                                                                           deparse(substitute(row))], "\n")
    options(warn = TRUE)
  }
  else {
    first.line <- paste(deparse(substitute(row)), ":", "\n")
  }
  if (gen.ind.vars) {
    if(!is.factor(row)) {
      warning(paste(as.character(substitute(row)),"is not factor. Indicator variables have not been generated!"))
    }else{  
      mod.mat <- model.matrix (~ row -1)
      for (i in 1:ncol(mod.mat)) {
        assign (paste(deparse(substitute(row)),substr(colnames(mod.mat)[i],3,nchar(colnames(mod.mat)[i])), sep=""), mod.mat[,i], pos=1)
      }}
  }
  returns <- list(first.line = first.line, output.table = output)
  class(returns) <- c("tab1", "list")
  returns
}

