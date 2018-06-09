sam1 <- function (x0, decimal = 1, sort.group = c(FALSE, "decreasing", 
                                                  "increasing"), cum.percent = !any(is.na(x0)), graph = FALSE, 
                  missing = TRUE, bar.values = c("frequency", "percent", "none"), 
                  horiz = FALSE, cex = 1, cex.names = 1, main = "auto", xlab = "auto", 
                  ylab = "auto", col = "auto", gen.ind.vars = FALSE, ...) 
  x0<- as.factor(x0)
  x0<- forcats:: fct_explicit_na(x0,na_level = "Missing")
{
  if (graph) {
    var1 <- deparse(substitute(x0))
    if (length(var1) > 1) {
      string2 <- var1[length(var1)]
    }
    else if (substring(search()[2], first = 1, last = 8) != 
             "package:") {
      string2 <- attr(get(search()[2]), "var.labels")[attr(get(search()[2]), 
                                                           "names") == deparse(substitute(x0))]
      if (length(string2) == 0) {
        string2 <- deparse(substitute(x0))
      }
      if (string2 == "") {
        string2 <- deparse(substitute(x0))
      }
    }
    else {
      string2 <- deparse(substitute(x0))
    }
    string3 <- paste(titleString()$distribution.of, string2)
    table.to.plot <- table(x0)
    if (missing == TRUE) {
      table.to.plot <- table(x0, exclude = NULL)
      if (is.factor(x0)) {
        table.to.plot <- as.table(summary(x0))
      }
      if (is.na(names(table.to.plot)[length(names(table.to.plot))]) | 
          names(table.to.plot)[length(names(table.to.plot))] == 
          "NA's") 
        names(table.to.plot)[length(names(table.to.plot))] <- "Missing"
    }
    scale.label <- as.character(titleString()$frequency)
    suppressWarnings(if (bar.values == "percent") {
      table.to.plot <- round(table.to.plot/sum(table.to.plot) * 
                               100, decimal)
      scale.label <- "%"
    })
    suppressWarnings(if (sort.group == "decreasing") {
      table.to.plot <- table.to.plot[order(table.to.plot, 
                                           names(table.to.plot), decreasing = TRUE)]
      if (max(nchar(names(table.to.plot))) > 8 & length(table.to.plot) > 
          6) {
        table.to.plot <- table.to.plot[order(table.to.plot, 
                                             names(table.to.plot), decreasing = FALSE)]
      }
    })
    suppressWarnings(if (sort.group == "increasing") {
      table.to.plot <- table.to.plot[order(table.to.plot, 
                                           names(table.to.plot), decreasing = FALSE)]
      if (max(nchar(names(table.to.plot))) > 8 & length(table.to.plot) > 
          6) {
        table.to.plot <- table.to.plot[order(table.to.plot, 
                                             names(table.to.plot), decreasing = TRUE)]
      }
    })
    if(any(col == "auto")){
      if (length(names(table.to.plot)) < 3){
        colours <- "grey"
      }else{
        colours <- c("white",2:length(names(table.to.plot)))
      }
    }else{
      colours <- col
    }
    if ((max(nchar(names(table.to.plot))) > 8 & length(table.to.plot) > 
         6) | horiz == TRUE) {
      par(mai = c(0.95625, 0.1, 0.76875, 0.39375) + 0.1 + 
            c(0, par()$cin[1] * max(nchar(names(table.to.plot))) * 
                0.75 * cex.names, 0, 0))
      y.coordinates <- barplot(table.to.plot, main = ifelse(main == 
                                                              "auto", string3, main), horiz = TRUE, las = 1, 
                               xlim = c(0, max(table.to.plot) * 1.2), xlab = ifelse(xlab == 
                                                                                      "auto", scale.label, xlab), cex.names = cex.names, col=colours,
                               ...)
      suppressWarnings(if (bar.values == "frequency" | 
                           bar.values == "percent" | length(bar.values) == 
                           3) {
        text(table.to.plot, y.coordinates, as.character(table.to.plot), 
             pos = 4, offset = 0.3, cex = cex)
      })
      par(mai = c(0.95625, 0.76875, 0.76875, 0.39375))
    }
    else {
      x.coordinates <- barplot(table.to.plot, main = ifelse(main == 
                                                              "auto", string3, main), ylab = ifelse(ylab == 
                                                                                                      "auto", scale.label, ylab), cex.names = cex.names, 
                               ylim = c(0, max(table.to.plot) * 1.1), col=colours,
                               ...)
      suppressWarnings(if (bar.values == "frequency" | 
                           bar.values == "percent" | length(bar.values) == 
                           3) {
        text(x.coordinates, table.to.plot, as.character(table.to.plot), 
             pos = 3, cex = cex)
      })
    }
  }
  if (any(is.na(x0))) {
    if (is.factor(x0)) {
      output0 <- t(t(as.table(summary(x0))))
      output1 <- (t(t(table(x0))))
    }
    else {
      output0 <- t(t(table(x0, exclude = NULL)))
      output1 <- (t(t(table(x0))))
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
    output <- (t(t(table(x0))))
    suppressWarnings(if (sort.group == "decreasing") {
      output <- output[order(table(x0), names(table(x0)), 
                             decreasing = TRUE), ]
    })
    suppressWarnings(if (sort.group == "increasing") {
      output <- output[order(table(x0), names(table(x0)), 
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
    first.line <- paste(deparse(substitute(x0)), ":", attr(get(search()[2]), 
                                                           "var.labels")[attr(get(search()[2]), "names") == 
                                                                           deparse(substitute(x0))], "\n")
    options(warn = TRUE)
  }
  else {
    first.line <- paste(deparse(substitute(x0)), ":", "\n")
  }
  if (gen.ind.vars) {
    if(!is.factor(x0)) {
      warning(paste(as.character(substitute(x0)),"is not factor. Indicator variables have not been generated!"))
    }else{  
      mod.mat <- model.matrix (~ x0 -1)
      for (i in 1:ncol(mod.mat)) {
        assign (paste(deparse(substitute(x0)),substr(colnames(mod.mat)[i],3,nchar(colnames(mod.mat)[i])), sep=""), mod.mat[,i], pos=1)
      }}
  }
  returns <- list(first.line = first.line, output.table = output)
  class(returns) <- c("tab1", "list")
  returns
}

