#' Two Way Analysis That includes NA's 
#' @name sampct
#' @param row The factors to be analysed
#' @param  column the groups to be considered 
#' @return value
#'              
#' @export sampct
#' 

sampct <- function(row, column, decimal=1, percent=c("both","col","row"), graph=FALSE, las=0, main = "auto", xlab = "auto", 
                   ylab = "auto", col="auto",  ...) {
  row<- as.factor(row)
  rowr<- forcats:: fct_explicit_na(row,na_level = "(Missing)")
  dose<- data.frame(rowr)
  colnames(dose)<-c("Factors")
  row <- dose$Factors
  tab <- table(row, column, deparse.level=1, dnn=list(deparse(substitute(row)),deparse(substitute(column))))
  # column percent
  cpercent <-tab
  for(i in 1:ncol(tab)) { cpercent[,i] <-paste("(",format(round(tab[,i]/colSums(tab)[i]*100, digits=decimal),trim=TRUE),")", sep="")}
  cpercent <- rbind(cpercent, rep("(100)", ncol(tab)))
  col.1.1 <- cbind(format(c(tab[,1],sum(tab[,1])), trim=TRUE), cpercent[,1])
  for(i in 2:ncol(tab)){
    col.1.1 <- cbind(col.1.1, c(format(tab[,i], trim=TRUE), format(sum(tab[,i]), trim=TRUE)), cpercent[,i])
  }
  cpercent <- col.1.1
  cnames <- character(0)
  for(i in 1:ncol(tab) ){ cnames <- c(cnames, colnames(tab)[i], "%")}
  colnames(cpercent) <- cnames
  rownames(cpercent)[nrow(cpercent)] <- "Total"
  
  # rowpercent
  rpercent <-tab
  for(i in 1:nrow(tab)) { rpercent[i,] <-paste("(",round(tab[i,]/rowSums(tab)[i]*100, digits=1),")", sep="")}
  rpercent <- cbind(rpercent,c(rep("(100)",nrow(tab))))
  row.1.1 <- rbind(format(c(tab[1,],sum(tab[1,])), trim=TRUE), rpercent[1,])
  for(i in 2:nrow(tab)){
    row.1.1 <- rbind(row.1.1, c(format(tab[i,], trim=TRUE), format(sum(tab[i,]), trim=TRUE)), rpercent[i,])
  }
  rpercent <- row.1.1
  rnames <- character(0)
  for(i in 1:nrow(tab) ){ rnames <- c(rnames, rownames(tab)[i], "")}
  rownames(rpercent) <- rnames
  colnames(rpercent)[ncol(rpercent)] <- "Total"
  
  var1 <- deparse(substitute(row))
  if(length(var1)>1){
    string2 <- var1[length(var1)]	
  }else
    if(substring(search()[2],first=1,last=8)!="package:"){
      string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==deparse(substitute(row))]
      if(length(string2)==0){
        string2 <- deparse(substitute(row))
      }
      if(string2==""){
        string2 <- deparse(substitute(row))
      }
    }else{
      string2 <- deparse(substitute(row))
    }
  if(substring(search()[2],first=1,last=8)!="package:"){
    string4 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==deparse(substitute(column))]
    if(length(string4)==0){
      string4 <- deparse(substitute(column))
    }else{
      if(string4==""){
        string4 <- deparse(substitute(column))
      }
    }
  }else{
    string4 <- deparse(substitute(column))
  }
  names(attr(tab,"dimnames")) <-c("Factors", string4)
  cat( "\n")
  suppressWarnings(if(percent=="both"){
    cat("Original table", "\n")
    tabtotal <- addmargins(tab)
    colnames(tabtotal)[ncol(tabtotal)] <- "Total"
    rownames(tabtotal)[nrow(tabtotal)] <- "Total"
    print(tabtotal, print.gap=2)
    cat( "\n")})
  
  suppressWarnings(if(percent=="both" | percent=="row"){
    cat("Row percent", "\n")
    names(attr(rpercent,"dimnames")) <- c("Factors", string4)
    print.table(rpercent, right=TRUE, print.gap=2)
    cat( "\n")})
  
  suppressWarnings(if(percent=="both" | percent=="col"){
    cat("Column percent", "\n")
    names(attr(cpercent,"dimnames")) <- c("Factors", string4)
    print.table(cpercent, right=TRUE, print.gap=2)
    cat( "\n")})
  
  if(graph==TRUE){
    rownames(tab)[is.na(rownames(tab))] <- "missing"
    colnames(tab)[is.na(colnames(tab))] <- "missing"
    las.value <- las
    if(any(col=="auto")) {colours <- c("white",2:length(column))}else{colours=col}
    if(nchar(paste(titleString()$distribution.of,string4,titleString()$by,string2))>45){
      mosaicplot(as.table(tab),xlab=ifelse(xlab=="auto",string2,xlab), ylab=ifelse(ylab=="auto",string4, ylab), 
                 main= ifelse(main=="auto",paste(titleString()$distribution.of,string4,"\n",titleString()$by,string2), main),
                 col=colours, las=las.value,  ...)
    }else{
      mosaicplot(as.table(tab),xlab=ifelse(xlab=="auto",string2,xlab), ylab=ifelse(ylab=="auto",string4,ylab), 
                 main=ifelse(main=="auto",paste(titleString()$distribution.of,string4,titleString()$by,string2),main),
                 col=colours, las=las.value, ...)}}
  
  cpercent <- tab
  for(i in 1:ncol(tab)) {cpercent[,i] <- tab[,i]/colSums(tab)[i]*100}
  
  rpercent <- tab
  for(i in 1:nrow(tab)) {rpercent[i,] <- tab[i,]/rowSums(tab)[i]*100}
  returns <- list(table.row.percent=rpercent, table.column.percent=cpercent)
} 