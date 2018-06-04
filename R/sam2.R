#' Two Way Tabulation That Includes the Missing values (Columnswise) 
#' @name sam2
#' @param row The factors to be analysed
#' @param  column The groups to be considered 
#' @return  value
#' @export sam2
#' 

sam2 <- function(row, column, decimal=2, percent=c("col","row"), missing = TRUE,sort.group = c(decreasing, 
                                                                                                 "increasing"), cum.percent = !any(is.na(row)),las=0, main = "auto", xlab = "auto", 
                   ylab = "auto", col="auto",  ...) {
  row<- as.factor(row)
  rowr<- forcats:: fct_explicit_na(row,na_level = "(Missing)")
  dose<- data.frame(rowr)
  colnames(dose)<-c("Factors")
  row <- dose$Factors
  tab <- table(row, column, deparse.level=1,dnn=list(deparse(substitute(row)),deparse(substitute(column)))) 
  
  # column percent
  cpercent <-tab
  for(i in 1:ncol(tab)) { cpercent[,i] <-paste("",format(round(tab[,i]/colSums(tab)[i]*100, digits=decimal),trim=TRUE),"", sep="")}
  cpercent <- rbind(cpercent, rep("100", ncol(tab)))
  col.1.1 <- cbind(format(c(tab[,1],sum(tab[,1])), trim=TRUE), cpercent[,1])
  for(i in 2:ncol(tab)){
    col.1.1 <- cbind(col.1.1, c(format(tab[,i], trim=TRUE), format(sum(tab[,i]), trim=TRUE)), cpercent[,i])
  }
  cpercent <- col.1.1
  cnames <- character(0)
  for(i in 1:ncol(tab) ){ cnames <- c(cnames, colnames(tab)[i], "%")}
  colnames(cpercent) <- cnames
  rownames(cpercent)[nrow(cpercent)] <- "Total"
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
  
  suppressWarnings(if(percent=="col"){
    cat("Column percent", "\n")
    names(attr(cpercent,"dimnames")) <- c("Factors", string4)
    print.table(cpercent, right=TRUE, print.gap=2)
    cat( "\n")})
  
  cpercent <- tab
  for(i in 1:ncol(tab)) {cpercent[,i] <- tab[,i]/colSums(tab)[i]*100}
  returns<- cpercent
} 
