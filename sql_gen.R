#' SQL generation
#'
#' \code{sql_generation} generates SQL code from bins a Model and ultimately sums the estimates
#'
#' @param Model specific variables and their estimates
#' @param bins binning rules
#'
#' @return SQL code in a vector format
#'
#' @examples
#'
#' Model_Expanded<-model_summary(m21,bins=bins)
#' SQL<-sql_generation(Model_Expanded,"train")
#' SQL_score<-sqldf(SQL)
#' R_score<-predict(m21,newdata=train_woe)
#' plot(R_score,SQL_score$out_sum,xlab="Score Using R",ylab="Score using SQL",main="Scoring test on Myrtle Beach outbound model"); lines(c(-11,11),c(-11,11),col="red",lwd=3)
#'
#'
#' SQL<-sql_generation(Model_Expanded,"Outbound_Pkg_Wai")
#' sink("SQL_Outbound_Pkg_Wai.txt")
#' cat(SQL)
#' sink()
#'
#' @import stringr
#' @export
#'
