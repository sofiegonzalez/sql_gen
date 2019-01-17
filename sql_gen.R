#' SQL generation
#'
#' \code{sql_generation} generates SQL code from a model summary and ultimately sums the estimates
#' Creates hash of variables and their values as well, can alter code to return this instead
#'
#' @param Model model summary, specific variables and their estimates- Data Table
#' @param table_name name of the table being parsed- a String
#'
#' @return SQL code in a string format
#'
#' @examples
#'
#' require(devtools)
#' require(dplyr)
#' require(hash)
#' require(stringr)
#' require(sqldf)
#'
#' Model_Expanded<-model_summary(m21,bins=bins)
#' load("data.RData")
#' SQL<-sql_generation(Model_Expanded,"data")
#' SQL_score<-sqldf(SQL)
#' R_score<-predict(m21,newdata=data_woe)
#' plot(R_score,SQL_score$out_sum,xlab="Score Using R",ylab="Score using SQL",main="Scoring test on Myrtle Beach outbound model"); lines(c(-11,11),c(-11,11),col="red",lwd=3)
#'
#'
#' SQL<-sql_generation(Model_Expanded,"Outbound_Pkg_Wai")
#' data_table <- sqldf(SQL)
#'
#' @export
#'
sql_generation = function(Model, table_name){

  #variable names from Model
  holder <- unique(Model$variable)
  #remove intercept
  holder <- holder[-1]

  #list to hold the list with values- either the binning rules or estimates
  estimates <- list()
  binning <- list()

  #counters
  count <- 1
  vector_index <- 1

  #for loop to fill list estimates with the specific estimates and binning with specific binning rules from Model
  for(i in holder){
    #x is the index(es) of the specific variable i where it appears in Expan1ded_Model
    x <- which(Model$variable == i)
    a <- vector()
    b <- vector()

    #for loop to go through the index(es) and place the specific estimates at those indexes into vector a and binning rules into b
    for(i in x){
      a[vector_index] <- Model$Estimate[i]
      b[vector_index] <- Model$bin[i]
      vector_index <- vector_index + 1
    }
    #place vector a into estimates for that specific variable and b into binning
    estimates[[count]] <- a
    binning[[count]] <- b
    count <- count + 1
    vector_index <- 1
  }

  #create hash from holder-variable names, and binning-binning rules
  hash_bins <- hash(holder, binning)

  #create hash from holder-variable names, and estimates-estimates from Model
  hash_estimates <- hash(holder, estimates)

  #intercept to be added to the sum at the end of the case statements
  intercept <- Model$Estimate[1]

  #################################################################

  #SQL vector that holds generated code
  sql <- paste("", sep=" ")
  sql_sel <- paste("SELECT", sep=" ")

  # loops through variables in hash_bins ###############################
  for(i in keys(hash_bins)){

    #vec is the binning rules for a specific variable i
    vec <- values(hash_bins[i])
    len <- length(vec)

    name_holder <- gsub("\\.","_",i)

    #vec2 is the estimates for that variable
    vec2 <- values(hash_estimates[i])

    #starting sql command
    sql <- paste(sql, "   CASE", sep=" ")

    #counter for case
    count <- 1

    # quantitative  #################################################
    if(grepl("\\[", vec[2])){

      #missing bin
      if(grepl("missing", vec[1])){
        #sql <- paste(sql,"WHEN", paste(name_holder), "in (NULL, '', ' ') THEN", vec2[count], "\n", sep=" ")
        sql <- paste(sql, "WHEN", paste(name_holder), "is NULL or ", paste(name_holder)," in  ('', ' ') THEN" , vec2[count], " \n", sep=" ")
        vec <- vec[-1]
        vec2 <- vec2[-1]
        len <- len - 1
      }else{
        #sql <- paste(sql, "WHEN", paste(name_holder), "in (NULL, '', ' ') THEN 0 \n", sep=" ")
        sql <- paste(sql, "WHEN", paste(name_holder), "is NULL or ", paste(name_holder)," in  ('', ' ') THEN 0 \n", sep=" ")
      }

      #goes through each case for the specific variable, depending on number of binning rules
      for(j in vec){

        #if scientific notation
        if(grepl("e",j)){

          #extract the scientific notation string, then use format to create an integer
          case <- as.character(str_extract_all(j, "[0-9]+.?+([0-9]{1,6})?+e?[-+]?+([0-9]{1,6})?")[[1]])

        }else{
          #extract the range values for each range. Currently for e-06
          case <- as.numeric(str_extract_all(j, "[0-9]+(\\.[0-9]{1,6})?")[[1]])

        }

          #Generate cases
          if((count == 1) && (count != len)){
            #case for 1st case
            sql <- paste(sql, "        WHEN",  paste(name_holder),  "<",case[1], "THEN", vec2[count], "\n",sep= " ")
          }else if(count == len){

            if((match(i, keys(hash_estimates)) == length(holder))){
              #case for when it reaches the end of the cases vector, adds else statement,intercept and end statement
              sql <- paste(sql, "        ELSE", vec2[count], "END + \n    CASE WHEN 1 = 1 THEN", intercept, "END AS out_sum", "\n", sep=" ")
              #end sql command
              sql <- paste(sql, "   FROM", paste(table_name, ";", sep=""), "\n", sep=" ")

            }else{
              #case for when it reaches the end of the cases vector, adds else statement and end statement
              sql <- paste(sql, "        ELSE", vec2[count], "END +", "\n", sep=" ")

            }
          }else{
            #generic in-between cases (not null, first case, or last case)
            sql <- paste(sql, "        WHEN",  paste(name_holder), ">=", case[1], "AND", paste(name_holder), "<", case[2], "THEN", vec2[count], "\n",sep= " ")
          }

        #count is incirimented
        count <- count + 1

      }

    # categorical #############################################
    }else{
      if(grepl("missing", vec[1])){
        #sql <- paste(sql,"WHEN", paste(name_holder), "in (NULL, '', ' ') THEN", vec2[count], "\n", sep=" ")
        sql <- paste(sql, "WHEN", paste(name_holder), "is NULL or ", paste(name_holder)," in  ('', ' ') THEN" , vec2[count], " \n", sep=" ")

        vec <- vec[-1]
        vec2 <- vec2[-1]
        len <- len - 1
      }else{
        #sql <- paste(sql, "WHEN", paste(name_holder), "in (NULL, '', ' ') THEN 0 \n", sep=" ")

        sql <- paste(sql, "WHEN", paste(name_holder), "is NULL or ", paste(name_holder)," in  ('', ' ') THEN 0 \n", sep=" ")


      }

      #goes through each case for the specific variable, depending on number of binning rules
      for(n in vec){

        #is %,% present in the specific binning rule, meaning multiple conditions
        if(grepl('%,%', n)){
          #splits the string into the individual cases if multiple conditions are present for one bin
          cases_str <- strsplit(n, "%,%")

          #start sql code for the split case
          sql_hold <- paste("        WHEN", paste(name_holder), "in (", sep=" ")

          cases_count <- 1

          #loop through the split case string and create sql_hold string (each case with an OR inbetween)
          for(m in cases_str[[1]]){

            if(cases_count == length(cases_str[[1]])){
              sql_hold <- paste(sql_hold, paste("'", m, "'", sep=""),")", sep=" ")
            }else{
              sql_hold <- paste(sql_hold,paste("'", m, "'", sep=""), ",", sep=" ")
            }

            #incriment count
            cases_count <- cases_count + 1

          }

          #Generate cases for when case has %,% present
          if((count == 1) && (count != len)){
            #case for when bin is null and 1st case
            sql <- paste(sql,sql_hold, "THEN", vec2[count], "\n", sep= " ")
          }else if(count == len){

            if((match(i, keys(hash_estimates)) == length(holder))){
              #case for when it reaches the end of the cases vector, adds else statement, intercept and end statement
              sql <- paste(sql, "        ELSE", vec2[count], "END + \n    CASE WHEN 1 = 1 THEN", intercept, "END AS out_sum", "\n", sep=" ")
              #end sql command
              sql <- paste(sql, "   FROM", paste(table_name, ";", sep=""), "\n", sep=" ")

            }else{
              #case for when it reaches the end of the cases vector, adds else statement and end statement
              sql <- paste(sql, "        ELSE", vec2[count], "END +", "\n", sep=" ")

            }
          }else{
            #generic in-between cases (not null, first case, or last case)
            sql <- paste(sql, sql_hold, "THEN", vec2[count], "\n", sep= " ")
          }

          #count is incirimented
          count <- count + 1

        }else{
          #Generate cases for normal case

          if((count == 1) && (count != len)){
            #case for when bin is null and 1st case
            sql <- paste(sql, "        WHEN",  paste(name_holder),  "=", paste("'", n, "'", sep=""), "THEN", vec2[count], "\n",sep= " ")
          }else if(count == len){

            if((match(i, keys(hash_estimates)) == length(holder))){
              #case for when it reaches the end of the cases vector, adds else statement, intercept and end statement
              sql <- paste(sql, "        ELSE", vec2[count],"END + \n    CASE WHEN 1 = 1 THEN", intercept, "END AS out_sum", "\n", sep=" ")
              #end sql command
              sql <- paste(sql, "   FROM", paste(table_name, ";", sep=""), "\n", sep=" ")

            }else{
              #case for when it reaches the end of the cases vector, adds else statement and end statement
              sql <- paste(sql, "        ELSE", vec2[count], "END +", "\n", sep=" ")

            }
          }else{
            #generic in-between cases (not null, first case, or last case)
            sql <- paste(sql, "        WHEN",  paste(name_holder), "=",paste("'", n, "'", sep=""),"THEN", vec2[count], "\n",sep= " ")
          }

          #count is incirimented
          count <- count + 1
        }
      }

    }

  }

  #sql code to add the estimates
  sql <- paste(sql_sel, "\n", sql,  sep= " ")

  return(sql)

}