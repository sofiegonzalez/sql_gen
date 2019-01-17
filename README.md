# sql_gen
Generates SQL code to parse a data set and sum estimates 
Will provide mock data and model summary for testing

Example Implementation:

require(devtools)
require(dplyr)
require(hash)
require(stringr)
require(sqldf)

Model<-model_summary(m21,bins=bins) #provided
load("data_1.RData") #provided
load("data_2.RData")

SQL<-sql_generation(Model,"data_1") #pass the model summary data frame and name of data frame containing variable values
SQL_score<-sqldf(SQL) #SQL code creates a data frame from containing "data_1.RData" and the new estimates column

R_score<-predict(m21,newdata=data_woe)
plot(R_score,SQL_score$out_sum,xlab="Score Using R",ylab="Score using SQL",main="Scoring test on Myrtle Beach outbound model"); lines(c(-11,11),c(-11,11),col="red",lwd=3)

SQL<-sql_generation(Model,"data_2")
data_table <- sqldf(SQL) #new data frame

Example Output:

SELECT 
     CASE WHEN var1 is NULL or  var1  in  ('', ' ') THEN 0 
         WHEN var1 = '< 0' THEN -0.926258504 
         WHEN var1 = '0 <= < 200' THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN var10 is NULL or  var10  in  ('', ' ') THEN 0 
         WHEN var10 = 'none' THEN -0.926258504 
         WHEN var10 = 'co' THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN var13 is NULL or  var13  in  ('', ' ') THEN 0 
         WHEN var13 < 8 THEN -0.926258504 
         WHEN var13 >= 8 AND var13 < 18 THEN -0.926258504 
         WHEN var13 >= 18 AND var13 < 26 THEN -0.926258504 
         WHEN var13 >= 26 AND var13 < 44 THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN var16 is NULL or  var16  in  ('', ' ') THEN 0 
         WHEN var16 < 2 THEN -0.926258504 
         WHEN var16 >= 2 AND var16 < 3 THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN var2 is NULL or  var2  in  ('', ' ') THEN 0 
         WHEN var2 < 26 THEN -0.926258504 
         WHEN var2 >= 26 AND var2 < 28 THEN -0.926258504 
         WHEN var2 >= 28 AND var2 < 35 THEN -0.926258504 
         WHEN var2 >= 35 AND var2 < 37 THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN var20 is NULL or  var20  in  ('', ' ') THEN 0 
         WHEN var20 = 'yes' THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN var9 is NULL or  var9  in  ('', ' ') THEN 0 
         WHEN var9 = 'divorced' THEN -0.926258504 
         WHEN var9 = 'single' THEN -0.926258504 
         ELSE -0.926258504 END + 
    CASE WHEN 1 = 1 THEN -0.926258504 END AS out_sum_column 
    FROM data_1;
