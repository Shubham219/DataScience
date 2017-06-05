#install.packages("RODBC") #installing the package RODBC

library(RODBC) #loading the package RODBC

odbcDataSources() # to check all available ODBC data sources

#creating a Database connection
# for username,password,database name and DSN name

chan=odbcConnect("PostgreSQL30","postgres;Password=12345;Database=datascience")
#to list all table names
sqlTables(chan)
#and fetch some data
diamond <- sqlFetch(chan,"diamond")

library(sqldf)

sqldf("select * From diamond")

sqldf("Select carat, colour From diamond ")

sqldf("Select Distinct colour, carat From diamond")

sqldf("Select Count(Distinct colour) From diamond")

sqldf("Select Price from diamond 
      where colour == 'D'")

sqldf("Select Price, Colour From diamond
      where carat <= 0.3")

sqldf("Select Count (Distinct Price) From diamond
      where carat <= 0.3")

sqldf("Select Price,Colour,Carat from diamond
      where carat = 0.1 OR colour = 'D'")

sqldf("Select Price,Colour,Carat from diamond
      where carat = 1.0 AND colour = 'D'")

sqldf("Select Distinct(Colour)  from diamond
      where  Not colour =  'D'")

sqldf("Select * from diamond
      where colour =  'D' AND carat == 1
      Order By Price ASC, carat DESC")

# Insert Not Working
sqldf("Insert Into diamond (carat, colour, clarity, certification, price)
      values (1.00, 'A', 'XXX', 'ABC', 3)")

sqldf("Select * from diamond
      where colour =  'A'")

# DELAING WITH NULL VALUE
sqldf("SELECT * From diamond
      Where carat Is Not Null")

# Update
sqldf("Update diamond
      Set colour = 'A', carat = 9
      Where Colour = 'D' AND carat == 1.00")
