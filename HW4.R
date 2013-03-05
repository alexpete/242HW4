# Data prep

setw('Data/') # Data must be in a subdirectory named Data
#system('tar -xjvf Years1987_1999.tar.bz2')
#system('tar -xjvf Years2000_2008.tar.bz2')
#system('sqlite 3 air.db < airline.sqlite')


##############################################
# Use shell to get number of departing flights
# from LAX, SFO, SMF and OAK
##############################################

# Note - the csv files must be in current directory

counts = system("cut -d',' -f17 [12]*.csv | egrep '(LAX|SFO|SMF|OAK)' | sort | uniq -c", intern = T)


##############################################
# Combine shell with R connections to obtain 
# mean and standard eviations of delay times 
# for LAX, OAK, SFO and SMF
##############################################


B = 10000;
apts = c('LAX', 'OAK', 'SFO', 'SMF')
sum_xi = structure(numeric(4), names = apts)
sum_xi2 = sum_xi
total = structure(integer(4), names = apts)

temp = readLines(con, n = B)

while(length(temp) > 0){
  
  temp = strsplit(temp, split = ',')
  data = t(sapply(1:length(temp), function(j){
    temp[[j]][1:3]
  }))
  for(i in apts){
    ind = (data[,2] == i | data[,3] == i) & data[, 1] != 'NA'
    sum_xi[i] = sum_xi[i] + sum(as.numeric(data[ind,1]))
    sum_xi2[i] = sum_xi2[i] + sum(as.numeric(data[ind,1])^2)
    total[i] = total[i] + sum(ind)
  }
  
  temp = readLines(con, n = B)
}   

close(con)


avg_delay1 = sum_xi/total
sd_delay1 = sqrt(sum_xi2/total - avg_delay1^2)


##############################################
# Use connections to obtain mean and standard
# deviations of delay times for LAX, OAK,
# SFO and SMF
##############################################


Sys.setlocale(locale="C") # for strange characters in 2001.csv and 2002.csv

# Be careful that this regular expression only matches the files you want

confiles = list.files(pattern = '[12]*.csv')
B = 10000;
sum_xi = structure(numeric(4), names = apts)
sum_xi2 = sum_xi
total = structure(integer(4), names = apts)

for(i in 1:length(confiles)){
  
  con = file(confiles[i], open = 'r')
  temp = readLines(con, n = B)
  
  while(length(temp) > 0){
    
    temp = strsplit(grep('(LAX|OAK|SFO|SMF|)', temp, value = T), split = ',')
    data = t(sapply(1:length(temp), function(j){
      temp[[j]][c(15,17,18)]
    }))
    for(i in apts){
      ind = (data[,2] == i | data[,3] == i) & data[, 1] != 'NA'
      sum_xi[i] = sum_xi[i] + sum(as.numeric(data[ind,1]))
      sum_xi2[i] = sum_xi2[i] + sum(as.numeric(data[ind,1])^2)
      total[i] = total[i] + sum(ind)
    }
    
    temp = readLines(con, n = B)
  }   
  
  close(con)
}

avg_delay2 = sum_xi/total
sd_delay2 = sqrt(sum_xi2/total - avg_delay2^2)


##############################################
# Use database queries to get same information
# as above
##############################################


library(RSQLite)

drv = dbDriver('SQLite')
con = dbConnect(drv, dbname = 'air.db')

sql.qry1 = "SELECT Origin, AVG(ArrDelay) AS avg_delay, AVG(ArrDelay*ArrDelay) AS avg_delay_sq, COUNT(*) AS total FROM delays WHERE Origin IN ('LAX', 'OAK', 'SFO', 'SMF') AND NOT ArrDelay = 'NA' GROUP BY Origin"

rslt = dbSendQuery(con, sql.qry1)
nums_origin = fetch(rslt, n = -1)
sqliteCloseResult(rslt)

sql.qry2 = "SELECT Dest, AVG(ArrDelay) AS avg_delay, AVG(ArrDelay*ArrDelay) AS avg_delay_sq, COUNT(*) AS total FROM delays WHERE Dest IN ('LAX', 'OAK', 'SFO', 'SMF') AND NOT ArrDelay = 'NA' GROUP BY Dest"


rslt = dbSendQuery(con, sql.qry2)
nums_dest = fetch(rslt, n = -1)
sqliteCloseResult(rslt)

total = nums_origin[,4] + nums_dest[,4]
avg_delay3 = (nums_origin[,2]*nums_origin[,4] + nums_dest[,2]*nums_dest[,4])/total
sd_delay3 = sqrt((nums_origin[,3]*nums_origin[,4] + nums_dest[,3]*nums_dest[,4])/total - avg_delay3^2)

