##############################################
# Use shell to get number of departing flights
# from LAX, SFO, SMF and OAK
##############################################

# Check OS type

if(.Platform$OS.type == 'windows') system = shell;

# Note - the csv files must be in current directory
counts = system("cut -d',' -f17 [12]*.csv | egrep '(SFO|SMF|OAK)' | sort | uniq -c", intern = T)



confiles = list.files(pattern = 'csv')
B = 10000;
sum_xi = structure(numeric(4), names = c('LAX', 'OAK', 'SFO', 'SMF'))
sum_xi2 = sum_xi
total = 0

for(i in 1:length(confiles)){
  
  con = file(confiles[i], open = 'r')
  temp = character(B)
  
  while(length(temp) == B){
    
    temp = readLines(con, n=B)
    temp = temp[grep('(LAX|OAK|SFO|SMF)', temp)]
    temp2 = strsplit(temp, split = ',')
    arrdelays = sapply(1:length(temp2), function(j){
      as.integer(temp2[[j]][15])
    })
    
    for(k in 1:4){
      
      wchlns = grep(names(sum_xi)[k], temp)
      sum_xi[k] = sum_xi[k] + sum(arrdelays[wchlns])
      sum_xi2[k] = sum_xi2[k] + sum(arrdelays[wchlns]^2)
    }
  }
}
