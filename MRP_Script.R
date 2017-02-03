library("devtools")
install_github("mrp", "malecki", sub="mrpdata")
install_github("mrp", "malecki", sub="mrp")
library("mrp")

source_gist('https://gist.github.com/wlattner/9321823060b27885792f4adb9cfd7ffa',filename='civis_r_api.R')

cluster <- 'Global Strategy Group'

# Bring in the MRP data
mrp_table <- 'mrp.sf_data'

# Also bring in the pop table for postratification purposes
mrp_pop <- 'mrp.sf_pop'

#Specify the table we want to write the output to
mrp_output <- 'mrp.output'

# read_civis takes a table (or a SQL query) and converts it into a data frame
data <- read_civis(table = mrp_table,database = cluster)
pop <- read_civis(table = mrp_pop,database = cluster)

data <- within(data, 
               {reg.gender <- interaction(region,gender)
               reg.party <- interaction(region,party)
               reg.race <- interaction(region,race)
               reg.age <- interaction(region,age)
               reg.edu <- interaction(region,edu)
               gender.party <- interaction(gender,party)                
               gender.race <- interaction(gender,race)
               gender.age <- interaction(gender,age)                
               gender.edu <- interaction(gender,edu)                
               party.race <- interaction(party,race)				
               party.age <- interaction(party,age)				
               party.edu <- interaction(party,edu)				
               race.age <- interaction(race,age)
               race.edu <- interaction(race,edu)
               age.edu <- interaction(age,edu)
               })

pop <- within(pop, 
              {reg.gender <- interaction(region,gender)
              reg.party <- interaction(region,party)
              reg.race <- interaction(region,race)
              reg.age <- interaction(region,age)
              reg.edu <- interaction(region,edu)
              gender.party <- interaction(gender,party)                
              gender.race <- interaction(gender,race)
              gender.age <- interaction(gender,age)                
              gender.edu <- interaction(gender,edu)                
              party.race <- interaction(party,race)				
              party.age <- interaction(party,age)				
              party.edu <- interaction(party,edu)				
              race.age <- interaction(race,age)
              race.edu <- interaction(race,edu)
              age.edu <- interaction(age,edu)
              })

mrp.yes <- mrp(yesvote ~ age + race + gender + party + edu + region
               + reg.gender + reg.party + gender.party + age.edu,
               data = data,
               formula.pop.update= .~.-wave,
               population=pop,
               pop.weights="sum")

mrp.no <- mrp(novote ~ age + race + gender + party + edu + region
              + reg.gender + reg.party + gender.party + age.edu,
              data = data,
              formula.pop.update= .~.-wave,
              population=pop,
              pop.weights="sum")


#Okay, now let's make a punch of nice data frames for the candidate results by wave
print("Yes Share")
print(100*poststratify(mrp.yes))
print("No Share")
print(100*poststratify(mrp.no))
