# emergence_exp


# Experiment dvancing emergennce time of blue tit males to examine effects on extra-pair siring success


### Description of scripts
data
  Produces all main data files and saves them in 'data' folder
  NOTE: Retrieves data from local database and cannot be run without credentials. However, the resulting files are available in 'data' folder.
  
treatment-emergence
  Analyses and figures examining the relation between the treatment and emergence time
  
treatment-paternity
  Analyses and figures examining the relation between the treatment and paternity
  
score-paternity
  Analyses and figures examining the relation between the strength of the treatment and paternity

emergence-paternity
  Analyses and figures examining the relation between the emergence time and paternity

neighbour
  Calculates neighbourhood order between each combination of two territories

treatment_score
  Calculates the strength of the treatment for each male (based on the number of mornings it was manipulated and the number of females in the local neighbourhood that were fertile on those days)



### Description of data files

age.csv (contains ages of all individuals in each season)
  ID  - unique idenity for each individual
  age - age as category (1=yearling, 2=adult)
  
 
em.csv (contains all emergence time data)
  transponder - unique transponder number for each individual
  yday        - day of year (1 = January 1st)
  box         - unique identity for each nestbox
  date_       - date
  time_       - time of day in decimal hours
  april_day   - day of Arpil (1 = April 1st)
  sunrise     - sunrise time in decimal hours
  em_time     - emergence time in minutes relative to sunrise
  ID          - unique idenity for each individual
  
ffirstEgg.csv 
  box   - unique identity for each nestbox
  date_ - date of first egg
  



