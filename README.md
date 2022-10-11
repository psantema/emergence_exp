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
    ID  - unique identity for each individual
    age - age as category (1=yearling, 2=adult)
    
  em.csv (contains all emergence time data)
    transponder - unique transponder number for each individual
    yday        - day of year (1 = January 1st)
    box         - unique identity for each nestbox
    date_       - date
    time_       - time of day in decimal hours
    april_day   - day of April (1 = April 1st)
    sunrise     - sunrise time in decimal hours
    em_time     - emergence time in minutes relative to sunrise
    ID          - unique identity for each individual
    
  firstEgg.csv (contains date of first egg for each box)
    box   - unique identity for each nestbox
    date_ - date of first egg
    
  males.csv (contains IDs of males that were part of the breeding population)
    ID  - unique identity for each individual
  
  neighbour.csv (contains neighbourhood order between all combinations fo territories)
    year_ - year of study
    no    - neighbourhood order
    box1  - identity of one nestbox
    box2  - identity of other nestbox
  
  pat.csv (contains summary data for extra-pair siring success)
    ID      - unique identity for each individual
    females - number of femles a males sired extra-pair young with
    young   - total number of extra-pair you sires
    epp     - whether or not males sired at least on extra-pair young
  
  score.csv (contains treatment score of manipulated males)
    ID      - unique identity for each individual
    score1  - treatment score 1st order neighbourhood
    score2  - treatment score 1st + 2nd order neighbourhood
 
  sex.csv (contains sex of each individual)
    ID  - unique identity for each individual
    sex - sex(1=male, 2=female)
 
  sunrise.csv (contains sunrise time for each day) 
    sunrise - sunrise time in decimal hours
    sunset  - sunset time in decimal hours
    yday    - day of year (1 = January 1st)
 
  terrbox.csv (contains territory (i.e. ID of breeding box) in which each nestbox is located)   
    year_     - year of study
    box       - unique identity for each nestbox
    territory - identity of territory
 
  transponders.csv  
    ID          - unique identity for each individual
    transponder - unique transponder number for each individual
 
  treat.csv (raw treatment data)
    box       - unique identity for each nestbox
    transp    - unique transponder number for each individual
    ID        - unique identity for each individual
    age       - age as category (1=yearling, 2=adult)
    treatment - experimental treatment (1=light, 0=control)
    put_out   - date experimental equipment was put out
    removed   - date experimental equipment was removed
 
  treat2.csv (summarised treatment data)
    ID        - unique identity for each individual
    em_time   - emergence time in minutes relative to sunrise
    age       - age as category (1=yearling, 2=adult)
    treatment - experimental treatment (1=light, 0=control)
    nights    - number of mornings male was manipulated
    score1    - treatment score 1st order neighbourhood
    score2    - treatment score 1st + 2nd order neighbourhood
 
 
 