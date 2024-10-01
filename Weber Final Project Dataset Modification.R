#Packages
library(dplyr)
library(readxl)

# Adding the dataset
fangraphs_data <- read_excel(("C:/Users/weber/OneDrive/Desktop/Baseball/Weber Final Project/Fangraphs Data Weber.xlsx"))

# Changing innings pitched to fraction format, otherwise the dataset will not run certain functions correctly
# R didn't like the code I was trying, but the Dollarde function on excel allowed me to make the change there

# Creating opponent SAC's to find AB's against, to then calculate average against
# Issue was not having access to the amount of runners inherited that got out on the base paths. Can't formulate it
# without this as it causes for there to be more outs recorded than batters faced.

#fangraphs_data <- fangraphs_data %>%
  #mutate(SAC = round(TBF - (3 * IPDec) - H - BB - IBB - HBP))

# Making necessary changes to the dataset: Combining players stats and either averaging them or summing them depending on the nature of the stat



fangraphs_data <- fangraphs_data %>%
  group_by(PlayerId) %>%
  summarize(
    MLBAMID = first(MLBAMID),
    Name = first(NameASCII),
    Seasons = paste(min(Season), max(Season), sep = "-"),
    Age = paste(min(Age), max(Age), sep = "-"),
    Teams_Pitched_For = paste(unique(Team), collapse = ", "),
    G = sum(G),
    GS = sum(GS),
    IP = sum(IPDec),
    TBF = sum(TBF),
    W = sum(W),
    L = sum(L),
    CG = sum(CG),
    ShO = sum(ShO),
    SV = sum(SV),
    BS = sum(BS),
    HLD = sum(HLD),
    SD = sum(SD),
    MD = sum(MD),
    Pulls = sum(Pulls),
    ERA = (9 * sum(ER) / sum(IPDec)),
    R = sum(R),
    ER = sum(ER),
    H = sum(H),
    HR = sum(HR),
    SO = sum(SO),
    BB = sum(BB),
    IBB = sum(IBB),
    HBP = sum(HBP),
    WP = sum(WP),
    BK = sum(BK),
    Events = sum(Events),
    GB = sum(GB),
    LD = sum(LD),
    FB = sum(FB),
    IFFB = sum(IFFB),
    BU = sum(BU),
    Balls = sum(Balls),
    Strikes = sum(Strikes),
    Pitches = sum(Pitches),
    RS = sum(RS),
    RS_per_9 = (sum(RS) / 9),
    K_pct = (sum(SO) / sum(TBF)),
    BB_pct = (sum(BB) / sum(TBF)),
    K_per_9 = (sum(SO) / 9),
    BB_per_9 = (sum(BB) / 9),
    K_to_BB = (sum(SO) / sum(BB)),
    H_per_9 = (sum(H) / 9),
    HR_per_9 = (sum(HR) / 9),
    # can't add avg against,so using a modified formula to get as close as possible. Missing Sacrifices
    Mod_AVG = sum(H) / (sum(TBF) - sum(BB) - sum(HBP) - sum(IBB)),
    WHIP = ((sum(H) + sum(BB))/ sum(IPDec)),
    
    
    
    
  )