
# Load Required Packages: ---------------------------------------------------------------------

rm(list = ls())

source(file = "code/myFunctions.r")

packages_load_install(c("dplyr", "lubridate"))

# Load and Modified Data: ---------------------------------------------------------------------

# HOURLY WEATHER DATA:
#   date   = mm/dd/yyyy
#   time   = 0 - 2300
#   hT     = HOURLY TEMPERATURE (CELSIUS)
#   hRH    = HOURLY RELATIVE HUMIDITY (%)
#   hLW    = HOURLY LEAF WETNESS (SCALE 1-10)

data <- read.csv(file = "data/data_6days.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)

summary(data)
str(data)

data <- data %>% 
    mutate(date = mdy_hm(paste0(date, " ", time / 100, ":00"))) %>% 
    select(date, hT, hRH, hLW)

summary(data)
str(data)

# Variable Calculation: -----------------------------------------------------------------------

calculatedVariables = data %>%
    group_by(Date = as.Date(date)) %>%
    summarise(
        # CONSTANT VALUES:
        CONSTANT = 1,
        
        # AVERAGE OF THE AMOUNT BY WHICH hT IS ABOVE 27.4:
        NCHTEMP = NCHTEMP(temp = hT),
        
        # NUMBER OF HOURS hT IS BETWEEN 27.5 AND 32.4 (INCLUSIVE):
        T30 = T30(temp = hT),
        
        # NUMBER OF HOURS LEAF WETNESS IS 5-10 (SCALE 0-10):
        NLW = NLW(x = hLW),
        
        # AVERAGE OF THE HOURLY RANGE OF hT:
        HTRANGE = HTRANGE(temp = hT),
        
        # NUMBER OF HOURS hT IS BETWEEN 17.5 AND 22.4 (INCLUSIVE) AND hRH IS 40% OR HIGHER:
        COND20 = COND20(temp = hT, rh = hRH),
        
        # AVERAGE OF THE HOURLY RANGE OF hRH:
        HRHRANGE = HRHRANGE(rh = hRH),
        
        # AVERAGE hRH:
        ICRH = ICRH(rh = hRH),
        
        # MAXIMUM hT:
        MTMA = MTMA(temp = hT),
        
        # NUMBER OF HOURS hT IS ABOVE 32.5 (INCLUSIVE):
        T35 = T35(temp = hT),
        
        # Range of Tmax
        RANGET = RANGET(temp = hT),
        
        # MAXIMUM hRH:
        MRHMA = MRHMA(rh = hRH),
        
        # AVERAGE hT:
        ICT = ICT(temp = hT),
        
        # Range of RHmax:
        RANGERH = RANGERH(rh = hRH),
        
        # AVERAGE OF THE AMOUNT BY WHICH hRH IS BELOW 40%:
        NCLRH = NCLRH(rh = hRH),
        
        # NUMBER OF HOURS hT IS BELOW 12.5:
        T10 = T10(temp = hT),
    )

calVar <- calculatedVariables

result <- data.frame(Date = calculatedVariables$Date,
                     NonConducive = NaN,
                     Moderate     = NaN,
                     Severe       = NaN,
                     Class        = NaN)

rownames(calculatedVariables) <- calculatedVariables$Date

calculatedVariables <- calculatedVariables %>% 
    select(-Date)

coeff = data.frame(NonConducive = c(-2607, -127.39563, -27.51843, -1.52602, 239.64311, -11.79873, -7.21593, 4.5519,
                                    45.91549, -27.91955, -52.90772, 25.45922, 85.78867, 0.84523, 36.66102, 27.49011),
                   Moderate     = c(-2583, -127.0104, -28.83397, -1.0775, 239.8116, -11.1779, -6.88243, 4.49656,
                                    47.75289, -29.31146, -52.77535, 25.23328, 83.57142, 0.66204, 35.42098, 27.52647),
                   Severe       = c(-2528, -126.51857, -27.47595, -1.45506, 227.29673, -11.56707, -5.84917, 4.8329, 
                                    44.06653, -28.43123, -50.6479, 24.62006, 86.17385, 0.91081, 35.34618, 27.08354)) %>% 
    t() %>% 
    as.data.frame()

colnames(coeff) <- c("CONSTANT", "NCHTEMP", "T30", "NLW", "HTRANGE", "COND20", "HRHRANGE", "ICRH", 
                     "MTMA", "T35", "RANGET", "MRHMA", "ICT", "RANGERH", "NCLRH", "T10")


for (i in 1:3) {
    
    for (j in 1:nrow(result)) {
        
        result[j, i + 1] = sum(coeff[i,] * calculatedVariables[j,])
        
    }
    
}

for (i in 1:nrow(result)) {
    
    result[i, 5] <- names(which.max(result[i, 2:4]))
    
}

resultCal <- left_join(x = calVar,
                       y = result,
                       by = "Date")

write.csv(x = resultCal,
          file = "result/calculatedVariables.csv",
          row.names = FALSE)

# Action Threshold: ---------------------------------------------------------------------------

# According to the model, daily conditions evaluated over a six-day period 
# yield the disease assessments and recommended actions:

actionTable <- data.frame(date                 = NaN,
                          expected_disease     = NaN,
                          spray_recommendation = NaN,
                          next_evaluation      = NaN,
                          PMI                  = NaN)

currentDay <- min(result$Date) + 6
i = 1

while (currentDay <= max(result$Date) + 1) {
    
    classVector <- result[which(x = result$Date == currentDay - 6):(which(x = result$Date == currentDay - 1)), "Class"]
    
    resultCondition <- left_join(x = data.frame(Class = c("NonConducive", "Moderate", "Severe")),
                                 y = as.data.frame(table(Class = classVector)))
    
    resultCondition[is.na(resultCondition)] <- 0
    
    aT <- actionThreshold(nNonConducive = resultCondition[which(resultCondition$Class == "NonConducive"), "Freq"], 
                          nModerate     = resultCondition[which(resultCondition$Class == "Moderate"), "Freq"],
                          nSevere       = resultCondition[which(resultCondition$Class == "Severe"), "Freq"],
                          vector        = classVector, 
                          value         = "NonConducive")
    
    actionTable[i, ] <- c(as.character(currentDay), aT[1], aT[2], aT[3], aT[4])
    
    
    if (aT[3] == "1 day later") {
        currentDay = currentDay + 1
        i = i + 1
    } else if (aT[3] == "3 days later") {
        for (k in 1:2) {
            currentDay = currentDay + 1
            i = i + 1
            if (currentDay > max(result$Date) + 1) {
                break
            }
            actionTable[i, ] <- c(as.character(currentDay), "No evaluation", "Don't spray", paste0((3 - k), ifelse(test = (3 - k) > 1, yes = " days later", no = " day later")), PMI(result, currentDay))
        }
        currentDay = currentDay + 1
        i = i + 1
    } else if (aT[3] == "6 days later") {
        for (k in 1:5) {
            currentDay = currentDay + 1
            i = i + 1
            if (currentDay > max(result$Date) + 1) {
                break
            }
            actionTable[i, ] <- c(as.character(currentDay), "No evaluation", "Don't spray", paste0((6 - k), ifelse(test = (6 - k) > 1, yes = " days later", no = " day later")), PMI(result, currentDay))
        }
        currentDay = currentDay + 1
        i = i + 1
    } else if (aT[3] == "16 days after the last spray") {
        for (k in 1:15) {
            currentDay = currentDay + 1
            i = i + 1
            if (currentDay > max(result$Date) + 1) {
                break
            }
            actionTable[i, ] <- c(as.character(currentDay), "No evaluation", "Don't spray", paste0((16 - k), ifelse(test = (16 - k) > 1, yes = " days later", no = " day later")), PMI(result, currentDay))
        }
        currentDay = currentDay + 1
        i = i + 1
    }
    
}



write.csv(x = actionTable %>% arrange(desc(date)), file = "result/result.csv", row.names = FALSE)
