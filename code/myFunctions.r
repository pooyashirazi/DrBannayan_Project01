
# INSTALL OR LOAD REQUIRED PACKAGES: ----------------------------------------------------------

packages_load_install <- function(requirePackages) {
    
    installedPackages = as.data.frame(installed.packages())[["Package"]]
    for (package in requirePackages) {
        if (package %in% installedPackages) {
            library(package = package,
                    character.only = TRUE, 
                    warn.conflicts = FALSE)
        } else {
            install.packages(package)
        }
    }
    
}

# FIND CONSECUTIVE VALUES: --------------------------------------------------------------------

consValus <- function(vector, value) {
    
    x <- rle(x = vector)
    len <- x$lengths[x$value == value]
    ifelse(test = length(len) > 0, yes = max(len), no = 0)
    
}

# AVERAGE OF THE AMOUNT BY WHICH HT IS ABOVE 27.4: --------------------------------------------

NCHTEMP <- function(temp) {
    
    mean(x = ifelse(test = temp > 27.4, 
                    yes = temp - 27.4, 
                    no = 0),
         na.rm = TRUE)
    
}

# NUMBER OF HOURS hT IS BETWEEN 27.5 AND 32.4 (INCLUSIVE): ------------------------------------

T30 <- function(temp) {
    
    sum(ifelse(test = temp >= 27.5 & temp <= 32.4,
               yes = 1,
               no = 0),
        na.rm = TRUE)
    
}

# NUMBER OF HOURS LEAF WETNESS IS 5-10 (SCALE 0-10): ------------------------------------------

NLW <- function(x) {
    
    sum(ifelse(test = x > 5,
               yes = 1,
               no = 0),
        na.rm = TRUE)
    
}

# AVERAGE OF THE HOURLY RANGE OF hT: ----------------------------------------------------------

HTRANGE <- function(temp) {
    
    mean(x = abs(temp[1:23] - temp[2:24]), na.rm = TRUE)
    
}

# NUMBER OF HOURS hT IS BETWEEN 17.5 AND 22.4 (INCLUSIVE) AND hRH IS 40% OR HIGHER: -----------

COND20 <- function(temp, rh) {
    
    sum(ifelse(test = temp >= 17.5 & temp <= 22.4 & rh >= 40,
               yes = 1,
               no = 0),
        na.rm = TRUE)
    
}

# AVERAGE OF THE HOURLY RANGE OF hRH: ---------------------------------------------------------

HRHRANGE <- function(rh) {
    
    mean(x = abs(rh[1:23] - rh[2:24]), na.rm = TRUE)
    
}

# AVERAGE hRH: --------------------------------------------------------------------------------

ICRH <- function(rh) {
    
    mean(x = rh, na.rm = TRUE)
    
}

# MAXIMUM hT: ---------------------------------------------------------------------------------

MTMA <- function(temp) {
    
    max(temp, na.rm = TRUE)
}

# NUMBER OF HOURS hT IS ABOVE 32.5 (INCLUSIVE): -----------------------------------------------

T35 <- function(temp) {
    
    sum(ifelse(test = temp >= 32.5,
               yes = 1,
               no = 0),
        na.rm = TRUE)
    
}

# Range of Tmax: ------------------------------------------------------------------------------

RANGET <- function(temp) {
    
    max(temp, na.rm = TRUE) - min(temp, na.rm = TRUE)
    
}

# MAXIMUM hRH: --------------------------------------------------------------------------------

MRHMA <- function(rh) {
    
    max(rh, na.rm = TRUE)
}

# AVERAGE hT: ---------------------------------------------------------------------------------

ICT <- function(temp) {
    
    mean(x = temp, na.rm = TRUE)
    
}

# Range of RHmax: -----------------------------------------------------------------------------

RANGERH <- function(rh) {
    
    max(rh, na.rm = TRUE) - min(rh, na.rm = TRUE)
    
}

# AVERAGE OF THE AMOUNT BY WHICH hRH IS BELOW 40%: --------------------------------------------

NCLRH <- function(rh) {
    
    mean(x = ifelse(test = rh < 40,
                    yes = 40 - rh,
                    no = 0),
         na.rm = TRUE)
    
}

# NUMBER OF HOURS hT IS BELOW 12.5: -----------------------------------------------------------

T10 <- function(temp) {
    
    sum(ifelse(test = temp < 12.5,
               yes = 1,
               no = 0),
        na.rm = TRUE)
    
}

# CALCULATE ACTION THRESHOLD: -----------------------------------------------------------------

actionThreshold <- function(nNonConducive, nModerate, nSevere, vector, value) {
    
    if (nNonConducive == 6) {
        c("none", "Don't spray", "6 days later", "0.0")
    } else if (nModerate == 6) {
        c("moderate", "Don't spray", "3 days later", "0.5")
    } else if (nSevere == 6) {
        c("severe", "Spray", "16 days after the last spray", "1.0")
    } else if (nSevere == 0 & consValus(vector, value) <= 1) {
        c("none to moderate", "Don't spray", "3 days later", "0.5")
    } else if (consValus(vector, value) >= 2) {
        c("none to moderate", "Don't spray", "6 days later", "0.0")
    } else if (nSevere >= 3 & consValus(vector, value) <= 1) {
        c("moderate to severe", "Spray", "16 days after the last spray", "1.0")
    } else if (nSevere < 3 & consValus(vector, value) <= 1) {
        c("moderate", "Don't spray", "1 day later", "0.5")
    }
    
}



# Calculate PMI
PMI <- function(result, currentDay) {
    classVector <- result[which(x = result$Date == currentDay - 6):(which(x = result$Date == currentDay - 1)), "Class"]
    
    resultCondition <- left_join(x = data.frame(Class = c("NonConducive", "Moderate", "Severe")),
                                 y = as.data.frame(table(Class = classVector)))
    
    resultCondition[is.na(resultCondition)] <- 0
    
    aT <- actionThreshold(nNonConducive = resultCondition[which(resultCondition$Class == "NonConducive"), "Freq"], 
                          nModerate     = resultCondition[which(resultCondition$Class == "Moderate"), "Freq"],
                          nSevere       = resultCondition[which(resultCondition$Class == "Severe"), "Freq"],
                          vector        = classVector, 
                          value         = "NonConducive")
    
    return(aT[4])
}























