#Analyses Fake News Exposure
library(stargazer)
library(multiwayvcov)
library(lmtest)
library(haven)
library(readstata13)
library(fabricatr)
library(survey)
library(clubSandwich)
library(KRLS)
library(estimatr)
library(dplyr)
library(miceadds)
library(wesanderson)
library(srvyr)
library(ggplot2)
library(texreg)
library(sjPlot)
library(insight)
library(magrittr)


#SET WD AND LOAD DATA
setwd("C:/Users/dl0ck/Dropbox/EBMA_experiments")
oct <- read.dta13('October Wave/Data_1018/oct_whole.dta')
jan <- read.dta13('January Wave/Data_0219/jan_whole.dta')



#a<-read.csv('October Wave/Data_1018/oct_HL.csv')
#b<-read.csv('January Wave/Data_0219/jan_HL.csv')
#make som factored variables

oct$agecat <- (as.factor(oct$agecat))
jan$agecat <- (as.factor(jan$agecat))

#set up October dataset
octHead <- tidyr::gather(oct, headline_number, HL_accuracy, headline_accuracy_1_w2, headline_accuracy_2_w2, headline_accuracy_3_w2, headline_accuracy_4_w2, headline_accuracy_5_w2, headline_accuracy_6_w2, headline_accuracy_7_w2, headline_accuracy_8_w2, headline_accuracy_9_w2, headline_accuracy_10_w2, headline_accuracy_11_w2, headline_accuracy_12_w2, headline_accuracy_13_w2, headline_accuracy_14_w2, headline_accuracy_15_w2, headline_accuracy_16_w2)

octHead$HL_accuracy <- as.character(octHead$HL_accuracy)
octHead$HL_accuracy[octHead$HL_accuracy == 'Not at all accurate'] <- 1
octHead$HL_accuracy[octHead$HL_accuracy == 'Not very accurate'] <- 2
octHead$HL_accuracy[octHead$HL_accuracy == 'Somewhat accurate'] <- 3
octHead$HL_accuracy[octHead$HL_accuracy == 'Very accurate'] <- 4
octHead$HL_accuracy <- as.numeric(octHead$HL_accuracy)



#Set up January dataset
janHead <- tidyr::gather(jan, headline_number, HL_accuracy, headline_accuracy_1_w2, headline_accuracy_2_w2, headline_accuracy_3_w2, headline_accuracy_4_w2, headline_accuracy_5_w2, headline_accuracy_6_w2, headline_accuracy_7_w2, headline_accuracy_8_w2, headline_accuracy_9_w2, headline_accuracy_10_w2, headline_accuracy_11_w2, headline_accuracy_12_w2, headline_accuracy_13_w2, headline_accuracy_14_w2, headline_accuracy_15_w2, headline_accuracy_16_w2)

janHead$HL_accuracy <- as.character(janHead$HL_accuracy)
janHead$HL_accuracy[janHead$HL_accuracy == 'Not at all accurate'] <- 1
janHead$HL_accuracy[janHead$HL_accuracy == 'Not very accurate'] <- 2
janHead$HL_accuracy[janHead$HL_accuracy == 'Somewhat accurate'] <- 3
janHead$HL_accuracy[janHead$HL_accuracy == 'Very accurate'] <- 4

janHead$HL_accuracy <- as.numeric(janHead$HL_accuracy)


##Associating the headline number with its name
#headline_accuracy_1_w2 -- Trump Caught --"s_accuracy_donald_trump_caught" 

#headline_accuracy_2_w2 -- Franklin Graham -- "s_accuracy_franklin_graham"

#headline_accuracy_3_w2 -- VP Mike Pence -- "s_accuracy_vp_mike_pence"

#headline_accuracy_4_w2 -- Vice President Pence -- "s_accuracy_vice_president_pence"

#headline_accuracy_5_w2 -- Soros Money -- "s_accuracy_soros_money_behind" 

#headline_accuracy_6_w2 -- Kavanaugh Accuser -- "s_accuracy_kavanaugh_accuser"
#headline_accuracy_7_w2 -- FBI agent who -- "s_accuracy_fbi_agent_who" 

#headline_accuracy_8_w2 -- Lisa Page -- "s_accuracy_lisa_page"

#headline_accuracy_9_w2 -- A series -- "s_accuracy_a_series1" 

#headline_accuracy_10_w2 -- A border patrol -- "s_accuracy_a_border_patrol"

#headline_accuracy_11_w2 -- Detention of Migrant -- "s_accuracy_detention_of_migrant" 

#headline_accuracy_12_w2 -- and now1 -- "s_accuracy_and_now1"

#headline_accuracy_13_w2 -- Google employees -- "s_accuracy_google_employees" 

#headline_accuracy_14_w2 -- Feds said alleged -- "s_accuracy_feds_said_alleged"

#headline_accuracy_15_w2 -- Accuracy small bus -- "s_accuracy_small_busisness_opt" 

#headline_accuracy_16_w2 -- Economy adds more -- "s_accuracy_economy_adds_more"



#Generate variable indicating the type of story

# proD hyper 1 = donald_trump_caught_png, 2 = franklin_graham_png
# proR hyper 5 = soros_money_behind_png, 6 = kavanaugh_accuser_png
# proD fake 3 = vp_mike_pence_png 4 = vice_president_pence_png
# proR fake 7 = fbi_agent_who_png, 8 = lisa_page_png
# proD real 9 = a_series_of_suspicious_png, 10 = a_border_patrol_png, 11 = detention_of_migrant__png 12 = and_now_its_the_tallest_png
# proR real 13 = google_employees_png, 14 = feds_said_alleged_png, 15 = small_busisness_optimism_, 16 = economy_adds_more_png

octHead$proD <- 0
octHead$proR <- 0
octHead$highProm <- 0
octHead$lowProm <- 0
octHead$fake <- 0
octHead$hyper <- 0
octHead$real <- 0
octHead$type <- NA
for (i in 1:nrow(octHead)) {
    if (octHead$headline_number[i] == 'headline_accuracy_11_w2' | octHead$headline_number[i] == 'headline_accuracy_12_w2') {
        octHead$type[i] <- 'ProD Real High Prominence'
        octHead$proD[i] <- 1
        octHead$highProm[i] <- 1
        octHead$real[i] <- 1

    } else if (octHead$headline_number[i] == 'headline_accuracy_9_w2' | octHead$headline_number[i] == 'headline_accuracy_10_w2') {
        octHead$type[i] <- 'ProD Real Low Prominence'
        octHead$proD[i] <- 1
        octHead$lowProm[i] <- 1
        octHead$real[i] <- 1

    } else if (octHead$headline_number[i] == 'headline_accuracy_15_w2' | octHead$headline_number[i] == 'headline_accuracy_16_w2') {
        octHead$type[i] <- 'ProR Real High Prominence'
        octHead$proR[i] <- 1
        octHead$highProm[i] <- 1
        octHead$real[i] <- 1

    } else if (octHead$headline_number[i] == 'headline_accuracy_13_w2' | octHead$headline_number[i] == 'headline_accuracy_14_w2') {
        octHead$type[i] <- 'ProR Real Low Prominence'
        octHead$proR[i] <- 1
        octHead$lowProm[i] <- 1
        octHead$real[i] <- 1
    }
}

for (i in 1:nrow(octHead)) {
    if (octHead$headline_number[i] == 'headline_accuracy_1_w2' | octHead$headline_number[i] == 'headline_accuracy_2_w2') {
        octHead$type[i] <- 'ProD Hyper'
        octHead$proD[i] <- 1
        octHead$hyper[i] <- 1

    } else if (octHead$headline_number[i] == 'headline_accuracy_5_w2' | octHead$headline_number[i] == 'headline_accuracy_6_w2') {
        octHead$type[i] <- 'ProR Hyper'
        octHead$proR[i] <- 1
        octHead$hyper[i] <- 1

    } else if (octHead$headline_number[i] == 'headline_accuracy_3_w2' | octHead$headline_number[i] == 'headline_accuracy_4_w2') {
        octHead$type[i] <- 'ProD Fake'
        octHead$proD[i] <- 1
        octHead$fake[i] <- 1

    } else if (octHead$headline_number[i] == 'headline_accuracy_7_w2' | octHead$headline_number[i] == 'headline_accuracy_8_w2') {
        octHead$type[i] <- 'ProR Fake'
        octHead$proR[i] <- 1
        octHead$fake[i] <- 1

    }
}




janHead$proD <- 0
janHead$proR <- 0
janHead$highProm <- 0
janHead$lowProm <- 0
janHead$fake <- 0
janHead$hyper <- 0
janHead$real <- 0
janHead$type <- NA
for (i in 1:nrow(janHead)) {
    if (janHead$headline_number[i] == 'headline_accuracy_11_w2' | janHead$headline_number[i] == 'headline_accuracy_12_w2') {
        janHead$type[i] <- 'ProD Real High Prominence'
        janHead$proD[i] <- 1
        janHead$highProm[i] <- 1
        janHead$real[i] <- 1



    } else if (janHead$headline_number[i] == 'headline_accuracy_9_w2' | janHead$headline_number[i] == 'headline_accuracy_10_w2') {
        janHead$type[i] <- 'ProD Real Low Prominence'
        janHead$proD[i] <- 1
        janHead$lowProm[i] <- 1
        janHead$real[i] <- 1

    } else if (janHead$headline_number[i] == 'headline_accuracy_15_w2' | janHead$headline_number[i] == 'headline_accuracy_16_w2') {
        janHead$type[i] <- 'ProR Real High Prominence'
        janHead$proR[i] <- 1
        janHead$highProm[i] <- 1
        janHead$real[i] <- 1

    } else if (janHead$headline_number[i] == 'headline_accuracy_13_w2' | janHead$headline_number[i] == 'headline_accuracy_14_w2') {
        janHead$type[i] <- 'ProR Real Low Prominence'
        janHead$proR[i] <- 1
        janHead$lowProm[i] <- 1
        janHead$real[i] <- 1
    }
}

for (i in 1:nrow(janHead)) {
    if (janHead$headline_number[i] == 'headline_accuracy_1_w2' | janHead$headline_number[i] == 'headline_accuracy_2_w2') {
        janHead$type[i] <- 'ProD Hyper'
        janHead$proD[i] <- 1
        janHead$hyper[i] <- 1

    } else if (janHead$headline_number[i] == 'headline_accuracy_5_w2' | janHead$headline_number[i] == 'headline_accuracy_6_w2') {
        janHead$type[i] <- 'ProR Hyper'
        janHead$proR[i] <- 1
        janHead$hyper[i] <- 1

    } else if (janHead$headline_number[i] == 'headline_accuracy_3_w2' | janHead$headline_number[i] == 'headline_accuracy_4_w2') {
        janHead$type[i] <- 'ProD Fake'
        janHead$proD[i] <- 1
        janHead$fake[i] <- 1
    } else if (janHead$headline_number[i] == 'headline_accuracy_7_w2' | janHead$headline_number[i] == 'headline_accuracy_8_w2') {
        janHead$type[i] <- 'ProR Fake'
        janHead$proR[i] <- 1
        janHead$fake[i] <- 1
    }
}

#make a treatment indicator
octHead$treat <- 0
for (i in 1:nrow(octHead)) {
    if (octHead$headline_number[i] == 'headline_accuracy_1_w2') {
        octHead$treat[i] <- octHead$s_accuracy_donald_trump_caught[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_2_w2') {
        octHead$treat[i] <- octHead$s_accuracy_franklin_graham[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_3_w2') {
        octHead$treat[i] <- octHead$s_accuracy_vp_mike_pence[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_4_w2') {
        octHead$treat[i] <- octHead$s_accuracy_vice_president_pence[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_5_w2') {
        octHead$treat[i] <- octHead$s_accuracy_soros_money_behind[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_6_w2') {
        octHead$treat[i] <- octHead$s_accuracy_kavanaugh_accuser[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_7_w2') {
        octHead$treat[i] <- octHead$s_accuracy_fbi_agent_who[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_8_w2') {
        octHead$treat[i] <- octHead$s_accuracy_lisa_page[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_9_w2') {
        octHead$treat[i] <- octHead$s_accuracy_a_series1[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_10_w2') {
        octHead$treat[i] <- octHead$s_accuracy_a_border_patrol[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_11_w2') {
        octHead$treat[i] <- octHead$s_accuracy_detention_of_migrant[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_12_w2') {
        octHead$treat[i] <- octHead$s_accuracy_and_now1[i]
    } else if (octHead$headline_number[i] == '#headline_accuracy_13_w2') {
        octHead$treat[i] <- octHead$s_accuracy_google_employee[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_14_w2') {
        octHead$treat[i] <- octHead$s_accuracy_feds_said_alleged[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_15_w2') {
        octHead$treat[i] <- octHead$s_accuracy_small_busisness_opt[i]
    } else if (octHead$headline_number[i] == 'headline_accuracy_16_w2') {
        octHead$treat[i] <- octHead$s_accuracy_economy_adds_more[i]
    }
}

janHead$treat <- 0
for (i in 1:nrow(janHead)) {
    if (janHead$headline_number[i] == 'headline_accuracy_1_w2') {
        janHead$treat[i] <- janHead$s_accuracy_donald_trump_caught[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_2_w2') {
        janHead$treat[i] <- janHead$s_accuracy_franklin_graham[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_3_w2') {
        janHead$treat[i] <- janHead$s_accuracy_vp_mike_pence[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_4_w2') {
        janHead$treat[i] <- janHead$s_accuracy_vice_president_pence[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_5_w2') {
        janHead$treat[i] <- janHead$s_accuracy_soros_money_behind[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_6_w2') {
        janHead$treat[i] <- janHead$s_accuracy_kavanaugh_accuser[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_7_w2') {
        janHead$treat[i] <- janHead$s_accuracy_fbi_agent_who[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_8_w2') {
        janHead$treat[i] <- janHead$s_accuracy_lisa_page[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_9_w2') {
        janHead$treat[i] <- janHead$s_accuracy_a_series1[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_10_w2') {
        janHead$treat[i] <- janHead$s_accuracy_a_border_patrol[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_11_w2') {
        janHead$treat[i] <- janHead$s_accuracy_detention_of_migrant[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_12_w2') {
        janHead$treat[i] <- janHead$s_accuracy_and_now1[i]
    } else if (janHead$headline_number[i] == '#headline_accuracy_13_w2') {
        janHead$treat[i] <- janHead$s_accuracy_google_employee[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_14_w2') {
        janHead$treat[i] <- janHead$s_accuracy_feds_said_alleged[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_15_w2') {
        janHead$treat[i] <- janHead$s_accuracy_small_busisness_opt[i]
    } else if (janHead$headline_number[i] == 'headline_accuracy_16_w2') {
        janHead$treat[i] <- janHead$s_accuracy_economy_adds_more[i]
    }
}



#Brainstorm
for (i in 1:ncol(octHead)) {
print(c(colnames(octHead[i]),i))
}

for (i in 1:ncol(janHead)) {
print(c(colnames(janHead[i]), i))
}

#sort(colnames(octHead[, c(1, 2, 20:23, 25, 52, 127, 129:133, 167:171, 174, 232, 289, 290, 292:299, 301:303, 305, 318, 329, 339,  643:653)]))== sort(colnames(janHead[, c(2, 3, 21:24, 27, 50, 139, 143:147, 169, 176:180, 203, 237:241, 246:248, 250:252, 254, 267, 278, 288, 488, 489,  537, 555:564)]))


octHead <- octHead[, c(1, 2, 20:23, 52, 127, 129:133, 167:171, 232, 289, 290, 292:299, 301:303, 305, 318, 329, 339, 643:653)]

janHead <- janHead[, c(2, 3, 21:24,  50, 139, 143:147, 176:180, 203, 237:241, 246:248, 250:252, 254, 267, 278, 288, 488, 489,  537, 555:564)]


octHead$media_trust <- as.character(octHead$media_trust)
octHead$media_trust[octHead$media_trust == 'None at all'] <- 1
octHead$media_trust[octHead$media_trust == 'Not very much'] <- 2
octHead$media_trust[octHead$media_trust == 'A fair amount'] <- 3
octHead$media_trust[octHead$media_trust == 'A great deal'] <- 4


octHead$media_trust_w2 <- as.character(octHead$media_trust_w2)
octHead$media_trust_w2[octHead$media_trust_w2 == 'None at all'] <- 1
octHead$media_trust_w2[octHead$media_trust_w2 == 'Not very much'] <- 2
octHead$media_trust_w2[octHead$media_trust_w2 == 'A fair amount'] <- 3
octHead$media_trust_w2[octHead$media_trust_w2 == 'A great deal'] <- 4




#headline_accuracy_1_w2 -- Trump Caught --"s_accuracy_donald_trump_caught" 


#headline_accuracy_2_w2 -- Franklin Graham -- "s_accuracy_franklin_graham"


#headline_accuracy_3_w2 -- VP Mike Pence -- "s_accuracy_vp_mike_pence"


#headline_accuracy_4_w2 -- Vice President Pence -- "s_accuracy_vice_president_pence"


#headline_accuracy_5_w2 -- Soros Money -- "s_accuracy_soros_money_behind" 


#headline_accuracy_6_w2 -- Kavanaugh Accuser -- "s_accuracy_kavanaugh_accuser"

#headline_accuracy_7_w2 -- FBI agent who -- "s_accuracy_fbi_agent_who" 


#headline_accuracy_8_w2 -- Lisa Page -- "s_accuracy_lisa_page"


#headline_accuracy_9_w2 -- A series -- "s_accuracy_a_series1" 


#headline_accuracy_10_w2 -- A border patrol -- "s_accuracy_a_border_patrol"


#headline_accuracy_11_w2 -- Detention of Migrant -- "s_accuracy_detention_of_migrant" 


#headline_accuracy_12_w2 -- and now1 -- "s_accuracy_and_now1"


#headline_accuracy_13_w2 -- Google employees -- "s_accuracy_google_employees" 


#headline_accuracy_14_w2 -- Feds said alleged -- "s_accuracy_feds_said_alleged"


#headline_accuracy_15_w2 -- Accuracy small bus -- "s_accuracy_small_busisness_opt" 


#headline_accuracy_16_w2 -- Economy adds more -- "s_accuracy_economy_adds_more"

setwd("C:/Users/dl0ck/Dropbox/EBMA_experiments/October Wave/Data_1018")
write.csv(octHead, 'oct_HL.csv')
setwd("C:/Users/dl0ck/Dropbox/EBMA_experiments/January Wave/Data_0219")
write.csv(janHead, 'jan_HL.csv')

