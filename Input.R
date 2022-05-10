# load library
library(tidyverse)
library(readr)
library(readxl)
library(MASS)
library(lme4)

# read data
df <- read_excel(file.choose())
df <- df[1:6]
View(df)

# data inspection
str(df)
dim(df)

# get summary statistics
summary(df)

# data transformation
df <- df %>%
  mutate(sd = sd(df$score))

res <- model.matrix(~genre, data=df)

my_sum <- df %>%
  group_by(text_coverage) %>%
  summarise( 
    n=n(),
    mean=mean(score),
    sd=sd(score)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# plot
p1 <-
ggplot(data=df) +
  geom_bar(aes(x=text_coverage, fill = genre), position = "dodge")+
  ylim(0,150)+
  theme_light()+
  labs(
    y="genre", 
    x="text coverage",
    title ="Text coverage of 6 have least amount of genre"
    )

p2 <-
ggplot(data=df) +
  geom_bar(aes(x=genre, y = text_coverage), stat = "identity", fill="lightgrey")+
  theme_light()+
  labs(
    y="text coverage", 
    x="genre",
    title ="Two genre have the same amount of total text coverage"
  )

p3 <-
ggplot(data=my_sum) +
  geom_bar(aes(x=text_coverage, y = mean), stat = "identity", fill="lightblue")+
  geom_errorbar(aes(x=text_coverage, ymin = mean-ic, ymax = mean+ic), width=0.4, color="orange")+
  theme_light()+
  labs(
    y="score", 
    x="text coverage",
    title ="Text coverage of 6 have lowest score"
  )

# LM model
lm <- lm(score ~ text_coverage + genre + exam + occurrence, data = df)
coef(lm)
confint(lm)
summary(lm)
summary(lm)$coef

# LMM model
lmer1 <- lmer(score ~ exam + occurrence + (1|text_coverage), data = df)
summary(lmer1)
ranef(lmer1)$text_coverage 
coef(lmer1)$text_coverage 

lmer2 <- lmer(score ~ text_coverage + occurrence + (1|exam), data = df)
summary(lmer2)
ranef(lmer2)$exam 
coef(lmer2)$exam 

lmer3 <- lmer(score ~ text_coverage + exam + (1|occurrence), data = df)
summary(lmer3)
ranef(lmer3)$occurrence 
coef(lmer3)$occurrence

lmer4 <- lmer(score ~ exam + occurrence + (1 + exam|text_coverage), data = df)
summary(lmer4)
ranef(lmer4)$text_coverage 
coef(lmer4)$text_coverage

lmer5 <- lmer(score ~ exam + occurrence + (1 + occurrence|text_coverage), data = df)
summary(lmer5)
ranef(lmer5)$text_coverage 
coef(lmer5)

# second part
df1 <- read_excel("/Users/LuHongTu/Desktop/0-Tuo/23-Linear Mixed Model/dataset.xlsx", 
                  sheet = "exam 1_score (n=150)",
                  skip = 2)

df2 <- read_excel("/Users/LuHongTu/Desktop/0-Tuo/23-Linear Mixed Model/dataset.xlsx", 
                  sheet = "exam 2_score ",
                  skip = 2)
View(df1)
View(df2)

# exam 1  - narrative -  occurrence 1
lmerdf1naoc1item3 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                + (1 + `Stu1ent I1`|`Item3`), data = df1)
summary(lmerdf1naoc1item3)

lmerdf1naoc1item5 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                          + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                          + (1 + `Stu1ent I1`|`Item5...6`), data = df1)
summary(lmerdf1naoc1item5)

lmerdf1naoc1item11 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                          + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                          + (1 + `Stu1ent I1`|`Item11...7`), data = df1)
summary(lmerdf1naoc1item11)

lmerdf1naoc1item14 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                          + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                          + (1 + `Stu1ent I1`|`Item14`), data = df1)
summary(lmerdf1naoc1item14)

lmerdf1naoc1item17 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                          + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                          + (1 + `Stu1ent I1`|`Item17`), data = df1)
summary(lmerdf1naoc1item17)

lmerdf1naoc1item25 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                           + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                           + (1 + `Stu1ent I1`|`Item25`), data = df1)
summary(lmerdf1naoc1item25)

lmerdf1naoc1item26 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                           + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                           + (1 + `Stu1ent I1`|`Item26`), data = df1)
summary(lmerdf1naoc1item26)

lmerdf1naoc1item29 <- lmer(`SUM...13` ~ `Item3` + `Item5...6` + `Item11...7`  
                           + `Item14` +  `Item17` +  `Item25`  + `Item26`  + `Item29` 
                           + (1 + `Stu1ent I1`|`Item29`), data = df1)
summary(lmerdf1naoc1item29)

# exam 1  - narrative -  occurrence 2
lmerdf1naoc2item1 <- lmer(`SUM...13` ~ `Item1` + `Item9...3` + `Item33...4` 
                     + (1 + `Stu1ent I1`|`Item1`), data = df1)
summary(lmerdf1naoc2item1)

lmerdf1naoc2item9 <- lmer(`SUM...13` ~ `Item1` + `Item9...3` + `Item33...4` 
                     + (1 + `Stu1ent I1`|`Item9...3`), data = df1)
summary(lmerdf1naoc2item9)

lmerdf1naoc2item33 <- lmer(`SUM...13` ~ `Item1` + `Item9...3` + `Item33...4` 
                     + (1 + `Stu1ent I1`|`Item33...4` ), data = df1)
summary(lmerdf1naoc2item33)


# exam 1  - expository -  occurrence 1
lmerdf1exoc1item6 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                          + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                          + (1 + `Student ID...15`|`Item6...19`), data = df1)
summary(lmerdf1exoc1item6)

lmerdf1exoc1item10 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item10`), data = df1)
summary(lmerdf1exoc1item10)

lmerdf1exoc1item12 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item12`), data = df1)
summary(lmerdf1exoc1item12)

lmerdf1exoc1item15 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item15...22`), data = df1)
summary(lmerdf1exoc1item15)

lmerdf1exoc1item18 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item18`), data = df1)
summary(lmerdf1exoc1item18)

lmerdf1exoc1item24 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item24`), data = df1)
summary(lmerdf1exoc1item24)

lmerdf1exoc1item30 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item30`), data = df1)
summary(lmerdf1exoc1item30)

lmerdf1exoc1item32 <- lmer(`SUM...27` ~ `Item6...19` + `Item10` + `Item12`  
                           + `Item15...22` +  `Item18` +  `Item24`  + `Item30`  + `Item32` 
                           + (1 + `Student ID...15`|`Item32`), data = df1)
summary(lmerdf1exoc1item32)


# exam 1  - expository -  occurrence 2
lmerdf1exoc2item7 <- lmer(`SUM...27` ~ `Item7` + `Item16` + `Item31...18` 
                          + (1 + `Student ID...15`|`Item7`), data = df1)
summary(lmerdf1exoc2item7)

lmerdf1exoc2item16 <- lmer(`SUM...27` ~ `Item7` + `Item16` + `Item31...18` 
                          + (1 + `Student ID...15`|`Item16`), data = df1)
summary(lmerdf1exoc2item16)

lmerdf1exoc2item31 <- lmer(`SUM...27` ~ `Item7` + `Item16` + `Item31...18` 
                          + (1 + `Student ID...15`|`Item31...18`), data = df1)
summary(lmerdf1exoc2item31)

# exam 2  - narrative -  occurrence 1
lmerdf2naoc1item4 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item4`), data = df2)
summary(lmerdf2naoc1item4)

lmerdf2naoc1item12 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item12`), data = df2)
summary(lmerdf2naoc1item4)

lmerdf2naoc1item14 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item14`), data = df2)
summary(lmerdf2naoc1item14)

lmerdf2naoc1item16 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item16`), data = df2)
summary(lmerdf2naoc1item16)

lmerdf2naoc1item18 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item18`), data = df2)
summary(lmerdf2naoc1item18)

lmerdf2naoc1item23 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item23`), data = df2)
summary(lmerdf2naoc1item23)

lmerdf2naoc1item26 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                          + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                          + (1 + `Stu1ent I1`|`Item26`), data = df2)
summary(lmerdf2naoc1item26)

lmerdf2naoc1item27 <- lmer(`SUM...13` ~ `Item4` + `Item12` + `Item14`  
                           + `Item16` +  `Item18` +  `Item23`  + `Item26`  + `Item27` 
                           + (1 + `Stu1ent I1`|`Item27`), data = df2)
summary(lmerdf2naoc1item27)



# exam 2  - narrative -  occurrence 2
lmerdf2naoc2item1 <- lmer(`SUM...13` ~ `Item1` + `Item29` + `Item32` 
                          + (1 + `Stu1ent I1`|`Item1`), data = df2)
summary(lmerdf1naoc2item1)

lmerdf2naoc2item29 <- lmer(`SUM...13` ~ `Item1` + `Item29` + `Item32` 
                          + (1 + `Stu1ent I1`|`Item29`), data = df2)
summary(lmerdf2naoc2item29)

lmerdf2naoc2item32 <- lmer(`SUM...13` ~ `Item1` + `Item29` + `Item32` 
                          + (1 + `Stu1ent I1`|`Item32`), data = df2)
summary(lmerdf2naoc2item32)

# exam 2  - expository -  occurrence 1
lmerdf2exoc1item5 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item11` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item5`), data = df2)
summary(lmerdf2exoc1item5)

lmerdf2exoc1item6 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item11` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item6`), data = df2)
summary(lmerdf2exoc1item6)

lmerdf2exoc1item9 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item1` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item9`), data = df2)
summary(lmerdf2exoc1item9)

lmerdf2exoc1item11 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item1` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item11`), data = df2)
summary(lmerdf2exoc1item11)

lmerdf2exoc1item13 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item1` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item13`), data = df2)
summary(lmerdf2exoc1item13)

lmerdf2exoc1item15 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item1` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item15`), data = df2)
summary(lmerdf2exoc1item15)

lmerdf2exoc1item19 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item1` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item19`), data = df2)
summary(lmerdf2exoc1item19)

lmerdf2exoc1item22 <- lmer(`SUM...28` ~ `Item5` + `Item6` + `Item9`  
                          + `Item1` +  `Item13` +  `Item15`  + `Item19`  + `Item22` 
                          + (1 + `Student ID`|`Item22`), data = df2)
summary(lmerdf2exoc1item22)


# exam 2  - expository -  occurrence 2
lmerdf2exoc2item8 <- lmer(`SUM...28` ~ `Item8` + `Item31` + `Item33` 
                          + (1 + `Student ID`|`Item8`), data = df2)
summary(lmerdf21exoc2item8)

lmerdf2exoc2item31 <- lmer(`SUM...28` ~ `Item8` + `Item31` + `Item33` 
                          + (1 + `Student ID`|`Item31`), data = df2)
summary(lmerdf21exoc2item31)

lmerdf2exoc2item33 <- lmer(`SUM...28` ~ `Item8` + `Item31` + `Item33` 
                          + (1 + `Student ID`|`Item33`), data = df2)
summary(lmerdf21exoc2item33)
