---
title: "EDA HDV"
output:
  html_document: 
    keep_md: true
  word_document: default
date: "2025-11-25"
---





``` r
setwd("~/Documents/GW Time/Fall 2025/Linear Reg/")
```



``` r
load("~/Documents/GW Time/Fall 2025/Linear Reg/hypoxia.Rdata")
head(hypoxia)
```

```
## # A tibble: 6 × 36
##     Age Female  Race   BMI Sleeptime `Min Sao2`   AHI Smoking Diabetes Hyper
##   <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl> <dbl>   <dbl>    <dbl> <dbl>
## 1  29.9      1     2  44.5       0.9         90     1       0        0     0
## 2  52.2      1     2  40.6       0           94     3       1        1     1
## 3  37.3      1     1  61.7       1.9         76     1       0        0     0
## 4  52.2      1     2  40.2      17           52     4       1        0     0
## 5  26.7      1     2  57.6       0           95     3       0        0     1
## 6  54.6      1     2  36        13           51     3       0        1     1
## # ℹ 26 more variables: CAD <dbl>, `Preop AntiHyper Med` <dbl>, CPAP <dbl>,
## #   `Type Surg` <dbl>, `Duration of Surg` <dbl>, `Duration of Surg1` <dbl>,
## #   `Duration of Surg2` <dbl>, `TWA MAP` <dbl>, `TWA MAP1` <dbl>,
## #   `TWA MAP2` <dbl>, `TWA HR` <dbl>, `TWA HR1` <dbl>, `TWA HR2` <dbl>,
## #   `Intraop AntiHyper Med` <dbl>, Vasopressor <dbl>, Ephedrine <dbl>,
## #   `Ephedrine Amt` <dbl>, Epinephrine <dbl>, `Epinephrine Amt` <dbl>,
## #   Phenylephrine <dbl>, `Phenylephrine Amt` <dbl>, MAC <dbl>, …
```

``` r
hypoxia <- hypoxia[hypoxia$`Type Surg` != 4,]
hypoxia <- hypoxia[hypoxia$BMI != 23.4,]
```





``` r
# make a default theme for the boxplots 
bxplt_theme <-    theme_minimal(base_size = 14)+ theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Centered, bold title    
      axis.text = element_text(size = 12, color = "black"),
      legend.position = "none",
      strip.background = element_rect(fill = "gray90", color = "black"),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(color = "gray80", linetype = "dashed"), 
      panel.grid.minor = element_blank()
    ) 

# make a default theme for the barplots
brplt_theme <- theme_minimal(base_size = 14) +theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Centered, bold title    
      axis.text = element_text(size = 12, color = "black"),
      legend.position = "top",
      panel.grid.major = element_line(color = "gray80", linetype = "dashed"), 
      panel.grid.minor = element_blank()
)
```



``` r
# contingency table for type of surgery
type_counts <- xtabs(~ `Type Surg` , data = hypoxia)

# reshape into long format where we see which diseases are present at each type of surgery
outcome_counts <- hypoxia %>%   
  dplyr::select(`Type Surg`, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Condition", value = "Diagnosis", -c(`Type Surg`))  %>%
  filter(Diagnosis == 1)

com_counts <- outcome_counts %>%  
group_by(Condition, `Type Surg`) %>%
  count(`Type Surg`, Condition)
com_counts
```

```
## # A tibble: 9 × 3
## # Groups:   Condition, Type Surg [9]
##   Condition `Type Surg`     n
##   <chr>           <dbl> <int>
## 1 CAD                 1    17
## 2 CAD                 2     9
## 3 Diabetes            1    80
## 4 Diabetes            2    16
## 5 Hyper               1   159
## 6 Hyper               2    35
## 7 Hyper               3     2
## 8 Smoking             1   119
## 9 Smoking             2    21
```


``` r
com_props <- com_counts %>%  
group_by(Condition, `Type Surg`) %>%  
group_by(`Condition`) %>%  
mutate(Proportion = n / sum(n))

com_props$`Type Surg` <- as.factor(com_props$`Type Surg`)

png("finalfig1.png", width = 1920, height = 1080, res = 300)

ggplot(com_props, aes(fill=Condition, x=`Type Surg`, y = Proportion)) + # might be better with stacked barplot? 
  geom_bar(position = "fill", stat="identity", width = 0.6, color = "black")+
  scale_y_continuous(labels = scales::percent)+ 
  brplt_theme +
  labs(
    title = "Patient Comorbidities by Surgery Type", 
    x = "Surgery Type",
    y = "Percentage of Patients",
    fill = "Disease"
  )
 dev.off()  
```

```
## quartz_off_screen 
##                 2
```


``` r
# contingency table for gender
gender_counts <- xtabs(~ Female+`Type Surg` , data = hypoxia)

# convert counts to proportions within each surgery type
gender_prop <- prop.table(gender_counts, margin = 2)*100

# convert contingency table to df
gender_df <- as.data.frame(gender_prop)

# rename columns 
colnames(gender_df) <- c("Female", "TypeSurg", "Percent")

png("finalfig2.png", width = 1920, height = 1080, res = 300)
ggplot(gender_df, aes(x = TypeSurg, y = Percent, fill = Female)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  scale_fill_manual(values = c("steelblue", "lightpink"),
                    labels = c("Males", "Females"),
                    name = "Gender") +
  labs(
    title = "Gender Makeup for Each Type of Surgery",
    x = "Type of Surgery",
    y = "Percentage of Each Gender"
  ) + brplt_theme
dev.off()
```

```
## quartz_off_screen 
##                 2
```

``` r
# # barplot to show makeup of gender for each type of surgery
# barplot(gender_prop, col = c("steelblue", "lightpink"), beside = TRUE, main = "Count of Gender for Each Type of Surgery", xlab = "Type of Surgery", ylab = "Percentage of Each Gender")
# legend(
#   "topleft",
#   legend = c("Males", "Females"),
#   fill = c("steelblue", "lightpink")
# ) + brplt_theme

# contingency table for race
race_counts <- xtabs(~ Race+ `Type Surg` , data = hypoxia)

# convert counts to proportions within each surgery type
race_prop <- prop.table(race_counts, margin = 2)*100

# convert contingency table to df
race_df <- as.data.frame(race_prop)

# rename columns
colnames(race_df) <- c("Race", "TypeSurg", "Percent")

png("finalfig3.png", width = 1920, height = 1080, res = 300)
ggplot(race_df, aes(x = TypeSurg, y = Percent, fill = Race)) +
  geom_col(position = position_dodge(width = 0.9),color = "black") +
  scale_fill_manual(values = c("darkgreen", "lightgreen", "green"),
                    name = "Race", labels = c("African American","Caucasian", "Other")) +
  labs(
    title = "Race Makeup for Each Type of Surgery",
    x = "Type of Surgery",
    y = "Percentage of Each Race"
  ) + brplt_theme
dev.off()
```

```
## quartz_off_screen 
##                 2
```

``` r
# # bar plot to show makeup of race for each type of surgery
# barplot(race_prop, col = c("darkgreen", "lightgreen","green"), beside = TRUE, legend.text = c("African American", "Caucasian", "Other"), main = "Count of Race for Each Type of Surgery", xlab = "Type of Surgery", ylab = "Percentage of Each Race") + brplt_theme

# boxplot to compare duration of surgery between each type of surgery
boxplot(`Duration of Surg` ~ `Type Surg`, data = hypoxia, main = "Surgery Duration by Type", xlab = "Type of Surgery", ylab = "Duration of Surgery (hours)", col = c("lightblue", "steelblue","blue","darkblue"))
legend(
  "top",  
  horiz = TRUE,
  legend = c(1,2,3,4),       
  fill = c("lightblue", "steelblue", "blue", "darkblue"),
  title = "Type of Surgery"
) + bxplt_theme
```

![](EDA-HDV-Final_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```
## NULL
```




``` r
# bmi vs duration of surgery
hypoxia <- hypoxia %>%
  mutate(
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal Weight",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 & BMI < 35 ~ "Class 1 Obesity",
      BMI >= 35 & BMI < 40 ~ "Class 2 Obesity",
      BMI >= 40 ~ "Class 3 Obesity",
      TRUE ~ "UNKNOWN"
    )
  )
png("finalfig4.png", width = 1920, height = 1080, res = 300)
hypoxia$BMI_Category <- factor(hypoxia$BMI_Category,levels = c("Underweight", "Normal Weight", "Overweight","Class 1 Obesity", "Class 2 Obesity", "Class 3 Obesity"))
ggplot(hypoxia, aes(x = BMI_Category, y = `Duration of Surg`, fill = BMI_Category)) +
  geom_boxplot(color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparing BMI vs Duration of Surgery",
    x = "BMI Category",
    y = "Duration of Surgery (hours)"
  ) +
  bxplt_theme
dev.off()
```

```
## quartz_off_screen 
##                 2
```





# Seble Plots

``` r
# select the variables we care about
#in clude CPAP + AHI. well maybe not. AHI

data <- dplyr::select(hypoxia, 1:4,7:11,13:23, 25:34)

head(data)
```

```
## # A tibble: 6 × 30
##     Age Female  Race   BMI   AHI Smoking Diabetes Hyper   CAD  CPAP `Type Surg`
##   <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>    <dbl> <dbl> <dbl> <dbl>       <dbl>
## 1  29.9      1     2  44.5     1       0        0     0     0     0           2
## 2  52.2      1     2  40.6     3       1        1     1     0     0           1
## 3  37.3      1     1  61.7     1       0        0     0     0     0           1
## 4  52.2      1     2  40.2     4       1        0     0     0     1           1
## 5  26.7      1     2  57.6     3       0        0     1     0     1           1
## 6  54.6      1     2  36       3       0        1     1     0     1           2
## # ℹ 19 more variables: `Duration of Surg` <dbl>, `Duration of Surg1` <dbl>,
## #   `Duration of Surg2` <dbl>, `TWA MAP` <dbl>, `TWA MAP1` <dbl>,
## #   `TWA MAP2` <dbl>, `TWA HR` <dbl>, `TWA HR1` <dbl>, `TWA HR2` <dbl>,
## #   Vasopressor <dbl>, Ephedrine <dbl>, `Ephedrine Amt` <dbl>,
## #   Epinephrine <dbl>, `Epinephrine Amt` <dbl>, Phenylephrine <dbl>,
## #   `Phenylephrine Amt` <dbl>, MAC <dbl>, `Propofol Induction` <dbl>,
## #   `IV Morphine Eq` <dbl>
```

``` r
# make a correlation plot

cor_matrix <- cor(data, use = "complete.obs")

test = cor.mtest(data)
```



``` r
png("finalfig5.png", width = 2250, height = 1550, res = 300)
corrplot(cor_matrix,                                           
order = "FPC",                                            
p.mat = test$p,                                            
sig.level = c(0.001, 0.01, 0.05),                                   
insig = 'label_sig',                                    
pch.cex = 0.5,                                            
type = "upper",                                   
diag = FALSE,                                   
method="color")
dev.off()
```

```
## quartz_off_screen 
##                 2
```


``` r
data$CPAP <- dplyr::recode(as.factor(data$CPAP), "0" = "No CPAP", "1" = "Use CPAP")

data$Smoking <-as.factor(data$Smoking)

data$Diabetes <-as.factor(data$Diabetes)

data$Hyper <-as.factor(data$Hyper)

data$CAD <-as.factor(data$CAD)

data$Phenylephrine <-dplyr::recode(as.factor(data$Phenylephrine), "0" = "No Phenylephrine", "1" = "Use Phenylephrine")


data$Epinephrine <-dplyr::recode(as.factor(data$Epinephrine), "0" = "No Epinephrine", "1" = "Use Epinephrine")

data$Female <-dplyr::recode(as.factor(data$Female), "0" = "Male", "1" = "Female")

data$Race <-dplyr::recode(as.factor(data$Race), "1" = "African American", "2" = "White", "3" = "Other")
```



``` r
# comorbidities x Epinephrine graph version 3 - grouped prop graph comaring pos/neg for each comorbidity and epinephrine use. yay.

epi_com_counts_whole <- data %>%   
  dplyr::select(Epinephrine, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Usage", value = "Diagnosis", -c(Epinephrine)) 

epi_com_prop_whole <- epi_com_counts_whole %>%  
group_by(Usage, Epinephrine, Diagnosis) %>% 
summarise(Frequency = n(), .groups = "drop") %>%  
group_by(Usage) %>%  
mutate(Proportion = Frequency / sum(Frequency)) 

epi_com_prop_whole
```

```
## # A tibble: 14 × 5
## # Groups:   Usage [4]
##    Usage    Epinephrine     Diagnosis Frequency Proportion
##    <chr>    <fct>           <chr>         <int>      <dbl>
##  1 CAD      No Epinephrine  0               251    0.903  
##  2 CAD      No Epinephrine  1                25    0.0899 
##  3 CAD      Use Epinephrine 0                 1    0.00360
##  4 CAD      Use Epinephrine 1                 1    0.00360
##  5 Diabetes No Epinephrine  0               181    0.651  
##  6 Diabetes No Epinephrine  1                95    0.342  
##  7 Diabetes Use Epinephrine 0                 1    0.00360
##  8 Diabetes Use Epinephrine 1                 1    0.00360
##  9 Hyper    No Epinephrine  0                82    0.295  
## 10 Hyper    No Epinephrine  1               194    0.698  
## 11 Hyper    Use Epinephrine 1                 2    0.00719
## 12 Smoking  No Epinephrine  0               138    0.496  
## 13 Smoking  No Epinephrine  1               138    0.496  
## 14 Smoking  Use Epinephrine 1                 2    0.00719
```

``` r
# grouped bar chart of comorbidities 
#tiff("epinephrine_stacked.tiff", width = 2500, height = 1500, res = 300)
epi_com_prop_whole$Epinephrine <-dplyr::recode(as.factor(epi_com_prop_whole$Epinephrine), "No Epinephrine"= "No Usage",  "Use Epinephrine" = "Usage")
png("finalfig6.png", width = 1920, height = 1350, res = 300)
ggplot(epi_com_prop_whole, aes(fill=Diagnosis, x=Epinephrine, y = Proportion)) + # might be better with stacked barplot? 
  geom_bar(position = "fill", stat="identity", width = 0.6, color = "black")+ scale_fill_brewer(palette = "Set2", name = "Diagnosis", labels = c("Not Present", "Present"))+
  scale_y_continuous(labels = scales::percent)+ facet_wrap(~Usage)+
  brplt_theme +
  labs(
    title = "Epinephrine Use in Surgery\n Across Patient Comorbidities", 
    x = "Epinephrine Use",
    y = "Percentage of Patients",
    fill = "Diagnosis"
  )
dev.off()
```

```
## quartz_off_screen 
##                 2
```


``` r
# comorbidities x Phenylephrine graph version 3 was done - grouped prop graph comaring pos/neg for each comorbidity and epinephrine use. yay.
# basically no real difference this is consistent with correlation plot

ph_com_counts_whole <- data %>%   
  dplyr::select(Phenylephrine, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Usage", value = "Diagnosis", -c(Phenylephrine)) 

ph_com_prop_whole <- ph_com_counts_whole %>%  
group_by(Usage, Phenylephrine, Diagnosis) %>% 
summarise(Frequency = n(), .groups = "drop") %>%  
group_by(Usage) %>%  
mutate(Proportion = Frequency / sum(Frequency))
```



``` r
# grouped bar chart of comorbidities 
#tiff("phenylephrine_stacked.tiff", width = 2500, height = 1500, res = 300)
ph_com_prop_whole$Phenylephrine <-dplyr::recode(as.factor(ph_com_prop_whole$Phenylephrine), "No Phenylephrine"= "No Usage",  "Use Phenylephrine" = "Usage")

png("finalfig7.png", width = 1920, height = 1350, res = 300)
ggplot(ph_com_prop_whole, aes(fill=Diagnosis, x=Phenylephrine, y = Proportion)) + # might be better with stacked barplot? 
  geom_bar(position = "fill", stat="identity", width = 0.6, color = "black")+ scale_fill_brewer(palette = "Set2", name = "Diagnosis", labels = c("Not Present", "Present"))+ 
  scale_y_continuous(labels = scales::percent)+ facet_wrap(~Usage)+
  brplt_theme +
  labs(
    title = "Phenylephrine Use in Surgery\n Across Patient Comorbidities", 
    x = "Phenylephrine Use",
    y = "Percentage of Patients",
    fill = "Diagnosis"
  )
dev.off()
```

```
## quartz_off_screen 
##                 2
```


``` r
# no instead we need cpap vs epi/phenyl use okay

cpap_com_counts_whole <- data %>%   
  dplyr::select(CPAP, Phenylephrine,Epinephrine) %>%  
  gather(key = "Usage", value = "Diagnosis", -c(CPAP)) 
```

```
## Warning: attributes are not identical across measure variables; they will be
## dropped
```

``` r
cpap_com_prop_whole <- cpap_com_counts_whole %>%  
group_by(Usage, CPAP, Diagnosis) %>% 
summarise(Frequency = n(), .groups = "drop") %>%  
group_by(Usage) %>%  
mutate(Proportion = Frequency / sum(Frequency))
```



``` r
cpap_com_prop_whole$Usage <- as.factor(cpap_com_prop_whole$Usage)
cpap_com_prop_whole$Diagnosis <- dplyr::recode(as.factor(cpap_com_prop_whole$Diagnosis),
                                               "No Epinephrine" = "No Usage", "Use Epinephrine" = "Usage",
                                               "No Phenylephrine" = "No Usage", "Use Phenylephrine" = "Usage")
cpap_com_prop_whole
```

```
## # A tibble: 7 × 5
## # Groups:   Usage [2]
##   Usage         CPAP     Diagnosis Frequency Proportion
##   <fct>         <fct>    <fct>         <int>      <dbl>
## 1 Epinephrine   No CPAP  No Usage        102    0.367  
## 2 Epinephrine   Use CPAP No Usage        174    0.626  
## 3 Epinephrine   Use CPAP Usage             2    0.00719
## 4 Phenylephrine No CPAP  No Usage         65    0.234  
## 5 Phenylephrine No CPAP  Usage            37    0.133  
## 6 Phenylephrine Use CPAP No Usage        105    0.378  
## 7 Phenylephrine Use CPAP Usage            71    0.255
```

``` r
# grouped bar chart of comorbidities 
#png("finalfig8.png", width = 1920, height = 1350, res = 300)
ggplot(cpap_com_prop_whole, aes(fill=CPAP, x=Diagnosis, y = Proportion)) + # might be better with stacked barplot? 
  facet_wrap(~Usage, nrow=2)+
  geom_bar(position = "fill", stat="identity", width = 0.6, color = "black")+ scale_fill_brewer(palette = "Set2", name = "Diagnosis", labels = c("No CPAP", "Use CPAP "))+ 
  scale_y_continuous(labels = scales::percent)+
  brplt_theme +
  labs(
    title = "Vascoconstrictors Across \n  CPAP Usage", 
    x = "Vasoconstrictor Use",
    y = "Percentage of Patients",
    fill = ""
  )
```

![](EDA-HDV-Final_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
dev.off()
```

```
## null device 
##           1
```



``` r
# heart rate vs comobidities vers 2

hr_outcome <- hypoxia %>%   
  dplyr::select(`TWA HR`, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Condition", value = "Diagnosis", -c(`TWA HR`))

hr_outcome$Diagnosis <- factor(hr_outcome$Diagnosis,levels = c(0,1))

#png("finalfig9.png", width = 1920, height = 1080, res = 300)
ggplot(hr_outcome, aes(x = Diagnosis, y = `TWA HR`, fill = Diagnosis)) +  
geom_boxplot()+bxplt_theme+ coord_flip() +
  scale_fill_brewer(palette = "Set2", direction = 1, labels = c("Not Present", "Present"))+
  labs(title = "Heart Rate During Surgery Across Comorbidities",
       x = "", y = "Surgical Heart Rate (beats/min)")+ 
  theme(
        legend.position = "top",
        strip.background = element_blank(),
        ) + facet_wrap(~Condition) 
```

```
## Warning: Removed 308 rows containing non-finite outside the scale range
## (`stat_boxplot()`).
```

![](EDA-HDV-Final_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

``` r
dev.off()  
```

```
## null device 
##           1
```



``` r
# Dur vs BMI


p1<- ggplot(hypoxia, aes(x = `BMI`, y = `Duration of Surg`)) +  # Define data and aesthetics  
  geom_point(alpha = 0.6, size= 3, shape = 19, aes(color = BMI_Category)) +  
 theme_minimal(base_size = 12) +
  geom_smooth(method = "lm", se=FALSE)+  
  theme(
     plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Centered, bold title    
    axis.text = element_text(size = 12, color = "black"), legend.position = "bottom",
     strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
      panel.grid.major = element_line(color = "gray80", linetype = "dashed"), 
      panel.grid.minor = element_blank(),
    plot.caption = element_text(size=10, color="red", face="italic")
     
  )+scale_color_brewer(palette = "Set2")+
  labs(
    title = "Duration of Surgery vs BMI", 
    x = "BMI Score",
    y = "Duration of Surgery (hours)",
    caption = "BMI Categories based on CDC guidelines"
  )
#png("finalfig11.png", width = 1920, height = 1080, res = 300)
ggMarginal(p1, type="boxplot",groupColour = TRUE, groupFill = TRUE, size=7) 
```

```
## `geom_smooth()` using formula = 'y ~ x'
## `geom_smooth()` using formula = 'y ~ x'
## `geom_smooth()` using formula = 'y ~ x'
```

![](EDA-HDV-Final_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

``` r
dev.off() 
```

```
## null device 
##           1
```






``` r
# contingency table of comorbidities and type of surgery
outcome_counts <- hypoxia %>%   
  dplyr::select(`Type Surg`, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Condition", value = "Diagnosis", -c(`Type Surg`))  %>%
  filter(Diagnosis == 1)
outcome_counts$`Type Surg` <- as.factor(outcome_counts$`Type Surg`)
outcome_counts$Condition   <- as.factor(outcome_counts$Condition)

condition_surg_table <- xtabs(~ `Type Surg` + Condition, data = outcome_counts)
condition_surg_table
```

```
##          Condition
## Type Surg CAD Diabetes Hyper Smoking
##         1  17       80   159     119
##         2   9       16    35      21
##         3   0        0     2       0
```

``` r
kable(condition_surg_table, caption = "Type of Surgery by Comorbidity", row.names = T) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Type of Surgery by Comorbidity</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> CAD </th>
   <th style="text-align:right;"> Diabetes </th>
   <th style="text-align:right;"> Hyper </th>
   <th style="text-align:right;"> Smoking </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 159 </td>
   <td style="text-align:right;"> 119 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>


``` r
# contingency table of comorbidities and epinephrine usage
epi_com_counts <- data %>%   
  dplyr::select(Epinephrine, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Usage", value = "Diagnosis", -c(Epinephrine))  %>%
  filter(Diagnosis == 1)

epi_com_counts$Usage <- as.factor(epi_com_counts$Usage)
epi_com_counts$Epinephrine <- as.factor(epi_com_counts$Epinephrine)

condition_ep_table <- xtabs(~ Usage + Epinephrine, data = epi_com_counts)
condition_ep_table
```

```
##           Epinephrine
## Usage      No Epinephrine Use Epinephrine
##   CAD                  25               1
##   Diabetes             95               1
##   Hyper               194               2
##   Smoking             138               2
```

``` r
kable(condition_ep_table, caption = "Epinephrine Usage for Comorbidities", row.names = T) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Epinephrine Usage for Comorbidities</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> No Epinephrine </th>
   <th style="text-align:right;"> Use Epinephrine </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> CAD </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Diabetes </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hyper </td>
   <td style="text-align:right;"> 194 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Smoking </td>
   <td style="text-align:right;"> 138 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>


``` r
# contingency table of comorbidities and phenylephrine usage

ph_com_counts <- data %>%   
  dplyr::select(Phenylephrine, Smoking, Diabetes, Hyper, CAD) %>%  
  gather(key = "Usage", value = "Diagnosis", -c(Phenylephrine)) %>%
  filter(Diagnosis == 1)

ph_com_counts$Usage <- as.factor(ph_com_counts$Usage)
ph_com_counts$Phenylephrine <- as.factor(ph_com_counts$Phenylephrine)

condition_ph_table <- xtabs(~ Usage + Phenylephrine, data = ph_com_counts)
condition_ph_table
```

```
##           Phenylephrine
## Usage      No Phenylephrine Use Phenylephrine
##   CAD                    15                11
##   Diabetes               54                42
##   Hyper                 119                77
##   Smoking                85                55
```

``` r
kable(condition_ph_table, caption = "Phenylephrine Usage for Comorbidities", row.names = T) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Phenylephrine Usage for Comorbidities</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> No Phenylephrine </th>
   <th style="text-align:right;"> Use Phenylephrine </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> CAD </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Diabetes </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hyper </td>
   <td style="text-align:right;"> 119 </td>
   <td style="text-align:right;"> 77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Smoking </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 55 </td>
  </tr>
</tbody>
</table>



``` r
# summary table

data$AHI <- as.factor(data$AHI)

library(arsenal)

controls_report <- tableby.control(test=FALSE, total=FALSE,                                   
numeric.test="kwt", cat.test="chisq",                                   
numeric.stats=c("Nmiss", "median", "q1q3", "range"),                                   
cat.stats=c("Nmiss","countpct"),                                   
stats.labels=list(median="Median", q1q3="Q1,Q3", range = "Range"))

controls_report2 <- tableby.control(test=FALSE, total=FALSE,                                   
numeric.test="kwt", cat.test="chisq",                                   
numeric.stats=c("Nmiss", "median", "range"),                                   
cat.stats=c("Nmiss","countpct"),                                   
stats.labels=list(median="Median", range = "Range"))

#Create Labels
report_labels = c( `Duration of Surg` = "Duration of Surgery (hours)",                   
 Female = "Sex (0= male)",                          
 Race = "Race",                      
 BMI = "BMI Score",                       
`BMI Category` = "BMI Category",                       
Smoking =  "Smoking History",
Diabetes = "Diabetes History",
 CAD = "Coronary Heart Disease",
`TWA HR` = "Heart Rate During Surgery",
Epinephrine = "Use of Epinephrine",
Phenylephrine = "Use of Phenylephrine",
CPAP = "Use of CPAP"
)

report_1 <- tableby(`Type Surg`~ Female + Race + BMI + BMI_Category + `Duration of Surg` 
                    + Smoking +Diabetes + Hyper + CAD + `TWA HR` + Epinephrine + Phenylephrine + CPAP + AHI 
                      , data=hypoxia,
                    digits= 1, test= TRUE, digits.p=2, total = T, control=controls_report)

report_2 <- tableby(`Type Surg`~ Female + Race + BMI + `Duration of Surg` 
                    + Smoking +Diabetes + Hyper + CAD + `TWA HR` + Epinephrine + Phenylephrine + CPAP + AHI
                      , data=data,
                    digits= 1, test= TRUE, digits.p=2, total = T, control=controls_report2)

summary(report_2, test=TRUE, labelTranslations=report_labels, control=controls_report, title="Summary: Overall by Surgery Type", text = TRUE)
```

```
## 
## 
## Table: Summary: Overall by Surgery Type
## 
## |                            |  1 (N=224)  |   2 (N=51)   |   3 (N=3)   | Total (N=278) | p value|
## |:---------------------------|:-----------:|:------------:|:-----------:|:-------------:|-------:|
## |Sex (0= male)               |             |              |             |               |    0.32|
## |-  Male                     | 60 (26.8%)  |  19 (37.3%)  |  1 (33.3%)  |  80 (28.8%)   |        |
## |-  Female                   | 164 (73.2%) |  32 (62.7%)  |  2 (66.7%)  |  198 (71.2%)  |        |
## |Race                        |             |              |             |               |    0.34|
## |-  African American         | 43 (19.2%)  |  12 (23.5%)  |  2 (66.7%)  |  57 (20.5%)   |        |
## |-  White                    | 175 (78.1%) |  38 (74.5%)  |  1 (33.3%)  |  214 (77.0%)  |        |
## |-  Other                    |  6 (2.7%)   |   1 (2.0%)   |  0 (0.0%)   |   7 (2.5%)    |        |
## |BMI Score                   |             |              |             |               |  < 0.01|
## |-  Median                   |    47.0     |     43.5     |    48.6     |     46.0      |        |
## |-  Range                    | 31.3 - 70.1 | 34.0 - 71.7  | 45.9 - 50.2 |  31.3 - 71.7  |        |
## |Duration of Surgery (hours) |             |              |             |               |  < 0.01|
## |-  Median                   |     4.4     |     2.8      |     4.1     |      4.2      |        |
## |-  Range                    |  2.9 - 7.9  |  2.0 - 4.8   |  4.1 - 5.5  |   2.0 - 7.9   |        |
## |Smoking History             |             |              |             |               |    0.07|
## |-  0                        | 105 (46.9%) |  30 (58.8%)  | 3 (100.0%)  |  138 (49.6%)  |        |
## |-  1                        | 119 (53.1%) |  21 (41.2%)  |  0 (0.0%)   |  140 (50.4%)  |        |
## |Diabetes History            |             |              |             |               |    0.38|
## |-  0                        | 144 (64.3%) |  35 (68.6%)  | 3 (100.0%)  |  182 (65.5%)  |        |
## |-  1                        | 80 (35.7%)  |  16 (31.4%)  |  0 (0.0%)   |  96 (34.5%)   |        |
## |Hyper                       |             |              |             |               |    0.94|
## |-  0                        | 65 (29.0%)  |  16 (31.4%)  |  1 (33.3%)  |  82 (29.5%)   |        |
## |-  1                        | 159 (71.0%) |  35 (68.6%)  |  2 (66.7%)  |  196 (70.5%)  |        |
## |Coronary Heart Disease      |             |              |             |               |    0.07|
## |-  0                        | 207 (92.4%) |  42 (82.4%)  | 3 (100.0%)  |  252 (90.6%)  |        |
## |-  1                        |  17 (7.6%)  |  9 (17.6%)   |  0 (0.0%)   |   26 (9.4%)   |        |
## |Heart Rate During Surgery   |             |              |             |               |  < 0.01|
## |-  N-Miss                   |     67      |      8       |      2      |      77       |        |
## |-  Median                   |    76.7     |     69.3     |    79.7     |     75.7      |        |
## |-  Range                    | 54.3 - 99.0 | 54.2 - 109.1 | 79.7 - 79.7 | 54.2 - 109.1  |        |
## |Use of Epinephrine          |             |              |             |               |    0.51|
## |-  No Epinephrine           | 223 (99.6%) |  50 (98.0%)  | 3 (100.0%)  |  276 (99.3%)  |        |
## |-  Use Epinephrine          |  1 (0.4%)   |   1 (2.0%)   |  0 (0.0%)   |   2 (0.7%)    |        |
## |Use of Phenylephrine        |             |              |             |               |    0.04|
## |-  No Phenylephrine         | 129 (57.6%) |  39 (76.5%)  |  2 (66.7%)  |  170 (61.2%)  |        |
## |-  Use Phenylephrine        | 95 (42.4%)  |  12 (23.5%)  |  1 (33.3%)  |  108 (38.8%)  |        |
## |Use of CPAP                 |             |              |             |               |    0.50|
## |-  No CPAP                  | 80 (35.7%)  |  20 (39.2%)  |  2 (66.7%)  |  102 (36.7%)  |        |
## |-  Use CPAP                 | 144 (64.3%) |  31 (60.8%)  |  1 (33.3%)  |  176 (63.3%)  |        |
## |AHI                         |             |              |             |               |    0.06|
## |-  N-Miss                   |      3      |      0       |      0      |       3       |        |
## |-  1                        | 23 (10.4%)  |  12 (23.5%)  |  1 (33.3%)  |  36 (13.1%)   |        |
## |-  2                        | 57 (25.8%)  |  9 (17.6%)   |  2 (66.7%)  |  68 (24.7%)   |        |
## |-  3                        | 54 (24.4%)  |  10 (19.6%)  |  0 (0.0%)   |  64 (23.3%)   |        |
## |-  4                        | 87 (39.4%)  |  20 (39.2%)  |  0 (0.0%)   |  107 (38.9%)  |        |
```



``` r
# to export this change the inline output settings for R markdonw

#write2word(report_2,labelTranslations=report_labels,("hdv_final_summary"), quiet = FALSE) # to make this work remove the inline output setting
```



``` r
# comorbidities x cpap graph version 3 was done - grouped prop graph comaring pos/neg for each comorbidity and epinephrine use. yay.
# basically no real difference this is consistent with correlation plot

cpap_counts <- hypoxia %>%   
  dplyr::select(AHI, CPAP) %>%  
  gather(key = "Label", value = "CPAP", -c(`AHI`))  

cpap_ahi_prop_whole <- cpap_counts %>%  
group_by(AHI, CPAP) %>% 
summarise(Frequency = n(), .groups = "drop") %>%  
group_by(CPAP) %>%  
mutate(Proportion = Frequency / sum(Frequency))

cpap_ahi_prop_whole
```

```
## # A tibble: 10 × 4
## # Groups:   CPAP [2]
##      AHI  CPAP Frequency Proportion
##    <dbl> <dbl>     <int>      <dbl>
##  1     1     0        33    0.324  
##  2     1     1         3    0.0170 
##  3     2     0        38    0.373  
##  4     2     1        30    0.170  
##  5     3     0        17    0.167  
##  6     3     1        47    0.267  
##  7     4     0        12    0.118  
##  8     4     1        95    0.540  
##  9    NA     0         2    0.0196 
## 10    NA     1         1    0.00568
```

``` r
cpap_ahi_prop_whole$CPAP <- as.factor(cpap_ahi_prop_whole$CPAP)

# grouped bar chart of comorbidities 
#tiff("phenylephrine_stacked.tiff", width = 2500, height = 1500, res = 300)
png("finalfig10.png", width = 1920, height = 1080, res = 300)
ggplot(cpap_ahi_prop_whole, aes(fill=CPAP, x=AHI, y = Proportion)) + # might be better with stacked barplot? 
  geom_bar(position = "fill", stat="identity", width = 0.6, color = "black")+ 
    scale_fill_brewer(palette = "Set2", direction = 1, labels = c("No CPAP", "Use CPAP"))+
  scale_y_continuous(labels = scales::percent)+
  brplt_theme +
  labs(
    title = "CPAP Use in Surgery\n Across Sleep Apnea Index", 
    x = "Sleep Apnea Index (AHI)",
    y = "Percentage of Patients",
    fill = "CPAP Use"
  )
```

```
## Warning: Removed 2 rows containing missing values or values outside the scale range
## (`geom_bar()`).
```

``` r
dev.off()
```

```
## quartz_off_screen 
##                 2
```









