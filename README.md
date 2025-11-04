Deploy Shiny App to shinyapps.io

1. Run "deploy.R" locally in R (e.g. `source('deploy.R')`).
2. Follow the prompts for rsconnect authentication if necessary.

Variables for PLFS shiny dashboard

•	Time period: Year, quarter, month (especially after PLFS’s recent move to high-frequency estimates).
•	Gender: Male, female, other categories.
•	Age group: Standard population segments such as 15-24, 25-34, etc..
•	Education level: Illiterate, primary, secondary, higher.
•	Labor force status: Employed, unemployed, out of labor force.
•	Type of employment: Self-employed, regular wage/salaried, casual labor.
•	Sector: Agriculture, industry, services.
•	Indicators: Labour Force Participation Rate (LFPR), Worker Population Ratio (WPR), Unemployment Rate (UR).
•	Employment conditions: Hours worked, earnings, social security coverage (where available).

Documentation

Data file: perv1

Gender (b4q5_perv1)
1 = male
2 = female
3 = transgender

Age (b4q6_perv1)
continuous

General Educaion Level (b4q8_perv1)
01 	not literate
02 	literate without formal schooling
03 	literate without formal schooling
04 	others
05 	below primary
06 	primary
07 	middle
08 	secondary
10 	higher secondary
11 	diploma/certificate course
12 	graduate
13 	postgraduate and above

Type of employment
Status Code (b5pt2q3_perv1)
12, 12, 31, 41, 51 - employed
21, 91, 92, 93, 94, 95, 97 - inactive
81 - unemployed
Earnings For Regular Salaried/Wage Activity (b6q9_perv1)
Earnings For Self Employed (b6q10_perv1)

Sector (b1q3_perv1)
1 = rural
2 = urban
