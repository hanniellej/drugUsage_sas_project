
*Data Analytics  Project
Title: 2017 Drug Utilization Data Analysis
By: BY             
	
	Hannielle Joseph

* Descriptive statistics for: (1) one continuous variable (2) one
categorical variable and (3) one continuous variable grouped by a
categorical variable, including data visualization (histogram &
boxplot for a continuous variable and pie graph and bar chart for acategorical variable, etc.) and numerical measures of location and 
variation (that we have covered in the course) for a continuous 
variable, as well as frequencies and percentages for a categorical 
variable.:;


data drugUtilisation;
set 'C:\Users\hanni\Documents\SAS\sdued2017.sas7bdat' ;
run;
proc univariate data=drugUtilisation;
var numberofprescriptions;*specify which variable that we want descriptive stat for;
title 'Number of Drugs prescription in 2017'; *add title;
ods graphics / NBINSMAX=3000;  * increase bin size to 3000;
histogram numberofprescriptions/ midpoints=(10 to 500000 by 1000)vscale=count; * graph and add scale histogram;
WHERE numberofprescriptions<500000;
footnote; * continuous variable;
Run;
* frequencies and percentages for a categorical 
variable;
proc freq data=drugUtilisation order=freq ;
title' Descriptive statistics for suppression of drug used in 2017';
tables suppressionused;
run;

* Bar graph;
proc gchart data=drugUtilisation;
title 'Bar graph for suppressiond drug used';
vbar suppressionused / type=PERCENT DESCENDING;
run;
* Pie chart;
proc gchart data=drugUtilisation;
title ' Pie for suppressiond drug used';
pie suppressionused / type=PERCENT DESCENDING;
run;
*one continuous variable grouped by a categorical variable
data drugUtilisation;
set 'C:\Users\hanni\Documents\SAS\sdued2017.sas7bdat' ;
run;
proc means data=drugUtilisation ;
title' Descriptive statistics of Number of Drug Prescription in 2017 group by utilization Type ';
class utilizationtype;
var numberofprescriptions;
run;
* Box plot for number of drug prescription;
proc sgplot data=drugUtilisation ;
ODS GRAPHICS / MAXOBS=3516205 ;
title' Descriptive statistics of Number of Drug Prescription in 2017 group by utilization Type ';
vbox numberofprescriptions  / group =utilizationtype ;
run;
* QQ-plots for number of drug prescription;
proc univariate data=drugUtilisation ;
ODS GRAPHICS / MAXOBS=3516205 ;
var numberofprescriptions;
title' QQ-plots of Number of Drug Prescription in 2017 ';
qqplot numberofprescriptions  / normal ;
run;

* Test of Normality, including: measures of skewness, comparing
the mean to the median, histogram with Normal curve overlaid,
Normal q-q plot, and significance test for normality.;
data drugUtilisation;
set 'C:\Users\hanni\Documents\SAS\sdued2017.sas7bdat' ;
run;
proc univariate data=drugUtilisation ;
title' Descriptive Statistics  for the Total Amount of Money Reimbursed for Drug in 2017 ';
ods select BasicMeasures;
var totalamountreimbursed;
where totalamountreimbursed~=.;
run;
* QQ-plots for number of drug prescription;
proc univariate data=drugUtilisation ;
ODS GRAPHICS / MAXOBS=3516205 ;
var totalamountreimbursed;
title' QQ-plots for the Total Amount of Money Reimbursed for Drug in 2017  ';
qqplot totalamountreimbursed/ normal ;
where totalamountreimbursed~=.;
run;

proc univariate data=drugUtilisation;
var totalamountreimbursed;
title ' Histogram for the Total Amount of Money Reimbursed for Drug in 2017';
*ods graphics / NBINSMAX=;  
histogram totalamountreimbursed / midpoints=(10 to 50000 by 100)vscale=count ;
where totalamountreimbursed < 50000;
run;

data mysubset;
set drugUtilisation;
logtotalAmountreimb = log(totalamountreimbursed);
run;
proc univariate data=mysubset;
var logtotalAmountreimb ;
title ' Histogram for Log the Total Amount of Money Reimbursed for Drug in 2017';
histogram  logtotalAmountreimb / midpoints=(0 to 50 by 2)vscale=count ;
where logtotalAmountreimb< 50;
run;


*Test for H0: 
? =/</>, etc. hypothesized value, and include the
95% confidence interval (CI).;
DATA projectdata ;
SET 'C:\Users\demun\Downloads\sdued2017.sas7bdat' ;
KEEP medicaidamountreimbursed utilizationtype numberofprescriptions ;
run ;
DATA newdata ;
SET projectdata ;
*create a variable for claims paid at zero dollars' ;
if medicaidamountreimbursed = 0 then cap_amt = '1' ;
if medicaidamountreimbursed > 0 then cap_amt = '2' ;
*include only MCO claims ;
where utilizationtype = 'MCOU' ;
run ;
DATA finaldata ;
SET newdata ;
*create a variable for percent of prescription claims paid at zero dollars ;
pct_cap = (cap_amt/numberofprescriptions) * 100 ;
where cap_amt = '1' and utilizationtype = 'MCOU' ;
run ;
PROC ttest h0=5 sides=l alpha=0.05 data=finaldata plots=none;
title 'One sided t test percent capitated claims' ;
var pct_cap ;
run ;



*Contingency table with a Chi-square significance test for H0:
Independence between categorical variables. Include an odds ratio
and relative risk for one comparison of categorical variables.;

Proc freq data=Drug2017 order=data;
Table utilizationtype*suppressionused / chisq measures;
run;


* Independent t-test (or paired t-test, as appropriate), and include the
95% CI;
DATA projectdata ;
SET 'C:\Users\demun\Downloads\sdued2017.sas7bdat' ;
KEEP medicaidamountreimbursed utilizationtype numberofprescriptions ;
run ;
DATA mydata ;
SET projectdata ;
*create a variable to calculate average rx cost ;
avg_rxcost = medicaidamountreimbursed / numberofprescriptions ;
run;
PROC ttest sides=2 alpha=0.05 data=mydata plots=none ;
title 'Avg Rx Cost FFS and MCO' ;
*run t test to look for significant difference between FFS and MCO ;
var avg_rxcost ;
class utilizationtype ;
run ;

DATA mydata ;
SET projectdata ;
*create a variable to calculate average rx cost ;
avg_rxcost = medicaidamountreimbursed / numberofprescriptions ;
run;
PROC univariate data=mydata ;
ods select basicmeasures ;
var avg_rxcost ;
title 'Descriptive Statistics' ;
run ;



*Scatter plot of two continuous variables and linear correlation test
for H0: ;


DATA reimbursedAmountData;
set 'C:/SAS/Data/sdued2017.sas7bdat';
keep totalamountreimbursed medicaidamountreimbursed nonmedicaidamountreimbursed;
run;

DATA reimbursedAmountData2;
set reimbursedAmountData;
percentMedicaidReimbursed = .;
if missing(totalamountreimbursed) then percentMedicaidReimbursed=.;
if not missing(totalamountreimbursed) then percentMedicaidReimbursed = (medicaidamountreimbursed/totalamountreimbursed) * 100;
run;

title 'Correlation Analysis';
proc corr data=reimbursedAmountData2 pearson;
var totalamountreimbursed percentMedicaidReimbursed;
where totalamountreimbursed ~= . and percentMedicaidReimbursed ~= .;
run;

title 'Scatter plot for Percentage of Amount Reimbursed by Medicaid by Total Amount Reimbursed';
PROC sgplot data=reimbursedAmountData2 (firstobs=1 obs=500);
scatter x=percentMedicaidReimbursed y=totalamountreimbursed;
where totalamountreimbursed ~= . and percentMedicaidReimbursed ~= .;
footnote 'Data visualization only for 500 observations';
Run;



*Scatter plot with estimated simple linear regression line and a full
linear regression analysis, including a residual plot.;

DATA reimbursedAmountData;
set 'C:/SAS/Data/sdued2017.sas7bdat';
keep utilizationtype totalamountreimbursed medicaidamountreimbursed nonmedicaidamountreimbursed;
run;

DATA reimbursedAmountData2;
set reimbursedAmountData;
percentMedicaidReimbursed = .;
if missing(totalamountreimbursed) then percentMedicaidReimbursed=.;
if not missing(totalamountreimbursed) then percentMedicaidReimbursed = (medicaidamountreimbursed/totalamountreimbursed) * 100;
run;

ods graphics on;
proc reg data=reimbursedAmountData2 (firstobs=1 obs=500)
title 'Percentage of Amount Reimbursed by Medicaid as a function of Total Amount Reimbursed';
model percentMedicaidReimbursed = totalamountreimbursed;
where totalamountreimbursed ~= . and percentMedicaidReimbursed ~= .;
footnote '';
Run;





* Multiple regression analysis, including variable selection;
data project; 
set '\\apporto.com\dfs\UNCC\Users\tmuhamma_uncc\Downloads\sdued2017.sas7bdat';
keep utilizationtype medicaidamountreimbursed nonmedicaidamountreimbursed;
run;
ods graphics off;
*Convert to binary;
data project_01;
 set project;
    if utilizationtype = 'MCOU' then butilizationtype=0;
    if utilizationtype = 'FFSU' then butilizationtype=1;
proc reg data=project_01;
title '';
label butilizationtype = 'Utilization Type';
model medicaidamountreimbursed nonmedicaidamountreimbursed = butilizationtype;
where medicaidamountreimbursed >0 and nonmedicaidamountreimbursed >0;


*Logistic regression analysis.;
DATA projectdata ;
SET 'C:\Users\demun\Downloads\sdued2017.sas7bdat' ;
KEEP totalamountreimbursed quarter ;
where quarter ~= 2 ;
run ;
DATA mydata ;
SET projectdata ;
*create a binary variable for season ;
if quarter = 1 then season = 1 ;
if quarter = 3 then season = 0 ;
run ;
PROC logistic data=mydata ;
model season (event = '1') = totalamountreimbursed ;
where totalamountreimbursed >= 0 ;
*logistic regression for effect of season on total amount ;
run ;
