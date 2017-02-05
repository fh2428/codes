libname mmse 'C:\Users\user\Dropbox\MultilevelAssigment\Multilevel-assignment';
run;

proc means data = mmse.mmse;  *mean = 78;
var Age ;
run;

data mmse;
 set mmse.mmse;
 ltime = log(time);
 ltime2=log(time)**2;
 c_age = Age - 78;
run;
ods pdf file = 'C:\Users\user\Dropbox\MultilevelAssigment\v3 Output.pdf';
ods graphics on;
/* Linear mixed model of question 2 */
proc mixed data=mmse covtest ;
class id Neuro (ref = "0");
model mmse=Neuro ltime ltime*Neuro / solution;
random intercept ltime / solution type=un subject=id; /* you can erase the solution argument and the 2 following lines */
ods listing exclude solutionr;
ods output solutionr=mmse.out;
run;
*quadratic;
 proc mixed data=mmse covtest ;
class id Neuro (ref = "0");
model mmse=Neuro ltime ltime*Neuro ltime2 ltime2*Neuro / solution;
random intercept ltime ltime2 / type=un subject=id; /* you can erase the solution argument and the 2 following lines */
run;

/* Question 3: plot in R*/

/* Linear mixed model of question 4 */
proc mixed data=mmse covtest ;
class id Neuro(ref = "0") housing;
model mmse= Age Neuro housing ltime ltime*Neuro / solution;
random intercept ltime / type=un subject=id;
run;
*age centered;
proc mixed data=mmse;
class id Neuro(ref = "0") housing;
model mmse= c_age Neuro housing ltime ltime*Neuro / solution;
random intercept ltime / type=un subject=id;
run;
*quadratic time;
proc mixed data=mmse;
class id Neuro(ref = "0") housing;
model mmse= Age Neuro housing ltime ltime*Neuro ltime2 ltime2*Neuro/ solution;
random intercept ltime ltime2/ type=un subject=id;
run;

*adding interactions;
proc mixed data=mmse;
class id Neuro(ref = "0") housing;
model mmse= Age Neuro housing ltime ltime*Neuro Neuro*Age Neuro*housing ltime*Age ltime*housing/ solution;
random intercept ltime / type=un subject=id;
run;


/* Question 5 dichotomization of the MMSE score */
/* (I choose 10 but can be any value, we have to read about this MMSE) */
data mmse;
 set mmse;
  dMMSE = .;
  IF (mmse >= 24) THEN dMMSE = 0; 
  IF (mmse <  24) THEN dMMSE = 1;
run;
/* Question 6 logistic random-Intercepts model */
proc glimmix data=mmse;
class id Neuro(ref = "0") housing;
model dMMSE (event='1') = Age Neuro housing ltime ltime*Neuro / dist=binary solution;
random intercept / subject=id;
run;
*centered age;
proc glimmix data=mmse;
class id Neuro(ref = "0") housing;
model dMMSE (event='1') = c_age Neuro housing ltime ltime*Neuro / dist=binary solution;
random intercept / subject=id;
run;

ods pdf close;
