
*////////////////////////////////////////////////////////////////////////////////////////
*Author: Matthew Dunn, MPH, PhD
*Code for article "A latent class assessment of healthcare access factors and disparities in breast cancer care timeliness" 
published in PLOS medicine 

* Datasets used in this analysis are available upon submission of a letter of intent and approval from 
the Carolina Breast Study Steering Committee (https://ciphr.unc.edu/cbcs-loi-form.php) and IRB approval. 
The data are not publicly available to protect privacy of study participants. 

*////////////////////////////////////////////////////////////////////////

*TABLE 1 RFDs showing difference by race ;

PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") ses3classlvl2 (ref="1")  ; 
MODEL ses3classlvl2 = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") ses3classlvl3 (ref="1")  ; 
MODEL ses3classlvl3 = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") barriers2class (ref="1")  ; 
MODEL barriers2class = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") util5lvl2 (ref="1")  ; 
MODEL util5lvl2 = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") util5lvl3 (ref="0")  ; 
MODEL util5lvl3 = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") util5lvl4 (ref="0")  ; 
MODEL util5lvl4 = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") util5lvl5 (ref="0")  ; 
MODEL util5lvl5 = race  / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") latestage (ref="0")  ; 
MODEL latestage = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") delay (ref="0")  ; 
MODEL delay = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") PRO_TREAT (ref="0")  ; 
MODEL PRO_TREAT = race  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS race (ref="1") odxtested2 (ref="1")  ; 
MODEL odxtested2 = race  / link=identity dist=binomial ;
RUN ; 



*Fig 2A delayed diagnosis models for all patients ;

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") ses3classfinal (ref="1")   ; 
MODEL latestage= ses3classfinal agesel  / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") barriers2class (ref="1")    ; 
MODEL latestage= barriers2class agesel / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") util5class2 ( ref="1")  ; 
MODEL latestage = util5class2 agesel / link=identity dist=binomial ;
RUN ; 


*Fig 3A delayed treatment models for all patients ;



PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

*Fig 4A prolonged treatment models for all patients ;

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4 ; 
RUN ; 


*S3 Fig, sensitivity analyses for prolonged treatment models ;

*Stratify by HER2 status ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE her2_status="Positive" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE her2_status="Positive" and stage NE 4; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE her2_status="Positive" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE her2_status="Negative" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE her2_status="Negative"and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE her2_status="Negative" and stage NE 4 ; 
RUN ; 



*Stratify by ER status ; 
PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE er_status="Positive" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE er_status="Positive" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE er_status="Positive" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE er_status="Negative" and stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE er_status="Negative" and stage NE 4 ; 
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE er_status="Negative" and stage NE 4 ; 
RUN ; 

ODS EXCEL CLOSE ; 



*S4 Figure - OncotypeDx models for all patients ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") ses3classfinal (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= ses3classfinal agesel  estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") barriers2class (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL odxtested2= barriers2class agesel  estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") util5class2 ( ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2 = util5class2 agesel  estsize grade / link=identity dist=binomial ;
RUN ; 


**Fig 2B - Delayed diagnosis models race-stratified ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") ses3classfinal (ref="1")   ; 
MODEL latestage= ses3classfinal agesel  / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") barriers2class (ref="1")    ; 
MODEL latestage= barriers2class agesel / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") util5class2 ( ref="1")  ; 
MODEL latestage = util5class2 agesel / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 


PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") ses3classfinal (ref="1")   ; 
MODEL latestage= ses3classfinal agesel  / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") barriers2class (ref="1")    ; 
MODEL latestage= barriers2class agesel / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") util5class2 ( ref="1")  ; 
MODEL latestage = util5class2 agesel / link=identity dist=binomial ;
WHERE race=1 ;
RUN ;



*Fig 3B - delayed treatment models race-stratified ;

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 


PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 


*S2 Fig - Sensitivity analyses for alternate 45-day cutoff for delayed treatment ; 

	*all patients ; 
PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay45= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay45= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay45 = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

	*Black patients ; 
PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay45= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay45= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay45= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

	*non-black patients;

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay45= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay45= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay45 (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay45 = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 


* Fig 4B - prolonged treatment models race stratified;

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 AND stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 AND stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=2 AND stage NE 4;
RUN ; 


PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 AND stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 AND stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE race=1 AND stage NE 4;
RUN ; 

*Figure S4 - oncotypeDX models race-stratified ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") ses3classfinal (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= ses3classfinal agesel  estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") barriers2class (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL odxtested2= barriers2class agesel  estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") util5class2 ( ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2 = util5class2 agesel  estsize grade / link=identity dist=binomial ;
WHERE race=2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") ses3classfinal (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= ses3classfinal agesel  estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") barriers2class (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL odxtested2= barriers2class agesel  estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") util5class2 ( ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2 = util5class2 agesel  estsize grade / link=identity dist=binomial ;
WHERE race=1 ;
RUN ; 


****TABLE 2;
**Isolate Fragmented care;

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") fragflag15 (ref="0")   ; 
MODEL latestage= fragflag15 agesel  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") fragflag30 (ref="0")  ; 
MODEL latestage= fragflag30 agesel  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") fragflag60 (ref="0")  ; 
MODEL latestage= fragflag60 agesel  / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") fragflag15 (ref="0") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= fragflag15 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") fragflag30 (ref="0") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= fragflag30 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") fragflag60 (ref="0") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= fragflag60 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") fragflag15 (ref="0") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= fragflag15 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") fragflag30 (ref="0") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= fragflag30 agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4 ; 
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") fragflag60 (ref="0") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= fragflag60 agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4 ; 
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") fragflag15 (ref="0")  estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= fragflag15 agesel estsize grade / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") fragflag30 (ref="0") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= fragflag30 agesel estsize grade / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") fragflag60 (ref="0") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= fragflag60 agesel estsize grade / link=identity dist=binomial ;
RUN ; 


**Isolate Income ;
PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") incomecat2  ; 
MODEL latestage= incomecat2 agesel  / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") incomecat2 stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= incomecat2 agesel stage estsize grade / link=identity dist=binomial ;
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") incomecat2 stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= incomecat2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4 ; 
RUN ; 
PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") incomecat2 estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= incomecat2 agesel estsize grade / link=identity dist=binomial ;
RUN ; 


ODS EXCEL CLOSE ;



*Fig 5 - the cross classification of each outcome ;


PROC FREQ DATA=merge1;
TABLES crossclass*latestage /  NOPERCENT NOCOL  ; 
RUN ; 
PROC FREQ DATA=merge1;
TABLES crossclass*delay /  NOPERCENT NOCOL  ; 
RUN ; 
PROC FREQ DATA=merge1;
TABLES crossclass*PRO_TREAT /   NOPERCENT NOCOL  ; 
RUN ; 



*Table S3 - additional sensitivity analyses with different eligibiltiy criteria ; 

PROC CONTENTS DATA=merge1;
RUN;

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") ses3classfinal (ref="1")   ; 
MODEL latestage= ses3classfinal agesel  / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") barriers2class (ref="1")    ; 
MODEL latestage= barriers2class agesel / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") util5class2 ( ref="1")  ; 
MODEL latestage = util5class2 agesel / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") ses3classfinal (ref="1")   ; 
MODEL latestage= ses3classfinal agesel  / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") barriers2class (ref="1")    ; 
MODEL latestage= barriers2class agesel / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS latestage (ref="0") util5class2 ( ref="1")  ; 
MODEL latestage = util5class2 agesel / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 


PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL delay= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS delay (ref="0") util5class2 (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL delay= util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") ses3classfinal (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT= ses3classfinal agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") barriers2class (ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL PRO_TREAT= barriers2class agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS PRO_TREAT (ref="0") util5class2 ( ref="1") stage (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL PRO_TREAT = util5class2 agesel stage estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 


PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") ses3classfinal (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= ses3classfinal agesel  estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") barriers2class (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL odxtested2= barriers2class agesel  estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") util5class2 ( ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2 = util5class2 agesel  estsize grade / link=identity dist=binomial ;
WHERE self_race = 1 | self_race = 2 ;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") ses3classfinal (ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2= ses3classfinal agesel  estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") barriers2class (ref="1") estsize(ref="1") grade (ref="1")   ; 
MODEL odxtested2= barriers2class agesel  estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

PROC GENMOD DATA=merge1 ; 
CLASS odxtested2 (ref="1") util5class2 ( ref="1") estsize(ref="1") grade (ref="1")  ; 
MODEL odxtested2 = util5class2 agesel  estsize grade / link=identity dist=binomial ;
WHERE stage NE 4;
RUN ; 

*Table S2 ; 

PROC FREQ DATA=merge1;
TABLES crossclass ; 
RUN ; 

