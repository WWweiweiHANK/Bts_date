* Encoding: UTF-8.

* Analyses of INDIVIDUAL-LEVEL DATA for tables.

* Table 1.
MIXED Hygiene_WashHands_1 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_WashHands_2 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_WashHands_3 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_WashHands_4 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_WashHands_5 WITH FemDum
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_WashHands_6 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_Spit_1 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_Spit_2 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_Spit_3 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_Spit_4 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_Spit_5 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Hygiene_Spit_6 WITH FemDum 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1)
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=FemDum | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).

* Table 2.

COMPUTE filter_$=( ~ sysmis(SexEqualityx100)).
VARIABLE LABELS filter_$ ' ~ sysmis(SexEqualityx100) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
SORT CASES  BY MoreEgalitarianThanAverage.
SPLIT FILE LAYERED BY MoreEgalitarianThanAverage.

MIXED Handwash_12456 WITH SexEquality_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*SexEquality_centered_in_each_half SexEquality_centered_in_each_half  | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Handwash_12456 WITH SexEquality_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*SexEquality_centered_in_each_half SexEquality_centered_in_each_half Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8 | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Spit_12456 WITH SexEquality_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*SexEquality_centered_in_each_half SexEquality_centered_in_each_half   | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Spit_12456 WITH SexEquality_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*SexEquality_centered_in_each_half SexEquality_centered_in_each_half  Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8 | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).

* Table 2 with GGI2020 instead.
USE ALL.
COMPUTE filter_$=( ~ sysmis(GGGI2020)).
VARIABLE LABELS filter_$ ' ~ sysmis(GGGI2020) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
SORT CASES  BY MoreGGGIThanAverage.
SPLIT FILE LAYERED BY MoreGGGIThanAverage.

MIXED Handwash_12456 WITH GGGI2020_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*GGGI2020_centered_in_each_half GGGI2020_centered_in_each_half  | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Handwash_12456 WITH GGGI2020_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*GGGI2020_centered_in_each_half GGGI2020_centered_in_each_half Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8 | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Spit_12456 WITH GGGI2020_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*GGGI2020_centered_in_each_half GGGI2020_centered_in_each_half   | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).
MIXED Spit_12456 WITH GGGI2020_centered_in_each_half FemDum StudentDummy Age Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=FemDum FemDum*GGGI2020_centered_in_each_half GGGI2020_centered_in_each_half  Response_style 
    Important_children_3_mean Important_children_3 Perceived_threat_8_mean Perceived_threat_8 | 
    SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT FemDum | SUBJECT(SiteCountry) COVTYPE(VC).

* Analyses- of COUNTRY-LEVEL data for figures.
    
 * Fig 1.
EXAMINE VARIABLES=diff.WashHands_before_eating diff.WashHands_after_eating 
    diff.WashHands_after_defecating diff.WashHands_after_urinating diff.WashHands_come_home 
    diff.WashHands_after_shaking_hands 
  /COMPARE VARIABLE
  /PLOT=BOXPLOT
  /STATISTICS=NONE
  /NOTOTAL
  /ID=SiteCountry
  /MISSING=LISTWISE.

 * Fig 2.
EXAMINE VARIABLES=diff.Spit_kitchen_sink diff.Spit_sidewalk diff.Spit_kitchen_floor 
    diff.Spit_soccer_field diff.Spit_swimming_pool diff.Spit_forest 
  /COMPARE VARIABLE
  /PLOT=BOXPLOT
  /STATISTICS=NONE
  /NOTOTAL
  /ID=SiteCountry
  /MISSING=LISTWISE.

* Fig 3.
GRAPH
  /SCATTERPLOT(BIVAR)=diff_spit_12456 WITH diff_wash_12456 BY CountryISO (IDENTIFY)
  /MISSING=LISTWISE
  /TEMPLATE='C:\Users\ken05\Dropbox\Public\Spel-labb\Metanorms RJ\Largescale '+
    'study\Hygiene\Gender\scatter sex equality.sgt'.

* Fig 4.
GRAPH
  /SCATTERPLOT(BIVAR)=SexEqualityx100 WITH diff_spit_12456 BY CountryISO (IDENTIFY)
  /MISSING=LISTWISE
  /TEMPLATE='C:\Users\ken05\Dropbox\Public\Spel-labb\Metanorms RJ\Largescale '+
    'study\Hygiene\Gender\scatter sex equality.sgt'.


* S1 Fig.
GRAPH
  /SCATTERPLOT(BIVAR)=GGGI2020 WITH diff_spit_12456 BY CountryISO (IDENTIFY)
  /MISSING=LISTWISE
  /TEMPLATE='C:\Users\ken05\Dropbox\Public\Spel-labb\Metanorms RJ\Largescale '+
    'study\Hygiene\Gender\scatter GGGI.sgt'.


* Fig 5.
GRAPH
  /SCATTERPLOT(BIVAR)=SexEqualityx100 WITH diff_wash_12456 BY CountryISO (IDENTIFY)
  /MISSING=LISTWISE
  /TEMPLATE='C:\Users\ken05\Dropbox\Public\Spel-labb\Metanorms RJ\Largescale '+
    'study\Hygiene\Gender\scatter sex equality.sgt'.

* S2 Fig.
GRAPH
  /SCATTERPLOT(BIVAR)=GGGI2020 WITH diff_wash_12456 BY CountryISO (IDENTIFY)
  /MISSING=LISTWISE
  /TEMPLATE='C:\Users\ken05\Dropbox\Public\Spel-labb\Metanorms RJ\Largescale '+
    'study\Hygiene\Gender\scatter GGGI.sgt'.

* Other analyses of COUNTRY-LEVEL DATA.

* Internal consistency of sex differences in strictness about handwashing.
RELIABILITY
  /VARIABLES=diff.WashHands_before_eating diff.WashHands_after_eating
    diff.WashHands_after_defecating diff.WashHands_after_urinating diff.WashHands_come_home
    diff.WashHands_after_shaking_hands
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /SUMMARY=TOTAL.


* Internal consistency of sex differences in strictness about spitting.
RELIABILITY
  /VARIABLES=diff.Spit_kitchen_sink diff.Spit_sidewalk diff.Spit_kitchen_floor
    diff.Spit_soccer_field diff.Spit_swimming_pool diff.Spit_forest
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /SUMMARY=TOTAL.

* Handwashing: Quadratic model using sex equality.
TSET NEWVAR=NONE.
CURVEFIT
  /VARIABLES=diff_wash_12456 WITH SexEqualityx100
  /CONSTANT
  /MODEL=QUADRATIC
  /PLOT FIT.
  
* Spitting: Quadratic model using sex equality.
TSET NEWVAR=NONE.
CURVEFIT
  /VARIABLES=diff_spit_12456 WITH SexEqualityx100
  /CONSTANT
  /MODEL=QUADRATIC
  /PLOT FIT.

* Handwashing: Quadratic model using religiosity.
TSET NEWVAR=NONE.
CURVEFIT
  /VARIABLES=diff_wash_12456 WITH GWP_Relig
  /CONSTANT
  /MODEL=QUADRATIC
  /PLOT FIT.

* Spitting:  Quadratic model using religiosity.
TSET NEWVAR=NONE.
CURVEFIT
  /VARIABLES=diff_spit_12456 WITH GWP_Relig
  /CONSTANT
  /MODEL=QUADRATIC
  /PLOT FIT.

* Handwashing:  Quadratic model using pathogen prevalence.
TSET NEWVAR=NONE.
CURVEFIT
  /VARIABLES=diff_wash_12456 WITH Historical_prevalence_pathogens
  /CONSTANT
  /MODEL=QUADRATIC
  /PLOT FIT.

* Spitting: Quadratic model using pathogen prevalence.
TSET NEWVAR=NONE.
CURVEFIT
  /VARIABLES=diff_spit_12456 WITH Historical_prevalence_pathogens
  /CONSTANT
  /MODEL=QUADRATIC
  /PLOT FIT.

