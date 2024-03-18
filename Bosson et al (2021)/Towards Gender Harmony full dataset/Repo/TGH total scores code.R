# The source code below allows you to load the TGH database from csv files. 
# This code assumes that the script file is in the same directory as the 
# database file. Otherwise, modify the path to the csv file in line number 10.

rm(list = ls())

# used package:
library(tidyverse)

# data import:
data <- read.csv("./Repo/DataBaseTGH.csv")

data_scores <- data %>% 
  rowwise() %>% mutate(
  PRECARIOUS_MANHOOD = mean(c(Precarious_manhood_1,
                              Precarious_manhood_2,
                              Precarious_manhood_3,
                              Precarious_manhood_4)),
  GECAI = mean(c(Collective_action_intentions_normative_long_1,
                 Collective_action_intentions_normative_long_2, 
                 Collective_action_intentions_normative_long_3,
                 Collective_action_intentions_normative_long_4, 
                 Collective_action_intentions_normative_long_5,
                 Collective_action_intentions_normative_long_6)),
  CAI_SHORT_normative = mean(c(Collective_action_intentions_normative1,
                               Collective_action_intentions_normative2)),
  CAI_SHORT_radical = mean(c(Collective_action_intentions_radical1,
                             Collective_action_intentions_radical2)),
  ZEROSUM = mean(c(ZeroSum_GenderPerspective_1,
                   ZeroSum_GenderPerspective_2,
                   ZeroSum_GenderPerspective_3,
                   ZeroSum_GenderPerspective_4,
                   ZeroSum_GenderPerspective_5,
                   ZeroSum_GenderPerspective_6)),
  BENEVOLENT_SEXISM = mean(c(AmbivalentSexism_1,
                             AmbivalentSexism_3,
                             AmbivalentSexism_6)),
  HOSTILE_SEXISM = mean(c(AmbivalentSexism_2,
                          AmbivalentSexism_4,
                          AmbivalentSexism_5)),
  AMBIVALENCE_TOWARDS_MEN = mean(c(Ambivalence_toward_men_1,
                                   Ambivalence_toward_men_2,
                                   Ambivalence_toward_men_3,
                                   Ambivalence_toward_men_4,
                                   Ambivalence_toward_men_5,
                                   Ambivalence_toward_men_6)),
  POWER_DISTANCE_BELIEFS = mean(c(PowerDistanceBeliefs_1,
                                  PowerDistanceBeliefs_2,
                                  PowerDistanceBeliefs_3,
                                  PowerDistanceBeliefs_4)),
  GENDER_ESSENTIALISM = mean(c(GenderEssentialism_1,
                               GenderEssentialism_2,
                               GenderEssentialism_3)),
  PVQ_AUTONONOMY = mean(c(Autonomy_1,
                          Autonomy_2,
                          Autonomy_3,
                          Autonomy_4,
                          Autonomy_5)),
  PVQ_EMBEDDEDNESS = mean(c(Embeddedness_1,
                            Embeddedness_2,
                            Embeddedness_3,
                            Embeddedness_4,
                            Embeddedness_5)),
  SELFCONSTRUAL_COMMUNION = mean (c(SelfConstrual_communion_1_compassionate,
                                    SelfConstrual_communion_2_helpful_to_others,
                                    SelfConstrual_communion_3_sympathetic,
                                    SelfConstrual_communion_4_understanding_of_others,
                                    SelfConstrual_communion_5_sensitive,
                                    SelfConstrual_communion_6_soft_hearted,
                                    SelfConstrual_communion_7_aware_of_others_feelings,
                                    SelfConstrual_communion_8_cooperative,
                                    SelfConstrual_communion_9_devoted_to_others,
                                    SelfConstrual_communion_10_trusting,
                                    SelfConstrual_communion_11_warm,
                                    SelfConstrual_communion_12_supportive)),
  SELFCONSTRUAL_WEAKNESS = mean(c(SelfConstrual_weakness_1_worrying,
                                  SelfConstrual_weakness_2_weak,
                                  SelfConstrual_weakness_3_timid,
                                  SelfConstrual_weakness_4_submissive,
                                  SelfConstrual_weakness_5_fearful,
                                  SelfConstrual_weakness_6_cowardly,
                                  SelfConstrual_weakness_7_dependent,
                                  SelfConstrual_weakness_8_infantile,
                                  SelfConstrual_weakness_9_uncertain,
                                  SelfConstrual_weakness_10_approval_seeking,
                                  SelfConstrual_weakness_11_subordinates_self_to_others,
                                  SelfConstrual_weakness_12_insecure)),
  SELFCONSTRUAL_DOMINANCE = mean(c(SelfConstrual_dominance_1_demanding,
                                   SelfConstrual_dominance_2_controlling,
                                   SelfConstrual_dominance_3_bossy,
                                   SelfConstrual_dominance_4_dominant,
                                   SelfConstrual_dominance_5_intimadting,
                                   SelfConstrual_dominance_7_forceful,
                                   SelfConstrual_dominance_6_feels_superior,
                                   SelfConstrual_dominance_8_dictatorial,
                                   SelfConstrual_dominance_9_aggressive,
                                   SelfConstrual_dominance_10_stubborn,
                                   SelfConstrual_dominance_11_arrogant,SelfConstrual_dominance_12_boastful)),
  SELFCONSTRUAL_AGENCY = mean(c(SelfConstrual_agency_1_decisive,
                                SelfConstrual_agency_2_ambitious,
                                SelfConstrual_agency_3_competitive,
                                SelfConstrual_agency_4_competent,
                                SelfConstrual_agency_5_confident,
                                SelfConstrual_agency_6_has_leadership_abilities,
                                SelfConstrual_agency_7_efficient,
                                SelfConstrual_agency_8_determined,
                                SelfConstrual_agency_10_active,
                                SelfConstrual_agency_9_courageous,
                                SelfConstrual_agency_11_capable,
                                SelfConstrual_agency_12_independent)),
  PRESCRIPTIONS_WOMEN_COMMUNION = mean(c(Prescriptions_women_communion_1_compassionate,
                                         Prescriptions_women_communion_2_helpful_to_others,
                                         Prescriptions_women_communion_3_sympathetic,
                                         Prescriptions_women_communion_4_understanding_of_others,Prescriptions_women_communion_5_sensitive,
                                         Prescriptions_women_communion_6_soft_hearted,
                                         Prescriptions_women_communion_7_aware_of_others_feelings,
                                         Prescriptions_women_communion_8_cooperative,Prescriptions_women_communion_9_devoted_to_others,
                                         Prescriptions_women_communion_10_trusting,Prescriptions_women_communion_11_warm,
                                         Prescriptions_women_communion_12_supportive)),
  PRESCRIPTIONS_WOMEN_WEAKNESS = mean(c(Prescriptions_women_weakness_1_worrying,
                                        Prescriptions_women_weakness_2_weak,Prescriptions_women_weakness_3_timid,
                                        Prescriptions_women_weakness_4_submissive,Prescriptions_women_weakness_5_fearful,
                                        Prescriptions_women_weakness_6_cowardly,Prescriptions_women_weakness_7_dependent,
                                        Prescriptions_women_weakness_8_infantile,Prescriptions_women_weakness_9_uncertain,
                                        Prescriptions_women_weakness_10_approval_seeking,
                                        Prescriptions_women_weakness_11_subordinates_self_to_others,Prescriptions_women_weakness_12_insecure)),
  PRESCRIPTIONS_WOMEN_DOMINANCE =mean(c(Prescriptions_women_dominance_1_demanding,
                                        Prescriptions_women_dominance_2_controlling,Prescriptions_women_dominance_3_bossy,
                                        Prescriptions_women_dominance_4_dominant,Prescriptions_women_dominance_5_intimadting,
                                        Prescriptions_women_dominance_6_feels_superior,Prescriptions_women_dominance_7_forceful,
                                        Prescriptions_women_dominance_8_dictatorial,Prescriptions_women_dominance_9_aggressive,
                                        Prescriptions_women_dominance_10_stubborn,Prescriptions_women_dominance_11_arrogant,
                                        Prescriptions_women_dominance_12_boastful)),
  PRESCRIPTIONS_WOMEN_AGENCY = mean(c(Prescriptions_women_agency_1_decisive,
                                      Prescriptions_women_agency_2_ambitious,
                                      Prescriptions_women_agency_3_competitive,
                                      Prescriptions_women_agency_4_competent,
                                      Prescriptions_women_agency_5_confident,
                                      Prescriptions_women_agency_6_has_leadership_abilities,
                                      Prescriptions_women_agency_7_efficient,
                                      Prescriptions_women_agency_8_determined,
                                      Prescriptions_women_agency_9_courageous,
                                      Prescriptions_women_agency_10_active,
                                      Prescriptions_women_agency_11_capable,
                                      Prescriptions_women_agency_12_independent)),
  PRESCRIPTIONS_MEN_COMMUNION = mean(c(Prescriptions_men_communion_1_compassionate,
                                       Prescriptions_men_communion_2_helpful_to_others,Prescriptions_men_communion_3_sympathetic,
                                       Prescriptions_men_communion_4_understanding_of_others,Prescriptions_men_communion_5_sensitive,
                                       Prescriptions_men_communion_6_soft_hearted,Prescriptions_men_communion_7_aware_of_others_feelings,
                                       Prescriptions_men_communion_8_cooperative,Prescriptions_men_communion_9_devoted_to_others,
                                       Prescriptions_men_communion_10_trusting,Prescriptions_men_communion_11_warm,
                                       Prescriptions_men_communion_12_supportive)),
  PRESCRIPTIONS_MEN_WEAKNESS = mean (c(Prescriptions_men_weakness_1_worrying,
                                        Prescriptions_men_weakness_2_weak,Prescriptions_men_weakness_3_timid,
                                        Prescriptions_men_weakness_4_submissive,Prescriptions_men_weakness_5_fearful,
                                        Prescriptions_men_weakness_6_cowardly,Prescriptions_men_weakness_7_dependent,
                                        Prescriptions_men_weakness_8_infantile,Prescriptions_men_weakness_9_uncertain,
                                        Prescriptions_men_weakness_10_approval_seeking,
                                        Prescriptions_men_weakness_11_subordinates_self_to_others,Prescriptions_men_weakness_12_insecure)),
  PRESCRIPTIONS_MEN_DOMINANCE = mean(c(Prescriptions_men_dominance_1_demanding,
                                         Prescriptions_men_dominance_2_controlling,Prescriptions_men_dominance_3_bossy,
                                         Prescriptions_men_dominance_4_dominant,Prescriptions_men_dominance_5_intimadting,
                                         Prescriptions_men_dominance_6_feels_superior,Prescriptions_men_dominance_8_dictatorial,
                                         Prescriptions_men_dominance_7_forceful,Prescriptions_men_dominance_9_aggressive,
                                         Prescriptions_men_dominance_10_stubborn,Prescriptions_men_dominance_11_arrogant,
                                         Prescriptions_men_dominance_12_boastful)),
  PRESCRIPTIONS_MEN_AGENCY = mean (c(Prescriptions_men_agency_1_decisive,
                                      Prescriptions_men_agency_2_ambitious,Prescriptions_men_agency_3_competitive,
                                      Prescriptions_men_agency_4_competent,Prescriptions_men_agency_5_confident,
                                      Prescriptions_men_agency_6_has_leadership_abilities,Prescriptions_men_agency_7_efficient,
                                      Prescriptions_men_agency_8_determined,Prescriptions_men_agency_9_courageous,
                                      Prescriptions_men_agency_10_active,Prescriptions_men_agency_11_capable,
                                      Prescriptions_men_agency_12_independent)),
  GENDERSTEREOTYPES_COMMUNION = mean(c(GenderStereotypes_communion_1_compassionate,
                                         GenderStereotypes_communion_2_helpful_to_others,GenderStereotypes_communion_3_sympathetic,
                                         GenderStereotypes_communion_4_understanding_of_others,GenderStereotypes_communion_5_sensitive,
                                         GenderStereotypes_communion_6_soft_hearted,GenderStereotypes_communion_7_aware_of_others_feelings,
                                         GenderStereotypes_communion_8_cooperative,GenderStereotypes_communion_9_devoted_to_others,
                                         GenderStereotypes_communion_10_trusting,GenderStereotypes_communion_11_warm,
                                         GenderStereotypes_communion_12_supportive)),
  GENDERSTEREOTYPES_WEAKNESS = mean(c(GenderStereotypes_weakness_1_worrying,
                                        GenderStereotypes_weakness_2_weak,GenderStereotypes_weakness_3_timid,
                                        GenderStereotypes_weakness_4_submissive,GenderStereotypes_weakness_5_fearful,
                                        GenderStereotypes_weakness_6_cowardly,GenderStereotypes_weakness_7_dependent,
                                        GenderStereotypes_weakness_8_infantile,GenderStereotypes_weakness_9_uncertain,
                                        GenderStereotypes_weakness_10_approval_seeking,
                                        GenderStereotypes_weakness_11_subordinates_self_to_others,GenderStereotypes_weakness_12_insecure)),
  GENDERSTEREOTYPES_DOMINANCE = mean(c(GenderStereotypes_dominance_1_demanding,
                                         GenderStereotypes_dominance_2_controlling,GenderStereotypes_dominance_3_bossy,
                                         GenderStereotypes_dominance_4_dominant,GenderStereotypes_dominance_5_intimadting,
                                         GenderStereotypes_dominance_6_feels_superior,GenderStereotypes_dominance_7_forceful,
                                         GenderStereotypes_dominance_8_dictatorial,GenderStereotypes_dominance_9_aggressive,
                                         GenderStereotypes_dominance_10_stubborn,GenderStereotypes_dominance_11_arrogant,
                                         GenderStereotypes_dominance_12_boastful)),
  GENDERSTEREOTYPES_AGENCY = mean(c(GenderStereotypes_agency_1_decisive,
                                      GenderStereotypes_agency_2_ambitious,GenderStereotypes_agency_3_competitive,
                                      GenderStereotypes_agency_4_competent,GenderStereotypes_agency_5_confident,
                                      GenderStereotypes_agency_6_has_leadership_abilities,GenderStereotypes_agency_7_efficient,
                                      GenderStereotypes_agency_8_determined,GenderStereotypes_agency_9_courageous,
                                      GenderStereotypes_agency_10_active,GenderStereotypes_agency_11_capable,
                                      GenderStereotypes_agency_12_independent))
  )

names(data_scores)

# sources:

#PRECARIOUS_MANHOOD; 4 items; Vandello, J. A., Bosson, J. K., Cohen, D., Burnaford, R. M., & Weaver, J. R. (2008)
#Precarious manhood. Journal of Personality and Social Psychology, 95, 1325–1339. doi:10.1037/a0012453.

#GENDER EQUALITY COLLECTIVE ACTION INTENSION SCALE (GECAI); 6 items;based on Alisat, S. Reimer, M. (2015) The environmental action scale: Development and psychometric evaluation
#Journal of Environmental Psychology, 43, 13-23. http://dx.doi.org/10.1016/j.jenvp.2015.05.006.

#COLLECTIVE ACTION INTENSION (CAI) NORMATIVE AND RADICAL; 2 X 2 items; Tausch et all (2011) Explaining radical group behavior
#Developing emotion and efficacy routes to normative and nonnormative collective action. Journal of Personality and Social Psychology, 101(1), 129-148.

#ZEROSUM perspective with regard to gender equality; 7 items; Ruthig et al. (2017) When Women’s Gains Equal Men’s Losses: 
#  Predicting a Zero-Sum Perspective of Gender Status, Sex Roles, 72(1–2), 17–26. DOI: https://doi.org/10.1007/s11199-016-0651-9.

#Ambivalent sexism: hostile and benevolent; 2 x 3 items; Glick & Fiske (1996).

#AMBIVALENCE_TOWARDS_MEN; 6 items; Rollero et al (2014) Psychometric properties of short versions of the 
#Ambivalent Sexism Inventory and Ambivalence Toward Men Inventory. TPM - Testing, Psychometrics, 
#Methodology in Applied Psychology, 21, 149-159. 10.4473/TPM21.2.3.

#POWER_DISTANCE_BELIEFS; 4 items; Brockner et al, 2001.

#GENDER_ESSENTIALISM; 3 items; Skewes et al, 2018.

#PVQ_AUTONONOMY; 5 items; Vignoles et al.. (2016). Beyond the ‘East–West’ Dichotomy:
#  Global Variation in Cultural Models of Selfhood

#PVQ_EMBEDDEDNESS; 5 items; Vignoles et al.. (2016). Beyond the ‘East–West’ Dichotomy:
#  Global Variation in Cultural Models of Selfhood

#AWARENESS_GENDER_INEQUALITY; 1 item; Glick & Whitehead (2010).

#Political_orientation_tradition; 1 item; Brandt & Reyna (2017).

#Political_orientation_equality; 1 item; Brandt & Reyna (2017).

#Feminine_identity; 1 item; van Breen et al (2017).

#Masculine_identity; 1 item; van Breen et al (2017).

#Identification_with_gender_group; 1 item; van Breen et al (2017).

#Compute SELF-COSTRUALS,PRESCRIPTIONS,DESCRIPTIONS.
