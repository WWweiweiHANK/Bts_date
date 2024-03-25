* Encoding: UTF-8.

GET FILE = 'D:\COVID-Intl-Study\New data\Person\JN_individual_level_v1.sav'.
dataset name person.
sort cases by iso3(a).
compute idnum = $casenum.

compute Contct4M = mean(contact1, contact3 to contact5).
compute HygieneM = mean(hygiene1 to hygiene5).
compute PSupportM = mean(psupport1 to psupport5).

compute CnarcM = mean(cnarc1 to cnarc3).
compute NIdentM = mean(nidentity1, nidentity2).
compute ConspirM = mean(ctheory1 to ctheory4).
compute BelongM = mean(sbelong1 to sbelong4).

*Open mindedness.
compute Open_OM = mean(omind2 to omind4).
compute Close_OM = mean(omind1, omind5, omind6).
compute Total_OM = mean(Open_OM, 10-Close_OM).

* Moral ID.
compute moralid7 = 10 - moralid7.
compute Int_MID = mean(moralid1,moralid2,moralid7,moralid10).
compute Sym_MID = mean(moralid3,moralid5,moralid6,moralid8,moralid9).
compute Tot_MID = mean(moralid1, moralid10, moralid2 to moralid9).

*For moral id, a multilevel EFA also finds two factors,
items 1, 2, 7, and 10: corresponding to feel good, part of who I am, not important, want these characteristics
items 3, 5, 6, 8, and 9: corresponding to wear clothes, hobbies, reading material, memberships, activities
Item 4 (ashamed) does not fit well with the other items, it is split
These two factors clearly correspond to an internal factor and an appearance factor, described by Aquino and Read (2002) as internalization and symbolization..

variable labels
Contct4M 'Mean of Contact items 1, 3, 4, 5'
HygieneM 'Mean of all (5) hygiene items'
PsupportM 'Mean of all (5) support items'
CnarcM 'Mean all (3) collective narcissism items'
NidentM 'Mean all (2) national identity items'
ConspirM 'Mean all (4) conspiracy items'
BelongM 'Mean all (4) social belonging items'
Open_OM 'Open open mindedness 2, 3, 4'
Close_OM 'Close open mindedness 1, 5, 6'
Total_OM 'Total open mindedness'
Int_MID 'Internalized.moral id 1, 2, 7 10'
Sym_MID 'Symbolic moral id 3, 5, 6, 8, 9'
Tot_MID 'Total moral id -- all ten items'.

get file = 'D:\COVID-Intl-Study\New data\Country\JN_country_level_v1-jbn.sav'
/keep = iso3 UN.
sort cases by iso3(a).
dataset name country.

match files file = person
/table = country
/by iso3.

dataset name person2.
sort cases by un (a).
dataset close country.

SAVE OUTFILE = 'D:\COVID-Intl-Study\New data\Person\Person level-score.sav'
/keep = un idnum iso3, country, age,sex1,
Contct4M to Tot_MID, political_ideology,Self_Esteem, 
contact1 to contact5
hygiene1 to hygiene5
psupport1 to psupport5
cnarc1 to cnarc3
nidentity1 nidentity2
moralid1 moralid2 to moralid9 moralid10
omind1 to omind6.

dataset close person.

get file = 'D:\COVID-Intl-Study\New data\Country\JN_country_level_v1-jbn.sav'.
sort cases by un (a).
save outfile = 'D:\COVID-Intl-Study\New data\Country\JN_country_level_v1-jbn.sav'.

execute.
