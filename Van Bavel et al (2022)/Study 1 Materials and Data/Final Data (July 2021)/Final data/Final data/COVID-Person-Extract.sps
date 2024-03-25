* Encoding: UTF-8.
get FILE = 'D:\COVID-Intl-Study\New data\Person\Person level-score.sav'.

select if nmiss(Contct4M to NIdentM, political_ideology) ne 6.

SAVE OUTFILE = 'D:\COVID-Intl-Study\New data\Person\Person level-Extract.sav'
/keep = un idnum iso3, country, age, sex1
Contct4M to NIdentM, political_ideology
contact1 to contact5
hygiene1 to hygiene5
psupport1 to psupport5
cnarc1 to cnarc3
nidentity1 nidentity2.

execute.

