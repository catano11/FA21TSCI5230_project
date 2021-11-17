import pandas as pd
census2015 = pd.read_excel('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_2015.xlsx');
ahrqCodebook2015 = pd.read_excel('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/sdoh_codebook_final.xlsx',sheet_name='ZCTA_2015');
sdi = pd.read_excel('https://www.graham-center.org/content/dam/rgc/documents/maps-data-tools/sdi/ACS2015_zctaallvars.xlsx');
outcomes = pd.read_csv('data/OutcomeData.csv');
