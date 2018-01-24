raw_purgedata = read.csv('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Derico Setyabrata_Meat Metabolomics_ALL/Purge Metabolomics_ALL/Purge metabolomics_ALL_Raw.tsv', sep = '\t')
norm_purgedata = read.csv('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Derico Setyabrata_Meat Metabolomics_ALL/Purge Metabolomics_ALL/Purge metabolomics_ALL_Raw.tsv', sep = '\t')

raw_purgedata$Compound = gsub('@.*$','',raw_purgedata$Compound)
norm_purgedata$Compound = gsub('@.*$','',norm_purgedata$Compound)

ld_column_numbers = c(1,2,3,4,5,6,7,14,15,16,17,18,19,26,27,28,29,30,31)
raw_purgedata = raw_purgedata[,ld_column_numbers]
norm_purgedata = norm_purgedata[,ld_column_numbers]

raw_purgedata = raw_purgedata[,c(1,14:19,2:7,8:13)]
norm_purgedata = norm_purgedata[,c(1,14:19,2:7,8:13)]

log_purgedata = raw_purgedata
log_purgedata[,2:19] = log(raw_purgedata[,2:19],2)



# Data has 6 animals = 1, 3, 4, 5, 7, 8
# Each animal has 2 muscles = LD, PM
# Each muscle goes through 3 days = 9, 16, 23 days

source('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/C_Timeonly_Functions.R')

#------------ Each sample controlled for only time ------------#
raw_temp = get_values(raw_purgedata)
norm_temp = get_values(norm_purgedata)
log_temp = get_values(log_purgedata)
















