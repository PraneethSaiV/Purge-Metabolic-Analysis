raw_purgedata = read.csv('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Derico Setyabrata_Meat Metabolomics_ALL/Purge Metabolomics_ALL/Purge metabolomics_ALL_Raw.tsv', sep = '\t')
norm_purgedata = read.csv('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Derico Setyabrata_Meat Metabolomics_ALL/Purge Metabolomics_ALL/Purge Metabolomics_ALL_Normalized.tsv', sep = '\t')

raw_purgedata$Compound = gsub('@.*$','',raw_purgedata$Compound)
norm_purgedata$Compound = gsub('@.*$','',norm_purgedata$Compound)

pm_column_numbers = c(1,8,9,10,11,12,13,20,21,22,23,24,25,32,33,34,35,36,37)
raw_purgedata = raw_purgedata[,pm_column_numbers]
norm_purgedata = norm_purgedata[,pm_column_numbers]

raw_purgedata = raw_purgedata[,c(1,14:19,2:7,8:13)]
norm_purgedata = norm_purgedata[,c(1,14:19,2:7,8:13)]

log_purgedata = raw_purgedata
log_purgedata[,2:19] = log(raw_purgedata[,2:19],2)



# Data has 6 animals = 1, 3, 4, 5, 7, 8
# Each animal has 2 muscles = LD, PM
# Each muscle goes through 3 days = 9, 16, 23 days

source('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/C_Timeonly_Functions.R')
library(PerformanceAnalytics)
library(ggbiplot)

#------------ Correlation ------------#

# Sample-wise
cplot(raw_purgedata[,c(2,8,14)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Sample_1')
cplot(raw_purgedata[,c(3,9,15)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Sample_3')
cplot(raw_purgedata[,c(4,10,16)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Sample_4')
cplot(raw_purgedata[,c(5,11,17)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Sample_5')
cplot(raw_purgedata[,c(6,12,18)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Sample_7')
cplot(raw_purgedata[,c(7,13,19)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Sample_8')

cplot(norm_purgedata[,c(2,8,14)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Sample_1')
cplot(norm_purgedata[,c(3,9,15)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Sample_3')
cplot(norm_purgedata[,c(4,10,16)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Sample_4')
cplot(norm_purgedata[,c(5,11,17)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Sample_5')
cplot(norm_purgedata[,c(6,12,18)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Sample_7')
cplot(norm_purgedata[,c(7,13,19)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Sample_8')

cplot(log_purgedata[,c(2,8,14)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Sample_1')
cplot(log_purgedata[,c(3,9,15)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Sample_3')
cplot(log_purgedata[,c(4,10,16)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Sample_4')
cplot(log_purgedata[,c(5,11,17)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Sample_5')
cplot(log_purgedata[,c(6,12,18)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Sample_7')
cplot(log_purgedata[,c(7,13,19)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Sample_8')

# Day-wise
cplot(raw_purgedata[,c(2:7)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Day_9')
cplot(raw_purgedata[,c(8:13)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Day_16')
cplot(raw_purgedata[,c(14:19)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/PM/Raw_Day_23')

cplot(norm_purgedata[,c(2:7)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Day_9')
cplot(norm_purgedata[,c(8:13)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Day_16')
cplot(norm_purgedata[,c(14:19)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/PM/Norm_Day_23')

cplot(log_purgedata[,c(2:7)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Day_9')
cplot(log_purgedata[,c(8:13)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Day_16')
cplot(log_purgedata[,c(14:19)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/PM/Log_Day_23')


#------------ Controlled for only time ------------#
raw_time = get_values_TimeOnly(raw_purgedata)
norm_time = get_values_TimeOnly(norm_purgedata)
log_time = get_values_TimeOnly(log_purgedata)

write.table(raw_time, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/PM/raw_time_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(norm_time, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/PM/normalized_time_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(log_time, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/PM/log_time_controlled.tsv', sep = '\t', row.names = FALSE)

#------------ Controlled for only sample ------------#
raw_sample = get_values_SampleOnly(raw_purgedata)
norm_sample = get_values_SampleOnly(norm_purgedata)
log_sample = get_values_SampleOnly(log_purgedata)

write.table(raw_sample, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/PM/raw_sample_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(norm_sample, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/PM/normalized_sample_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(log_sample, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/PM/log_sample_controlled.tsv', sep = '\t', row.names = FALSE)
