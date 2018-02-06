raw_purgedata = read.csv('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Derico Setyabrata_Meat Metabolomics_ALL/Purge Metabolomics_ALL/Purge metabolomics_ALL_Raw.tsv', sep = '\t')
norm_purgedata = read.csv('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Derico Setyabrata_Meat Metabolomics_ALL/Purge Metabolomics_ALL/Purge Metabolomics_ALL_Normalized.tsv', sep = '\t')

raw_purgedata$Compound = gsub('@.*$','',raw_purgedata$Compound)
norm_purgedata$Compound = gsub('@.*$','',norm_purgedata$Compound)

raw_purgedata = raw_purgedata[,1:37]
norm_purgedata = norm_purgedata[,1:37]

raw_purgedata = raw_purgedata[,c(1,26:37,2:13,14:25)]
norm_purgedata = norm_purgedata[,c(1,26:37,2:13,14:25)]

log_purgedata = raw_purgedata
log_purgedata[,2:37] = log(raw_purgedata[,2:37],2)



# Data has 6 animals = 1, 3, 4, 5, 7, 8
# Each animal has 2 muscles = LD, Both
# Each muscle goes through 3 days = 9, 16, 23 days

source('/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/C_Timeonly_Functions.R')
library(PerformanceAnalytics)
library(ggbiplot)

#------------ Correlation ------------#

# Sample-wise
cplot(raw_purgedata[,c(2,14,26,8,20,32)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Sample_1')
cplot(raw_purgedata[,c(3,15,27,9,21,33)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Sample_3')
cplot(raw_purgedata[,c(4,16,28,10,22,34)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Sample_4')
cplot(raw_purgedata[,c(5,17,29,11,23,35)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Sample_5')
cplot(raw_purgedata[,c(6,18,30,12,24,36)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Sample_7')
cplot(raw_purgedata[,c(7,19,31,13,25,37)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Sample_8')

cplot(norm_purgedata[,c(2,14,26,8,20,32)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Sample_1')
cplot(norm_purgedata[,c(3,15,27,9,21,33)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Sample_3')
cplot(norm_purgedata[,c(4,16,28,10,22,34)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Sample_4')
cplot(norm_purgedata[,c(5,17,29,11,23,35)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Sample_5')
cplot(norm_purgedata[,c(6,18,30,12,24,36)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Sample_7')
cplot(norm_purgedata[,c(7,19,31,13,25,37)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Sample_8')

cplot(log_purgedata[,c(2,14,26,8,20,32)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Sample_1')
cplot(log_purgedata[,c(3,15,27,9,21,33)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Sample_3')
cplot(log_purgedata[,c(4,16,28,10,22,34)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Sample_4')
cplot(log_purgedata[,c(5,17,29,11,23,35)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Sample_5')
cplot(log_purgedata[,c(6,18,30,12,24,36)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Sample_7')
cplot(log_purgedata[,c(7,19,31,13,25,37)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Sample_8')

# Day-wise
cplot(raw_purgedata[,c(2:13)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Day_9')
cplot(raw_purgedata[,c(14:25)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Day_16')
cplot(raw_purgedata[,c(26:37)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Raw/Both/Raw_Day_23')

cplot(norm_purgedata[,c(2:13)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Day_9')
cplot(norm_purgedata[,c(14:25)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Day_16')
cplot(norm_purgedata[,c(26:37)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Normalized/Both/Norm_Day_23')

cplot(log_purgedata[,c(2:13)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Day_9')
cplot(log_purgedata[,c(14:25)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Day_16')
cplot(log_purgedata[,c(26:37)],name = '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/Correlations/Log/Both/Log_Day_23')


#------------ Controlled for only time ------------#
raw_time = get_values_TimeOnly(raw_purgedata)
norm_time = get_values_TimeOnly(norm_purgedata)
log_time = get_values_TimeOnly(log_purgedata)

write.table(raw_time, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/Both/raw_time_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(norm_time, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/Both/normalized_time_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(log_time, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/Both/log_time_controlled.tsv', sep = '\t', row.names = FALSE)

#------------ Controlled for only sample ------------#
raw_sample = get_values_SampleOnly(raw_purgedata)
norm_sample = get_values_SampleOnly(norm_purgedata)
log_sample = get_values_SampleOnly(log_purgedata)

write.table(raw_sample, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/Both/raw_sample_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(norm_sample, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/Both/normalized_sample_controlled.tsv', sep = '\t', row.names = FALSE)
write.table(log_sample, '/media/vsppraneeth/01D3522569C0B1A0/Work/Meat Metabolics/Praneeth/Purge/Purge Results 2/OutputFiles/Both/log_sample_controlled.tsv', sep = '\t', row.names = FALSE)
