#!/bin/sh

qiime2.sif tools import --type 'SampleData[PairedEndSequencesWithQuality]' --input-path reads/ --input-format CasavaOneEightSingleLanePerSampleDirFmt --output-path jrl2020.qza

mkdir 16S

qiime2.sif cutadapt trim-paired --p-front-f file:16S/16S_forward.fas --p-front-r file:16S/16S_reverse.fas --p-error-rate 0.05 --p-times 1 --p-match-adapter-wildcards TRUE --verbose --p-minimum-length 100 --p-discard-untrimmed TRUE --i-demultiplexed-sequences jrl2020.qza --o-trimmed-sequences 16S/16S_trimmed_jrl2020.qza

qiime2.sif tools export --input-path 16S/16S_trimmed_jrl2020.qza --output-path 16S/16S_reads/

qiime2.sif dada2 denoise-paired --i-demultiplexed-seqs 16S/16S_trimmed_jrl2020.qza --p-trunc-len-f 180 --p-trunc-len-r 180 --p-chimera-method 'consensus' --p-n-reads-learn 10000000 --p-hashed-feature-ids TRUE --o-table 16S/16S_ASVS-table_jrl2020.qza --o-representative-sequences 16S/16S_ASVS-sequences_jrl2020.qza --o-denoising-stats 16S/16S_denoising-stats_jrl2020.qza --verbose

qiime2.sif   feature-classifier classify-sklearn --i-classifier Silva138_classifier.qza --i-reads 16S/16S_ASVS-sequences_jrl2020.qza --p-reads-per-batch 100 --p-n-jobs 1 --o-classification 16S/16S-Taxonomy_silva138_jrl2020.qza

qiime2.sif tools export --input-path 16S/16S_denoising-stats_jrl2020.qza --output-path 16S/16S_export_output_jrl2020

mv 16S/16S_export_output_jrl2020/stats.tsv 16S/16S_export_output_jrl2020/16S_stats_jrl2020.tsv

qiime2.sif tools export --input-path 16S/16S-Taxonomy_silva138_jrl2020.qza --output-path 16S/16S_export_output_jrl2020

mv 16S/16S_export_output_jrl2020/taxonomy.tsv 16S/16S_export_output_jrl2020/16S_taxonomy_jrl2020.tsv

qiime2.sif tools export --input-path 16S/16S_ASVS-table_jrl2020.qza --output-path 16S/16S_export_output_jrl2020

# from within the qiime2.sif container 
biom convert -i 16S/16S_dada2_output_jrl2020/feature-table.biom -o 16S/16S_dada2_output_jrl2020/16S_feature-table_jrl2020.tsv --to-tsv --table-type "OTU table"

mv 16S/16S_export_output_jrl2020/feature-table.tsv 16S/16S_export_output_jrl2020/16S_feature-table_jrl2020.tsv
