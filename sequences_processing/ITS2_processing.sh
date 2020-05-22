#!/bin/sh

qiime2.sif tools import --type 'SampleData[PairedEndSequencesWithQuality]' --input-path reads/ --input-format CasavaOneEightSingleLanePerSampleDirFmt --output-path jrl2020.qza

mkdir ITS2

qiime2.sif cutadapt trim-paired --p-front-f file:ITS2/ITS2_forward.fas --p-front-r file:ITS2/ITS2_reverse.fas --p-error-rate 0.05 --p-times 1 --p-match-adapter-wildcards TRUE --verbose --p-minimum-length 100 --p-discard-untrimmed TRUE --i-demultiplexed-sequences jrl2020.qza --o-trimmed-sequences ITS2/ITS2_trimmed_jrl2020.qza

qiime2.sif tools export --input-path 16S/16S_trimmed_jrl2020.qza --output-path 16S/16S_reads/

qiime2.sif dada2 denoise-paired --i-demultiplexed-seqs ITS2/ITS2_trimmed_jrl2020.qza --p-trunc-len-f 180 --p-trunc-len-r 180 --p-chimera-method 'consensus' --p-n-reads-learn 10000000 --p-hashed-feature-ids TRUE --o-table ITS2/ITS2_ASVS-table_jrl2020.qza --o-representative-sequences ITS2/ITS2_ASVS-sequences_jrl2020.qza --o-denoising-stats ITS2/ITS2_denoising-stats_jrl2020.qza --verbose

qiime2.sif   feature-classifier classify-sklearn --i-classifier unite8.2_dynamic-classifier.qza --i-reads ITS2/ITS2_ASVS-sequences_jrl2020.qza --p-reads-per-batch 100 --p-n-jobs 1 --o-classification ITS2/ITS2-Taxonomy__unite8.2_jrl2020.qza

qiime2.sif tools export --input-path ITS2/ITS2_denoising-stats_jrl2020.qza --output-path ITS2/ITS2_export_output_jrl2020

mv ITS2/ITS2_export_output_jrl2020/stats.tsv ITS2/ITS2_export_output_jrl2020/ITS2_stats_jrl2020.tsv

qiime2.sif tools export --input-path ITS2/ITS2-Taxonomy_unite8.2_jrl2020.qza --output-path ITS2/ITS2_export_output_jrl2020

mv ITS2/ITS2_export_output_jrl2020/taxonomy.tsv ITS2/ITS2_export_output_jrl2020/ITS2_taxonomy_jrl2020.tsv

qiime2.sif tools export --input-path ITS2/ITS2_ASVS-table_jrl2020.qza --output-path ITS2/ITS2_export_output_jrl2020

# from within the qiime2.sif container 
biom convert -i ITS2/ITS2_dada2_output_jrl2020/feature-table.biom -o ITS2/ITS2_dada2_output_jrl2020/ITS2_feature-table_jrl2020.tsv --to-tsv --table-type "OTU table"

mv ITS2/ITS2_export_output_jrl2020/feature-table.tsv ITS2/ITS2_export_output_jrl2020/ITS2_feature-table_jrl2020.tsv
