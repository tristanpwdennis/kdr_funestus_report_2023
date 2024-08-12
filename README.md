This project was a big IR phenotype and genomic surveillance study in Tanzania. An. funestus were collected from across Tanzania, phenotyped for IR, and genome sequenced to identify selection at IR loci, and IR allele frequencies, as well as population structure and history. The IR phenotype surveillance data is published in [Joel's paper here](https://link.springer.com/article/10.1186/s13071-024-06315-4). These data showed that mosquitoes were resistant to DDT - strange, given DDT is an obsolete, banned, pesticide. 

Genome sequencing data collected and analysed as part of this project showed evidence of weak selection at the _Vgsc_ locus, in samples from a single region in Tanzania. Further examination of the _Vgsc_ locus revealed that _knock-down resistance_, in the form of the L976F (995S/F in _An. gambiae_ and 1014F/S in _M. domestica_), had appeared in _An. funestus_ where, hitherto, IR had only emerged in the form of metabolic resistance through detoxification enzymes and others. _Kdr_ in _An. funestus_ appeared often in concert with another linked mutation, P1842S, and appeared as part of a weak selective sweep in Morogoro region. We found that _Kdr_ appeared to confer resistance to DDT, and not to other, more widely used vector control pesticides.

This repository contains the code used to analyse the data generated in this analysis. The R-markdown contains:
1. Map of sequenced samples.
2. The statistical models associating DDT resistance phenotypes with _Kdr_.
3. Plots of the previously published bioassay data.
4. Allele frequency by location and timepoint, showing the frequency of _Vsgc_ mutations, and a possible decline in frequency over time in Morogoro region.
5. LD heatmap plots showing that 976F and 1842S occur as linked haplotypes.

The jupyter notebook contains:
1. The H12 and G123 selection scans showing signatures of a weak selective sweep around the _Vgsc_ locus in Morogoro region.
2. The code generating the raw data for the heatmaps (plotted in R as I prefer plotting heatmaps in R).
3. The haplotype clustering dendrogram showing that the linked P976F/1842S haplotype is responsible for the weak sweep in Morogoro region.

If you wish to replicate the analysis, clone this repo, and run the jupyter notebook first. Make sure [you are able to access malariagen_data](https://malariagen.github.io/vector-data/vobs/vobs-data-access.html). The notebook should run and download the data required to plot the rest of the analyses in the RMarkdown.

Apologies for the two different plotting notebooks. I find myself using Python increasingly, but I am still a fan of R when it comes to statistics and heatmaps!

Tristan Dennis, Aug 2024.
