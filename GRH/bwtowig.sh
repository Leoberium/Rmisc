#!/bin/bash
for filename in ~/BioData/ATAC/DGRPlinesBW/*.bw; do
	~/Software/kent/bigWigToWig "$filename" "$filename.wig"
	echo "$(basename "$filename") processed"
done
