#!/bin/bash
for filename in ~/BioData/ATAC/DGRPlinesWIG/*.wig; do
	where="$(basename $filename .bw.wig).bed"	
	wig2bed -x < $filename > $where

done
