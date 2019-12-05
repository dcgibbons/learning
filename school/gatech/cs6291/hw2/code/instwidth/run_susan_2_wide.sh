#! /bin/sh
./susan_2_wide input_large.pgm output_large.smoothing.pgm -s
echo "#####################"; \
echo "        STATS        "; \
echo "#####################"; \
head -1 ta.log.000; \
rm gmon* ta.log* output_large.smoothing.pgm
