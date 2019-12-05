#! /bin/sh
./imgpipe_4_wide -s 2 -o pasta.ppm pasta.jpg; \
echo "#####################"; \
echo "        STATS        "; \
echo "#####################"; \
head -1 ta.log.000; \
rm gmon* ta.log*
