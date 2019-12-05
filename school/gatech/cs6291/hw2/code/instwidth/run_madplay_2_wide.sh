#! /bin/sh
./madplay_2_wide -v test.mp3 -s 10 -t 0:0:10 -o test.out.pcm; \
echo "#####################"; \
echo "        STATS        "; \
echo "#####################"; \
head -1 ta.log.000; \
rm gmon* ta.log* *.pcm
