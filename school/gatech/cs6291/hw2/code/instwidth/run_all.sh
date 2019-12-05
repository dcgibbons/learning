echo "Running imgpipe_2_wide..."
./run_imgpipe_2_wide.sh > imgpipe_2_wide_cycles.txt
echo "Running imgpipe_4_wide..."
./run_imgpipe_4_wide.sh > imgpipe_4_wide_cycles.txt
echo "Running madplay_2_wide..."
./run_madplay_2_wide.sh > madplay_2_wide_cycles.txt
echo "Running madplay_4_wide..."
./run_madplay_4_wide.sh > madplay_4_wide_cycles.txt
echo "Running susan_2_wide..."
./run_susan_2_wide.sh > susan_2_wide_cycles.txt
echo "Running susan_4_wide..."
./run_susan_4_wide.sh > susan_4_wide_cycles.txt
echo "Done"
