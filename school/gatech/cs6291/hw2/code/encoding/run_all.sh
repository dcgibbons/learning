make clean
make all
echo bitcnts_8_wide
./vliwEncoder bitcnts_8_wide.s
mv maskedEncoding.txt bitcnts_8_wide_maskedEncoding.txt
mv templateEncoding.txt bitcnts_8_wide_templateEncoding.txt
echo bitcnts_4_wide
./vliwEncoder bitcnts_4_wide.s
mv maskedEncoding.txt bitcnts_4_wide_maskedEncoding.txt
mv templateEncoding.txt bitcnts_4_wide_templateEncoding.txt
echo basicmath_4_wide
./vliwEncoder basicmath_4_wide.s
mv maskedEncoding.txt basicmath_4_wide_maskedEncoding.txt
mv templateEncoding.txt basicmath_4_wide_templateEncoding.txt
echo basicmath_8_wide
./vliwEncoder basicmath_8_wide.s
mv maskedEncoding.txt basicmath_8_wide_maskedEncoding.txt
mv templateEncoding.txt basicmath_8_wide_templateEncoding.txt
