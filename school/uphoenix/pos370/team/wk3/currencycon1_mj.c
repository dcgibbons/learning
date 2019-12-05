/*Currency Exchange Program */
//by Marilyn Johnson
//POS 370
//Week 3 - Currency Exchange Program

#include <stdio.h>
#include <math.h>
int multiplyTwoNumbers(int, int); //function prototype

main()
{
// the following exchange rates were obtained from
  // http://finance.yahoo.com/currency on October 26, 2005
	float fcand, feuro, ffrf, fjpy, fnlg, usd;
	fcand = 1.17050;
	feuro = 0.8236;
	ffrf = 5.40322;
	fjpy = 115.458;
	fnlg = 1.815;
	usd = 25.00;
	can = multiplyTwoNumbers(usd, fcand);
	euro = multiplyTwoNumbers(usd, feuro);
	frf = multiplyTwoNumbers(usd, ffrf);
	yen = multiplyTwoNumbers(usd, fyen);
	nlg = multiplyTwoNumbers(usd, fnlg);
	
	printf("\nCurrency Conversions\n");
	printf("U.S.\tCanadian\tEurope\tFrench\tJapanese\tNetherlands\n");
	printf("Dollar\tDollar\t\tEuro\tDollar\t\tFrancs\t\tDollar\n");
	printf(" 1.00\t1.17050\t0.8236\t5.40322\t\t115.458\t\t1.815\n");
	printf("1.00\t%f\t%f\t%f\t%f\t\t%f\t\t%f\n", fcand, feuro, ffrf, fjpy, fnlg);
	printf("%.2f\n", usd);
	//printf("%.3f\t%.3f\t%.3f\t%.3f\t\t%.3f\t\t%.3f\n", usd, cand, euro, frf, jpy, nlg);

		getchar();	
}

//math definition
float multiplyTwoNumbers(float num1, float num2);
{
	return num1 * num2;
}
