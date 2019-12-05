#include "Fuel.h"

class CMileage : public CFuel
{ public:
	int mileage;
	int mpg;
        
  CMileage()
  {
    mpg = (mileage / gallons);
  }
};
