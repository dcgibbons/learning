#include <iostream>
#include <iterator>
#include <map>

using namespace std;

int main()
{
  typedef map<string, string> AddressMap;

  AddressMap addresses;
  addresses["davek"] = "david.kensfield@gmail.com";
  addresses["nikki"] = "nikkitaylor@yahoo.com";
  addresses["angie"] = "angela.jones@gmail.com";
  addresses["billg"] = "billg@microsoft.com";

  AddressMap::iterator myIter;
  for (myIter = addresses.begin(); myIter != addresses.end(); myIter++)
  {
    const string& nickname = myIter->first;
    const string& email = myIter->second;

    cout << "Nickname: \t" << nickname 
         << " \tEmail: \t" << email
         << endl; 
  }
}

