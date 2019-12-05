class Money {
  public:
    Money(long cents);
    Money(double val);
    Money(const Money& other);

    Money operator +(const Money& other);
    Money operator -(const Money& other);
    bool operator ==(const Money& other);

  private:
    long cents;
};

Money::Money(long cents) {
  this->cents = cents;
}

Money::Money(double val) {
  cents = static_cast<long>(val * 100.0);
}

Money::Money(const Money& other) {
  cents = other.cents;
}

Money Money::operator +(const Money& other) {
  Money m(cents + other.cents);
  return m;
}

Money Money::operator -(const Money& other) {
  Money m(cents - other.cents);
  return m;
}


int main() {
  Money a(3.35);
  Money b(2.75);
  Money c = a + b;
  Money d = a - c;
}


