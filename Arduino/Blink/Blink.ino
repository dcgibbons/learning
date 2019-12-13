/*
  Blink
 Turns on an LED on for one second, then off for one second, repeatedly.
 
 This example code is in the public domain.
 */

// Pin 13 has an LED connected on most Arduino boards.
// give it a name:
int ledPin = 13;
int redPin = 12;
int greenPin = 11;
int bluePin = 10;

// the setup routine runs once when you press reset:
void setup() {                
  // initialize the digital pin as an output.
  pinMode(ledPin, OUTPUT);
  pinMode(redPin, OUTPUT);     
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);
}

// the loop routine runs over and over again forever:
void loop() {
  digitalWrite(ledPin, HIGH);
  digitalWrite(redPin, HIGH);   // turn the LED on (HIGH is the voltage level)
  delay(500);  
  digitalWrite(greenPin, HIGH);
  digitalWrite(redPin, LOW);
  delay(500);
  digitalWrite(bluePin, HIGH);
  digitalWrite(greenPin, LOW);
  delay(500);               // wait for a second
  digitalWrite(redPin, LOW);    // turn the LED off by making the voltage LOW
  delay(500);
  digitalWrite(greenPin, LOW);
  delay(500);
  digitalWrite(bluePin, LOW);
  digitalWrite(ledPin, LOW);
  delay(500);               // wait for a second
}

