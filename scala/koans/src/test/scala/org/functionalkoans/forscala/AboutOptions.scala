package org.functionalkoans.forscala

import support.KoanSuite

class AboutOptions extends KoanSuite {

  koan("Option can have one of two values - Some or None") {
    val someValue: Option[String] = Some("I am wrapped in something")
    someValue.get should be(__)

    val nullValue: Option[String] = None
    nullValue should be(__)
  }

  def maybeItWillReturnSomething(flag: Boolean): Option[String] = {
    if (flag) Some("Found value") else None
  }  

  koan("Represent null with None because null is a bad idea") {
    val value1 = maybeItWillReturnSomething(true)
    val value2 = maybeItWillReturnSomething(false)

    value1.get should be(__)
    intercept[java.util.NoSuchElementException] {
      value2.get
    }
  }

  koan("Provide a default value for None") {
    val value1 = maybeItWillReturnSomething(true)
    val value2 = maybeItWillReturnSomething(false)

    value1 getOrElse "No value" should be(__)
    value2 getOrElse "No value" should be(__)
    value2 getOrElse {
      "default function"
    } should be(__)

  }

  koan("checking whether option has value") {
    val value1 = maybeItWillReturnSomething(true)
    val value2 = maybeItWillReturnSomething(false)

    value1.isEmpty should be(__)
    value2.isEmpty should be(__)
  }

  koan("Option can also be used with pattern matching") {
    val someValue: Option[Double] = Some(20.0)
    val value = someValue match {
      case Some(v) => v
      case None => 0.0
    }
    value should be(__)
    val noValue: Option[Double] = None
    val value1 = noValue match {
      case Some(v) => v
      case None => 0.0
    }
    value1 should be(__)
  }
}
