package org.functionalkoans.forscala

import support.KoanSuite


class AboutEmptyValues extends KoanSuite {

  koan("None equals None") {
    assert(None === __)
  }

  koan("None should be identical to None") {
    val a = None
    val b = None
    assert(a eq __)
  }

  koan("None can be converted to a String") {
    assert(None.toString === __)
  }

  koan("An empty list can be represented by another nothing value: Nil") {
    assert(List() === __)
  }

  koan("None can be converted to an empty list") {
    val a = None
    assert(a.toList === __)
  }

  koan("None is considered empty") {
    assert(None.isEmpty === __)
  }

  koan("None can be cast Any, AnyRef or AnyVal") {
    assert(None.asInstanceOf[Any] === __)
    assert(None.asInstanceOf[AnyRef] === __)
    assert(None.asInstanceOf[AnyVal] === __)
  }

  koan("None cannot be cast to all types of objects") {
    intercept[ClassCastException] {
      // put the exception you expect to see in place of the blank
      assert(None.asInstanceOf[String] === __)
    }
  }

  koan("None can be used with Option instead of null references") {
    val optional: Option[String] = None
    assert(optional.isEmpty === __)
    assert(optional === __)
  }

  koan("Some is the opposite of None for Option types") {
    val optional: Option[String] = Some("Some Value")
    assert((optional == None) === __, "Some(value) should not equal None")
    assert(optional.isEmpty === __, "Some(value) should not be empty")
  }

  koan("Option.getOrElse can be used to provide a default in the case of None") {
    val optional: Option[String] = Some("Some Value")
    val optional2: Option[String] = None
    assert(optional.getOrElse("No Value") === __, "Should return the value in the option")
    assert(optional2.getOrElse("No Value") === __, "Should return the specified default value")
  }
}
