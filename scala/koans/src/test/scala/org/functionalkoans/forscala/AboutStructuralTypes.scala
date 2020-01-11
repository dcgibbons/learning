package org.functionalkoans.forscala

import org.functionalkoans.forscala.support.KoanSuite
import org.scalatest.matchers.ShouldMatchers

import scala.language.reflectiveCalls

class AboutStructuralTypes extends KoanSuite with ShouldMatchers{
   class Duck {
      def quack = "Quack"
   }

   object BadDoctor {
     def quack = "Here try these experimental pills"
   }

   koan(
     """Structural typing is the ability to allow to the best of Scala's ability, duck typing.
       |Duck Typing is where the method signatures and properties become valid semantics, not inheritence.
       |Duck Typing comes from the term "If it walk like duck, and talks like a duck, ...""".stripMargin) {

      def onlyThoseThatCanPerformQuacks(quacker: {def quack:String}): String = {
        "received message: %s".format(quacker.quack)
      }

      onlyThoseThatCanPerformQuacks(new Duck()) should be (__)
      onlyThoseThatCanPerformQuacks(BadDoctor) should be (__)
   }

   koan("""Structural typing can also be used to assign values and variables""") {
       val quacker:{def quack:String} = BadDoctor
       quacker.quack should be (__)
   }

   koan("""Use a semicolon (;) to add more methods for the structural type""") {
       class Human {
         def speak = "Uh, hey"
         def move(steps:Int, direction:String) = "Don't want to, rather just chill"
       }

       class Cow {
         def speak = "Whattup?"
         def move(steps:Int, direction: String) = s"Ok, moving $steps steps..heading $direction"
       }

       val someDude: {def speak:String; def move(steps:Int, direction:String):String} = new Human()
       val someBovine: {def speak:String; def move(steps:Int, direction:String):String} = new Cow()
       someDude.move(4, "North") should be (__)
       someBovine.move(3, "South") should be (__)
   }

   koan("""You can use type aliasing if the structural type gets to be too much typing""") {
     class Goose {
       def speak = "HONK!"
       def move(steps:Int, direction:String) = direction match {
         case "North" => "Nope, not in winter"
         case _ => s"Ok, moving $steps steps..heading $direction"
       }
     }

     type SpeakerAndMover = {def speak:String; def move(steps:Int, direction:String):String}

     val someGoose:SpeakerAndMover = new Goose()
     someGoose.move(4, "North") should be (__)
   }
}
