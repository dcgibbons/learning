package org.functionalkoans.forscala

import org.scalatest._
import support.Master

class Koans extends Suite {
  override def nestedSuites = List(
    new AboutAsserts,
    new AboutValAndVar,
    new AboutLiteralBooleans,
    new AboutLiteralNumbers,
    new AboutLiteralStrings,
    new AboutMethods,
    new AboutClasses,
    new AboutUniformAccessPrinciple,
    new AboutConstructors,
    new AboutParentClasses,
    new AboutOptions,
    new AboutObjects,
    new AboutApply,
    new AboutTuples,
    new AboutHigherOrderFunctions,
    new AboutEmptyValues,
    new AboutLists,
    new AboutMaps,
    new AboutSets,
    new AboutFormatting,
    new AboutStringInterpolation,
    new AboutPatternMatching,
    new AboutCaseClasses,
    new AboutRange,
    new AboutPartiallyAppliedFunctions,
    new AboutPartialFunctions,
    new AboutImplicits,
    new AboutTraits,
    new AboutForExpressions,
    new AboutInfixPrefixAndPostfixOperators,
    new AboutInfixTypes,
    new AboutMutableMaps,
    new AboutMutableSets,
    new AboutSequencesAndArrays,
    new AboutIterables,
    new AboutTraversables,
    new AboutNamedAndDefaultArguments,
    new AboutTypeTags,
    new AboutPreconditions,
    new AboutExtractors,
    new AboutByNameParameter,
    new AboutRepeatedParameters,
    new AboutTypeSignatures,
    new AboutTypeVariance,
    new AboutEnumerations
  )

  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
                   configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
    super.run(testName, reporter, Master, filter, configMap, distributor, tracker)
  }

}
