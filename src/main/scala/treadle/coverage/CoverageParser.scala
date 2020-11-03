/*
Copyright 2020 Danmarks Tekniske Universitet

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.coverage

import treadle.TreadleTester
import scala.annotation.tailrec

object CoverageParser {

  final val validatorName: String = "io_cov_valid_"

  /**
    * Source to source transform of the input loFIRRTL code
    * The input is transformed as follows: for each mux do the following:
    *        r_1 <= mux(c, in_a, in_b_1)
    * becomes
    *        r_1 <= mux(c, in_a, in_b_1)
    *        cov_val_i <= mux(c, 1, 0)
    *        cov_val_i+1 <= mux(c, 0, 1)
    * where i is the current coverage validation index
    * @note it is assumed that the i/o is always declared before the architecture
    */
  def transform(firrtlSource: String): String = {

    /*
      * Genereated the loFIRRTL source needed to defined the additional validators
      * @param firrtlSourceList the loFIRRTL source we are transforming
      * @param neededVals the amount of validators that should be generated
      * @return a list of source lines to add to our final output
      */
    def generateValidatorDef(firrtlSourceList: List[String], neededVals: Int) : List[String] = {
      val outputLine = firrtlSourceList.find("\\boutput *\\b".r.findFirstIn(_).isDefined).orNull
      val lineIndent = outputLine.substring(0, outputLine.indexWhere(_ != ' '))
      (for(i <- 0 until neededVals)
        yield List(
          lineIndent + s"output $validatorName${2 * i}: UInt<1>",
          lineIndent + s"output $validatorName${2 * i + 1}: UInt<1>"
        )).toList.flatten
    }

    /*
      * Constructs the second part of the transformed source code (containing the validators)
      * @param sourcePostDecl The FIRRTL source after the i/o declaration
      * @param muxLines the lines of source code containing multiplexers
      * @param valids the modified versions of the muxLines with the added validators
      * @param acc the accumulator used to construct the final result
      * @return A List containing the lines of the final source (post i/o declaration)
      */
    @tailrec
    def constructSourceWithVals(
                                 sourcePostDecl: List[String],
                                 muxLines: List[String],
                                 valids: List[List[String]],
                                 acc: List[String]
                               ): List[String] = {
      if(muxLines.isEmpty) {
        acc ++ sourcePostDecl
      } else {
        val (sourceBeforeMux, sourceAfterMux) = sourcePostDecl.splitAt(sourcePostDecl indexOf muxLines.head)
        constructSourceWithVals(sourceAfterMux.tail, muxLines.tail, valids.tail,
          acc ++ sourceBeforeMux ++ valids.head
        )
      }
    }

    /*
      * Constructs the validator code to add to the firrtl source as follows
      * for each mux do the following:
      *        r_1 <= mux(c, in_a, in_b_1)
      * becomes
      *        r_1 <= mux(c, in_a, in_b_1)
      *        cov_val_i <= mux(c, 1, 0)
      *        cov_val_i+1 <= mux(c, 0, 1)
      * where i is the current coverage validation index
      * @param muxLines The lines containing the muxes
      * @param muxConditions The extracted conditions for each mux (see retrieveMuxCondition)
      * @param index the current validator index
      * @param acc the accumulator
      * @return the final code to be inserted into the firrtl source
      */
    @tailrec
    def constructValidatorCode(
                                muxLines: List[String],
                                muxConditions: List[String],
                                index: Int,
                                acc: List[List[String]]
                              ): List[List[String]] = {
      if(muxLines.isEmpty) {
        acc
      } else {
        val muxLineIndent = muxLines.head.substring(0, muxLines.head.indexWhere(_ != ' '))
        constructValidatorCode(muxLines.tail, muxConditions.tail, index + 1,
          acc :+ List(muxLines.head,
            muxLineIndent + validatorName  + (2 * index) +
              s" <= ${muxConditions.head}",
            muxLineIndent + validatorName + (2 * index + 1) +
              s" <= not(${muxConditions.head})"
          )
        )}
    }

    /*
      * Retrieves the condition (select) used in a mux
      * @param muxLine the line of code in which the mux is used
      * @return the condition used inside the given mux
      */
    def retrieveMuxCondition(muxLine: String): String =
      "\\bmux.*\\b".r.findFirstMatchIn(muxLine).get.matched.split(",").head.split('(').tail.head


    /*
      * Computes the index at which the last input is found in the firrtl source
      * @param firrtlSourceList the firrtl source split by lines
      * @return the index of the last input in the source
      */
    def getIndexOfInputs(firrtlSourceList: List[String]) : Int =
      firrtlSourceList.indexOf(
        firrtlSourceList.find("output *\\b".r.findFirstIn(_).isDefined
        ).getOrElse("Undefined"))

    //Split the source by line
    val firrtlSourceList: List[String] = List(firrtlSource.split("\n")).flatten

    //Find location of validator declaration and split the source
    val (sourcePreDecl: List[String], sourcePostDecl: List[String]) =
      firrtlSourceList.splitAt(getIndexOfInputs(firrtlSourceList))

    //Go through post declaration to find the location of the muxes
    val muxLines: List[String] = sourcePostDecl.filter("\\bmux.*\\b".r.findFirstMatchIn(_).isDefined)
    val muxConditions: List[String] = muxLines.map(retrieveMuxCondition)

    //Construct the final source code
    (sourcePreDecl ++ generateValidatorDef(firrtlSourceList, muxLines.length)
      ++ constructSourceWithVals(sourcePostDecl, muxLines, constructValidatorCode(muxLines, muxConditions, 0, Nil), Nil)
      ).foldLeft("")((s1, s2) => s1 + "\n" + s2) ++ "\n"
  }

  /**
    * Retrieves the number of coverage validators in a given loFIRRTL source.
    * @param firrtlSource the source code that will be parsed
    * @return the number of added coverage validators in the source
    */
  def getNumValidators(firrtlSource: String): Int =
    firrtlSource.split("\n").count(("\\boutput " + validatorName + ".*\\b").r.findFirstMatchIn(_).isDefined)

  /**
    * Computes the coverage percentage as a value between 0 and 1
    * @param values the values found at the validators after execution
    * @return a value between 0 and 1 representing the amount of coverage
    */
  def getCoverage(values: List[Int]): Double = if(values.isEmpty) 1 else values.sum.toDouble / values.length.toDouble

  /**
    * Retrieves the validator values at a given point in a test
    * @param firrtlSource the source code of the current DUT
    * @param tester the tester that is currently running the test
    * @return a list containing the values of the different validators
    */
  def getValidators(firrtlSource: String, tester: TreadleTester): List[Int] =
    (for (i <- 0 until CoverageParser.getNumValidators(firrtlSource))
      yield CoverageParser.validatorName + i).map(tester.peek(_).toInt).toList

  /**
    * Retrieves the lineNumbers of the lines that are covered
    * @param firrtlSourceList the DUT's firrtl source code split by lines
    * @param tester the tester currently running on the given DUT
    * @param validators the validator ouptuts from the current test
    * @return a list containing the line numbers of the lines covered by current tests
    */
  def getLineCoverage(firrtlSourceList: List[String], tester: TreadleTester, validators: List[Int]): List[Int] = {
    (for(i <- validators.indices) yield
      firrtlSourceList.indexWhere(s"\\b$validatorName$i <=.*\\b".r.findFirstMatchIn(_).isDefined)
      ).toList.zip(validators).map(v => v._1 * v._2).filter(_ != 0)
  }

  /**
    * Prints out a modified version of the DUT's source containing coverage information
    * @param firrtlSource the DUT's firrtl source code
    * @param tester the tester currently running the DUT
    */
  def reportCoverage(firrtlSource: String, tester: TreadleTester): Unit = {
    /*
      * Constructs a new version of the given source that contains line coverage information
      * in the form of "+ line" if covered and "- line" otherwise
      * @param firrtlSourceList the DUT's source code split by line
      * @param coveredValidators the line indexes that where covered
      * @param acc the accumulator for the new source
      * @param index the accumulator for the current line index
      * @param validIndex the accumulator for the current validator index
      * @return a new version of the source containing coverage information
      */
    @tailrec
    def constructNewSource(
                            firrtlSourceList: List[String],
                            coveredValidators: List[Int],
                            acc: List[String],
                            index: Int,
                            validIndex: Int
                          ): String = {
      if(firrtlSourceList.isEmpty) {
        acc.foldLeft("")(_ + "\n" + _)
      } else {
        //Check if the source line contains a validator
        if(s"$validatorName$validIndex <=.*\\b".r.findFirstMatchIn(firrtlSourceList.head).isEmpty) {
          //println(s"NO VALIDATOR LINE $index")
          constructNewSource(
            firrtlSourceList.tail,
            coveredValidators,
            acc :+ s"+ ${firrtlSourceList.head}",
            index + 1,
            validIndex
          )
        }
        //If not check if the line's validator is covered
        else if(coveredValidators.contains(index)) {
          //println(s"COVERED VALIDATOR LINE $index")
          constructNewSource(
            firrtlSourceList.tail,
            coveredValidators,
            acc :+ s"+ ${firrtlSourceList.head}",
            index + 1,
            validIndex + 1
          )
        } else {
          //println(s"NOT COVERED VALIDATOR LINE $index")
          constructNewSource(
            firrtlSourceList.tail,
            coveredValidators,
            acc :+ s"- ${firrtlSourceList.head}",
            index + 1,
            validIndex + 1
          )
        }
      }
    }

    val firrtlSourceList = firrtlSource.split("\n").toList

    //Compute and print out the final coverage percentage
    println("COVERAGE: " + getCoverage(tester.lineValidators) * 100 + "% of multiplexer paths tested")

    //Print out the final coverage report
    println("COVERAGE REPORT:\n" +
      constructNewSource(
        firrtlSourceList,
        getLineCoverage(firrtlSourceList, tester, tester.lineValidators),
        Nil,
        0,
        0
      )
    )
  }
}
