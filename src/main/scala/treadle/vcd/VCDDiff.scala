// See LICENSE for license details.

package treadle.vcd

import scala.collection.mutable

object VCDDiff {
  val ignoreTempWires = true
  val ignoreUniqueWires = true

  //scalastyle:off method.length cyclomatic.complexity
  def compare(vcd1: VCD, vcd2: VCD): Unit = {
    val currentDifferences = new mutable.HashSet[String]()

    val differencesAppearingAtTime = new mutable.HashMap[Long, mutable.HashSet[String]] {
      override def default(key: Long): mutable.HashSet[String] = {
        this(key) = new mutable.HashSet[String]
        this(key)
      }
    }

    val differencesDisappearingAtTime = new mutable.HashMap[Long, mutable.HashSet[String]] {
      override def default(key: Long): mutable.HashSet[String] = {
        this(key) = new mutable.HashSet[String]
        this(key)
      }
    }
    val currentValues1 = new mutable.HashMap[String, BigInt]
    val currentValues2 = new mutable.HashMap[String, BigInt]

    val diffVCD = new VCD(date = vcd1.date,
      version = "1",
      comment = "diff of vcd1 vc2",
      timeScale = vcd1.timeScale,
      scope = vcd1.scope,
      ignoreUnderscoredNames = false
    )

    def addDifference(name: String, time: Long): Unit = {
      if(! currentDifferences.contains(name) &&
         ! (name.startsWith("_") && ignoreTempWires)) {
        currentDifferences += name
        differencesAppearingAtTime(time) += name
      }
    }

    def subDifferences(name: String, time: Long): Unit = {
      if(currentDifferences.contains(name)) {
        currentDifferences -= name
        differencesDisappearingAtTime(time) += name
      }
    }

    def compareChangeSets(set1: List[Change], set2: List[Change], time: Long): Unit = {
      val map1 = set1.map { change => change.wire.name -> change }.toMap
      val map2 = set2.map { change => change.wire.name -> change }.toMap

      val allKeys = (map1.keys ++ map2.keys).toList.distinct

      def getValue(
        key: String,
        changeSet: Map[String, Change],
        currentValue: mutable.HashMap[String, BigInt]
      ): Option[BigInt] = {

        changeSet.get(key) match {
          case Some(change) => Some(change.value)
          case None         => currentValue.get(key)
        }
      }

      for(key <- allKeys) {
        (getValue(key, map1, currentValues1), getValue(key, map2, currentValues2)) match {
          case (Some(value1), Some(value2)) =>
            if(value1 != value2) {
              addDifference(key, time)
            }
            else {
              subDifferences(key, time)
            }
          case (Some(_), None) =>
            if(!ignoreUniqueWires) addDifference(key, time)
          case (None, Some(_)) =>
            if(!ignoreUniqueWires) addDifference(key, time)
          case _ =>
            throw new Exception(s"key $key in list at time $time has no wires")
        }
      }
    }

    def checkWiresAtTime(timeList1: List[Long], timeList2: List[Long]): Unit = {
      (timeList1, timeList2) match {
        case (Nil, Nil) =>
        case (head :: tail, Nil) =>
          compareChangeSets(vcd1.valuesAtTime(head).toList, Nil, head)
          checkWiresAtTime(tail, Nil)
        case (Nil, head :: tail) =>
          compareChangeSets(Nil, vcd2.valuesAtTime(head).toList, head)
          checkWiresAtTime(Nil, tail)
        case (head1 :: tail1, head2 :: tail2) =>
          if(head1 < head2) {
            compareChangeSets(vcd1.valuesAtTime(head1).toList, Nil, head1)
            checkWiresAtTime(tail1, timeList2)
          }
          else if(head1 > head2) {
            compareChangeSets(Nil, vcd2.valuesAtTime(head2).toList, head2)
            checkWiresAtTime(timeList1, tail2)
          }
          else {
            compareChangeSets(vcd1.valuesAtTime(head1).toList, vcd2.valuesAtTime(head1).toList, head1)
            checkWiresAtTime(tail1, tail2)
          }

      }
    }

    compareChangeSets(vcd1.initialValues.toList, vcd2.initialValues.toList, 0L)

    val vcd1Times = vcd1.valuesAtTime.keys.toList.sorted
    val vcd2Times = vcd2.valuesAtTime.keys.toList.sorted

    checkWiresAtTime(vcd1Times, vcd2Times)

    val interestingTimes = (differencesAppearingAtTime.keys ++ differencesDisappearingAtTime.keys).toList.distinct.sorted

    for(time <- interestingTimes) {
      println(
        s"$time: ${differencesAppearingAtTime(time).size} diverged," +
                s" ${differencesDisappearingAtTime(time).size} merged"
      )
      println(s"diverged : ${differencesAppearingAtTime(time).toList.sorted.mkString(", ")}")
      println(s"merged   : ${differencesDisappearingAtTime(time).toList.sorted.mkString(", ")}")
    }

  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case fileName1 :: fileName2 :: Nil =>
        val vcd1 = VCD.read(fileName1)
        val vcd2 = VCD.read(fileName2)
        compare(vcd1, vcd2)
      case _ =>
        println(s"Usage VCDDiff file1 file2")
    }
  }
}
