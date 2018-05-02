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
    val keysShown = new mutable.HashSet[String]()

    val diffVCD = new VCD(date = vcd1.date,
      version = "1",
      comment = "diff of vcd1 vc2",
      timeScale = vcd1.timeScale,
      scope = vcd1.scope,
      ignoreUnderscoredNames = false
    )

    def addDifference(name: String, time: Long): Unit = {
      if(! currentDifferences.contains(name) &&
         ! (name.startsWith("_") && ignoreTempWires) &&
         ! keysShown(name)
      ) {
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
      val map1 = set1.map { change => change.wire.fullName -> change }.toMap
      val map2 = set2.map { change => change.wire.fullName -> change }.toMap

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
            currentValues1(key) = value1
            currentValues2(key) = value2
            if(value1 != value2) {
              addDifference(key, time)
            }
            else {
              subDifferences(key, time)
            }
          case (Some(value1), None) =>
            currentValues1(key) = value1
            if(!ignoreUniqueWires) addDifference(key, time)
          case (None, Some(value2)) =>
            currentValues2(key) = value2
            if(!ignoreUniqueWires) addDifference(key, time)
          case _ =>
            throw new Exception(s"key $key in list at time $time has no wires")
        }
      }
    }

    def getValueAtTime(time: Long, vcd: VCD, key: String): String = {
      vcd.valuesAtTime.get(time) match {
        case Some(values) =>
          values.find(change => change.wire.fullName == key) match {
            case Some(change) => change.value.toString()
            case _ => "NA"
          }
        case _ => "NA"
      }
    }

    def showDifference(time: Long, key: String): String = {
      val v1 = currentValues1(key)
      val v2 = currentValues2(key)
      keysShown += key
      s"$key[$v1,$v2]"
    }

    def mergeInitialValues(vcd: VCD): Unit = {
      if(vcd.valuesAtTime.contains(0L)) {
        vcd.valuesAtTime(0) = new mutable.HashSet[Change]()
      }
      val target = vcd.valuesAtTime(0L)
      val initialValues = vcd.initialValues

      initialValues.foreach { initialChange =>
        target.find { change => change.wire.fullName == initialChange.wire.fullName } match {
          case Some(laterChange) =>
          case _ => target += initialChange
        }
      }
    }

    def showChanges(time: Long): Unit = {
      if(differencesAppearingAtTime(time).nonEmpty) {
        println(
          s"$time: ${differencesAppearingAtTime(time).size} diverged," +
                  s" ${differencesDisappearingAtTime(time).size} merged"
        )
        println(s"diverged : " +
                s"${differencesAppearingAtTime(time).toList.sorted.map(x => showDifference(time, x)).mkString(", ")}")
        val mergedValues = differencesDisappearingAtTime(time)
                .toList
                .sorted
                .map(x => s"$x:[${currentValues1(x)}]")
                .mkString(", ")
        println(s"merged   : $mergedValues")
      }
    }

    mergeInitialValues(vcd1)
    mergeInitialValues(vcd2)

    val maxTime = vcd1.valuesAtTime.keys.max.min(vcd2.valuesAtTime.keys.max)

    for(currentTime <- 0L to maxTime) {
      if(vcd1.valuesAtTime.contains(currentTime) || vcd2.valuesAtTime.contains(currentTime)) {
        compareChangeSets(
          vcd1.valuesAtTime.getOrElse(currentTime, Seq.empty).toList,
          vcd2.valuesAtTime.getOrElse(currentTime, Seq.empty).toList,
          currentTime
        )
        showChanges(currentTime)
      }
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
