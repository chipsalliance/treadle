// See LICENSE for license details.

package treadle.stage.phases

import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.options.Phase
import firrtl.passes.RemoveCHIRRTL
import firrtl.passes.memlib.ReplSeqMem
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.transforms.BlackBoxSourceHelper
import firrtl.{
  AnnotationSeq, ChirrtlForm, CircuitForm, CircuitState,
  HighForm, LowForm, SeqTransform, Transform, UnknownForm, passes
}
import treadle.{TreadleCircuitStateAnnotation, TreadleFirrtlFormHint}
import treadle.utils.{AugmentPrintf, FixupOps}

trait TreadlePhase extends Phase {
  val transforms: Seq[Transform]

  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {
    val form = annotationSeq.collectFirst { case TreadleFirrtlFormHint(form) => form }.getOrElse(UnknownForm)

    annotationSeq.flatMap {
      case FirrtlCircuitAnnotation(circuit) =>
        val state = CircuitState(circuit, form, annotationSeq)
        val newState = transforms.foldLeft(state) {
          case (prevState, transform) => transform.runTransform(prevState)
        }
        Some(TreadleCircuitStateAnnotation(newState))
      case other =>
        Some(other)
    }
  }
}

/** This provides a series of transforms that
  * seem to be important to Treadle functionality
  * This was based on [[firrtl.LowFirrtlOptimization]] but that
  * has includes [[passes.memlib.VerilogMemDelays]] which can cause combinational loops
  * for some firrtl files
  */
class TreadleLowFirrtlOptimization extends SeqTransform {
  def inputForm: CircuitForm = LowForm
  def outputForm: CircuitForm = LowForm

  def transforms: Seq[Transform] = Seq(
    passes.RemoveValidIf,
    new firrtl.transforms.ConstantPropagation,
    passes.PadWidths,
    new firrtl.transforms.ConstantPropagation,
    passes.Legalize,
    new firrtl.transforms.ConstantPropagation,
    passes.SplitExpressions,
    new firrtl.transforms.CombineCats,
    passes.CommonSubexpressionElimination,
    new firrtl.transforms.DeadCodeElimination
  )
}

class ChirrtlToLow extends Transform {
  override def inputForm: CircuitForm = ChirrtlForm

  override def outputForm: CircuitForm = LowForm

  override protected def execute(state: CircuitState): CircuitState = {
    if(state.form == ChirrtlForm || state.form == UnknownForm) {
      val transformSeq = new SeqTransform {
        override def inputForm: CircuitForm = ChirrtlForm
        override def outputForm: CircuitForm = LowForm

        override def transforms: Seq[Transform] =  getLoweringTransforms(ChirrtlForm, LowForm)
      }
      transformSeq.execute(state)
    } else {
      state
    }
  }
}

class HighToLow extends Transform {
  override def inputForm: CircuitForm = HighForm

  override def outputForm: CircuitForm = LowForm

  override protected def execute(state: CircuitState): CircuitState = {
    if(state.form == HighForm) {
      val transformSeq = new SeqTransform {
        override def inputForm: CircuitForm = HighForm
        override def outputForm: CircuitForm = LowForm

        override def transforms: Seq[Transform] = Seq(new ReplSeqMem, RemoveCHIRRTL) ++ getLoweringTransforms(HighForm, LowForm)
      }
      transformSeq.execute(state)
    } else {
      state
    }
  }
}

/** Prepare the AST from low FIRRTL.
  *
  */
object PrepareAstFromLowFIRRTL extends TreadlePhase {
  val transforms: Seq[Transform] = Seq(new FixupOps, AugmentPrintf)
}

/**
  * Call a bunch of transforms so TreadleTester can operate
  */
object PrepareAst extends TreadlePhase {
  val transforms: Seq[Transform] = {
    Seq(
      new ChirrtlToLow,
      new HighToLow,
      new TreadleLowFirrtlOptimization,
      new BlackBoxSourceHelper,
      new FixupOps
    )
  }
}
