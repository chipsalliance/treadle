/*
Copyright 2020 The Regents of the University of California (Regents)

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

package treadle.blackboxes

import firrtl.ir.{IntParam, Param, Type}
import treadle.ScalaBlackBox
import treadle.executable.{PositiveEdge, Transition}

//scalastyle:off cyclomatic.complexity method.length
class RoccBlackBox(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "RoccBlackBox"

  var reset: BigInt = 0

  var xLen:                    BigInt = 0
  var PRV_SZ:                  BigInt = 0
  var coreMaxAddrBits:         BigInt = 0
  var dcacheReqTagBits:        BigInt = 0
  var M_SZ:                    BigInt = 0
  var mem_req_bits_size_width: BigInt = 0
  var coreDataBits:            BigInt = 0
  var coreDataBytes:           BigInt = 0
  var paddrBits:               BigInt = 0
  var FPConstants_RM_SZ:       BigInt = 0
  var fLen:                    BigInt = 0
  var FPConstants_FLAGS_SZ:    BigInt = 0

  val rocc_cmd_ready: BigInt = 1 // output
  var rocc_cmd_valid:                           BigInt = 0 // input
  var rocc_cmd_bits_inst_funct:                 BigInt = 0 // input
  var rocc_cmd_bits_inst_rs2:                   BigInt = 0 // input
  var rocc_cmd_bits_inst_rs1:                   BigInt = 0 // input
  var rocc_cmd_bits_inst_xd:                    BigInt = 0 // input
  var rocc_cmd_bits_inst_xs1:                   BigInt = 0 // input
  var rocc_cmd_bits_inst_xs2:                   BigInt = 0 // input
  var rocc_cmd_bits_inst_rd:                    BigInt = 0 // input
  var rocc_cmd_bits_inst_opcode:                BigInt = 0 // input
  var rocc_cmd_bits_rs1:                        BigInt = 0 // input
  var rocc_cmd_bits_rs2:                        BigInt = 0 // input
  var rocc_cmd_bits_status_debug:               BigInt = 0 // input
  var rocc_cmd_bits_status_cease:               BigInt = 0 // input
  var rocc_cmd_bits_status_wfi:                 BigInt = 0 // input
  var rocc_cmd_bits_status_isa:                 BigInt = 0 // input
  var rocc_cmd_bits_status_dprv:                BigInt = 0 // input
  var rocc_cmd_bits_status_prv:                 BigInt = 0 // input
  var rocc_cmd_bits_status_sd:                  BigInt = 0 // input
  var rocc_cmd_bits_status_zero2:               BigInt = 0 // input
  var rocc_cmd_bits_status_sxl:                 BigInt = 0 // input
  var rocc_cmd_bits_status_uxl:                 BigInt = 0 // input
  var rocc_cmd_bits_status_sd_rv32:             BigInt = 0 // input
  var rocc_cmd_bits_status_zero1:               BigInt = 0 // input
  var rocc_cmd_bits_status_tsr:                 BigInt = 0 // input
  var rocc_cmd_bits_status_tw:                  BigInt = 0 // input
  var rocc_cmd_bits_status_tvm:                 BigInt = 0 // input
  var rocc_cmd_bits_status_mxr:                 BigInt = 0 // input
  var rocc_cmd_bits_status_sum:                 BigInt = 0 // input
  var rocc_cmd_bits_status_mprv:                BigInt = 0 // input
  var rocc_cmd_bits_status_xs:                  BigInt = 0 // input
  var rocc_cmd_bits_status_fs:                  BigInt = 0 // input
  var rocc_cmd_bits_status_vs:                  BigInt = 0 // input
  var rocc_cmd_bits_status_mpp:                 BigInt = 0 // input
  var rocc_cmd_bits_status_spp:                 BigInt = 0 // input
  var rocc_cmd_bits_status_mpie:                BigInt = 0 // input
  var rocc_cmd_bits_status_hpie:                BigInt = 0 // input
  var rocc_cmd_bits_status_spie:                BigInt = 0 // input
  var rocc_cmd_bits_status_upie:                BigInt = 0 // input
  var rocc_cmd_bits_status_mie:                 BigInt = 0 // input
  var rocc_cmd_bits_status_hie:                 BigInt = 0 // input
  var rocc_cmd_bits_status_sie:                 BigInt = 0 // input
  var rocc_cmd_bits_status_uie:                 BigInt = 0 // input
  var rocc_resp_ready:                          BigInt = 0 // input
  val rocc_resp_valid: BigInt = 0 // output
  val rocc_resp_bits_rd: BigInt = 0 // output
  val rocc_resp_bits_data: BigInt = 0 // output
  var rocc_mem_req_ready:                       BigInt = 0 // input
  val rocc_mem_req_valid: BigInt = 0 // output
  val rocc_mem_req_bits_addr: BigInt = 0 // output
  val rocc_mem_req_bits_tag: BigInt = 0 // output
  val rocc_mem_req_bits_cmd: BigInt = 0 // output
  val rocc_mem_req_bits_size: BigInt = 0 // output
  val rocc_mem_req_bits_signed: BigInt = 0 // output
  val rocc_mem_req_bits_phys: BigInt = 0 // output
  val rocc_mem_req_bits_no_alloc: BigInt = 0 // output
  val rocc_mem_req_bits_no_xcpt: BigInt = 0 // output
  val rocc_mem_req_bits_dprv: BigInt = 0 // output
  val rocc_mem_req_bits_data: BigInt = 0 // output
  val rocc_mem_req_bits_mask: BigInt = 0 // output
  val rocc_mem_s1_kill: BigInt = 0 // output
  val rocc_mem_s1_data_data: BigInt = 0 // output
  val rocc_mem_s1_data_mask: BigInt = 0 // output
  var rocc_mem_s2_nack:                         BigInt = 0 // input
  var rocc_mem_s2_nack_cause_raw:               BigInt = 0 // input
  val rocc_mem_s2_kill: BigInt = 0 // output
  var rocc_mem_s2_uncached:                     BigInt = 0 // input
  var rocc_mem_s2_paddr:                        BigInt = 0 // input
  var rocc_mem_resp_valid:                      BigInt = 0 // input
  var rocc_mem_resp_bits_addr:                  BigInt = 0 // input
  var rocc_mem_resp_bits_tag:                   BigInt = 0 // input
  var rocc_mem_resp_bits_cmd:                   BigInt = 0 // input
  var rocc_mem_resp_bits_size:                  BigInt = 0 // input
  var rocc_mem_resp_bits_signed:                BigInt = 0 // input
  var rocc_mem_resp_bits_data:                  BigInt = 0 // input
  var rocc_mem_resp_bits_mask:                  BigInt = 0 // input
  var rocc_mem_resp_bits_replay:                BigInt = 0 // input
  var rocc_mem_resp_bits_has_data:              BigInt = 0 // input
  var rocc_mem_resp_bits_data_word_bypass:      BigInt = 0 // input
  var rocc_mem_resp_bits_data_raw:              BigInt = 0 // input
  var rocc_mem_resp_bits_store_data:            BigInt = 0 // input
  var rocc_mem_resp_bits_dprv:                  BigInt = 0 // input
  var rocc_mem_replay_next:                     BigInt = 0 // input
  var rocc_mem_s2_xcpt_ma_ld:                   BigInt = 0 // input
  var rocc_mem_s2_xcpt_ma_st:                   BigInt = 0 // input
  var rocc_mem_s2_xcpt_pf_ld:                   BigInt = 0 // input
  var rocc_mem_s2_xcpt_pf_st:                   BigInt = 0 // input
  var rocc_mem_s2_xcpt_ae_ld:                   BigInt = 0 // input
  var rocc_mem_s2_xcpt_ae_st:                   BigInt = 0 // input
  var rocc_mem_ordered:                         BigInt = 0 // input
  var rocc_mem_perf_acquire:                    BigInt = 0 // input
  var rocc_mem_perf_release:                    BigInt = 0 // input
  var rocc_mem_perf_grant:                      BigInt = 0 // input
  var rocc_mem_perf_tlbMiss:                    BigInt = 0 // input
  var rocc_mem_perf_blocked:                    BigInt = 0 // input
  var rocc_mem_perf_canAcceptStoreThenLoad:     BigInt = 0 // input
  var rocc_mem_perf_canAcceptStoreThenRMW:      BigInt = 0 // input
  var rocc_mem_perf_canAcceptLoadThenLoad:      BigInt = 0 // input
  var rocc_mem_perf_storeBufferEmptyAfterLoad:  BigInt = 0 // input
  var rocc_mem_perf_storeBufferEmptyAfterStore: BigInt = 0 // input
  val rocc_mem_keep_clock_enabled: BigInt = 0 // output
  var rocc_mem_clock_enabled:                   BigInt = 0 // input
  val rocc_busy: BigInt = 0 // output
  val rocc_interrupt: BigInt = 0 // output
  var rocc_exception:                           BigInt = 0 // input
  var rocc_fpu_req_ready:                       BigInt = 0 // input
  val rocc_fpu_req_valid: BigInt = 0 // output
  val rocc_fpu_req_bits_ldst: BigInt = 0 // output
  val rocc_fpu_req_bits_wen: BigInt = 0 // output
  val rocc_fpu_req_bits_ren1: BigInt = 0 // output
  val rocc_fpu_req_bits_ren2: BigInt = 0 // output
  val rocc_fpu_req_bits_ren3: BigInt = 0 // output
  val rocc_fpu_req_bits_swap12: BigInt = 0 // output
  val rocc_fpu_req_bits_swap23: BigInt = 0 // output
  val rocc_fpu_req_bits_singleIn: BigInt = 0 // output
  val rocc_fpu_req_bits_singleOut: BigInt = 0 // output
  val rocc_fpu_req_bits_fromint: BigInt = 0 // output
  val rocc_fpu_req_bits_toint: BigInt = 0 // output
  val rocc_fpu_req_bits_fastpipe: BigInt = 0 // output
  val rocc_fpu_req_bits_fma: BigInt = 0 // output
  val rocc_fpu_req_bits_div: BigInt = 0 // output
  val rocc_fpu_req_bits_sqrt: BigInt = 0 // output
  val rocc_fpu_req_bits_wflags: BigInt = 0 // output
  val rocc_fpu_req_bits_rm: BigInt = 0 // output
  val rocc_fpu_req_bits_fmaCmd: BigInt = 0 // output
  val rocc_fpu_req_bits_typ: BigInt = 0 // output
  val rocc_fpu_req_bits_in1: BigInt = 0 // output
  val rocc_fpu_req_bits_in2: BigInt = 0 // output
  val rocc_fpu_req_bits_in3: BigInt = 0 // output
  val rocc_fpu_resp_ready: BigInt = 1 // output
  var rocc_fpu_resp_valid:                      BigInt = 0 // input
  var rocc_fpu_resp_bits_data:                  BigInt = 0 // input
  var rocc_fpu_resp_bits_exc:                   BigInt = 0 // input

  var acc:                     BigInt = 0
  var doResp:                  BigInt = 0
  var rocc_cmd_bits_inst_rd_d: BigInt = 0

  /**
    * getOutput is called to determine the value for the named output at the
    * current state of the system. The proper way to do this is to not use the inputValues.
    * Instead use[[inputChanged]] to supply a black box with its inputs.
    *
    * @param inputValues This is a list of BigInt values that are in the same order
    *                    as the outputDependencies lists them
    * @param tpe         The concrete type of this output
    * @param outputName  The name of this output
    * @return Computed current concrete value for the name output
    */
  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    outputName match {
      case "rocc_cmd_ready"              => rocc_cmd_ready
      case "rocc_resp_valid"             => rocc_resp_valid
      case "rocc_resp_bits_rd"           => rocc_resp_bits_rd
      case "rocc_resp_bits_data"         => rocc_resp_bits_data
      case "rocc_mem_req_valid"          => rocc_mem_req_valid
      case "rocc_mem_req_bits_addr"      => rocc_mem_req_bits_addr
      case "rocc_mem_req_bits_tag"       => rocc_mem_req_bits_tag
      case "rocc_mem_req_bits_cmd"       => rocc_mem_req_bits_cmd
      case "rocc_mem_req_bits_size"      => rocc_mem_req_bits_size
      case "rocc_mem_req_bits_signed"    => rocc_mem_req_bits_signed
      case "rocc_mem_req_bits_phys"      => rocc_mem_req_bits_phys
      case "rocc_mem_req_bits_no_alloc"  => rocc_mem_req_bits_no_alloc
      case "rocc_mem_req_bits_no_xcpt"   => rocc_mem_req_bits_no_xcpt
      case "rocc_mem_req_bits_dprv"      => rocc_mem_req_bits_dprv
      case "rocc_mem_req_bits_data"      => rocc_mem_req_bits_data
      case "rocc_mem_req_bits_mask"      => rocc_mem_req_bits_mask
      case "rocc_mem_s1_kill"            => rocc_mem_s1_kill
      case "rocc_mem_s1_data_data"       => rocc_mem_s1_data_data
      case "rocc_mem_s1_data_mask"       => rocc_mem_s1_data_mask
      case "rocc_mem_s2_kill"            => rocc_mem_s2_kill
      case "rocc_mem_keep_clock_enabled" => rocc_mem_keep_clock_enabled
      case "rocc_busy"                   => rocc_busy
      case "rocc_interrupt"              => rocc_interrupt
      case "rocc_fpu_req_valid"          => rocc_fpu_req_valid
      case "rocc_fpu_req_bits_ldst"      => rocc_fpu_req_bits_ldst
      case "rocc_fpu_req_bits_wen"       => rocc_fpu_req_bits_wen
      case "rocc_fpu_req_bits_ren1"      => rocc_fpu_req_bits_ren1
      case "rocc_fpu_req_bits_ren2"      => rocc_fpu_req_bits_ren2
      case "rocc_fpu_req_bits_ren3"      => rocc_fpu_req_bits_ren3
      case "rocc_fpu_req_bits_swap12"    => rocc_fpu_req_bits_swap12
      case "rocc_fpu_req_bits_swap23"    => rocc_fpu_req_bits_swap23
      case "rocc_fpu_req_bits_singleIn"  => rocc_fpu_req_bits_singleIn
      case "rocc_fpu_req_bits_singleOut" => rocc_fpu_req_bits_singleOut
      case "rocc_fpu_req_bits_fromint"   => rocc_fpu_req_bits_fromint
      case "rocc_fpu_req_bits_toint"     => rocc_fpu_req_bits_toint
      case "rocc_fpu_req_bits_fastpipe"  => rocc_fpu_req_bits_fastpipe
      case "rocc_fpu_req_bits_fma"       => rocc_fpu_req_bits_fma
      case "rocc_fpu_req_bits_div"       => rocc_fpu_req_bits_div
      case "rocc_fpu_req_bits_sqrt"      => rocc_fpu_req_bits_sqrt
      case "rocc_fpu_req_bits_wflags"    => rocc_fpu_req_bits_wflags
      case "rocc_fpu_req_bits_rm"        => rocc_fpu_req_bits_rm
      case "rocc_fpu_req_bits_fmaCmd"    => rocc_fpu_req_bits_fmaCmd
      case "rocc_fpu_req_bits_typ"       => rocc_fpu_req_bits_typ
      case "rocc_fpu_req_bits_in1"       => rocc_fpu_req_bits_in1
      case "rocc_fpu_req_bits_in2"       => rocc_fpu_req_bits_in2
      case "rocc_fpu_req_bits_in3"       => rocc_fpu_req_bits_in3
      case "rocc_fpu_resp_ready"         => rocc_fpu_resp_ready
      case _                             => BigInt(0)
    }
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "reset"                                    => reset = value
      case "rocc_cmd_valid"                           => rocc_cmd_valid = value
      case "rocc_cmd_bits_inst_funct"                 => rocc_cmd_bits_inst_funct = value
      case "rocc_cmd_bits_inst_rs2"                   => rocc_cmd_bits_inst_rs2 = value
      case "rocc_cmd_bits_inst_rs1"                   => rocc_cmd_bits_inst_rs1 = value
      case "rocc_cmd_bits_inst_xd"                    => rocc_cmd_bits_inst_xd = value
      case "rocc_cmd_bits_inst_xs1"                   => rocc_cmd_bits_inst_xs1 = value
      case "rocc_cmd_bits_inst_xs2"                   => rocc_cmd_bits_inst_xs2 = value
      case "rocc_cmd_bits_inst_rd"                    => rocc_cmd_bits_inst_rd = value
      case "rocc_cmd_bits_inst_opcode"                => rocc_cmd_bits_inst_opcode = value
      case "rocc_cmd_bits_rs1"                        => rocc_cmd_bits_rs1 = value
      case "rocc_cmd_bits_rs2"                        => rocc_cmd_bits_rs2 = value
      case "rocc_cmd_bits_status_debug"               => rocc_cmd_bits_status_debug = value
      case "rocc_cmd_bits_status_cease"               => rocc_cmd_bits_status_cease = value
      case "rocc_cmd_bits_status_wfi"                 => rocc_cmd_bits_status_wfi = value
      case "rocc_cmd_bits_status_isa"                 => rocc_cmd_bits_status_isa = value
      case "rocc_cmd_bits_status_dprv"                => rocc_cmd_bits_status_dprv = value
      case "rocc_cmd_bits_status_prv"                 => rocc_cmd_bits_status_prv = value
      case "rocc_cmd_bits_status_sd"                  => rocc_cmd_bits_status_sd = value
      case "rocc_cmd_bits_status_zero2"               => rocc_cmd_bits_status_zero2 = value
      case "rocc_cmd_bits_status_sxl"                 => rocc_cmd_bits_status_sxl = value
      case "rocc_cmd_bits_status_uxl"                 => rocc_cmd_bits_status_uxl = value
      case "rocc_cmd_bits_status_sd_rv32"             => rocc_cmd_bits_status_sd_rv32 = value
      case "rocc_cmd_bits_status_zero1"               => rocc_cmd_bits_status_zero1 = value
      case "rocc_cmd_bits_status_tsr"                 => rocc_cmd_bits_status_tsr = value
      case "rocc_cmd_bits_status_tw"                  => rocc_cmd_bits_status_tw = value
      case "rocc_cmd_bits_status_tvm"                 => rocc_cmd_bits_status_tvm = value
      case "rocc_cmd_bits_status_mxr"                 => rocc_cmd_bits_status_mxr = value
      case "rocc_cmd_bits_status_sum"                 => rocc_cmd_bits_status_sum = value
      case "rocc_cmd_bits_status_mprv"                => rocc_cmd_bits_status_mprv = value
      case "rocc_cmd_bits_status_xs"                  => rocc_cmd_bits_status_xs = value
      case "rocc_cmd_bits_status_fs"                  => rocc_cmd_bits_status_fs = value
      case "rocc_cmd_bits_status_vs"                  => rocc_cmd_bits_status_vs = value
      case "rocc_cmd_bits_status_mpp"                 => rocc_cmd_bits_status_mpp = value
      case "rocc_cmd_bits_status_spp"                 => rocc_cmd_bits_status_spp = value
      case "rocc_cmd_bits_status_mpie"                => rocc_cmd_bits_status_mpie = value
      case "rocc_cmd_bits_status_hpie"                => rocc_cmd_bits_status_hpie = value
      case "rocc_cmd_bits_status_spie"                => rocc_cmd_bits_status_spie = value
      case "rocc_cmd_bits_status_upie"                => rocc_cmd_bits_status_upie = value
      case "rocc_cmd_bits_status_mie"                 => rocc_cmd_bits_status_mie = value
      case "rocc_cmd_bits_status_hie"                 => rocc_cmd_bits_status_hie = value
      case "rocc_cmd_bits_status_sie"                 => rocc_cmd_bits_status_sie = value
      case "rocc_cmd_bits_status_uie"                 => rocc_cmd_bits_status_uie = value
      case "rocc_resp_ready"                          => rocc_resp_ready = value
      case "rocc_mem_req_ready"                       => rocc_mem_req_ready = value
      case "rocc_mem_s2_nack"                         => rocc_mem_s2_nack = value
      case "rocc_mem_s2_nack_cause_raw"               => rocc_mem_s2_nack_cause_raw = value
      case "rocc_mem_s2_uncached"                     => rocc_mem_s2_uncached = value
      case "rocc_mem_s2_paddr"                        => rocc_mem_s2_paddr = value
      case "rocc_mem_resp_valid"                      => rocc_mem_resp_valid = value
      case "rocc_mem_resp_bits_addr"                  => rocc_mem_resp_bits_addr = value
      case "rocc_mem_resp_bits_tag"                   => rocc_mem_resp_bits_tag = value
      case "rocc_mem_resp_bits_cmd"                   => rocc_mem_resp_bits_cmd = value
      case "rocc_mem_resp_bits_size"                  => rocc_mem_resp_bits_size = value
      case "rocc_mem_resp_bits_signed"                => rocc_mem_resp_bits_signed = value
      case "rocc_mem_resp_bits_data"                  => rocc_mem_resp_bits_data = value
      case "rocc_mem_resp_bits_mask"                  => rocc_mem_resp_bits_mask = value
      case "rocc_mem_resp_bits_replay"                => rocc_mem_resp_bits_replay = value
      case "rocc_mem_resp_bits_has_data"              => rocc_mem_resp_bits_has_data = value
      case "rocc_mem_resp_bits_data_word_bypass"      => rocc_mem_resp_bits_data_word_bypass = value
      case "rocc_mem_resp_bits_data_raw"              => rocc_mem_resp_bits_data_raw = value
      case "rocc_mem_resp_bits_store_data"            => rocc_mem_resp_bits_store_data = value
      case "rocc_mem_resp_bits_dprv"                  => rocc_mem_resp_bits_dprv = value
      case "rocc_mem_replay_next"                     => rocc_mem_replay_next = value
      case "rocc_mem_s2_xcpt_ma_ld"                   => rocc_mem_s2_xcpt_ma_ld = value
      case "rocc_mem_s2_xcpt_ma_st"                   => rocc_mem_s2_xcpt_ma_st = value
      case "rocc_mem_s2_xcpt_pf_ld"                   => rocc_mem_s2_xcpt_pf_ld = value
      case "rocc_mem_s2_xcpt_pf_st"                   => rocc_mem_s2_xcpt_pf_st = value
      case "rocc_mem_s2_xcpt_ae_ld"                   => rocc_mem_s2_xcpt_ae_ld = value
      case "rocc_mem_s2_xcpt_ae_st"                   => rocc_mem_s2_xcpt_ae_st = value
      case "rocc_mem_ordered"                         => rocc_mem_ordered = value
      case "rocc_mem_perf_acquire"                    => rocc_mem_perf_acquire = value
      case "rocc_mem_perf_release"                    => rocc_mem_perf_release = value
      case "rocc_mem_perf_grant"                      => rocc_mem_perf_grant = value
      case "rocc_mem_perf_tlbMiss"                    => rocc_mem_perf_tlbMiss = value
      case "rocc_mem_perf_blocked"                    => rocc_mem_perf_blocked = value
      case "rocc_mem_perf_canAcceptStoreThenLoad"     => rocc_mem_perf_canAcceptStoreThenLoad = value
      case "rocc_mem_perf_canAcceptStoreThenRMW"      => rocc_mem_perf_canAcceptStoreThenRMW = value
      case "rocc_mem_perf_canAcceptLoadThenLoad"      => rocc_mem_perf_canAcceptLoadThenLoad = value
      case "rocc_mem_perf_storeBufferEmptyAfterLoad"  => rocc_mem_perf_storeBufferEmptyAfterLoad = value
      case "rocc_mem_perf_storeBufferEmptyAfterStore" => rocc_mem_perf_storeBufferEmptyAfterStore = value
      case "rocc_mem_clock_enabled"                   => rocc_mem_clock_enabled = value
      case "rocc_exception"                           => rocc_exception = value
      case "rocc_fpu_req_ready"                       => rocc_fpu_req_ready = value
      case "rocc_fpu_resp_valid"                      => rocc_fpu_resp_valid = value
      case "rocc_fpu_resp_bits_data"                  => rocc_fpu_resp_bits_data = value
      case "rocc_fpu_resp_bits_exc"                   => rocc_fpu_resp_bits_exc = value
      case _                                          =>
    }
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      if (reset > BigInt(0)) {
        acc = 0
      } else {
        if (rocc_cmd_valid > BigInt(0) && rocc_cmd_ready > BigInt(0)) {
          doResp = rocc_cmd_bits_inst_xd
          rocc_cmd_bits_inst_rd_d = rocc_cmd_bits_inst_rd
          acc = acc + rocc_cmd_bits_rs1 + rocc_cmd_bits_rs2
        } else {
          doResp = 0
        }
      }
    }
  }

  /**
    * returns a list of names of inputs that this output depends on.
    *
    * @note The order of this list will determine the order of the inputValues argument to the getOutput method
    * @param outputName the output whose dependencies are being described
    * @return
    */
  override def outputDependencies(outputName: String): Seq[String] = {
    Seq(
      "debug_req_ready",
      "debug_resp_valid",
      "debug_resp_bits_resp",
      "debug_resp_bits_data"
    )
  }

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case IntParam("xLen", value)                    => xLen = value
      case IntParam("PRV_SZ", value)                  => PRV_SZ = value
      case IntParam("coreMaxAddrBits", value)         => coreMaxAddrBits = value
      case IntParam("dcacheReqTagBits", value)        => dcacheReqTagBits = value
      case IntParam("M_SZ", value)                    => M_SZ = value
      case IntParam("mem_req_bits_size_width", value) => mem_req_bits_size_width = value
      case IntParam("coreDataBits", value)            => coreDataBits = value
      case IntParam("coreDataBytes", value)           => coreDataBytes = value
      case IntParam("paddrBits", value)               => paddrBits = value
      case IntParam("FPConstants_RM_SZ", value)       => FPConstants_RM_SZ = value
      case IntParam("fLen", value)                    => fLen = value
      case IntParam("FPConstants_FLAGS_SZ", value)    => FPConstants_FLAGS_SZ = value
      case _                                          =>
    }
  }

}
