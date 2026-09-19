/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * DiffTest is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package difftest.preprocess

import chisel3._
import chisel3.util._
import difftest._
import difftest.gateway.GatewayConfig
import difftest.util.PipelineConnect

object Preprocess {
  def apply(
    bundles: DecoupledIO[MixedVec[DifftestBundle]],
    config: GatewayConfig,
  ): DecoupledIO[MixedVec[DifftestBundle]] = {
    val module = Module(new PreprocessEndpoint(chiselTypeOf(bundles.bits).toSeq, config))
    module.in <> bundles
    module.out
  }

  def getArchRegs(bundles: Seq[DifftestBundle], isHardware: Boolean): Seq[ArchRegState with DifftestBundle] = {
    bundles.collect { case p: DiffPhyRegState => p }
      .groupBy(_.desiredCppName)
      .flatMap { case (name, pregs) =>
        val archTarget = pregs.head.archTarget
        val ratTarget = pregs.head.ratTarget
        require(!bundles.exists(_.isInstanceOf[archTarget.type]))
        if (isHardware) {
          val needRat = pregs.head.needRat
          val rats = bundles.collect {
            case rat: DiffArchRenameTable if rat.desiredCppName == ratTarget.desiredCppName => rat
          }
          require((needRat && rats.length == pregs.length) || (!needRat && rats.isEmpty))
          pregs.zipWithIndex.map { case (preg, idx) =>
            val archReg = Wire(archTarget)
            archReg.coreid := preg.coreid
            if (needRat) {
              val rat = rats(idx)
              require(rat.numPhyRegs == preg.numPhyRegs)
              archReg.value.zipWithIndex.foreach { case (data, vid) =>
                data := preg.value(rat.value(vid))
              }
            } else {
              archReg.value := preg.value
            }
            archReg
          }
        } else {
          Seq.fill(pregs.length)(archTarget)
        }
      }
      .toSeq
  }
  // Replace PhyReg + Rename with ArchReg + CommitData/VecCommitData
  def replaceRegs(bundles: Seq[DifftestBundle]): Seq[DifftestBundle] = {
    def getBundle[T <: DifftestBundle](name: String): Seq[T] =
      bundles.filter(_.desiredCppName == name).asInstanceOf[Seq[T]]

    val numCores = bundles.count(_.isUniqueIdentifier)
    val archRegs = getArchRegs(bundles, true)

    val commits = getBundle[DiffInstrCommit]("commit")
    val phyInts = getBundle[DiffPhyIntRegState]("pregs_xrf")
    val phyFps = getBundle[DiffPhyFpRegState]("pregs_frf")
    val phyVecs = getBundle[DiffPhyVecRegState]("pregs_vrf")
    // Emit only scalar commit_data. vec_commit_data only feeds the REF-based
    // vec load check, which FPGA basic-diff builds never run.
    val commitDatas = commits.zipWithIndex.map { case (c, idx) =>
      val coreID = idx / (commits.length / numCores)
      val intData = phyInts(coreID).value(c.wpdest)
      val fpData = if (phyFps.nonEmpty) phyFps(coreID).value(c.wpdest) else 0.U
      val cd = Wire(new DiffCommitData)
      cd.coreid := c.coreid
      cd.index := c.index
      cd.valid := c.valid && (c.rfwen || c.fpwen)
      cd.data := Mux(c.fpwen, fpData, intData)
      cd
    }

    bundles.filterNot(b => Seq("pregs_", "rat_").exists(s => b.desiredCppName.contains(s))) ++ archRegs ++ commitDatas
  }

  // Reconstruct per-instruction commit data from physical WriteBack ports.
  // WriteBacks and InstrCommit may not land in the same software buffer, so
  // keep a physical RF shadow until the matching commit arrives. Same-cycle
  // WriteBacks override the shadow so two writes to the same logical dest
  // still keep distinct wpdest values.
  def getCommitData(
    bundles: MixedVec[DifftestBundle],
    commits: Seq[DiffInstrCommit],
    wbName: String,
    regName: String,
  ): Seq[UInt] = {
    if (bundles.exists(_.desiredCppName == wbName)) {
      val numCores = bundles.count(_.isUniqueIdentifier)
      val writeBacks = bundles.filter(_.desiredCppName == wbName)
      val numElements = writeBacks.head.asInstanceOf[DataWriteback].numElements
      val phyRf = Reg(Vec(numCores, Vec(numElements, UInt(64.W))))
      for (wb <- writeBacks) {
        val valid = wb.asInstanceOf[DataWriteback].valid
        val coreid = wb.coreid
        val address = wb.asInstanceOf[DataWriteback].address
        val data = wb.asInstanceOf[DataWriteback].data
        when(valid) {
          phyRf(coreid)(address) := data
        }
      }
      commits.map { c =>
        val data = WireInit(phyRf(c.coreid)(c.wpdest))
        for (wb <- writeBacks) {
          val valid = wb.asInstanceOf[DataWriteback].valid
          val coreid = wb.coreid
          val address = wb.asInstanceOf[DataWriteback].address
          val wdata = wb.asInstanceOf[DataWriteback].data
          when(valid && coreid === c.coreid && address === c.wpdest) {
            data := wdata
          }
        }
        data
      }
    } else if (bundles.exists(_.desiredCppName == regName)) {
      val archRf = VecInit(bundles.filter(_.desiredCppName == regName).map(_.asInstanceOf[ArchIntRegState]).toSeq)
      commits.map { c => archRf(c.coreid).value(c.wdest) }
    } else {
      Seq.fill(commits.length)(0.U)
    }
  }

  def getVecCommitData(
    bundles: MixedVec[DifftestBundle],
    commits: Seq[DiffInstrCommit],
  ): Seq[Seq[Vec[UInt]]] = {
    if (bundles.exists(_.desiredCppName == "wb_vrf")) {
      val numCores = bundles.count(_.isUniqueIdentifier)
      val vecWriteBacks = bundles.filter(_.desiredCppName == "wb_vrf").map(_.asInstanceOf[DiffVecWriteback])
      val v0WriteBacks = bundles.filter(_.desiredCppName == "wb_v0").map(_.asInstanceOf[DiffVecV0Writeback])
      val vecPhyRf = Reg(Vec(numCores, Vec(vecWriteBacks.head.numElements, Vec(2, UInt(64.W)))))
      val v0PhyRf = Reg(Vec(numCores, Vec(v0WriteBacks.head.numElements, Vec(2, UInt(64.W)))))

      for (vecWb <- vecWriteBacks) {
        when(vecWb.valid) {
          vecPhyRf(vecWb.coreid)(vecWb.address) := vecWb.data
        }
      }
      for (v0Wb <- v0WriteBacks) {
        when(v0Wb.valid) {
          v0PhyRf(v0Wb.coreid)(v0Wb.address) := v0Wb.data
        }
      }

      commits.map { c =>
        val otherData = c.otherwpdest.map { pdest =>
          WireInit(vecPhyRf(c.coreid)(pdest))
        }

        when(c.valid) {
          c.otherwpdest.zipWithIndex.foreach { case (pdest, i) =>
            for (vecWb <- vecWriteBacks) {
              when(vecWb.valid && vecWb.coreid === c.coreid && vecWb.address === pdest) {
                otherData(i) := vecWb.data
              }
            }
          }
        }

        when(c.v0wen) {
          otherData(0) := v0PhyRf(c.coreid)(c.otherwpdest(0))
          when(c.valid) {
            for (v0Wb <- v0WriteBacks) {
              when(v0Wb.valid && v0Wb.coreid === c.coreid && v0Wb.address === c.otherwpdest(0)) {
                otherData(0) := v0Wb.data
              }
            }
          }
        }

        otherData
      }
    } else {
      Seq.fill(commits.length)(Seq.fill(8)(VecInit(Seq.fill(2)(0.U(64.W)))))
    }
  }

  def collectWritebackCommitData(bundles: MixedVec[DifftestBundle]): Seq[DifftestBundle] = {
    val commits = bundles.filter(_.desiredCppName == "commit").map(_.asInstanceOf[DiffInstrCommit]).toSeq
    val intData = getCommitData(bundles, commits, "wb_xrf", "xrf")
    val fpData = getCommitData(bundles, commits, "wb_frf", "frf")
    val commitData = commits.zip(fpData).zip(intData).map { case ((c, f), i) =>
      val cd = WireInit(0.U.asTypeOf(new DiffCommitData))
      cd.coreid := c.coreid
      cd.index := c.index
      cd.valid := c.valid && (c.rfwen || c.fpwen)
      cd.data := Mux(c.fpwen, f, i)
      cd
    }
    val noWriteBacks = bundles.filterNot(_.desiredCppName.contains("wb"))
    val vecCommitData = if (bundles.exists(_.desiredCppName == "wb_vrf")) {
      val vecData = getVecCommitData(bundles, commits)
      commits.zip(vecData).map { case (c, v) =>
        val vcd = WireInit(0.U.asTypeOf(new DiffVecCommitData))
        vcd.coreid := c.coreid
        vcd.index := c.index
        vcd.valid := c.valid && (c.v0wen || c.vecwen)
        when(c.v0wen || c.vecwen) {
          for (index <- 0 until 8) {
            vcd.data(2 * index) := v(index)(0)
            vcd.data(2 * index + 1) := v(index)(1)
          }
        }
        vcd
      }
    } else {
      Seq.empty[DiffVecCommitData]
    }
    noWriteBacks ++ commitData ++ vecCommitData
  }
}

class PreprocessEndpoint(bundles: Seq[DifftestBundle], config: GatewayConfig) extends Module {
  val in = IO(Flipped(Decoupled(MixedVec(bundles))))
  val pipelined = Wire(Decoupled(MixedVec(bundles)))
  PipelineConnect(in, pipelined, pipelined.fire)

  val replaceReg = if (!config.softArchUpdate && pipelined.bits.exists(_.desiredCppName == "pregs_xrf")) {
    // extract ArchReg in Hardware
    Preprocess.replaceRegs(pipelined.bits)
  } else if (pipelined.bits.exists(_.desiredCppName.contains("wb"))) {
    Preprocess.collectWritebackCommitData(pipelined.bits)
  } else {
    pipelined.bits
  }

  // LoadEvent will not be checked when single-core
  val skipLoad = if (replaceReg.count(_.isUniqueIdentifier) == 1) {
    replaceReg.filterNot(_.desiredCppName == "load")
  } else {
    replaceReg
  }

  val preprocessed = MixedVecInit(skipLoad.toSeq)
  val out = IO(Decoupled(chiselTypeOf(preprocessed)))
  pipelined.ready := out.ready
  out.valid := pipelined.valid
  out.bits := preprocessed
}
