//===----------------------- MIRNamer.cpp - MIR Namer ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// The purpose of this pass is to rename virtual register operands with the goal
// of making it easier to author easier to read tests for MIR. This pass reuses
// the vreg renamer used by MIRCanonicalizerPass.
//
// Basic Usage:
//
// llc -o - -run-pass mir-namer example.mir
//
//===----------------------------------------------------------------------===//

#include "MIRVRegNamerUtils.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/InitializePasses.h"

using namespace llvm;

namespace llvm {
extern char &MIRNamerID;
} // namespace llvm

#define DEBUG_TYPE "mir-namer"

namespace {

class MIRNamer : public MachineFunctionPass {
public:
  static char ID;
  MIRNamer() : MachineFunctionPass(ID) {}

  StringRef getPassName() const override {
    return "Rename virtual register operands";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override {
    bool Changed = false;

    if (MF.empty())
      return Changed;

    NamedVRegCursor NVC(MF.getRegInfo());

    ReversePostOrderTraversal<MachineBasicBlock *> RPOT(&*MF.begin());
    for (auto &MBB : RPOT)
      Changed |= NVC.renameVRegs(MBB);

    return Changed;
  }
};

} // end anonymous namespace

char MIRNamer::ID;

char &llvm::MIRNamerID = MIRNamer::ID;

INITIALIZE_PASS_BEGIN(MIRNamer, "mir-namer", "Rename Register Operands", false,
                      false)

INITIALIZE_PASS_END(MIRNamer, "mir-namer", "Rename Register Operands", false,
                    false)
