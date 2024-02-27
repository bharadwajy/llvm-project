//- DXIL.td - Describe DXIL operation -------------------------*- tablegen -*-//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This is a target description file for DXIL operations.
///
//===----------------------------------------------------------------------===//

include "llvm/IR/Intrinsics.td"
include "llvm/IR/Attributes.td"

// Abstract representation of the class a DXIL Operation belongs to.
class DXILOpClass<string name> {
  string Name = name;
}

// Abstract representation of the category a DXIL Operation belongs to
class DXILOpCategory<string name> {
  string Name = name;
}

def UnaryClass : DXILOpClass<"Unary">;
def BinaryClass : DXILOpClass<"Binary">;
def FlattenedThreadIdInGroupClass : DXILOpClass<"FlattenedThreadIdInGroup">;
def ThreadIdInGroupClass : DXILOpClass<"ThreadIdInGroup">;
def ThreadIdClass : DXILOpClass<"ThreadId">;
def GroupIdClass : DXILOpClass<"GroupId">;

def BinaryUintCategory : DXILOpCategory<"Binary uint">;
def UnaryFloatCategory : DXILOpCategory<"Unary float">;
def ComputeIDCategory : DXILOpCategory<"Compute/Mesh/Amplification shader">;

// LLVM intrinsic that DXIL operation maps to.
class LLVMIntrinsic<Intrinsic llvm_intrinsic_> { Intrinsic llvm_intrinsic = llvm_intrinsic_; }

// Abstraction DXIL Operation to LLVM intrinsic
class DXILOpMapping<int opCode, Intrinsic intrinsic, DXILOpCategory category, string doc> {
  int OpCode = opCode;                 // Opcode corresponding to DXIL Operation
  Intrinsic LLVMIntrinsic = intrinsic; // LLVM Intrinsic the DXIL Operation maps to
  string Doc = doc;                    // a short description of the operation
  DXILOpCategory OpCategory = category;// Category of the operation (Is this really needed??)
}

// Concrete definition of DXIL Operation mapping to corresponding LLVM intrinsic
def Sin      : DXILOpMapping<13, int_sin, UnaryFloatCategory, "Returns sine(theta) for theta in radians.">;
def UMax     : DXILOpMapping<39, int_umax, BinaryUintCategory, "Unsigned integer maximum. UMax(a,b) = a > b ? a : b">;
def ThreadId : DXILOpMapping<93, int_dx_thread_id, ComputeIDCategory, "Reads the thread ID">;
def GroupId  : DXILOpMapping<94, int_dx_group_id, ComputeIDCategory, "Reads the group ID (SV_GroupID)">;
def ThreadIdInGroup : DXILOpMapping<95, int_dx_thread_id_in_group, ComputeIDCategory,
                                    "Reads the thread ID within the group (SV_GroupThreadID)">;
def FlattenedThreadIdInGroup_New : DXILOpMapping<96, int_dx_flattened_thread_id_in_group, ComputeIDCategory,
                                    "Provides a flattened index for a given thread within a given group (SV_GroupIndex)">;