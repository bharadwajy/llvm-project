//===- DXILEmitter.cpp - DXIL operation Emitter ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// DXILEmitter uses the descriptions of DXIL operation to construct enum and
// helper functions for DXIL operation.
//
//===----------------------------------------------------------------------===//

#include "SequenceToOffsetTable.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/DXILABI.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"

#define NEW_CODE 1

using namespace llvm;
using namespace llvm::dxil;

namespace {

struct DXILShaderModel {
  int Major = 0;
  int Minor = 0;
};

struct DXILParameter {
  int Pos; // position in parameter list
  ParameterKind Kind;
  StringRef Name; // short, unique name
  StringRef Doc;  // the documentation description of this parameter
  bool IsConst;   // whether this argument requires a constant value in the IR
  StringRef EnumName; // the name of the enum type if applicable
  int MaxValue;       // the maximum value for this parameter if applicable
  DXILParameter(const Record *R);
};

struct DXILOperationDesc {
  StringRef OpName;   // name of DXIL operation
  int OpCode;         // ID of DXIL operation
  StringRef OpClass;  // name of the opcode class
  StringRef Category; // classification for this instruction
  StringRef Doc;      // the documentation description of this instruction

  SmallVector<DXILParameter> Params; // the operands that this instruction takes
  SmallVector<ParameterKind> OverloadTypes; // overload types if applicable
  std::string Attr; // operation attribute; reference to string representation
                  // of llvm::Attribute::AttrKind
  StringRef Intrinsic;  // The llvm intrinsic map to OpName. Default is "" which
                        // means no map exists
  bool IsDeriv = false; // whether this is some kind of derivative
  bool IsGradient = false; // whether this requires a gradient calculation
  bool IsFeedback = false; // whether this is a sampler feedback op
  bool IsWave =
      false; // whether this requires in-wave, cross-lane functionality
  bool RequiresUniformInputs = false; // whether this operation requires that
                                      // all of its inputs are uniform across
                                      // the wave
  SmallVector<StringRef, 4>
      ShaderStages; // shader stages to which this applies, empty for all.
  DXILShaderModel ShaderModel;           // minimum shader model required
  DXILShaderModel ShaderModelTranslated; // minimum shader model required with
                                         // translation by linker
  int OverloadParamIndex; // parameter index which control the overload.
                          // When < 0, should be only 1 overload type.
  SmallVector<StringRef, 4> counters; // counters for this inst.
  DXILOperationDesc(const Record *);
};

#if NEW_CODE
struct DXILOpIntrMap {
  int OpCode;           // Opcode corresponding to DXIL Operation
  StringRef Intrinsic;  // The llvm intrinsic that maps to map to DXIL Operation with OpCode.
                        // Default is "" which means no map exists
  DXILOpIntrMap(const Record *R);
};

struct DXILOperationDescNew {
  std::string OpName;   // name of DXIL operation
  int OpCode;         // ID of DXIL operation
  StringRef OpClass;  // name of the opcode class
  StringRef Category; // classification for this instruction
  StringRef Doc;      // the documentation description of this instruction

  SmallVector<std::string> OpTypeNames; // Vector of operand type name strings - return type is at index 0
  // SmallVector<ParameterKind> OverloadTypes; // overload types if applicable
  SmallVector<std::string> OpAttributes; // operation attribute represented as strings
  StringRef Intrinsic;  // The llvm intrinsic map to OpName. Default is "" which
                        // means no map exists
  bool IsDeriv = false; // whether this is some kind of derivative
  bool IsGradient = false; // whether this requires a gradient calculation
  bool IsFeedback = false; // whether this is a sampler feedback op
  bool IsWave =
      false; // whether this requires in-wave, cross-lane functionality
  bool RequiresUniformInputs = false; // whether this operation requires that
                                      // all of its inputs are uniform across
                                      // the wave
  SmallVector<StringRef, 4>
      ShaderStages; // shader stages to which this applies, empty for all.
  DXILShaderModel ShaderModel;           // minimum shader model required
  DXILShaderModel ShaderModelTranslated; // minimum shader model required with
                                         // translation by linker
  int OverloadParamIndex; // parameter index which control the overload.
                          // When < 0, should be only 1 overload type.
  SmallVector<StringRef, 4> counters; // counters for this inst.
  DXILOperationDescNew(const Record *);
};
#endif
} // end anonymous namespace

#if NEW_CODE
DXILOpIntrMap::DXILOpIntrMap(const Record *R) {
  OpCode = R->getValueAsInt("OpCode");
  if (R->getValue("LLVMIntrinsic")) {
    auto *IntrinsicDef = R->getValueAsDef("llvm_intrinsic");
    auto DefName = IntrinsicDef->getName();
    assert(DefName.starts_with("int_") && "invalid intrinsic name");
    // Remove the int_ from intrinsic name.
    Intrinsic = DefName.substr(4);
  }
}
#endif

/*!
 Convert DXIL type name string to dxil::ParameterKind

 @param typeNameStr Type name string
 @return ParameterKind As defined in llvm/Support/DXILABI.h
*/
static ParameterKind lookupParameterKind(StringRef typeNameStr) {
  auto paramKind = StringSwitch<ParameterKind>(typeNameStr)
                       .Case("llvm_void_ty", ParameterKind::VOID)
                       .Case("llvm_half_ty", ParameterKind::HALF)
                       .Case("llvm_float_ty", ParameterKind::FLOAT)
                       .Case("llvm_double_ty", ParameterKind::DOUBLE)
                       .Case("llvm_i1_ty", ParameterKind::I1)
                       .Case("llvm_i8_ty", ParameterKind::I8)
                       .Case("llvm_i16_ty", ParameterKind::I16)
                       .Case("llvm_i32_ty", ParameterKind::I32)
                       .Case("llvm_i64_ty", ParameterKind::I64)
                       .Case("llvm_anyfloat_ty", ParameterKind::OVERLOAD)
                       .Case("llvm_anyint_ty", ParameterKind::OVERLOAD)
                       .Case("dxil_handle_ty", ParameterKind::DXIL_HANDLE)
                       .Case("dxil_cbuffer_ty", ParameterKind::CBUFFER_RET)
                       .Case("dxil_resource_ty", ParameterKind::RESOURCE_RET)
                       .Default(ParameterKind::INVALID);
  assert(paramKind != ParameterKind::INVALID &&
         "Unsupported DXIL Type specified");
  return paramKind;
}

DXILOperationDesc::DXILOperationDesc(const Record *R) {
  OpName = R->getValueAsString("OpName");
  OpCode = R->getValueAsInt("OpCode");
  OpClass = R->getValueAsDef("OpClass")->getValueAsString("Name");
  Category = R->getValueAsDef("OpCategory")->getValueAsString("Name");

  if (R->getValue("llvm_intrinsic")) {
    auto *IntrinsicDef = R->getValueAsDef("llvm_intrinsic");
    auto DefName = IntrinsicDef->getName();
    assert(DefName.starts_with("int_") && "invalid intrinsic name");
    // Remove the int_ from intrinsic name.
    Intrinsic = DefName.substr(4);
  }

  Doc = R->getValueAsString("Doc");

  ListInit *ParamList = R->getValueAsListInit("Params");
  OverloadParamIndex = -1;
  for (unsigned I = 0; I < ParamList->size(); ++I) {
    Record *Param = ParamList->getElementAsRecord(I);
    Params.emplace_back(DXILParameter(Param));
    auto &CurParam = Params.back();
    if (CurParam.Kind >= ParameterKind::OVERLOAD)
      OverloadParamIndex = I;
  }
  ListInit *OverloadTypeList = R->getValueAsListInit("OverloadTypes");

  for (unsigned I = 0; I < OverloadTypeList->size(); ++I) {
    Record *R = OverloadTypeList->getElementAsRecord(I);
    OverloadTypes.emplace_back(lookupParameterKind(R->getNameInitAsString()));
  }
  Attr = R->getValue("Attribute")->getValue()->getAsString();
}

#if NEW_CODE
DXILOperationDescNew::DXILOperationDescNew(const Record *R) {
  OpName = R->getNameInitAsString();
  OpCode = R->getValueAsInt("OpCode");
  Category = R->getValueAsDef("OpCategory")->getValueAsString("Name");

  Doc = R->getValueAsString("Doc");

  if (R->getValue("LLVMIntrinsic")) {
    auto *IntrinsicDef = R->getValueAsDef("LLVMIntrinsic");
    auto DefName = IntrinsicDef->getName();
    assert(DefName.starts_with("int_") && "invalid intrinsic name");
    // Remove the int_ from intrinsic name.
    Intrinsic = DefName.substr(4);
    // NOTE: It is expected that return type and parameter types of
    // DXIL Operation are the same as that of the intrinsic. Deviations
    // are expected to be encoded in TableGen record specification and
    // handled accordingly here. Support to be added later, as needed.
    // Get parameter type list of the intrinsic. Types attribute contains
    // the list of as [returnType, param1Type,, param2Type, ...]
    auto TypeList = IntrinsicDef->getValueAsListInit("Types");
    unsigned TypeListSize = TypeList->size();
    OverloadParamIndex = -1;
    // Populate return type and parameter type names
    for (unsigned i = 0; i < TypeListSize; i++) {
        OpTypeNames.emplace_back(TypeList->getElement(i)->getAsString());
        // Get the overload parameter index.
        // REVISIT : Seems hacky. Is it possible that more than one parameter can be
        // of overload kind??
        if (i > 0) {
          auto &CurParam = OpTypeNames.back();
          if (lookupParameterKind(CurParam) >= ParameterKind::OVERLOAD) {
              OverloadParamIndex = i;
          }
        }
    }
    // Determine the operation class (unary/binary) based on the number of parameters
    // As parameter types are being considered, skip return type
    auto ParamSize = TypeListSize  - 1;
    if (ParamSize == 0) {
      OpClass = "Nullary";
    } else    if (ParamSize == 1) {
      OpClass = "Unary";
    } else if (ParamSize == 2) {
      OpClass = "Binary";
    } else {
      // TODO: Extend as needed
      llvm_unreachable("Unhandled parameter size");
    }
    // NOTE: For now, assume that attributes of DXIL Operation are the same as
    // that of the intrinsic. Deviations are expected to be encoded in TableGen
    // record specification and handled accordingly here. Support to be added later.
    auto IntrPropList = IntrinsicDef->getValueAsListInit("IntrProperties");
    auto IntrPropListSize = IntrPropList->size();
    for (unsigned i = 0; i < IntrPropListSize; i++) {
        OpAttributes.emplace_back(IntrPropList->getElement(i)->getAsString());
    }
  }
}
#endif

DXILParameter::DXILParameter(const Record *R) {
  Name = R->getValueAsString("Name");
  Pos = R->getValueAsInt("Pos");
  Kind =
      lookupParameterKind(R->getValue("ParamType")->getValue()->getAsString());
  if (R->getValue("Doc"))
    Doc = R->getValueAsString("Doc");
  IsConst = R->getValueAsBit("IsConstant");
  EnumName = R->getValueAsString("EnumName");
  MaxValue = R->getValueAsInt("MaxValue");
}

static std::string parameterKindToString(ParameterKind Kind) {
  switch (Kind) {
  case ParameterKind::INVALID:
    return "INVALID";
  case ParameterKind::VOID:
    return "VOID";
  case ParameterKind::HALF:
    return "HALF";
  case ParameterKind::FLOAT:
    return "FLOAT";
  case ParameterKind::DOUBLE:
    return "DOUBLE";
  case ParameterKind::I1:
    return "I1";
  case ParameterKind::I8:
    return "I8";
  case ParameterKind::I16:
    return "I16";
  case ParameterKind::I32:
    return "I32";
  case ParameterKind::I64:
    return "I64";
  case ParameterKind::OVERLOAD:
    return "OVERLOAD";
  case ParameterKind::CBUFFER_RET:
    return "CBUFFER_RET";
  case ParameterKind::RESOURCE_RET:
    return "RESOURCE_RET";
  case ParameterKind::DXIL_HANDLE:
    return "DXIL_HANDLE";
  }
  llvm_unreachable("Unknown llvm::dxil::ParameterKind enum");
}

#if !NEW_CODE
static void emitDXILOpEnum(DXILOperationDesc &Op, raw_ostream &OS) {
  // Name = ID, // Doc
  OS << Op.OpName << " = " << Op.OpCode << ", // " << Op.Doc << "\n";
}
#endif

#if NEW_CODE
static void emitDXILOpEnumNew(DXILOperationDescNew &Op, raw_ostream &OS) {
  // Name = ID, // Doc
  OS << Op.OpName << " = " << Op.OpCode << ", // " << Op.Doc << "\n";
}
#endif

static std::string buildCategoryStr(StringSet<> &Cetegorys) {
  std::string Str;
  raw_string_ostream OS(Str);
  for (auto &It : Cetegorys) {
    OS << " " << It.getKey();
  }
  return OS.str();
}

#if !NEW_CODE
// Emit enum declaration for DXIL.
static void emitDXILEnums(std::vector<DXILOperationDesc> &Ops,
                          raw_ostream &OS) {
  // Sort by Category + OpName.
  llvm::sort(Ops, [](DXILOperationDesc &A, DXILOperationDesc &B) {
    // Group by Category first.
    if (A.Category == B.Category)
      // Inside same Category, order by OpName.
      return A.OpName < B.OpName;
    else
      return A.Category < B.Category;
  });

  OS << "// Enumeration for operations specified by DXIL\n";
  OS << "enum class OpCode : unsigned {\n";

  StringMap<StringSet<>> ClassMap;
  StringRef PrevCategory = "";
  for (auto &Op : Ops) {
    StringRef Category = Op.Category;
    if (Category != PrevCategory) {
      OS << "\n// " << Category << "\n";
      PrevCategory = Category;
    }
    emitDXILOpEnum(Op, OS);
    auto It = ClassMap.find(Op.OpClass);
    if (It != ClassMap.end()) {
      It->second.insert(Op.Category);
    } else {
      ClassMap[Op.OpClass].insert(Op.Category);
    }
  }

  OS << "\n};\n\n";

  std::vector<std::pair<std::string, std::string>> ClassVec;
  for (auto &It : ClassMap) {
    ClassVec.emplace_back(
        std::pair(It.getKey().str(), buildCategoryStr(It.second)));
  }
  // Sort by Category + ClassName.
  llvm::sort(ClassVec, [](std::pair<std::string, std::string> &A,
                          std::pair<std::string, std::string> &B) {
    StringRef ClassA = A.first;
    StringRef CategoryA = A.second;
    StringRef ClassB = B.first;
    StringRef CategoryB = B.second;
    // Group by Category first.
    if (CategoryA == CategoryB)
      // Inside same Category, order by ClassName.
      return ClassA < ClassB;
    else
      return CategoryA < CategoryB;
  });

  OS << "// Groups for DXIL operations with equivalent function templates\n";
  OS << "enum class OpCodeClass : unsigned {\n";
  PrevCategory = "";
  for (auto &It : ClassVec) {

    StringRef Category = It.second;
    if (Category != PrevCategory) {
      OS << "\n// " << Category << "\n";
      PrevCategory = Category;
    }
    StringRef Name = It.first;
    OS << Name << ",\n";
  }
  OS << "\n};\n\n";
}
#endif

#if NEW_CODE
static void emitDXILEnumsNew(std::vector<DXILOperationDescNew> &Ops,
                          raw_ostream &OS) {
  // Sort by OpCode
  llvm::sort(Ops, [](DXILOperationDescNew &A, DXILOperationDescNew &B) {
      return A.OpCode < B.OpCode;
  });

  OS << "// Enumeration for operations specified by DXIL\n";
  OS << "enum class OpCode : unsigned {\n";

  StringMap<StringSet<>> ClassMap;
  StringRef PrevCategory = "";
  for (auto &Op : Ops) {
    StringRef Category = Op.Category;
    if (Category != PrevCategory) {
      OS << "\n// " << Category << "\n";
      PrevCategory = Category;
    }
    emitDXILOpEnumNew(Op, OS);
    auto It = ClassMap.find(Op.OpClass);
    if (It != ClassMap.end()) {
      It->second.insert(Op.Category);
    } else {
      ClassMap[Op.OpClass].insert(Op.Category);
    }
  }

  OS << "\n};\n\n";

  std::vector<std::pair<std::string, std::string>> ClassVec;
  for (auto &It : ClassMap) {
    ClassVec.emplace_back(
        std::make_pair(It.getKey().str(), buildCategoryStr(It.second)));
  }
  // Sort by Category + ClassName.
  llvm::sort(ClassVec, [](std::pair<std::string, std::string> &A,
                          std::pair<std::string, std::string> &B) {
    StringRef ClassA = A.first;
    StringRef CategoryA = A.second;
    StringRef ClassB = B.first;
    StringRef CategoryB = B.second;
    // Group by Category first.
    if (CategoryA == CategoryB)
      // Inside same Category, order by ClassName.
      return ClassA < ClassB;
    else
      return CategoryA < CategoryB;
  });

  OS << "// Groups for DXIL operations with equivalent function templates\n";
  OS << "enum class OpCodeClass : unsigned {\n";
  PrevCategory = "";
  for (auto &It : ClassVec) {
    StringRef Name = It.first;
    OS << Name << ",\n";
  }
  OS << "\n};\n\n";
}
#endif

#if !NEW_CODE
// Emit map from llvm intrinsic to DXIL operation.
static void emitDXILIntrinsicMap(std::vector<DXILOperationDesc> &Ops,
                                 raw_ostream &OS) {
  OS << "\n";
  // FIXME: use array instead of SmallDenseMap.
  OS << "static const SmallDenseMap<Intrinsic::ID, dxil::OpCode> LowerMap = "
        "{\n";
  for (auto &Op : Ops) {
    if (Op.Intrinsic.empty())
      continue;
    // {Intrinsic::sin, dxil::OpCode::Sin},
    OS << "  { Intrinsic::" << Op.Intrinsic << ", dxil::OpCode::" << Op.OpName
       << "},\n";
  }
  OS << "};\n";
  OS << "\n";
}
#endif

#if NEW_CODE
// Emit map from llvm intrinsic to DXIL operation.
static void emitDXILIntrinsicMapNew(std::vector<DXILOperationDescNew> &Ops,
                                    raw_ostream &OS) {
  OS << "\n";
  // FIXME: use array instead of SmallDenseMap.
  OS << "static const SmallDenseMap<Intrinsic::ID, dxil::OpCode> LowerMap = "
        "{\n";
  for (auto &Op : Ops) {
    if (Op.Intrinsic.empty())
      continue;
    // {Intrinsic::sin, dxil::OpCode::Sin},
    OS << "  { Intrinsic::" << Op.Intrinsic << ", dxil::OpCode::" << Op.OpName
       << "},\n";
  }
  OS << "};\n";
  OS << "\n";
}
#endif

#if !NEW_CODE
/*!
 Convert operation attribute string to Attribute enum

 @param Attr string reference
 @return std::string Attribute enum string
 */
static std::string emitDXILOperationAttr(StringRef Attr) {
  return StringSwitch<std::string>(Attr)
      .Case("ReadNone", "Attribute::ReadNone")
      .Case("ReadOnly", "Attribute::ReadOnly")
      .Default("Attribute::None");
}
#endif

#if NEW_CODE
/*!
 Convert operation attribute string to Attribute enum

 @param Attr string reference
 @return std::string Attribute enum string
 */
static std::string emitDXILOperationAttrNew(SmallVector<std::string> Attrs) {
  for (auto Attr : Attrs) {
    // For now just recognize IntrNoMem and IntrReadMem as valid and ignore others
    if (Attr == "IntrNoMem") {
      return "Attribute::ReadNone";
    } else if (Attr == "IntrReadMem") {
      return "Attribute::ReadOnly";
    }
  }
  return "Attribute::None";
}
#endif

#if !NEW_CODE
static std::string overloadKindStr(ParameterKind Overload) {
  switch (Overload) {
  case ParameterKind::HALF:
    return "OverloadKind::HALF";
  case ParameterKind::FLOAT:
    return "OverloadKind::FLOAT";
  case ParameterKind::DOUBLE:
    return "OverloadKind::DOUBLE";
  case ParameterKind::I1:
    return "OverloadKind::I1";
  case ParameterKind::I8:
    return "OverloadKind::I8";
  case ParameterKind::I16:
    return "OverloadKind::I16";
  case ParameterKind::I32:
    return "OverloadKind::I32";
  case ParameterKind::I64:
    return "OverloadKind::I64";
  case ParameterKind::VOID:
    return "OverloadKind::VOID";
  default:
    return "OverloadKind::UNKNOWN";
  }
}
#endif

#if !NEW_CODE
static std::string
getDXILOperationOverloads(SmallVector<ParameterKind> Overloads) {
  // Format is: OverloadKind::FLOAT | OverloadKind::HALF
  auto It = Overloads.begin();
  std::string Result;
  raw_string_ostream OS(Result);
  OS << overloadKindStr(*It);
  for (++It; It != Overloads.end(); ++It) {
    OS << " | " << overloadKindStr(*It);
  }
  return OS.str();
}
#endif

#if NEW_CODE
static std::string
emitOverloadKindStr(std::string OpTypeStr) {
  std::string Result = StringSwitch<std::string>(OpTypeStr)
    .Case("llvm_i16_ty", "OverloadKind::I16")
    .Case("llvm_i32_ty", "OverloadKind::I32")
    .Case("llvm_i64_ty", "OverloadKind::I64")
    .Case("llvm_anyint_ty", "OverloadKind::I16 | OverloadKind::I32 | OverloadKind::I64")
    .Case("llvm_half_ty", "OverloadKind::HALF")
    .Case("llvm_float_ty", "OverloadKind::FLOAT")
    .Case("llvm_double_ty", "OverloadKind::DOUBLE")
    .Case("llvm_anyfloat_ty", "OverloadKind::HALF | OverloadKind::FLOAT | OverloadKind::DOUBLE")
    .Default("UNHANDLED_TYPE");

  assert(Result != "UNHANDLED_TYPE" && "Unhandled parameter type");
  return Result;
}

#if 0
// Emit Overload Kind string representing the return type specified at
// index 0 of OpTypeStrVec
static std::string emitTypesAsOverloadKindStr(SmallVector<std::string> OpTypeStrVec) {
  std::string Result;
  raw_string_ostream OS(Result);
  if (OpTypeStrVec.size() > 0) {
    OS << emitOverloadKindStr(OpTypeStrVec[0]);
  }

  return OS.str();
}
#endif
#endif

static std::string lowerFirstLetter(StringRef Name) {
  if (Name.empty())
    return "";

  std::string LowerName = Name.str();
  LowerName[0] = llvm::toLower(Name[0]);
  return LowerName;
}

static std::string getDXILOpClassName(StringRef OpClass) {
  // Lower first letter expect for special case.
  return StringSwitch<std::string>(OpClass)
      .Case("CBufferLoad", "cbufferLoad")
      .Case("CBufferLoadLegacy", "cbufferLoadLegacy")
      .Case("GSInstanceID", "gsInstanceID")
      .Default(lowerFirstLetter(OpClass));
}

#if !NEW_CODE
static void emitDXILOperationTable(std::vector<DXILOperationDesc> &Ops,
                                   raw_ostream &OS) {
  // Sort by OpCode.
  llvm::sort(Ops, [](DXILOperationDesc &A, DXILOperationDesc &B) {
    return A.OpCode < B.OpCode;
  });

  // Collect Names.
  SequenceToOffsetTable<std::string> OpClassStrings;
  SequenceToOffsetTable<std::string> OpStrings;
  SequenceToOffsetTable<SmallVector<ParameterKind>> Parameters;

  StringMap<SmallVector<ParameterKind>> ParameterMap;
  StringSet<> ClassSet;
  for (auto &Op : Ops) {
    OpStrings.add(Op.OpName.str());

    if (ClassSet.contains(Op.OpClass))
      continue;
    ClassSet.insert(Op.OpClass);
    OpClassStrings.add(getDXILOpClassName(Op.OpClass));
    SmallVector<ParameterKind> ParamKindVec;
    for (auto &Param : Op.Params) {
      ParamKindVec.emplace_back(Param.Kind);
    }
    ParameterMap[Op.OpClass] = ParamKindVec;
    Parameters.add(ParamKindVec);
  }

  // Layout names.
  OpStrings.layout();
  OpClassStrings.layout();
  Parameters.layout();

  // Emit the DXIL operation table.
  //{dxil::OpCode::Sin, OpCodeNameIndex, OpCodeClass::Unary,
  // OpCodeClassNameIndex,
  // OverloadKind::FLOAT | OverloadKind::HALF, Attribute::AttrKind::ReadNone, 0,
  // 3, ParameterTableOffset},
  OS << "static const OpCodeProperty *getOpCodeProperty(dxil::OpCode Op) "
        "{\n";

  OS << "  static const OpCodeProperty OpCodeProps[] = {\n";
  for (auto &Op : Ops) {
    OS << "  { dxil::OpCode::" << Op.OpName << ", "
       << OpStrings.get(Op.OpName.str()) << ", OpCodeClass::" << Op.OpClass
       << ", " << OpClassStrings.get(getDXILOpClassName(Op.OpClass)) << ", "
       << getDXILOperationOverloads(Op.OverloadTypes) << ", "
       << emitDXILOperationAttr(Op.Attr) << ", " << Op.OverloadParamIndex
       << ", " << Op.Params.size() << ", "
       << Parameters.get(ParameterMap[Op.OpClass]) << " },\n";
  }
  OS << "  };\n";

  OS << "  // FIXME: change search to indexing with\n";
  OS << "  // Op once all DXIL operations are added.\n";
  OS << "  OpCodeProperty TmpProp;\n";
  OS << "  TmpProp.OpCode = Op;\n";
  OS << "  const OpCodeProperty *Prop =\n";
  OS << "      llvm::lower_bound(OpCodeProps, TmpProp,\n";
  OS << "                        [](const OpCodeProperty &A, const "
        "OpCodeProperty &B) {\n";
  OS << "                          return A.OpCode < B.OpCode;\n";
  OS << "                        });\n";
  OS << "  assert(Prop && \"fail to find OpCodeProperty\");\n";
  OS << "  return Prop;\n";
  OS << "}\n\n";

  // Emit the string tables.
  OS << "static const char *getOpCodeName(dxil::OpCode Op) {\n\n";

  OpStrings.emitStringLiteralDef(OS,
                                 "  static const char DXILOpCodeNameTable[]");

  OS << "  auto *Prop = getOpCodeProperty(Op);\n";
  OS << "  unsigned Index = Prop->OpCodeNameOffset;\n";
  OS << "  return DXILOpCodeNameTable + Index;\n";
  OS << "}\n\n";

  OS << "static const char *getOpCodeClassName(const OpCodeProperty &Prop) "
        "{\n\n";

  OpClassStrings.emitStringLiteralDef(
      OS, "  static const char DXILOpCodeClassNameTable[]");

  OS << "  unsigned Index = Prop.OpCodeClassNameOffset;\n";
  OS << "  return DXILOpCodeClassNameTable + Index;\n";
  OS << "}\n ";

  OS << "static const ParameterKind *getOpCodeParameterKind(const "
        "OpCodeProperty &Prop) "
        "{\n\n";
  OS << "  static const ParameterKind DXILOpParameterKindTable[] = {\n";
  Parameters.emit(
      OS,
      [](raw_ostream &ParamOS, ParameterKind Kind) {
        ParamOS << "ParameterKind::" << parameterKindToString(Kind);
      },
      "ParameterKind::INVALID");
  OS << "  };\n\n";
  OS << "  unsigned Index = Prop.ParameterTableOffset;\n";
  OS << "  return DXILOpParameterKindTable + Index;\n";
  OS << "}\n ";
}
#endif

#if NEW_CODE
static void emitDXILOperationTableNew(std::vector<DXILOperationDescNew> &Ops,
                                   raw_ostream &OS) {
  // Sort by OpCode.
  llvm::sort(Ops, [](DXILOperationDescNew &A, DXILOperationDescNew &B) {
    return A.OpCode < B.OpCode;
  });

  // Collect Names.
  SequenceToOffsetTable<std::string> OpClassStrings;
  SequenceToOffsetTable<std::string> OpStrings;
  SequenceToOffsetTable<SmallVector<ParameterKind>> Parameters;

  StringMap<SmallVector<ParameterKind>> ParameterMap;
  StringSet<> ClassSet;
  for (auto &Op : Ops) {
    OpStrings.add(Op.OpName);

    if (ClassSet.contains(Op.OpClass))
      continue;
    ClassSet.insert(Op.OpClass);
    OpClassStrings.add(getDXILOpClassName(Op.OpClass));
    SmallVector<ParameterKind> ParamKindVec;
    // ParamKindVec is a vector of parameters. Skip return type at index 0
    for (unsigned i = 1; i < Op.OpTypeNames.size(); i++) {
      ParamKindVec.emplace_back(lookupParameterKind(Op.OpTypeNames[i]));
    }
    ParameterMap[Op.OpClass] = ParamKindVec;
    Parameters.add(ParamKindVec);
  }

  // Layout names.
  OpStrings.layout();
  OpClassStrings.layout();
  Parameters.layout();

  // Emit the DXIL operation table.
  //{dxil::OpCode::Sin, OpCodeNameIndex, OpCodeClass::Unary,
  // OpCodeClassNameIndex,
  // OverloadKind::FLOAT | OverloadKind::HALF, Attribute::AttrKind::ReadNone, 0,
  // 3, ParameterTableOffset},
  OS << "static const OpCodeProperty *getOpCodeProperty(dxil::OpCode Op) "
        "{\n";

  OS << "  static const OpCodeProperty OpCodeProps[] = {\n";
  for (auto &Op : Ops) {
    OS << "  { dxil::OpCode::" << Op.OpName << ", "
       << OpStrings.get(Op.OpName) << ", OpCodeClass::" << Op.OpClass
       << ", " << OpClassStrings.get(getDXILOpClassName(Op.OpClass)) << ", "
       << emitOverloadKindStr(Op.OpTypeNames[0]) << ", "
       << emitDXILOperationAttrNew(Op.OpAttributes) << ", " << Op.OverloadParamIndex
       << ", " << Op.OpTypeNames.size() - 1 << ", "
       << Parameters.get(ParameterMap[Op.OpClass]) << " },\n";
  }
  OS << "  };\n";

  OS << "  // FIXME: change search to indexing with\n";
  OS << "  // Op once all DXIL operations are added.\n";
  OS << "  OpCodeProperty TmpProp;\n";
  OS << "  TmpProp.OpCode = Op;\n";
  OS << "  const OpCodeProperty *Prop =\n";
  OS << "      llvm::lower_bound(OpCodeProps, TmpProp,\n";
  OS << "                        [](const OpCodeProperty &A, const "
        "OpCodeProperty &B) {\n";
  OS << "                          return A.OpCode < B.OpCode;\n";
  OS << "                        });\n";
  OS << "  assert(Prop && \"fail to find OpCodeProperty\");\n";
  OS << "  return Prop;\n";
  OS << "}\n\n";

  // Emit the string tables.
  OS << "static const char *getOpCodeName(dxil::OpCode Op) {\n\n";

  OpStrings.emitStringLiteralDef(OS,
                                 "  static const char DXILOpCodeNameTable[]");

  OS << "  auto *Prop = getOpCodeProperty(Op);\n";
  OS << "  unsigned Index = Prop->OpCodeNameOffset;\n";
  OS << "  return DXILOpCodeNameTable + Index;\n";
  OS << "}\n\n";

  OS << "static const char *getOpCodeClassName(const OpCodeProperty &Prop) "
        "{\n\n";

  OpClassStrings.emitStringLiteralDef(
      OS, "  static const char DXILOpCodeClassNameTable[]");

  OS << "  unsigned Index = Prop.OpCodeClassNameOffset;\n";
  OS << "  return DXILOpCodeClassNameTable + Index;\n";
  OS << "}\n ";

  OS << "static const ParameterKind *getOpCodeParameterKind(const "
        "OpCodeProperty &Prop) "
        "{\n\n";
  OS << "  static const ParameterKind DXILOpParameterKindTable[] = {\n";
  Parameters.emit(
      OS,
      [](raw_ostream &ParamOS, ParameterKind Kind) {
        ParamOS << "ParameterKind::" << parameterKindToString(Kind);
      },
      "ParameterKind::INVALID");
  OS << "  };\n\n";
  OS << "  unsigned Index = Prop.ParameterTableOffset;\n";
  OS << "  return DXILOpParameterKindTable + Index;\n";
  OS << "}\n ";
}
#endif

static void EmitDXILOperation(RecordKeeper &Records, raw_ostream &OS) {
  OS << "// Generated code, do not edit.\n";
  OS << "\n";

#if !NEW_CODE
  std::vector<Record *> Ops = Records.getAllDerivedDefinitions("DXILOperation");
  std::vector<DXILOperationDesc> DXILOps;
  DXILOps.reserve(Ops.size());
  for (auto *Record : Ops) {
    DXILOps.emplace_back(DXILOperationDesc(Record));
  }

  OS << "#ifdef DXIL_OP_ENUM_OLD\n";
  emitDXILEnums(DXILOps, OS);
  OS << "#endif\n\n";

  OS << "#ifdef DXIL_OP_INTRINSIC_MAP_OLD\n";
  emitDXILIntrinsicMap(DXILOps, OS);
  OS << "#endif\n\n";

  OS << "#ifdef DXIL_OP_OPERATION_TABLE_OLD\n";
  emitDXILOperationTable(DXILOps, OS);
  OS << "#endif\n\n";

  OS << "\n";
#endif

#if NEW_CODE
  // New code
  std::vector<Record *> OpIntrMaps = Records.getAllDerivedDefinitions("DXILOpMapping");
  std::vector<DXILOperationDescNew> DXILOpsNew;
  for (auto *Record : OpIntrMaps) {
    DXILOpsNew.emplace_back(DXILOperationDescNew(Record));
  }
  OS << "#ifdef DXIL_OP_ENUM\n";
  emitDXILEnumsNew(DXILOpsNew, OS);
  OS << "#endif\n\n";
  OS << "#ifdef DXIL_OP_INTRINSIC_MAP\n";
  emitDXILIntrinsicMapNew(DXILOpsNew, OS);
  OS << "#endif\n\n";
  OS << "#ifdef DXIL_OP_OPERATION_TABLE\n";
  emitDXILOperationTableNew(DXILOpsNew, OS);
  OS << "#endif\n\n";
#endif
}

static TableGen::Emitter::Opt X("gen-dxil-operation", EmitDXILOperation,
                                "Generate DXIL operation information");
