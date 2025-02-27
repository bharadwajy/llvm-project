; RUN: llc -mtriple x86_64-pc-linux < %s  | FileCheck %s

; Test that we don't pollute the start of the file with debug sections

; CHECK:      .file	"<stdin>"
; CHECK-NEXT:  .text
; CHECK-NEXT: .globl	f
; CHECK-NEXT: .p2align	4
; CHECK-NEXT: .type	f,@function
; CHECK-NEXT: f:                                      # @f

; CHECK: .section .debug_str

define void @f() !dbg !4 {
  ret void, !dbg !9
}
!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!7, !8}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "foo", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, retainedTypes: !2, globals: !2, imports: !2)
!1 = !DIFile(filename: "/foo/test.c", directory: "/foo")
!2 = !{}
!4 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !5, isLocal: false, isDefinition: true, scopeLine: 1, flags: DIFlagPrototyped, isOptimized: true, unit: !0, retainedNodes: !2)
!5 = !DISubroutineType(types: !6)
!6 = !{null}
!7 = !{i32 2, !"Dwarf Version", i32 4}
!8 = !{i32 2, !"Debug Info Version", i32 3}
!9 = !DILocation(line: 1, column: 15, scope: !4)
