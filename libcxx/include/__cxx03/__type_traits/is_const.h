//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef _LIBCPP___CXX03___TYPE_TRAITS_IS_CONST_H
#define _LIBCPP___CXX03___TYPE_TRAITS_IS_CONST_H

#include <__cxx03/__config>
#include <__cxx03/__type_traits/integral_constant.h>

#if !defined(_LIBCPP_HAS_NO_PRAGMA_SYSTEM_HEADER)
#  pragma GCC system_header
#endif

_LIBCPP_BEGIN_NAMESPACE_STD

#if __has_builtin(__is_const)

template <class _Tp>
struct _LIBCPP_TEMPLATE_VIS is_const : _BoolConstant<__is_const(_Tp)> {};

#else

template <class _Tp>
struct _LIBCPP_TEMPLATE_VIS is_const : public false_type {};
template <class _Tp>
struct _LIBCPP_TEMPLATE_VIS is_const<_Tp const> : public true_type {};

#endif // __has_builtin(__is_const)

_LIBCPP_END_NAMESPACE_STD

#endif // _LIBCPP___CXX03___TYPE_TRAITS_IS_CONST_H
