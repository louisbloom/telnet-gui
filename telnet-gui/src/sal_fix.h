/* SAL annotation compatibility for clang on Windows */
#ifdef __clang__
#ifndef _MSC_VER
#ifndef _Scanf_format_string_impl_
typedef const char *_Scanf_format_string_impl_;
#endif
#endif
#endif
