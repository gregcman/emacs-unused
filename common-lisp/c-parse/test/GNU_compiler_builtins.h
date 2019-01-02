//https://stackoverflow.com/questions/28983726/pycparser-not-working-on-preprocessed-code
typedef struct __builtin_va_list { } __builtin_va_list;

#define __attribute__(x)
#define __extension__
//https://github.com/eliben/pycparser/issues/210
#define __asm__(x)
#define __const const
#define __inline__ inline
#define __inline inline
#define __restrict
#define __signed__ signed
#define __GNUC_VA_LIST
#define __gnuc_va_list char
//https://stackoverflow.com/questions/3385515/static-assert-in-c
#define _Static_assert(COND,MSG) 
