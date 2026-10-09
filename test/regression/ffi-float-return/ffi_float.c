#include <stdint.h>
#include <stddef.h>

/*
 * Foreign-call ABI fixture.
 *
 * The code generator declares a foreign function with its raw (unboxed)
 * return type and reads the result out of the machine register for that type
 * (see Coal.Kernel.LLVM.Codegen's ECall / irTailECall). This function returns
 * an unboxed `double` accordingly: it clobbers xmm0 with unrelated
 * floating-point work before returning, so the caller cannot be relying on a
 * stale register value.
 */
double ffi_double_clobber(void *_n)
{
    int32_t n = (int32_t) (intptr_t) _n;
    volatile float scratch = 1.0f;

    for (int i = 0; i < 8; i++) {
        scratch = scratch * 1.5f + (float) i;
    }

    return (double) n;
}
