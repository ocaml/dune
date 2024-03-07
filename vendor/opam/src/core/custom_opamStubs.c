#include <caml/mlvalues.h>
#include <caml/fail.h>

#if defined(_WIN32)

#include <caml/memory.h>
#include <caml/osdeps.h>
#include <caml/alloc.h>
#include <Windows.h>

typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS2) (HANDLE, USHORT *, USHORT *);

static LPFN_ISWOW64PROCESS2 pIsWow64Process2 = NULL;

CAMLprim value OPAMW_GetWindowsVersion(value unit)
{
  value result;
  result = caml_alloc_small(4, 0);
  Field(result, 0) = Val_int(caml_win32_major);
  Field(result, 1) = Val_int(caml_win32_minor);
  Field(result, 2) = Val_int(caml_win32_build);
  Field(result, 3) = Val_int(caml_win32_revision);

  return result;
}

static inline value get_native_cpu_architecture(void)
{
  SYSTEM_INFO SystemInfo;
  GetNativeSystemInfo(&SystemInfo);
  switch (SystemInfo.wProcessorArchitecture) {
  case PROCESSOR_ARCHITECTURE_AMD64:
    return Val_int(0);
  case PROCESSOR_ARCHITECTURE_ARM:
    return Val_int(1);
  case PROCESSOR_ARCHITECTURE_ARM64:
    return Val_int(2);
  case PROCESSOR_ARCHITECTURE_IA64:
    return Val_int(3); /* Wow! */
  case PROCESSOR_ARCHITECTURE_INTEL:
    return Val_int(4);
  default: /* PROCESSOR_ARCHITECTURE_UNKNOWN */
    return Val_int(5);
  }
}

static inline value get_PE_cpu_architecture(USHORT image)
{
  switch (image) {
  case IMAGE_FILE_MACHINE_I386:
    return Val_int(4);
  case IMAGE_FILE_MACHINE_ARM:
  case IMAGE_FILE_MACHINE_THUMB:
  case IMAGE_FILE_MACHINE_ARMNT:
    return Val_int(1);
  case IMAGE_FILE_MACHINE_IA64:
    return Val_int(3); /* Wow! */
  case IMAGE_FILE_MACHINE_AMD64:
    return Val_int(0);
  case IMAGE_FILE_MACHINE_ARM64:
    return Val_int(2);
  default:
    return Val_int(5);
  }
}

CAMLprim value OPAMW_GetArchitecture(value unit)
{
  USHORT ProcessMachine, NativeMachine;

  if (!pIsWow64Process2)
    pIsWow64Process2 =
      (LPFN_ISWOW64PROCESS2)GetProcAddress(GetModuleHandleW(L"kernel32"), "IsWow64Process2");

  if (!pIsWow64Process2 || !pIsWow64Process2(GetCurrentProcess(), &ProcessMachine, &NativeMachine))
    return get_native_cpu_architecture();
  else
    return get_PE_cpu_architecture(NativeMachine);
}

#else

static char * unavailable_message =
  "Windows stubs are only allowed to be called on Windows.";


CAMLprim value OPAMW_GetWindowsVersion(value unit)
{
  (void)unit;
  caml_failwith(unavailable_message);
}

CAMLprim value OPAMW_GetArchitecture(value unit)
{
  (void)unit;
  caml_failwith(unavailable_message);
}

#endif
