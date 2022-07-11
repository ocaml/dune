#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef _WIN32

#include <Windows.h>
#include <Knownfolders.h>
#include <Shlobj.h>

value xdg__get_user_cache_dir(value v_unit)
{
  CAMLparam0();
  CAMLlocal1(v_res);
  WCHAR* wcp = NULL;
  HRESULT res;
  int wlen, len;

  res = SHGetKnownFolderPath(&FOLDERID_InternetCache, 0, NULL, &wcp);
  if (res != S_OK) {
    CoTaskMemFree(wcp);
    caml_raise_not_found();
  }

  wlen = wcslen(wcp);
  len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wcp, wlen, NULL, 0, NULL, NULL);
  if (!len) {
    CoTaskMemFree(wcp);
    caml_raise_not_found();
  }

  v_res = caml_alloc_string(len);

  if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wcp, wlen, (char *)String_val(v_res), len, NULL, NULL)) {
    CoTaskMemFree(wcp);
    caml_raise_not_found();
  }
  CoTaskMemFree(wcp);

  CAMLreturn(v_res);
}

#else /* _WIN32 */

value xdg__get_user_cache_dir(value v_unit)
{
  caml_invalid_argument("xdg__get_user_cache_dir");
}

#endif /* _WIN32 */
