#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef _WIN32

#include <Windows.h>
#include <Knownfolders.h>
#include <Shlobj.h>

value dune_xdg__get_known_folder_path(value v_known_folder)
{
  CAMLparam1(v_known_folder);
  CAMLlocal2(v_res, v_path);
  WCHAR* wcp = NULL;
  HRESULT res;
  int wlen, len;
  const KNOWNFOLDERID *rfid;

  v_res = Val_int(0);

  switch (Int_val(v_known_folder)) {
    case 0:
      rfid = &FOLDERID_InternetCache;
      break;
    case 1:
      rfid = &FOLDERID_LocalAppData;
      break;
    default:
      caml_invalid_argument("get_known_folder_path");
      break;
  }

  res = SHGetKnownFolderPath(rfid, 0, NULL, &wcp);

  if (res != S_OK)
    goto done;

  wlen = wcslen(wcp);
  len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wcp, wlen, NULL, 0, NULL, NULL);

  if (!len)
    goto done;

  v_path = caml_alloc_string(len);

  if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wcp, wlen, (char *)String_val(v_path), len, NULL, NULL))
    goto done;

  v_res = caml_alloc_small(1, 0);
  Field(v_res, 0) = v_path;

 done:
  CoTaskMemFree(wcp);
  CAMLreturn(v_res);
}

#else /* _WIN32 */

value dune_xdg__get_known_folder_path(value v_unit)
{
  caml_invalid_argument("get_known_folder_path: not implemented");
}

#endif /* _WIN32 */
