#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/osdeps.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/custom.h>

#ifdef _WIN32

#include <windows.h>

#define BUFF_SIZE 1024

typedef struct
{
    char *buffer;
    HANDLE handle;
    OVERLAPPED *overlapped;
    value func;
} dune_t;

static void winwatch_watch(dune_t *t)
{
    memset(t->overlapped, 0, sizeof(OVERLAPPED));

    BOOL watch_path = ReadDirectoryChangesW(
            t->handle, t->buffer, BUFF_SIZE, TRUE,
            FILE_NOTIFY_CHANGE_FILE_NAME  |
            FILE_NOTIFY_CHANGE_DIR_NAME   |
            FILE_NOTIFY_CHANGE_LAST_WRITE,
            NULL, t->overlapped, NULL);
}

value winwatch_iocp_create(value v_unit)
{
    CAMLparam1(v_unit);
    HANDLE port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
    if (port == NULL)
      caml_failwith("CreateIoCompletionPort failed");
    CAMLreturn(caml_copy_nativeint((intnat)port));
}

value winwatch_iocp_run(value v_iocp)
{
    CAMLparam1(v_iocp);
    CAMLlocal2(v_file_name, v_action);
    HANDLE iocp = (HANDLE)Nativeint_val(v_iocp);
    DWORD num_bytes, name_len;
    WCHAR* file_name;
    OVERLAPPED *overlapped;
    FILE_NOTIFY_INFORMATION *event;
    dune_t *t;

    while (TRUE)
    {
        caml_release_runtime_system();
        BOOL ok = GetQueuedCompletionStatus(iocp, &num_bytes, (PULONG_PTR)&t, &overlapped, 100);
        caml_acquire_runtime_system();

        if (ok == FALSE && overlapped == NULL) /* Timeout */
            continue;

        if (ok == FALSE)
            /* Ignore errors */
            continue;

        event = (FILE_NOTIFY_INFORMATION*)t->buffer;

        for (;;)
        {
            name_len = event->FileNameLength / sizeof(WCHAR);

            switch (event->Action)
            {
                case FILE_ACTION_ADDED:
                    v_action = Val_int(0);
                    break;
                case FILE_ACTION_REMOVED:
                    v_action = Val_int(1);
                    break;
                case FILE_ACTION_MODIFIED:
                    v_action = Val_int(2);
                    break;
                case FILE_ACTION_RENAMED_OLD_NAME:
                    v_action = Val_int(3);
                    break;
                case FILE_ACTION_RENAMED_NEW_NAME:
                    v_action = Val_int(4);
                    break;
            }

            file_name = malloc(sizeof(WCHAR) * (name_len + 1));
            memcpy(file_name, event->FileName, sizeof(WCHAR) * name_len);
            file_name[name_len] = 0;
            v_file_name = caml_copy_string_of_utf16(file_name);

            caml_callback2(t->func, v_action, v_file_name);

            if (event->NextEntryOffset)
            {
                *((char**)&event) += event->NextEntryOffset;
            }
            else
            {
                break;
            }
        }

        winwatch_watch(t);
    }

    CAMLreturn(Val_unit);
}

value winwatch_create(value v_path, value v_func)
{
    CAMLparam2(v_path, v_func);
    CAMLlocal2(v_t, v_res);
    dune_t *t = caml_stat_alloc(sizeof(dune_t));
    WCHAR *path = caml_stat_strdup_to_utf16(String_val(v_path));

    HANDLE handle = CreateFileW(path,
            FILE_LIST_DIRECTORY,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            NULL,
            OPEN_EXISTING,
            FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
            NULL);
    caml_stat_free(path);

    if (handle == INVALID_HANDLE_VALUE)
    {
        v_res = Val_int(0);
        goto fail;
    }

    t->func = v_func;
    t->buffer = caml_stat_alloc(BUFF_SIZE);
    t->overlapped = caml_stat_alloc(sizeof(OVERLAPPED));
    t->handle = handle;

    caml_register_generational_global_root(&t->func);

    v_t = caml_copy_nativeint((intnat)t);
    v_res = caml_alloc_tuple(1);

    Store_field(v_res, 0, v_t);

fail:
    CAMLreturn(v_res);
}

value winwatch_start(value v_t, value v_iocp)
{
    CAMLparam2(v_t, v_iocp);
    HANDLE iocp = (HANDLE)Nativeint_val(v_iocp);
    dune_t *t = (dune_t *)Nativeint_val(v_t);
    iocp = CreateIoCompletionPort(t->handle, iocp, (ULONG_PTR)t, 1);
    if (iocp == NULL)
    {
        caml_failwith("File could not be added to completion port.");
    }
    winwatch_watch(t);
    CAMLreturn(Val_unit);
}

value winwatch_stop(value v_t)
{
    CAMLparam1(v_t);
    dune_t *t = (dune_t *)Nativeint_val(v_t);
    CancelIo(t->handle);
    CloseHandle(t->handle);
    CAMLreturn(Val_unit);
}

#else

value winwatch_iocp_create(value v_unit)
{
  caml_failwith("winwatch_iocp_create: not implemented");
}

value winwatch_iocp_run(value v_iocp)
{
  caml_failwith("winwatch_iocp_run: not implemented");
}

value winwatch_create(value v_path, value v_func)
{
  caml_failwith("winwatch_create: not implemented");
}

value winwatch_start(value v_t, value v_iocp)
{
  caml_failwith("winwatch_start: not implemented");
}

value winwatch_stop(value v_t)
{
  caml_failwith("winwatch_stop: not implemented");
}

#endif
