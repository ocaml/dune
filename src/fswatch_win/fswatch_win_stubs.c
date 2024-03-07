/* File-watching support under Windows

   We implement file-watching support under Windows using ReadDirectoryChangesW
   and I/O completion ports. The notification of events is pull-based (one needs
   to retrieve them explicitly, there is no callback that is invoked on each
   notification).

   The main type is [fsenv] which keeps track of the state of a file watcher
   (including all changes that have been notified by the operator system, but
   that have not yet been retrieved by the user).

   There is one value of type [watch] for each directory being watched. It keeps
   track of the various buffers needed to use the ReadDirectoryChangesW function
   call.

   When the first watch is created a native (not OCaml) thread is triggered
   which runs the main event loop, waiting for notifications from the I/O
   completion port. When a notification arrives, it is recorded in the [fsenv]
   and it will stay there until the user retrieves it by calling [wait].  We
   notify the user that new notifications have arrived by signalling an event
   object which causes the [wait] function to return.

   The [.events] field of the [fsenv] type which is mutated from multiple
   threads.  Standard lockless techniques are used to avoid data races.
 */

#define CAML_NAME_SPACE

#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/custom.h>
#include <caml/version.h>

#if OCAML_VERSION_MAJOR < 5
#define caml_uerror uerror
#define caml_win32_maperr win32_maperr
#endif

#if 0
#define DEBUG(fmt, ...) \
  do { fprintf(stderr, "DEBUG: " __FUNCTION__ ": " fmt "\n", __VA_ARGS__); fflush(stderr); } while (0)
#else
#define DEBUG(fmt, ...) ((void) 0)
#endif

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

#define FILE_NOTIFY_BUFFER_LENGTH ((sizeof(DWORD) * 4) * 1000)

struct events {
  struct events* next;
  char* path;
  LPVOID buffer;
};

struct fsenv {
  struct events* events; /* mutated from multiple threads */
  HANDLE completionPort;
  HANDLE signal;
  HANDLE thread;
  HANDLE afterAdd;
};

struct watch {
  struct watch* next;
  char* path;
  WCHAR* realpath;
  HANDLE handle;
  LPOVERLAPPED overlapped;
  LPVOID buffer;
};

#ifdef unix_error
#undef unix_error
#endif

#define unix_error(s, err) \
  do { \
    caml_win32_maperr(err); \
    caml_uerror(s, Nothing); \
  } while (0)

static void push_events(struct fsenv* fsenv, struct watch* w) {
  struct events* e = malloc(sizeof(struct events));
  e->buffer = w->buffer;
  e->path = _strdup(w->path);
  w->buffer = NULL;

  /* Perform [e->next = fsenv->events; fsenv->events = e] atomically */
  do {
    e->next = fsenv->events;
  } while (InterlockedCompareExchangePointer((PVOID volatile *)&(fsenv->events), e, e->next) != e->next);

  SetEvent(fsenv->signal); /* wakeup OCaml thread */
}

/* Retrieve the list of events from the shared list. */
static struct events* pop_events(struct fsenv* fsenv) {
  struct events* res;

  /* Perform [res = fsenv->events; fsenv->events = NULL] atomically */
  do {
    res = fsenv->events;
  } while (InterlockedCompareExchangePointer((PVOID volatile *)&(fsenv->events), NULL, res) != res);

  return res;
}

/* Same order as Event.action */
static DWORD actions[] = {
  FILE_ACTION_ADDED,
  FILE_ACTION_REMOVED,
  FILE_ACTION_MODIFIED,
  FILE_ACTION_RENAMED_OLD_NAME,
  FILE_ACTION_RENAMED_NEW_NAME,
};

static value Val_action(DWORD action) {
  for (int tag = 0; tag < sizeof(actions) / sizeof(DWORD); tag ++) {
    if (actions[tag] == action)
      return Val_int(tag);
  }
  assert(0);
  return Val_unit; /* unreachable */
}

/* Assumes [s] is NULL-terminated. */
static WCHAR* utf8_to_utf16(const char* s) {
  int buflen = MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0);
  if (buflen == 0) {
    DEBUG("MultiByteToWideChar: error %d", GetLastError());
    return NULL;
  }
  WCHAR* buf = malloc(buflen * sizeof(WCHAR));
  if (MultiByteToWideChar(CP_UTF8, 0, s, -1, buf, buflen) == 0) {
    DEBUG("MultiByteToWideChar: error %d", GetLastError());
    free(buf);
    return NULL;
  }
  return buf;
}

/* Assumes [s] is *not* NULL-terminated. */
static char* utf16_to_utf8(const WCHAR* s, int slen) {
  int buflen = WideCharToMultiByte(CP_UTF8, 0, s, slen, NULL, 0, NULL, NULL);
  if (buflen == 0) {
    DEBUG("WideCharToMultiByte: error %d", GetLastError());
    return NULL;
  }
  char* buf = malloc(buflen + 1);
  if (WideCharToMultiByte(CP_UTF8, 0, s, slen, buf, buflen, NULL, NULL) == 0) {
    DEBUG("WideCharToMultiByte: error %d", GetLastError());
    free(buf);
    return NULL;
  }
  buf[buflen] = 0;
  return buf;
}

static value parse_events(struct events* e) {
  CAMLparam0();
  CAMLlocal5(res, cell, ev, v_path, v_filename);

  while (e != NULL) {
    struct events* next = e->next;
    PFILE_NOTIFY_INFORMATION info = e->buffer;
    v_path = caml_copy_string(e->path);

    for (;;) {
      char* filename = utf16_to_utf8(info->FileName, info->FileNameLength / sizeof(WCHAR));
      if (filename == NULL) {
        DEBUG("%s: could not decode event", e->path);
        break; /* skip this packet of events */
      }
      v_filename = caml_copy_string(filename);
      free(filename);

      /* Allocate Event.t */
      ev = caml_alloc_tuple(3);
      Store_field(ev, 0, v_path);
      Store_field(ev, 1, v_filename);
      Store_field(ev, 2, Val_action(info->Action));

      /* Allocate list cell */
      cell = caml_alloc_tuple(2);
      Store_field(cell, 0, ev);
      Store_field(cell, 1, res);
      res = cell;

      if (info->NextEntryOffset == 0)
        break;

      info = (PFILE_NOTIFY_INFORMATION)((char*)info + info->NextEntryOffset);
    }

    free(e->buffer);
    free(e->path);
    free(e);

    e = next;
  }

  CAMLreturn(res);
}

static DWORD read_changes(struct watch *w) {
  memset(w->overlapped, 0, sizeof(OVERLAPPED));
  if (w->buffer == NULL) w->buffer = malloc(FILE_NOTIFY_BUFFER_LENGTH);
  BOOL res =
    ReadDirectoryChangesW(w->handle, w->buffer, FILE_NOTIFY_BUFFER_LENGTH, TRUE,
                          FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME | FILE_NOTIFY_CHANGE_LAST_WRITE,
                          NULL, w->overlapped, NULL);
  if (res == 0) DEBUG("ReadDirectoryChangesW: %s: error %d", w->path, GetLastError());
  return res;
}

static void free_watch(struct watch* w) {
  free(w->path);
  free(w->realpath);
  free(w->overlapped);
  free(w->buffer);
  if (w->handle != INVALID_HANDLE_VALUE) CloseHandle(w->handle);
  free(w);
}

static BOOL isprefix(WCHAR* prefix, WCHAR* s) {
  for (int i = 0; prefix[i]; i ++) {
    if (s[i] == 0 || prefix[i] != s[i])
      return FALSE;
  }
  return TRUE;
}

static WCHAR* get_final_path(HANDLE handle) {
  DWORD buflen = GetFinalPathNameByHandleW(handle, NULL, 0, 0);
  if (buflen == 0) {
    DEBUG("GetFinalPathNameByHandleW: error %d", GetLastError());
    return NULL;
  }
  WCHAR* buf = malloc(buflen * sizeof(WCHAR));
  if (GetFinalPathNameByHandleW(handle, buf, buflen, 0) == 0) {
    DEBUG("GetFinalPathNameByHandleW: error %d", GetLastError());
    free(buf);
    return NULL;
  }
  return buf;
}

/* Takes ownership of [path]. Return NULL if the path is already being watched
   or if an error occurs. */
static struct watch* add_watch(struct fsenv* fsenv, char* path, struct watch** lst) {
  DEBUG("adding: %s", path);
  WCHAR* utf16path = utf8_to_utf16(path);
  if (utf16path == NULL) {
    free(path);
    return NULL;
  }
  HANDLE handle =
    CreateFileW(utf16path, FILE_LIST_DIRECTORY, FILE_SHARE_READ | FILE_SHARE_DELETE,
                NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED, NULL);
  free(utf16path);
  if (handle == INVALID_HANDLE_VALUE) {
    DEBUG("%s could not be opened: CreateFileW: error %d", path, GetLastError());
    free(path);
    return NULL;
  }

  struct watch* w = malloc(sizeof(struct watch));
  w->next = *lst;
  w->path = path;
  w->realpath = get_final_path(handle);
  w->overlapped = malloc(sizeof(OVERLAPPED));
  w->handle = handle;
  w->buffer = NULL;

  for (struct watch *p = *lst; p; p = p->next) {
    if (isprefix(p->realpath, w->realpath)) {
      DEBUG("%ls is already being watched, ignoring", w->realpath);
      free_watch(w);
      return NULL;
    }
  }

  if (CreateIoCompletionPort(handle, fsenv->completionPort, (ULONG_PTR)w, 1) == NULL) {
    DEBUG("could not add %ls: CreateIoCompletionPort: error %d", w->realpath, GetLastError());
    free_watch(w);
    return NULL;
  }

  for (struct watch *p = *lst; p; p = p->next) {
    if (isprefix(w->realpath, p->realpath)) {
      DEBUG("%ls is included in %ls, removing", p->realpath, w->realpath);
      CancelIo(p->handle);
      CloseHandle(p->handle);
      p->handle = INVALID_HANDLE_VALUE;
    }
  }

  *lst = w;

  return w;
}

enum {
  ADDWATCH = 1,
  SHUTDOWN = 3
};

static void remove_watch(struct watch* w, struct watch** lst) {
  DEBUG("removing: %s", w->path);
  if (*lst == w) {
    *lst = w->next;
  } else {
    for (struct watch *p = *lst; p; p = p -> next) {
      if (p->next == w) {
        p->next = w->next;
        break;
      }
    }
  }
  free_watch(w);
}

static DWORD WINAPI watch_thread(LPVOID lpParameter) {
  struct fsenv* fsenv = lpParameter;
  struct watch* watches = NULL;
  BOOL shuttingDown = FALSE;

  while (watches != NULL || !shuttingDown) {
    LPVOID key;
    DWORD numBytes;
    LPOVERLAPPED overlapped;

    if (GetQueuedCompletionStatus(fsenv->completionPort, &numBytes, (PULONG_PTR)&key, &overlapped, INFINITE) == FALSE) {
      DEBUG("GetQueuedCompletionStatus: error %d", GetLastError());
      if (overlapped == NULL) {
        DEBUG("ignoring GetQueuedCompletionStatus error");
      } else {
        remove_watch(key, &watches);
      }
      continue;
    }

    if (numBytes > 0) {
      if (shuttingDown) {
        remove_watch(key, &watches);
      } else {
        push_events(fsenv, key);
        if (read_changes(key) == 0) remove_watch(key, &watches);
      }
    } else if (overlapped == (LPOVERLAPPED)ADDWATCH) {
      if (shuttingDown) {
        DEBUG("add received for %s; ignoring due to shutdown", (char *)key);
        free(key); /* ignore request */
      } else {
        struct watch *w = add_watch(fsenv, key, &watches);
        if (w != NULL && read_changes(w) == 0) remove_watch(w, &watches);
      }
      SetEvent(fsenv->afterAdd);
    } else if (overlapped == (LPOVERLAPPED)SHUTDOWN) {
      DEBUG("shutting down");
      shuttingDown = TRUE;
      for (struct watch *p = watches; p; p = p->next) {
        CancelIo(p->handle);
        CloseHandle(p->handle);
        p->handle = INVALID_HANDLE_VALUE;
      }
    }
  }

  return 0;
}

static struct custom_operations fsenv_ops = {
  "fswatch_win.fsenv",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

#define Fsenv_val(v) (*((struct fsenv**)Data_custom_val(v)))

value fswatch_win_create(value v_unit) {
  HANDLE completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
  if (completionPort == NULL) {
    unix_error("CreateIoCompletionPort", GetLastError());
  }

  HANDLE signal = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (signal == NULL) {
    DWORD err = GetLastError();
    CloseHandle(completionPort);
    unix_error("CreateEvent", err);
  }

  HANDLE afterAdd = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (afterAdd == NULL) {
    DWORD err = GetLastError();
    CloseHandle(signal);
    CloseHandle(completionPort);
    unix_error("CreateEvent", err);
  }

  struct fsenv* fsenv = caml_stat_alloc(sizeof(struct fsenv));
  fsenv->events = NULL;
  fsenv->completionPort = completionPort;
  fsenv->signal = signal;
  fsenv->afterAdd = afterAdd;

  HANDLE thread = CreateThread(NULL, 0, &watch_thread, fsenv, 0, NULL);
  if (thread == NULL) {
    DWORD err = GetLastError();
    caml_stat_free(fsenv);
    CloseHandle(afterAdd);
    CloseHandle(signal);
    CloseHandle(completionPort);
    unix_error("CreateThread", err);
  }
  fsenv->thread = thread;

  value v_fsenv = caml_alloc_custom(&fsenv_ops, sizeof(struct fsenv*), 0, 1);
  Fsenv_val(v_fsenv) = fsenv;

  return v_fsenv;
}

value fswatch_win_add(value v_fsenv, value v_path) {
  struct fsenv* fsenv = Fsenv_val(v_fsenv);

  if (fsenv == NULL)
    caml_invalid_argument("Fswatch_win.add: already shut down");

  if (! caml_string_is_c_safe(v_path))
    caml_invalid_argument("Fswatch_win.add");

  if (PostQueuedCompletionStatus(fsenv->completionPort, 0, (ULONG_PTR)_strdup(String_val(v_path)), (LPOVERLAPPED)ADDWATCH) == 0) {
    unix_error("PostQueuedCompletionStatus", GetLastError());
  }

  caml_release_runtime_system();
  DWORD res = WaitForSingleObject(fsenv->afterAdd, INFINITE);
  caml_acquire_runtime_system();
  if (res != WAIT_OBJECT_0) unix_error("WaitForSingleObject", GetLastError());

  return Val_unit;
}

value fswatch_win_wait(value v_fsenv, value v_debounce) {
  struct fsenv *fsenv = Fsenv_val(v_fsenv);

  if (fsenv == NULL)
    caml_invalid_argument("Fswatch_win.wait: already shut down");

  caml_release_runtime_system();
  DWORD res = WaitForSingleObject(fsenv->signal, INFINITE);
  if (res == WAIT_OBJECT_0 && Int_val(v_debounce) > 0) Sleep(Int_val(v_debounce));
  caml_acquire_runtime_system();

  if (res != WAIT_OBJECT_0)
    unix_error("WaitForSingleObject", GetLastError());

  struct events *e = pop_events(fsenv);

  if (ResetEvent(fsenv->signal) == 0)
    unix_error("ResetEvent", GetLastError());

  return parse_events(e);
}

value fswatch_win_shutdown(value v_fsenv) {
  struct fsenv *fsenv = Fsenv_val(v_fsenv);

  if (fsenv == NULL)
    return Val_unit;

  Fsenv_val(v_fsenv) = NULL;

  if (PostQueuedCompletionStatus(fsenv->completionPort, 0, 0, (LPOVERLAPPED)SHUTDOWN) == 0) {
    unix_error("PostQueuedCompletionStatus", GetLastError());
  }

  caml_release_runtime_system();

  if (WaitForSingleObject(fsenv->thread, 2000) != WAIT_OBJECT_0)
    DEBUG("thread did not terminate in time");

  CloseHandle(fsenv->thread);
  CloseHandle(fsenv->completionPort);
  CloseHandle(fsenv->signal);
  CloseHandle(fsenv->afterAdd);

  for (struct events *e = pop_events(fsenv); e; e = e->next) {
    free(e->path);
    free(e->buffer);
    free(e);
  }

  caml_acquire_runtime_system();

  caml_stat_free(fsenv);

  return Val_unit;
}

#else

value fswatch_win_create(value v_unit) {
  caml_invalid_argument("fswatch_win_create: not implemented");
}

value fswatch_win_wait(value v_fsenv, value v_debounce) {
  caml_invalid_argument("fswatch_win_wait: not implemented");
}

value fswatch_win_add(value v_fsenv, value v_path) {
  caml_invalid_argument("fswatch_win_add: not implemented");
}

value fswatch_win_shutdown(value v_fsenv) {
  caml_invalid_argument("fswatch_win_shutdown: not implemented");
}

#endif
