#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#if defined(__APPLE__)
#include <AvailabilityMacros.h>
#endif

#if defined(__APPLE__) && __MAC_OS_X_VERSION_MAX_ALLOWED >= 101000

#include <CoreFoundation/CoreFoundation.h>
#include <CoreServices/CoreServices.h>
#include <pthread.h>

// We use a condvar/mutex pair to allow the code calling
// `dune_fsevents_dispatch_queue_wait_until_stopped` to be signaled by either
//
// - An exception raised by the callback passed to the dispatch queue
// - Explicitly stopping via `dune_fsevents_stop`
//
// Since a dispatch queue uses an internal pool of threads, we need some
// synchronization around updating `v_exn`.
typedef struct dune_dispatch_queue {
  dispatch_queue_t dq;
  pthread_cond_t dq_finished;
  pthread_mutex_t dq_lock;
  uint32_t fsevent_streams;
  value v_exn;
} dune_dispatch_queue;

typedef struct dune_fsevents_t {
  dune_dispatch_queue *dq;
  value v_callback;
  FSEventStreamRef stream;
} dune_fsevents_t;

#define Dispatch_queue_val(v) (*((dune_dispatch_queue **)Data_custom_val(v)))

void dune_fsevents_dispatch_queue_finalize(value v_dq) {
  dune_dispatch_queue *dq = Dispatch_queue_val(v_dq);
  if (dq->dq)
    dispatch_release(dq->dq);
  pthread_cond_destroy(&dq->dq_finished);
  pthread_mutex_destroy(&dq->dq_lock);
  caml_stat_free(dq);
}

static struct custom_operations dune_fsevents_dispatch_queue_ops = {
    "build.dune.fsevents.dispatch_queue", dune_fsevents_dispatch_queue_finalize,
    custom_compare_default,               custom_hash_default,
    custom_serialize_default,             custom_deserialize_default,
    custom_compare_ext_default,           custom_fixed_length_default};

#define Fsevents_val(v) (*((dune_fsevents_t **)Data_custom_val(v)))

static void dune_fsevents_t_finalize(value v_t) {
  dune_fsevents_t *t = Fsevents_val(v_t);
  caml_remove_global_root(&t->v_callback);
  caml_stat_free(t);
}

static struct custom_operations dune_fsevents_t_ops = {
    "build.dune.fsevents.fsevents_t", dune_fsevents_t_finalize,
    custom_compare_default,           custom_hash_default,
    custom_serialize_default,         custom_deserialize_default,
    custom_compare_ext_default,       custom_fixed_length_default};

CAMLprim value dune_fsevents_dispatch_queue_create(value v_unit) {
  CAMLparam1(v_unit);
  CAMLlocal1(v_dq);
  dune_dispatch_queue *dq;
  dq = caml_stat_alloc(sizeof(dune_dispatch_queue));
  pthread_mutex_init(&dq->dq_lock, NULL);
  pthread_cond_init(&dq->dq_finished, NULL);
  dq->dq = dispatch_queue_create("build.dune.fsevents", DISPATCH_QUEUE_SERIAL);
  dq->fsevent_streams = 0;
  dq->v_exn = Val_unit;
  caml_register_global_root(&dq->v_exn);
  v_dq = caml_alloc_custom(&dune_fsevents_dispatch_queue_ops,
                           sizeof(dune_dispatch_queue *), 0, 1);
  Dispatch_queue_val(v_dq) = dq;
  CAMLreturn(v_dq);
}

CAMLprim value dune_fsevents_dispatch_queue_wait_until_stopped(value v_dq) {
  CAMLparam1(v_dq);
  CAMLlocal1(v_exn);
  dune_dispatch_queue *dq = Dispatch_queue_val(v_dq);
  caml_release_runtime_system();
  pthread_mutex_lock(&dq->dq_lock);
  pthread_cond_wait(&dq->dq_finished, &dq->dq_lock);
  v_exn = dq->v_exn;
  pthread_mutex_unlock(&dq->dq_lock);
  caml_acquire_runtime_system();
  caml_remove_global_root(&dq->v_exn);
  if (v_exn != Val_unit) {
    caml_raise(v_exn);
  }
  CAMLreturn(Val_unit);
}

// The thread-local storage key `register_thread` is intended to ensure that
// every thread that runs the `fsevents` callback only calls
// `caml_c_thread_register` and `caml_c_thread_unregister` once.
//
// macOS often reuses the same background thread for a serial dispatch queue,
// so this reduces the number of times `caml_c_thread_register` is called.
static pthread_key_t register_thread;
static pthread_once_t register_thread_once = PTHREAD_ONCE_INIT;

static void destroy_register_thread(__attribute__((unused)) void *value) {
  caml_c_thread_unregister();
}

static void make_register_thread() {
  pthread_key_create(&register_thread, destroy_register_thread);
}

static void set_register_thread() {
  pthread_once(&register_thread_once, make_register_thread);
  if (pthread_getspecific(register_thread) == NULL) {
    caml_c_thread_register();
    // Since the value doesn't matter, use a reference to `register_thread`
    pthread_setspecific(register_thread, &register_thread);
  }
}

static FSEventStreamEventFlags interesting_flags =
    kFSEventStreamEventFlagItemCreated | kFSEventStreamEventFlagItemRemoved |
    kFSEventStreamEventFlagItemRenamed | kFSEventStreamEventFlagItemModified |
    kFSEventStreamEventFlagMustScanSubDirs;

static void dune_fsevents_callback(const FSEventStreamRef streamRef,
                                   dune_fsevents_t *t, size_t numEvents,
                                   CFArrayRef eventPaths,
                                   const FSEventStreamEventFlags eventFlags[],
                                   const FSEventStreamEventId eventIds[]) {
  set_register_thread();
  caml_acquire_runtime_system();
  CAMLparam0();
  CAMLlocal5(v_events_xs, v_events_x, v_flags, v_id, v_event);
  CAMLlocal2(v_path, v_res);
  v_events_xs = Val_emptylist;

  // we iterate over the events backwards to avoid reversing the list in the end
  size_t j = 0;
  size_t i = numEvents - 1;
  for (; j < numEvents; j++, i = numEvents - j - 1) {
    FSEventStreamEventFlags flags = eventFlags[i];
    if (!(interesting_flags & flags)) {
      continue;
    }
    CFStringRef cf_path;
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
    CFDictionaryRef details = CFArrayGetValueAtIndex(eventPaths, i);
    cf_path =
        CFDictionaryGetValue(details, kFSEventStreamEventExtendedDataPathKey);
#else
    cf_path = (CFStringRef)CFArrayGetValueAtIndex(eventPaths, i);
#endif
    CFIndex len = CFStringGetLength(cf_path);
    CFIndex byte_len;
    CFIndex res =
        CFStringGetBytes(cf_path, CFRangeMake(0, len), kCFStringEncodingUTF8, 0,
                         0, NULL, 0, &byte_len);
    v_path = caml_alloc_string(byte_len);
    unsigned char *p = Bytes_val(v_path);
    res = CFStringGetBytes(cf_path, CFRangeMake(0, len), kCFStringEncodingUTF8,
                           0, 0, (UInt8 *)p, byte_len, NULL);
    assert(res == len);

    v_event = caml_alloc(3, 0);
    v_id = caml_copy_int64(eventIds[i]);
    v_flags = caml_copy_int32(flags);
    Store_field(v_event, 0, v_path);
    Store_field(v_event, 1, v_id);
    Store_field(v_event, 2, v_flags);

    v_events_x = caml_alloc(2, 0);
    Store_field(v_events_x, 0, v_event);
    Store_field(v_events_x, 1, v_events_xs);
    v_events_xs = v_events_x;
  }
  dune_dispatch_queue *dq = t->dq;
  v_res = caml_callback_exn(t->v_callback, v_events_xs);
  if (Is_exception_result(v_res)) {
    v_res = Extract_exception(v_res);
    caml_release_runtime_system();
    pthread_mutex_lock(&dq->dq_lock);
    dq->v_exn = v_res;
    pthread_cond_broadcast(&dq->dq_finished);
    pthread_mutex_unlock(&dq->dq_lock);
    caml_acquire_runtime_system();
  }
  CAMLdrop;
  caml_release_runtime_system();
}

CFMutableArrayRef paths_of_list(value v_paths) {
  CFMutableArrayRef paths =
      CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);

  int i = 0;
  CAMLparam0();
  CAMLlocal1(path);
  while (v_paths != Val_emptylist) {
    path = Field(v_paths, 0);
    CFStringRef s = CFStringCreateWithCString(
        kCFAllocatorDefault, String_val(path), kCFStringEncodingUTF8);
    CFArraySetValueAtIndex(paths, i, s);
    v_paths = Field(v_paths, 1);
    i++;
  }

  return paths;
}

CAMLprim value dune_fsevents_create(value v_paths, value v_latency,
                                    value v_callback) {
  CAMLparam3(v_paths, v_latency, v_callback);
  CAMLlocal1(v_t);

  CFMutableArrayRef paths = paths_of_list(v_paths);

  const FSEventStreamEventFlags flags =
      kFSEventStreamCreateFlagNoDefer |
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
      kFSEventStreamCreateFlagUseExtendedData |
#endif
      kFSEventStreamCreateFlagUseCFTypes | kFSEventStreamCreateFlagFileEvents;

  dune_fsevents_t *t;
  t = caml_stat_alloc(sizeof(dune_fsevents_t));

  FSEventStreamContext context = {0, (void *)t, NULL /*retain*/,
                                  NULL /*release*/, NULL};
  FSEventStreamRef stream = FSEventStreamCreate(
      kCFAllocatorDefault, (FSEventStreamCallback)&dune_fsevents_callback,
      &context, paths, kFSEventStreamEventIdSinceNow, Double_val(v_latency),
      flags);
  CFRelease(paths);
  t->dq = NULL;
  caml_register_global_root(&t->v_callback);
  t->v_callback = v_callback;
  t->stream = stream;

  v_t = caml_alloc_custom(&dune_fsevents_t_ops, sizeof(dune_fsevents_t *), 0, 1);
  Fsevents_val(v_t) = t;
  CAMLreturn(v_t);
}

CAMLprim value dune_fsevents_set_exclusion_paths(value v_t, value v_paths) {
  CAMLparam2(v_t, v_paths);
  dune_fsevents_t *t = Fsevents_val(v_t);
  CFMutableArrayRef paths = paths_of_list(v_paths);

  bool ret = FSEventStreamSetExclusionPaths(t->stream, paths);
  CFRelease(paths);
  if (!ret) {
    // wrapped with a code error in the caller
    caml_failwith("Fsevents.set_exclusion_paths: unable to set");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_start(value v_t, value v_dq) {
  CAMLparam2(v_t, v_dq);
  dune_fsevents_t *t = Fsevents_val(v_t);
  dune_dispatch_queue *dq = Dispatch_queue_val(v_dq);
  t->dq = dq;
  caml_release_runtime_system();
  pthread_mutex_lock(&dq->dq_lock);
  dq->fsevent_streams++;
  pthread_mutex_unlock(&dq->dq_lock);
  caml_acquire_runtime_system();
  FSEventStreamSetDispatchQueue(t->stream, dq->dq);
  bool res = FSEventStreamStart(t->stream);
  if (!res) {
    /* the docs say this is impossible anyway */
    caml_failwith("Fsevents.start: failed to start");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_stop(value v_t) {
  CAMLparam1(v_t);
  dune_fsevents_t *t = Fsevents_val(v_t);
  dune_dispatch_queue *dq = t->dq;
  if (dq == NULL) {
    // already stopped
    CAMLreturn(Val_unit);
  }
  FSEventStreamStop(t->stream);
  FSEventStreamInvalidate(t->stream);
  FSEventStreamRelease(t->stream);
  caml_release_runtime_system();
  pthread_mutex_lock(&dq->dq_lock);
  dq->fsevent_streams--;
  if (dq->fsevent_streams == 0) {
    pthread_cond_broadcast(&dq->dq_finished);
  }
  pthread_mutex_unlock(&dq->dq_lock);
  caml_acquire_runtime_system();
  t->dq = NULL;
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_flush_async(value v_t) {
  CAMLparam1(v_t);
  dune_fsevents_t *t = Fsevents_val(v_t);
  CAMLlocal1(v_event);
  uint64_t id = FSEventStreamFlushAsync(t->stream);
  v_event = caml_copy_int64(id);
  CAMLreturn(v_event);
}

CAMLprim value dune_fsevents_flush_sync(value v_t) {
  CAMLparam1(v_t);
  dune_fsevents_t *t = Fsevents_val(v_t);
  caml_release_runtime_system();
  FSEventStreamFlushSync(t->stream);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_kind(value v_flags) {
  CAMLparam1(v_flags);
  CAMLlocal1(v_kind);
  uint32_t flags = Int32_val(v_flags);
  if (flags & kFSEventStreamEventFlagItemIsDir) {
    v_kind = Val_int(flags & kFSEventStreamEventFlagMustScanSubDirs ? 2 : 0);
  } else {
    v_kind = Val_int(1);
  };
  CAMLreturn(v_kind);
}

static const FSEventStreamEventFlags action_mask =
    kFSEventStreamEventFlagItemCreated | kFSEventStreamEventFlagItemRemoved |
    kFSEventStreamEventFlagItemRenamed | kFSEventStreamEventFlagItemModified;

CAMLprim value dune_fsevents_action(value v_flags) {
  CAMLparam1(v_flags);
  CAMLlocal1(v_action);

  uint32_t flags = Int32_val(v_flags) & action_mask;
  if (flags & kFSEventStreamEventFlagItemCreated) {
    v_action = Val_int(0);
  } else if (flags & kFSEventStreamEventFlagItemRemoved) {
    v_action = Val_int(1);
  } else if (flags & kFSEventStreamEventFlagItemModified) {
    v_action = Val_int(2);
  } else if (flags & kFSEventStreamEventFlagItemRenamed) {
    v_action = Val_int(3);
  } else {
    v_action = Val_int(4);
  }

  CAMLreturn(v_action);
}
static const FSEventStreamEventFlags all_flags[] = {
    kFSEventStreamEventFlagMustScanSubDirs,
    kFSEventStreamEventFlagUserDropped,
    kFSEventStreamEventFlagKernelDropped,
    kFSEventStreamEventFlagEventIdsWrapped,
    kFSEventStreamEventFlagHistoryDone,
    kFSEventStreamEventFlagRootChanged,
    kFSEventStreamEventFlagMount,
    kFSEventStreamEventFlagUnmount,
    kFSEventStreamEventFlagItemCreated,
    kFSEventStreamEventFlagItemRemoved,
    kFSEventStreamEventFlagItemInodeMetaMod,
    kFSEventStreamEventFlagItemRenamed,
    kFSEventStreamEventFlagItemModified,
    kFSEventStreamEventFlagItemFinderInfoMod,
    kFSEventStreamEventFlagItemChangeOwner,
    kFSEventStreamEventFlagItemXattrMod,
    kFSEventStreamEventFlagItemIsFile,
    kFSEventStreamEventFlagItemIsDir,
    kFSEventStreamEventFlagItemIsSymlink,
    kFSEventStreamEventFlagOwnEvent,
    kFSEventStreamEventFlagItemIsHardlink,
    kFSEventStreamEventFlagItemIsLastHardlink,
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101300
    kFSEventStreamEventFlagItemCloned,
#endif
};

CAMLprim value dune_fsevents_raw(value v_flags) {
  CAMLparam1(v_flags);
  CAMLlocal1(v_raw);
  size_t len = sizeof(all_flags) / sizeof(FSEventStreamEventFlags);
  v_raw = caml_alloc(len, 0);
  uint32_t flags = Int32_val(v_flags);
  for (size_t i = 0; i < len; i++) {
    Store_field(v_raw, i, flags & all_flags[i] ? Val_true : Val_false);
  }
  CAMLreturn(v_raw);
}

CAMLprim value dune_fsevents_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}

#else

static char *unavailable_message =
#if defined(__APPLE__)
    "upgrade your macos sdk to enable watch mode";
#else
    "fsevents is only available on macos";
#endif

CAMLprim value dune_fsevents_stop(value v_t) {
  (void)v_t;
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_start(value v_t) {
  (void)v_t;
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_create(value v_paths, value v_latency,
                                    value v_callback) {
  (void)v_paths;
  (void)v_latency;
  (void)v_callback;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_set_exclusion_paths(value v_t, value v_paths) {
  (void)v_t;
  (void)v_paths;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_flush_async(value v_t) {
  (void)v_t;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_kind(value v_flags) {
  (void)v_flags;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_action(value v_flags) {
  (void)v_flags;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_raw(value v_flags) {
  (void)v_flags;
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_flush_sync(value v_t) {
  (void)v_t;
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_dispatch_queue_create(value v_unit) {
  (void)v_unit;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_dispatch_queue_wait_until_stopped(value v_unit) {
  (void)v_unit;
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_available(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Val_false);
}

#endif
