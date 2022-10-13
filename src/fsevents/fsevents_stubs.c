#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#if defined(__APPLE__)

#include <Availability.h>
#include <CoreFoundation/CoreFoundation.h>
#include <CoreServices/CoreServices.h>

typedef struct dune_runloop {
  CFRunLoopRef runloop;
  value v_exn;
} dune_runloop;

typedef struct dune_fsevents_t {
  dune_runloop *runloop;
  value v_callback;
  FSEventStreamRef stream;
} dune_fsevents_t;

#define Runloop_val(v) (*((dune_runloop **)Data_custom_val(v)))

void dune_fsevents_runloop_finalize(value v_runloop) {
  dune_runloop *runloop = Runloop_val(v_runloop);
  caml_stat_free(runloop);
}

static struct custom_operations dune_fsevents_runloop_ops = {
    "dune.fsevents.runloop",    dune_fsevents_runloop_finalize,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value dune_fsevents_runloop_current(value v_unit) {
  CAMLparam1(v_unit);
  dune_runloop *rl;
  rl = caml_stat_alloc(sizeof(dune_runloop));
  rl->runloop = CFRunLoopGetCurrent();
  rl->v_exn = Val_unit;
  caml_register_global_root(&rl->v_exn);
  value v_runloop = caml_alloc_custom(&dune_fsevents_runloop_ops,
                                      sizeof(dune_runloop *), 0, 1);
  Runloop_val(v_runloop) = rl;
  CAMLreturn(v_runloop);
}

CAMLprim value dune_fsevents_runloop_run(value v_runloop) {
  CAMLparam1(v_runloop);
  CAMLlocal1(v_exn);
  dune_runloop *runloop = Runloop_val(v_runloop);
  caml_release_runtime_system();
  CFRunLoopRun();
  caml_acquire_runtime_system();
  caml_remove_global_root(&runloop->v_exn);
  v_exn = runloop->v_exn;
  if (v_exn != Val_unit)
    caml_raise(v_exn);
  CAMLreturn(Val_unit);
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
    cf_path = CFDictionaryGetValue(details, kFSEventStreamEventExtendedDataPathKey);
#else
    cf_path = (CFStringRef) CFArrayGetValueAtIndex(eventPaths, i);
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
  v_res = caml_callback_exn(t->v_callback, v_events_xs);
  if (Is_exception_result(v_res)) {
    t->runloop->v_exn = Extract_exception(v_res);
    CFRunLoopStop(t->runloop->runloop);
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
  CAMLlocal1(path);

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
  caml_register_global_root(&t->v_callback);
  t->v_callback = v_callback;
  t->stream = stream;

  CAMLreturn(caml_copy_nativeint((intnat)t));
}

CAMLprim value dune_fsevents_set_exclusion_paths(value v_t, value v_paths) {
  CAMLparam2(v_t, v_paths);
  CAMLlocal1(path);
  dune_fsevents_t *t = (dune_fsevents_t *)Nativeint_val(v_t);
  CFMutableArrayRef paths = paths_of_list(v_paths);

  bool ret = FSEventStreamSetExclusionPaths(t->stream, paths);
  CFRelease(paths);
  if (!ret) {
    // wrapped with a code error in the caller
    caml_failwith("Fsevents.set_exclusion_paths: unable to set");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_start(value v_t, value v_runloop) {
  CAMLparam2(v_t, v_runloop);
  dune_fsevents_t *t = (dune_fsevents_t *)Nativeint_val(v_t);
  dune_runloop *runloop = Runloop_val(v_runloop);
  t->runloop = runloop;
  FSEventStreamScheduleWithRunLoop(t->stream, runloop->runloop,
                                   kCFRunLoopDefaultMode);
  bool res = FSEventStreamStart(t->stream);
  if (!res) {
    /* the docs say this is impossible anyway */
    caml_failwith("Fsevents.start: failed to start");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_stop(value v_t) {
  CAMLparam1(v_t);
  dune_fsevents_t *t = (dune_fsevents_t *)Nativeint_val(v_t);
  FSEventStreamStop(t->stream);
  FSEventStreamInvalidate(t->stream);
  FSEventStreamRelease(t->stream);
  caml_remove_global_root(&t->v_callback);
  caml_stat_free(t);
  CAMLreturn(Val_unit);
}

CAMLprim value dune_fsevents_runloop_get(value v_t) {
  CAMLparam1(v_t);
  CAMLlocal2(v_some, v_runloop);
  dune_fsevents_t *t = (dune_fsevents_t *)Nativeint_val(v_t);
  if (t->runloop == NULL) {
    CAMLreturn(Val_int(0));
  } else {
    v_runloop = caml_copy_nativeint((intnat)t->runloop);
    v_some = caml_alloc_small(1, 0);
    Store_field(v_some, 0, v_runloop);
    CAMLreturn(v_some);
  }
}

CAMLprim value dune_fsevents_flush_async(value v_t) {
  CAMLparam1(v_t);
  dune_fsevents_t *t = (dune_fsevents_t *)Nativeint_val(v_t);
  CAMLlocal1(v_event);
  uint64_t id = FSEventStreamFlushAsync(t->stream);
  v_event = caml_copy_int64(id);
  CAMLreturn(v_event);
}

CAMLprim value dune_fsevents_flush_sync(value v_t) {
  CAMLparam1(v_t);
  dune_fsevents_t *t = (dune_fsevents_t *)Nativeint_val(v_t);
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
  int count = __builtin_popcount(flags);

  flags = Int32_val(v_flags);
  if (count >= 2 || flags & kFSEventStreamEventFlagItemRenamed) {
    // we don't bother tracking renamed acurately for now. macos makes it
    // tricky by not telling is which path is created and which one is deleted.
    // it is possible to reverse engineer this from the chain of inodes in the
    // events, but it's also error prone as inodes can be reused. so for now, we
    // avoid this is and treat renamed as unknown
    v_action = Val_int(0);
  } else if (flags & kFSEventStreamEventFlagItemCreated) {
    v_action = Val_int(1);
  } else if (flags & kFSEventStreamEventFlagItemRemoved) {
    v_action = Val_int(2);
  } else if (flags & kFSEventStreamEventFlagItemModified) {
    v_action = Val_int(3);
  } else {
    caml_failwith("fsevents: unexpected event action");
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

static char *unavailable_message = "fsevents is only available on macos";

CAMLprim value dune_fsevents_stop(value v_t) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_start(value v_t) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_create(value v_paths, value v_latency,
                                    value v_callback) {
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_set_exclusion_paths(value v_t, value v_paths) {
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_flush_async(value v_t) {

  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_kind(value v_flags) {

  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_action(value v_flags) {

  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_raw(value v_flags) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_flush_sync(value v_t) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_destroy(value v_t) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_break(value v_t) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_loop(value v_t) {
  caml_failwith(unavailable_message);
}

CAMLprim value dune_fsevents_runloop_current(value v_unit) {
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_runloop_run(value v_unit) {
  caml_failwith(unavailable_message);
}
CAMLprim value dune_fsevents_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_false);
}

#endif
