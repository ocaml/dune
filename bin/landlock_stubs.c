#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <stdint.h>

#ifndef ENOSYS
#define ENOSYS EINVAL
#endif

#if defined(__linux__)
#include <fcntl.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#if defined(__has_include)
#if __has_include(<linux/landlock.h>)
#define DUNE_HAS_LINUX_LANDLOCK_H 1
#include <linux/landlock.h>
#endif
#endif
#endif

#if defined(__linux__) && defined(DUNE_HAS_LINUX_LANDLOCK_H) &&                 \
    defined(SYS_landlock_create_ruleset) && defined(SYS_landlock_add_rule) &&  \
    defined(SYS_landlock_restrict_self) &&                                     \
    defined(LANDLOCK_CREATE_RULESET_VERSION) &&                                \
    defined(LANDLOCK_ACCESS_FS_WRITE_FILE) &&                                  \
    defined(LANDLOCK_ACCESS_FS_REMOVE_DIR) &&                                  \
    defined(LANDLOCK_ACCESS_FS_REMOVE_FILE) &&                                 \
    defined(LANDLOCK_ACCESS_FS_MAKE_CHAR) &&                                   \
    defined(LANDLOCK_ACCESS_FS_MAKE_DIR) &&                                    \
    defined(LANDLOCK_ACCESS_FS_MAKE_REG) &&                                    \
    defined(LANDLOCK_ACCESS_FS_MAKE_SOCK) &&                                   \
    defined(LANDLOCK_ACCESS_FS_MAKE_FIFO) &&                                   \
    defined(LANDLOCK_ACCESS_FS_MAKE_BLOCK) &&                                  \
    defined(LANDLOCK_ACCESS_FS_MAKE_SYM) &&                                    \
    defined(LANDLOCK_ACCESS_FS_REFER) &&                                       \
    defined(LANDLOCK_ACCESS_FS_TRUNCATE)
#define DUNE_HAS_LANDLOCK 1
#else
#define DUNE_HAS_LANDLOCK 0
#endif

#if DUNE_HAS_LANDLOCK
static uint64_t landlock_file_write_access_rights(int abi) {
  uint64_t access = LANDLOCK_ACCESS_FS_WRITE_FILE;
  if (abi >= 3) {
    access |= LANDLOCK_ACCESS_FS_TRUNCATE;
  }
  return access;
}

static uint64_t landlock_write_access_rights(int abi) {
  uint64_t access = landlock_file_write_access_rights(abi) |
                    LANDLOCK_ACCESS_FS_REMOVE_DIR |
                    LANDLOCK_ACCESS_FS_REMOVE_FILE |
                    LANDLOCK_ACCESS_FS_MAKE_CHAR |
                    LANDLOCK_ACCESS_FS_MAKE_DIR |
                    LANDLOCK_ACCESS_FS_MAKE_REG |
                    LANDLOCK_ACCESS_FS_MAKE_SOCK |
                    LANDLOCK_ACCESS_FS_MAKE_FIFO |
                    LANDLOCK_ACCESS_FS_MAKE_BLOCK |
                    LANDLOCK_ACCESS_FS_MAKE_SYM;
  if (abi >= 2) {
    access |= LANDLOCK_ACCESS_FS_REFER;
  }
  return access;
}
#endif

CAMLprim value dune_landlock_abi_version(value unit) {
  (void)unit;
#if DUNE_HAS_LANDLOCK
  int abi = syscall(SYS_landlock_create_ruleset, NULL, 0,
                    LANDLOCK_CREATE_RULESET_VERSION);
  if (abi < 0) {
    switch (errno) {
    case ENOSYS:
    case EOPNOTSUPP:
    case EINVAL:
      return Val_int(0);
    default:
      uerror("landlock_create_ruleset", Nothing);
    }
  }
  return Val_int(abi);
#else
  return Val_int(0);
#endif
}

CAMLprim value dune_landlock_write_access_rights(value v_abi) {
  CAMLparam1(v_abi);
#if DUNE_HAS_LANDLOCK
  CAMLreturn(caml_copy_int64(landlock_write_access_rights(Int_val(v_abi))));
#else
  CAMLreturn(caml_copy_int64(0));
#endif
}

CAMLprim value dune_landlock_file_write_access_rights(value v_abi) {
  CAMLparam1(v_abi);
#if DUNE_HAS_LANDLOCK
  CAMLreturn(
      caml_copy_int64(landlock_file_write_access_rights(Int_val(v_abi))));
#else
  CAMLreturn(caml_copy_int64(0));
#endif
}

CAMLprim value dune_landlock_create_ruleset(value v_handled_access) {
  CAMLparam1(v_handled_access);
#if DUNE_HAS_LANDLOCK
  struct landlock_ruleset_attr ruleset_attr = {
      .handled_access_fs = Int64_val(v_handled_access),
  };
  int ruleset_fd = syscall(SYS_landlock_create_ruleset, &ruleset_attr,
                           sizeof(ruleset_attr), 0);
  if (ruleset_fd < 0) {
    uerror("landlock_create_ruleset", Nothing);
  }
  CAMLreturn(Val_int(ruleset_fd));
#else
  (void)v_handled_access;
  errno = ENOSYS;
  uerror("landlock_create_ruleset", Nothing);
#endif
}

CAMLprim value dune_landlock_add_rule(value v_ruleset_fd, value v_path,
                                      value v_allowed_access) {
  CAMLparam3(v_ruleset_fd, v_path, v_allowed_access);
#if DUNE_HAS_LANDLOCK
  int path_fd = open(String_val(v_path), O_PATH | O_CLOEXEC);
  if (path_fd < 0) {
    uerror("open", v_path);
  }

  struct landlock_path_beneath_attr path_beneath = {
      .allowed_access = Int64_val(v_allowed_access),
      .parent_fd = path_fd,
  };
  int ret = syscall(SYS_landlock_add_rule, Int_val(v_ruleset_fd),
                    LANDLOCK_RULE_PATH_BENEATH, &path_beneath, 0);
  int saved_errno = errno;
  close(path_fd);
  if (ret < 0) {
    errno = saved_errno;
    uerror("landlock_add_rule", v_path);
  }
  CAMLreturn(Val_unit);
#else
  (void)v_ruleset_fd;
  (void)v_allowed_access;
  errno = ENOSYS;
  uerror("landlock_add_rule", v_path);
#endif
}

CAMLprim value dune_landlock_restrict_self(value v_ruleset_fd) {
  CAMLparam1(v_ruleset_fd);
#if DUNE_HAS_LANDLOCK
  if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) {
    uerror("prctl", Nothing);
  }
  if (syscall(SYS_landlock_restrict_self, Int_val(v_ruleset_fd), 0) != 0) {
    uerror("landlock_restrict_self", Nothing);
  }
  CAMLreturn(Val_unit);
#else
  (void)v_ruleset_fd;
  errno = ENOSYS;
  uerror("landlock_restrict_self", Nothing);
#endif
}
