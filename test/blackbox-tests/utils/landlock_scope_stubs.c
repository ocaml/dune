#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <errno.h>

#ifndef ENOSYS
#define ENOSYS EINVAL
#endif

#if defined(__linux__)
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>
#if defined(__has_include)
#if __has_include(<linux/landlock.h>)
#include <linux/landlock.h>
#define DUNE_TEST_HAS_LANDLOCK_HEADER 1
#endif
#endif
#endif

#if defined(__linux__) && defined(DUNE_TEST_HAS_LANDLOCK_HEADER) && \
  defined(SYS_landlock_create_ruleset) &&                              \
  defined(SYS_landlock_restrict_self) &&                               \
  defined(LANDLOCK_CREATE_RULESET_VERSION) &&                          \
  defined(LANDLOCK_SCOPE_ABSTRACT_UNIX_SOCKET)
#define DUNE_TEST_HAS_LANDLOCK_SCOPE 1
#else
#define DUNE_TEST_HAS_LANDLOCK_SCOPE 0
#endif

CAMLprim value dune_test_landlock_scope_available(value unit)
{
  (void)unit;
#if DUNE_TEST_HAS_LANDLOCK_SCOPE
  int abi = syscall(SYS_landlock_create_ruleset, NULL, 0,
                    LANDLOCK_CREATE_RULESET_VERSION);
  return Val_bool(abi >= 6);
#else
  return Val_false;
#endif
}

CAMLprim value dune_test_landlock_scope_restrict(value unit)
{
  (void)unit;
#if DUNE_TEST_HAS_LANDLOCK_SCOPE
  struct landlock_ruleset_attr attr = {
    .scoped = LANDLOCK_SCOPE_ABSTRACT_UNIX_SOCKET,
  };
  int fd = syscall(SYS_landlock_create_ruleset, &attr, sizeof(attr), 0);
  if (fd < 0) uerror("landlock_create_ruleset", Nothing);
  if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) < 0)
    uerror("prctl", Nothing);
  if (syscall(SYS_landlock_restrict_self, fd, 0) < 0)
    uerror("landlock_restrict_self", Nothing);
  if (close(fd) < 0) uerror("close", Nothing);
  return Val_unit;
#else
  errno = ENOSYS;
  uerror("landlock_restrict_self", Nothing);
#endif
}
