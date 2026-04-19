// TODO port support

#define EV_MULTIPLICITY 1

#ifdef _MSC_VER
#define HAVE_SYS_SELECT_H 1
#define HAVE_SELECT 1
#else /* _MSC_VER */

#if __has_include(<sys/inotify.h>)
#define HAVE_SYS_INOTIFY_H 1
#endif

#ifdef _WIN32
#define HAVE_SYS_SELECT_H 1
#define HAVE_SELECT 1
#endif

#if __has_include(<sys/select.h>)
#define HAVE_SYS_SELECT_H 1
#define HAVE_SELECT 1
#endif

#if __has_include(<poll.h>)
#define HAVE_POLL_H 1
#define HAVE_POLL 1
#endif

#if __has_include(<sys/epoll.h>)
#define HAVE_SYS_EPOLL_H 1
#define HAVE_EPOLL 1
#define HAVE_EPOLL_CTL 1
#endif

#if __has_include(<sys/timerfd.h>)
#define HAVE_SYS_TIMERFD_H 1
#endif

#if __has_include(<sys/signalfd.h>)
#define HAVE_SYS_SIGNALFD_H 1
#define HAVE_SIGNALFD_H 1
#endif

#if __has_include(<sys/eventfd.h>)
#define HAVE_SYS_SIGNALFD_H 1
#define HAVE_SIGNALFD_H 1
#endif

#if __has_include(<sys/inotify.h>)
#define HAVE_INOTIFY_INIT 1
#endif

#if __has_include(<linux/fs.h>)
#define HAVE_LINUX_FS_H 1
#endif

#if __has_include(<linux/aio_abi.h>)
#define HAVE_AIO_ABI_H 1
#endif

#if __has_include(<sys/event.h>)
#define HAVE_SYS_EVENT_H 1
#endif

#if HAVE_SYS_EVENT_H && (defined(__APPLE__) || defined(__unix__))
#define HAVE_KQUEUE 1
#endif

#if defined(__linux__) || defined(__unix__) || defined(__APPLE__)
#define HAVE_NANOSLEEP 1
#endif

#endif /* _MSC_VER */

#define HAVE_FLOOR 1
