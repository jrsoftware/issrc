#define protected IncludeProtectedVar = 'from_include'
#define private IncludePrivateVar = 'from_include_private'
#define public IncludeSeesMainProtected = Defined(MainScopeProtectedVar)
#define public IncludeSeesMainPrivate = Defined(MainScopePrivateVar)
#define public IncludePathFilename = __PATHFILENAME__
// This is for the short-circuited sub call scope test
#ifdef CheckIncludeScopeLeak
#if 0 && ScopeLeakSub(1)
#endif
#endif
// The last two lines below each end with the span symbol '\' on purpose, to
// test that a span still pending at end of file is flushed in full and in
// order. Do not add any line after them.
; SPAN_EOF_INCLUDE_PART1 \
SPAN_EOF_INCLUDE_PART2 \