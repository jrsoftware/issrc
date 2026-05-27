#define protected IncludeProtectedVar = 'from_include'
#define private IncludePrivateVar = 'from_include_private'
#define public IncludeSeesMainProtected = Defined(MainScopeProtectedVar)
#define public IncludeSeesMainPrivate = Defined(MainScopePrivateVar)
#define public IncludePathFilename = __PATHFILENAME__
