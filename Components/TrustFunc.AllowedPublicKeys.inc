{ Inno Setup build: the second key in this file should be replaced by your
  own and this will happen automatically when using build.bat or build-ce.bat.
  To ignore this change consider using Git's assume-unchanged or skip-worktree
  functionality.
  
  Other builds: if you need only one key you can set AllowedPublicKey2Text To
  an empty string and remove the '//end' marker. }

AllowedPublicKey1Text := '''
format issig-public-key
key-id def0147c3bbc17ab99bf7b7a9c2de1390283f38972152418d7c2a4a7d7131a38
public-x e3e943066aff8f28d2219fd71c9ffff4c8d1aa26bc4225434be67180ab5e242d
public-y e419041c3f54551e86a1c47f387005cd535dfc9d64339b30d37f9a4f7866b650

''';

AllowedPublicKey2Text := '''
format issig-public-key
key-id def020edee3c4835fd54d85eff8b66d4d899b22a777353ca4a114b652e5e7a28
public-x 515dc7d6c16d4a46272ceb3d158c5630a96466ab4d948e72c2029d737c823097
public-y f3c21f6b5156c52a35f6f28016ee3e31a3ded60c325b81fb7b1f88c221081a61

''';//end