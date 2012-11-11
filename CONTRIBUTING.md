Inno Setup
==========

Copyright (C) 1997-2012 Jordan Russell. All rights reserved.  
Portions Copyright (C) 2000-2012 Martijn Laan. All rights reserved.  
For conditions of distribution and use, see LICENSE.TXT.

Contributing issues
-------------------

To contribute issues use the GitHub [issue tracker](https://www.github.com/jrsoftware/issrc/issues).
Should only be used for one of the following:

- To report bugs.

- To request new features. Make sure you first discussed the request on the Inno
 Setup [newsgroups](http://www.jrsoftware.org/newsgroups.php)
(web-based interface available).

Please do not use the issue tracker for anything else. Questions should be
posted on the [newsgroups](http://www.jrsoftware.org/newsgroups.php)
(web-based interface available) and not on the issue tracker.

Contributing new code or documentation updates
----------------------------------------------

To contribute new code or documentation updates to Inno Setup clone your own
fork instead of cloning the main Inno Setup repository, commit your work on
topic branches and make pull requests. In detail:

1. [Fork](http://help.github.com/fork-a-repo/) the project.

2. Clone your fork (`git clone git@github.com:<your-username>/issrc.git`).

3. Add an `upstream` remote (`git remote add upstream
   git://github.com/jrsoftware/issrc.git`).

4. Get the latest changes from upstream (e.g. `git pull upstream master`).

5. Create a new topic branch to contain your feature, change, or fix (`git
   checkout -b <topic-branch-name>`).

6. Make sure that your changes adhere to the current coding conventions used
   throughout the project - indentation, accurate comments, etc.

   Do not make mass whitespace, copyright date or $jrsoftware$ tag changes to
   files. The only time is it ok to make such changes is when you already needed
   to change the file to implement your feature, change, or fix.

7. Commit your changes to your topic branch.

8. Push your topic branch up to your fork (`git push origin
   <topic-branch-name>`).

9. [Open a Pull Request](http://help.github.com/send-pull-requests/) with a
   clear title and description.

10. Bask in the eternal glory of being an Inno Setup contributor :)

If you don't have the Git client (`git`), get it from:

http://git-scm.com/