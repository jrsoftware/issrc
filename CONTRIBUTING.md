Inno Setup
==========

Copyright (C) 1997-2025 Jordan Russell. All rights reserved.  
Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.  
For conditions of distribution and use, see LICENSE.TXT.

Contributing issues
-------------------

To report bugs or request new features use the [forum](https://jrsoftware.org/forums.php).

Contributing new code or documentation updates
----------------------------------------------

To contribute new code or documentation updates to Inno Setup clone your own
fork instead of cloning the main Inno Setup repository, commit your work on topic
branches and make pull requests. In detail:

1. [Fork](https://help.github.com/en/github/getting-started-with-github/fork-a-repo) the project.

2. Clone your fork (`git clone https://github.com/<your-username>/issrc.git`).

3. Add an `upstream` remote (`git remote add upstream
   https://github.com/jrsoftware/issrc.git`).

4. Get the latest changes from upstream (e.g. `git pull upstream main`).

5. Create a new topic branch to contain your feature, change, or fix (`git
   checkout -b <topic-branch-name>`).

6. Make sure that your changes adhere to the current coding conventions used
   throughout the project - indentation, accurate comments, etc.

   Do not make mass whitespace or copyright date changes to files nor anything
   similar. The only time is it ok to make such changes is when you already needed
   to change the file to implement your feature, change, or fix.

7. Commit your changes to your topic branch.

8. Push your topic branch up to your fork (`git push origin
   <topic-branch-name>`).

9. [Open a Pull Request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests) with a
    clear title and description. Please include your name and email address if
	  you are contributing a translation.

10. Sign our [Contributor License Agreement](https://gist.github.com/c6a4b3ca5e370d65b49746df848e12a2) when asked.

If you don't have the Git client (`git`), get it from:

https://git-scm.com/

Contributing translations
-------------------------

To contribute translations please see https://jrsoftware.org/files/istrans/send.php.

Please do NOT use GitHub's web editor to edit ANSI translations and create pull
requests. This editor doesn't work since it automatically turns ANSI files into
UTF8 files without a BOM which are not supported before Inno Setup 6.3.
