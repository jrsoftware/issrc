$jrsoftware: ishelp/README.txt,v 1.11 2006/09/28 10:04:25 mlaan Exp $
Original author of this file: Jordan Russell

To compile the Inno Setup help file:

1) Install Microsoft MSXML 4.0 SP2 if you haven't already done so.
   See http://www.microsoft.com/downloads/details.aspx?FamilyID=3144b72b-b4f2-46da-b4b6-c5d7485f2b42&DisplayLang=en

   If you are not sure whether you have MSXML 4.0 SP2 already, check for a
   file named msxml4.dll in your Windows System directory with a version number
   of 4.20.9818.0 (or later).

2) Install Microsoft HTML Help Workshop if you haven't already done so.
   See http://www.microsoft.com/downloads/details.aspx?familyid=00535334-c8a6-452f-9aa0-d597d16580cc&displaylang=en

3) Compile ISHelpGen\ISHelpGen.dpr

4) Run compile.bat

----------------------------------------------------------------

Inno Setup-specific editing guidelines:

- When mentioning something the user would type in a script, e.g. "MinVersion",
  surround it by <tt></tt> so that it's displayed in the Courier New font. This is
  a convention I've used throughout the help file.
    Example: <tt>MinVersion</tt>

