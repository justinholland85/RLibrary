# RLibrary
My personal library of R functions

I like to make functions in R to make my life easier, especially for graphics. I like doing things my own way, and I make functions that suit the way I like to do things. Its all use at own risk, undocumented, and , much will not make any sense to anyone but me, and possibly not even to me.

To set this up you will need to take the following steps:
1) Download the entire library into a single diretory.
2) Open the Start.R file in which you will need to make the following two changes
-2a) Make a copy of one of the "source()" lines, and change the source file to the appropriate one (you will just have to change the direcotry part, but leave the "/Start.R". Do not uncomment this or it will break. This is the line you put at the top of any script in which you want to use the library. Copy this line somewhere handy so you an just copy and paste it into your scripts.
-2b)  Lower down, you will find a definition of Lib.Dir. Point this to the the correct directory, and make sure that just the correct Lib.Dir line is uncommented. You are good to go. 
3) Test it out by running the source line in the R.
