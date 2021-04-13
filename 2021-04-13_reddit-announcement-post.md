<!-- Time-stamp: <2021-04-13 12:06:22 lockywolf> -->
<!-- 
#+author: lockywolf
#+created: <2021-04-13 Tue 11:59>
#+tags: post, reddit, announcement, scheme, programming, computers, lisp
#+category: programming
-->

# I've hacked a simple script for drawing a file system tree as a graph with graphviz. What's your opinion?

Here's the gitlab link: https://gitlab.com/Lockywolf/scsh-xattr-mindmap

Contrary to the name, it is actually in Chibi, not in scsh. I initially thought that scsh would be better due to more exensive posix support, but it turned out to be that Chibi was good enough.

It is a small-ish (500 lines of code) script to generate a graph from your filesystem tree. It accepts a few options (editable directly at the file top) and duplicates quite a lot of the GNU Find functionality, but I didn't find a way to avoid doing that, as it has to use heuristics in order to prune the tree to a reasonable size.

The resulting image is like this:

[Small, 1Mb](https://gitlab.com/Lockywolf/scsh-xattr-mindmap/-/raw/master/2021-04-13_Slarm64-repo-tree.smaller.png)

[Large, 44Mb](https://gitlab.com/Lockywolf/scsh-xattr-mindmap/-/blob/master/2021-04-13_Slarm64-repo-tree.png)

I plotted the Slarm64 repository tree, just for the demonstration purposes.

The size of the images above is 1x2.5 metres. It's large, but my original goal was to plot my whole file system. The 'size=' parameter is tunable. I think it is reasonable to assume that you need to have at least 4 square centimetres per node, so a graph that large would accommodate about 4000 nodes. In my opinion, 8000 is still possible, but too tight.

With the default settings the script ignores regular files, but traverses symlinks. In theory it also supports hardlinks, but you would need to turn on drawing regular files manually.

I made this script, because I started to feel that I am starting to forget what I have on my hard drive, that has amassed quite a lot of life history for the past 20 years. (Since hard drives became reasonably priced.)

Use-cases and pull requests welcome. One more reason to create this script was to prove that Scheme can be a practical programming language.

Technologically, this code is not terribly advanced, the only trick that may be interesting to programming nerds is having the r7rs module and the main function in the same file (like scsh/scheme48 suggest doing), which requires procedural module analysis. 

I had to glue on a couple of C bindings for sys/xattr.h, those are now available at the Snow-Fort repo. Those are Chibi-specific.

Hope you will enjoy it.
