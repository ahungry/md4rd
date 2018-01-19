# redditor-mode

Read reddit interactively from within Emacs.

# Usage

Clone the repo, then in your Emacs init file:

```lisp
(add-to-list 'load-path "/path/to/repo")
(require 'redditor-mode)
(redditor-mode) ;; to launch it
```

# Screenshots

See the following (shown in `emacs -nw` mode):

![subreddit](https://raw.githubusercontent.com/ahungry/redditor-mode/master/redditor-mode-1.png)
![comments](https://raw.githubusercontent.com/ahungry/redditor-mode/master/redditor-mode-2.png)

# todo

- multi-reddit (view many at once)
- comment / post upvotes/downvotes
- customize to view other reddits + various other UI customizations

# License

GPLv3

# Copyright

Matthew Carter <m@ahungry.com>
