# md4rd - Mode for Reddit

Read Reddit interactively from within Emacs.

# Usage

Clone the repo, then in your Emacs init file:

```lisp
(add-to-list 'load-path "/path/to/repo")
(require 'md4rd)
(md4rd) ;; to launch it
```

# Customization

You can subscribe to different reddits by customizing your
rm::subreddits-active variable.

```
(setq md4rd-subs-active '(lisp+Common_Lisp emacs prolog)
```

(A cool feature of reddit is you can view multiple reddits combined with
the `+` concatenation).

# Screenshots

See the following (shown in `emacs -nw` mode):

![subreddit](https://raw.githubusercontent.com/ahungry/redditor-mode/master/img/redditor-mode-3.png)
![comments](https://raw.githubusercontent.com/ahungry/redditor-mode/master/img/redditor-mode-2.png)

# todo

- comment / post upvotes/downvotes
- customize to view other reddits + various other UI customizations

# License

GPLv3

# Copyright

Matthew Carter <m@ahungry.com>
