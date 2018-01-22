# md4rd - Mode for Reddit

Read Reddit interactively from within Emacs.

# Why the odd name?

See the Reddit TOS/API guide - anything named after that trademark
(reddit) *must* be prefixed with a 'for', so 'mode for reddit' is
fine, however 'reddit mode' is not.

Hopefully you can help your friends find this package as their reddit
need's dictate :)

https://www.reddit.com/wiki/api

# Installation

Clone the repo, then in your Emacs init file:

```lisp
(add-to-list 'load-path "/path/to/repo")
(require 'md4rd)
(md4rd) ;; to launch it
```

## Signing in / authenticated features

Still a WIP / changes coming soon.

OAuth integration was just finished, and you can invoke the sign in
with:

```
M-x md4rd-login
```

This will pop up a permission grant page on reddit.com, which will
forward you to my site (reddit requires this redirection and it is easier than
running a localhost:port setup for each Emacs user who may or may not
have the port open, as it must match one single redirect_uri on the
app level config being used for OAuth).

After you see the code, simply paste it into your Emacs' minibuffer
prompt, which will have been waiting for you to finish the process.

It will then fetch your oauth access token and refresh tokens.

Bring on the upvotes!

# Usage

If you're signed in, you can highlight a post in the \*subreddit\*
buffer and upvote or downvote as you like.

# Keys

```
u	: md4rd-upvote
d	: md4rd-downvote
o	: md4rd-open (link-in-browser)
RET	: md4rd--fetch-comments
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

# License

GPLv3

# Copyright

Matthew Carter <m@ahungry.com>
