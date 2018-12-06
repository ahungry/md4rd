# md4rd - Mode for Reddit

[![MELPA](http://melpa.org/packages/md4rd-badge.svg)](http://melpa.org/#/md4rd)

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

```lisp
    (define-key map (kbd "u") 'tree-mode-goto-parent)
    (define-key map (kbd "o") 'md4rd-open)
    (define-key map (kbd "v") 'md4rd-visit)
    (define-key map (kbd "e") 'tree-mode-toggle-expand)
    (define-key map (kbd "E") 'md4rd-widget-expand-all)
    (define-key map (kbd "C") 'md4rd-widget-collapse-all)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "j") 'widget-forward)
    (define-key map (kbd "h") 'backward-button)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "k") 'widget-backward)
    (define-key map (kbd "l") 'forward-button)
    (define-key map (kbd "q") 'kill-current-buffer)
    (define-key map (kbd "r") 'md4rd-reply)
    (define-key map (kbd "u") 'md4rd-upvote)
    (define-key map (kbd "d") 'md4rd-downvote)
    (define-key map (kbd "t") 'md4rd-widget-toggle-line)

```

# Customization

You can subscribe to different reddits by customizing your
rm::subreddits-active variable.

```
(setq md4rd-subs-active '(lisp+Common_Lisp emacs prolog))
```

(A cool feature of reddit is you can view multiple reddits combined with
the `+` concatenation).

## Hooks

If you hate the really wide text / no default indent, try this new
function + hook combo.

```
(add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines)

```

# Screenshots

See the following (shown in `emacs -nw` mode):

Note - this first one is with the indent-all-the-lines hook enabled:
![indented](https://raw.githubusercontent.com/ahungry/md4rd-mode/master/img/md4rd-indented.png)
![subreddit](https://raw.githubusercontent.com/ahungry/md4rd-mode/master/img/redditor-mode-3.png)
![comments](https://raw.githubusercontent.com/ahungry/md4rd-mode/master/img/redditor-mode-2.png)

# License

GPLv3

# Copyright

Matthew Carter <m@ahungry.com>
