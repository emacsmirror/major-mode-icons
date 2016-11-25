# major-mode-icons

Show major-mode with icons on mode-line.

# Features

- support many programming languages
- support show extra of some modes like: Clojure, Ruby, Python etc.
- only show icon for current selected window, otherwise show mode name.

# Usage

If you use `use-package`:

```elisp
(use-package major-mode-icons
  :ensure t
  :config
  (major-mode-icons-mode 1))
```

If you use a customize mode-line:

```elisp
(setq
 mode-line-end-spaces
 (quote
  ((:eval
    (major-mode-icons/show))
    )))
```
