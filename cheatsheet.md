# Emacs Keybindings Cheatsheet

## Navigation & Editing                    | File & Buffer Management              | Window Management
-------------------------------------------|---------------------------------------|----------------------------------
`C-f / C-b` Forward / Back char            | `C-x C-f` Find file (open)            | `C-x 0` Delete current window
`M-f / M-b` Forward / Back word            | `C-x C-s` Save file                   | `C-x 1` Delete other windows
`C-n / C-p` Next / Previous line           | `C-c s` Quick save                    | `C-x 2` Split horizontal
`C-a / C-e` Start / End of line            | `C-x C-w` Write file (save as)        | `C-x 3` Split vertical
`M-< / M->` Start / End of buffer          | `C-x b` Switch buffer (Consult)       | `M-o` Switch to other window
`C-v / M-v` Page down / up                 | `C-x C-b` List buffers (ibuffer)      | `S-<arrows>` Move between windows
`C-l` Center cursor on screen              | `C-x k` Kill buffer                   | `C-c <left>` Winner undo (layout)
`C-s / C-r` Search forward / backward      | `C-c r` Revert buffer (refresh)       | `C-c <right>` Winner redo (layout)

## Search & Navigation (Consult)           | Embark (Context Actions)              | Project Management (Projectile)
-------------------------------------------|---------------------------------------|----------------------------------
`C-s` Search in buffer (consult-line)      | `C-.` Embark act (context menu)       | `C-c p p` Switch project
`M-g g` Go to line                         | `C-;` Embark DWIM                     | `C-c p f` Find file in project
`M-g i` Go to imenu (functions)            | `C-h B` Explore keybindings           | `C-c p s` Switch project (Consult)
`M-g o` Go to outline                      |                                       | `C-c p b` Switch project buffer
`M-s d` Find file in project               | File Tree                             | `C-c p g` Grep in project
`M-s g` Grep in project                    | `C-c t` Toggle dired sidebar          | `C-c p r` Replace in project
`M-s r` Ripgrep in project                 | `f` (in dired) Open file              |
`C-x r b` Jump to bookmark                 | `q` (in dired) Quit dired             |
`M-y` Yank pop (paste history)             |                                       |

## LSP (Eglot)                             | Flycheck (Linting)                    | Org Mode
-------------------------------------------|---------------------------------------|----------------------------------
`M-.` Go to definition                     | `C-c ! l` List errors                 | `C-c a` Org agenda
`M-,` Go back                              | `C-c ! n` Next error                  | `C-c c` Org capture
`M-?` Find references                      | `C-c ! p` Previous error              | `C-c l` Store link
`C-h .` Show documentation                 | `C-c ! v` Verify setup                | `TAB` Cycle visibility
`C-c e r` Rename symbol                    |                                       | `C-c C-c` Execute/toggle
`C-c e a` Code actions                     | Completion (Corfu)                    |
`C-c e f` Format buffer                    | `TAB` Next completion                 |
`C-c e o` Organize imports                 | `S-TAB` Previous completion           |
                                           | `RET` Insert completion               |
                                           | `M-d` Show documentation              |
                                           | `M-l` Show location                   |

## Editing Utilities                       | Help System                           | Quick Actions
-------------------------------------------|---------------------------------------|----------------------------------
`C-/` or `C-_` Undo                        | `C-h k` Describe key                  | `C-c /` Comment line
`C-g` Cancel/Quit                          | `C-h f` Describe function             | `C-c M-/` Comment region
`C-c w` Delete trailing whitespace         | `C-h v` Describe variable             | `C-c = / -` Increase/decrease font
`M-y` Yank pop (paste history)             | `C-h m` Describe mode                 | `C-c 0` Reset font size
                                           | `C-h b` List all keybindings          |
                                           | `C-h a` Apropos (search help)         |

---
**Tips:** • Press `C-h` + any prefix to see completions • Use `M-x` + type to discover commands • `C-g` cancels any operation
