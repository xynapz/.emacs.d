# My Emacs Configuration

This repository contains my personal Emacs configuration, organized as a modular
setup inside `~/.emacs.d/`. It includes UI tweaks, programming language support,
completion frameworks, version control integration, and various quality-of-life
enhancements.

The configuration is split across several files for readability and
maintainability:


---

## ✨ Features

- **Modular configuration** (`lisp/` directory structure)
- **Fast startup** with `early-init.el`
- **Improved UI** (fonts, theme, icons, modeline)
- **Completion stack** (Vertico/Corfu/Company depending on setup)
- **Tree-style navigation** (Treemacs)
- **Git integration** (Magit, Forge if needed)
- **Evil mode** configuration for Vim keybindings
- **Org-mode enhancements**
- **Language support** (C/C++, others via LSP)
- **Vterm integration**
- **Custom snippets**

---

## 📦 Installation

Clone into your home directory:

```bash
git clone https://github.com/xynapz/.emacs.d ~/.emacs.d
```

## 📄 Licensing

This Emacs configuration includes snippets collected from many public sources,
blogs, and documentation. All credit goes to the original authors. Some snippets
may be derived from GPL-licensed Emacs packages or community configurations.

Therefore:

 - All files in this repository containing my original work are released under
the MIT License (see the LICENSE file).

 - Any included snippets or code covered by the GPL are governed by their
respective original licenses.

 - I do not claim ownership of GPL-covered portions; they remain the property
of their original authors.

If you reuse parts of this configuration, please respect the corresponding
licenses and give attribution where appropriate.

## 📝 Notes

This setup is Linux-focused, but should work on macOS and Windows with minor
adjustments.

Some modules assume external tools (e.g., Git, clang, ripgrep, vterm libs).

## 🤝 Contributions

Suggestions, issues, and improvements are welcome! Feel free to fork this repo or
open a pull request.

##📬 Contact

If you have questions about the configuration, feel free to open an issue or
reach out through my website [angeld.me](https://angeld.me "angeld.me").
