# MarkEx

[![CI](https://github.com/iquiw/markex/actions/workflows/ci.yml/badge.svg)](https://github.com/iquiw/markex/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/iquiw/markex/branch/main/graph/badge.svg?token=Hgq8QKnHsa)](https://codecov.io/gh/iquiw/markex)

Mark things extra.

## Setup

### Install

```console
$ git clone https://github.com/iquiw/markex.git
```

### Configuration

```emacs-lisp
(add-to-list 'load-path "path/to/markex")
(require 'markex)
(keymap-global-set "C-c SPC" 'markex-prefix-command)
```

## Usage

### Mark commands

| Thing        | Key            | Description                            |
| ---          | ---            | ---                                    |
| Space        | <kbd>SPC</kbd> | Mark spaces by regexp.                 |
| Number       | <kbd>#</kbd>   | Mark number by regexp.                 |
| Symbol       | <kbd>'</kbd>   | Mark symbol by thing.                  |
| Filename     | <kbd>/</kbd>   | Mark filename by thing.                |
| IPv4 address | <kbd>4</kbd>   | Mark IPv4 address by regexp.           |
| UUID         | <kbd>U</kbd>   | Mark UUID by thing.                    |
| E-mail       | <kbd>e</kbd>   | Mark E-mail by thing.                  |
| Face         | <kbd>f</kbd>   | Mark same face.                        |
| Line         | <kbd>l</kbd>   | Mark line with trimming spaces around. |
| MAC address  | <kbd>m</kbd>   | Mark MAC address by regexp.            |
| Non Space    | <kbd>n</kbd>   | Mark non-spaces by regexp.             |
| Pair         | <kbd>p</kbd>   | Mark pair by syntax.                   |
| String       | <kbd>s</kbd>   | Mark inside of string by syntax.       |
| URL          | <kbd>u</kbd>   | Mark URL by thing.                     |
| Version      | <kbd>v</kbd>   | Mark version by regexp.                |
| Word         | <kbd>w</kbd>   | Mark word by thing.                    |

### Manipulation commands

| Key          | Description                           |
| ---          | ---                                   |
| <kbd>+</kbd> | Enlarge region.                       |
| <kbd>-</kbd> | Shrink region.                        |
| <kbd>a</kbd> | Add characters on region boundary.    |
| <kbd>c</kbd> | Change characters on region boundary. |
| <kbd>x</kbd> | Delete characters on region boundary. |

### Example

In `javascript-mode` buffer, cursor on "Hello",

```javascript
const foo = "Hello, world";
```

Type <kbd>C-c</kbd><kbd>SPC</kbd><kbd>s</kbd><kbd>C-c</kbd><kbd>SPC</kbd><kbd>c</kbd><kbd>'</kbd>, then

```javascript
const foo = 'Hello, world';
```

## License

Licensed under the GPL 3+.
