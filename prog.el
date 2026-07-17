(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX....."
    "XXX....."
    "XXX....."
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX....."
    "XXX....."
    "XXX....."
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."
    "XXXXX..."
    "XXXXXX.."
    "XXXXX..."
    "XXXX...."
    "XXX....."
    "XX......"
    "X......."))
