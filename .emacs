;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Init package manager. ;;;
(load-file "~/.emacs.files/init-packages.el")

;;; Load user config ;;;
(load-file "~/.emacs.files/general-config.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#ffffff" "#183691" "#969896" "#a71d5d" "#969896" "#969896" "#795da3" "#969896"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("e7b49145d311e86da34a32a7e1f73497fa365110a813d2ecd8105eaa551969da" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "5057614f7e14de98bbc02200e2fe827ad897696bfd222d1bcab42ad8ff313e20" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "8b4d8679804cdca97f35d1b6ba48627e4d733531c64f7324f764036071af6534" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "930f7841c24772dda4f22291e510dac1d58813b59dcb9f54ad4f1943ea89cdcd" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "d1ede12c09296a84d007ef121cd72061c2c6722fcb02cb50a77d9eae4138a3ff" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "68bf77811b94a9d877f9c974c19bafe5b67b53ed82baf96db79518564177c0fb" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "d14d421ff49120d2c2e0188bcef76008407b3ceff2cfb1d4bdf3684cf3190172" "420689cc31d01fe04b8e3adef87b8838ff52faa169e69ca4e863143ae9f3a9f9" "2f4f50d98073c01038b518066840638455657dc91dd1a225286d573926f36914" "1dacaddeba04ac1d1a2c6c8100952283b63c4b5279f3d58fb76a4f5dd8936a2c" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" default)))
 '(cycle-themes-theme-list
   (quote
    (quote
     (abyss f afternoon ahungry airline-badwolf airline-base16-gui-dark airline-base16-gui-light airline-base16-shell-dark airline-behelit airline-cool airline-dark airline-distinguished airline-doom-molokai airline-doom-one airline-durant airline-gruvbox-dark airline-hybridline airline-kalisi airline-kolor airline-light airline-luna airline-molokai airline-murmur airline-papercolor airline-powerlineish airline-raven airline-serene airline-silver airline-simple airline-sol airline-solarized-alternate-gui airline-solarized-gui airline-ubaryd airline-understated airline-wombat alect-black-alt alect-black alect-dark-alt alect-dark alect-light-alt alect-light almost-mono-black almost-mono-white ample-flat ample-light ample ample-zen anti-zenburn arc-dark arjen-grey atom-dark autumn-light avk-darkblue-white avk-darkblue-yellow avk-daylight badger badwolf basic blackboard bliss borland-blue boron brutalist-dark brutalist bubbleberry busybee caroline chocolate chyla cloud clues colonoscopy aalto-dark aalto-light aliceblue andreas arjen beige-diff beige-eshell bharadwaj-slate bharadwaj billw black-on-gray blippblopp blue-erc blue-eshell blue-gnus blue-mood blue-sea calm-forest charcoal-black clarity classic cobalt comidia dark-blue dark-blue2 dark-erc dark-font-lock dark-gnus dark-green dark-info dark-laptop deep-blue desert digital-ofs1 emacs-21 emacs-nw euphoria feng-shui fischmeister gnome gnome2 goldenrod gray1 gray30 greiner gtk-ide high-contrast hober infodoc jb-simple jedit-grey jonadabian-slate jonadabian jsc-dark jsc-light jsc-light2 julie katester kingsajz late-night lawrence ld-dark lethe marine marquardt matrix midnight mistyday montz oswald parus pierson pok-wob pok-wog railscast ramangalahy raspopovic renegade resolve retro-green retro-orange robin-hood rotor ryerson salmon-diff salmon-font-lock scintilla shaman simple-1 sitaramv-nt sitaramv-solaris snow snowish standard-ediff standard subdued subtle-blue subtle-hacker taming-mr-arneson taylor tty-dark vim-colors whateveryouwant wheat word-perfect xemacs xp commentary constant-light constant creamsody cyberpunk dakrone danneskjold darcula dark-krystal dark-mint darkburn darkmine darkokai darktooth django doneburn doom-Iosvkem doom-challenger-deep doom-city-lights doom-dracula doom-fairy-floss doom-gruvbox doom-molokai doom-moonlight doom-nord-light doom-nord doom-nova doom-one-light doom-one doom-opera-light doom-opera doom-outrun-electric doom-palenight doom-peacock doom-snazzy doom-solarized-dark doom-solarized-light doom-sourcerer doom-spacegrey doom-tomorrow-day doom-tomorrow-night doom-vibrant doom-wilmersdorf dracula eclipse eink espresso exotica eziam-dark eziam-dusk eziam-light faff farmhouse-dark farmhouse-light firecode flatland flatui-dark flatui flucui-dark flucui-light foggy-night forest-blue gandalf github goose gotham grandshell grayscale green-screen greymatters gruvbox-dark-hard gruvbox-dark-medium gruvbox-dark-soft gruvbox-light-hard gruvbox-light-medium gruvbox-light-soft gruvbox habamax hamburg hc-zenburn hemera hemisu-dark hemisu-light hemisu heroku idea-darkula immaterial inkpot intellij inverse-acme iodine ir-black jazz jbeans kaolin-aurora kaolin-breeze kaolin-bubblegum kaolin-dark kaolin-eclipse kaolin-galaxy kaolin-light kaolin-mono-dark kaolin-ocean kaolin-temple kaolin-valley-dark kaolin-valley-light klere kooten kosmos lab-dark lab-light labburn laguna lavender leuven-dark light-soap liso lush madhat2r majapahit-dark majapahit-light mandm material-light material mbo70s melancholy mellow metalheart minimal-black minimal-light minimal moe-dark moe-light moe molokai monochrome-bright monochrome monokai-alt monokai-pro monokai monotropic mood-one mustang mustard naquadah naysayer night-owl nimbus noctilux nofrils-acme nofrils-dark nofrils-light nofrils-sepia nord northcode nova nyx obsidian occidental oceanic oldlace one-dark one-light org-beautify overcast paganini panda paper parchment pastelmac peacock plain plan9 planet poet-dark-monochrome poet-dark poet-monochrome poet professional punpun-dark punpun-light blurb purp-light purp purple-haze railscasts rebecca reverse reykjavik rimero seoul256 seti silkworm slime smyx snazzy base16-3024 base16-apathy base16-ashes base16-atelier-cave-light base16-atelier-cave base16-atelier-dune-light base16-atelier-dune base16-atelier-estuary-light base16-atelier-estuary base16-atelier-forest-light base16-atelier-forest base16-atelier-heath-light base16-atelier-heath base16-atelier-lakeside-light base16-atelier-lakeside base16-atelier-plateau-light base16-atelier-plateau base16-atelier-savanna-light base16-atelier-savanna base16-atelier-seaside-light base16-atelier-seaside base16-atelier-sulphurpool-light base16-atelier-sulphurpool base16-atlas base16-bespin base16-black-metal-bathory base16-black-metal-burzum base16-black-metal-dark-funeral base16-black-metal-gorgoroth base16-black-metal-immortal base16-black-metal-khold base16-black-metal-marduk base16-black-metal-mayhem base16-black-metal-nile base16-black-metal base16-black-metal-venom base16-brewer base16-bright base16-brogrammer base16-brushtrees-dark base16-brushtrees base16-chalk base16-circus base16-classic-dark base16-classic-light base16-codeschool base16-cupcake base16-cupertino base16-darktooth base16-decaf base16-default-dark base16-default-light base16-dracula base16-eighties base16-embers base16-espresso base16-flat base16-framer base16-fruit-soda base16-github base16-google-dark base16-google-light base16-grayscale-dark base16-grayscale-light base16-greenscreen base16-gruvbox-dark-hard base16-gruvbox-dark-medium base16-gruvbox-dark-pale base16-gruvbox-dark-soft base16-gruvbox-light-hard base16-gruvbox-light-medium base16-gruvbox-light-soft base16-harmonic-dark base16-harmonic-light base16-heetch-light base16-heetch base16-helios base16-hopscotch base16-horizon-dark base16-horizon-light base16-horizon-terminal-dark base16-horizon-terminal-light base16-ia-dark base16-ia-light base16-icy base16-irblack base16-isotope base16-macintosh base16-marrakesh base16-materia base16-material-darker base16-material-lighter base16-material-palenight base16-material base16-material-vivid base16-mellow-purple base16-mexico-light base16-mocha base16-monokai base16-nord base16-nova base16-ocean base16-oceanicnext base16-one-light base16-onedark base16-outrun-dark base16-papercolor-dark base16-papercolor-light base16-paraiso base16-phd base16-pico base16-pop base16-porple base16-railscasts base16-rebecca base16-sandcastle base16-seti base16-shapeshifter base16-snazzy base16-solarflare base16-solarized-dark base16-solarized-light base16-spacemacs base16-summerfruit-dark base16-summerfruit-light base16-synth-midnight-dark base16 base16-tomorrow-night-eighties base16-tomorrow-night base16-tomorrow base16-tube base16-twilight base16-unikitty-dark base16-unikitty-light base16-woodland base16-xcode-dusk base16-zenburn soft-morning soft-stone solarized-dark solarized-light solarized soothe sourcerer spacegray spacemacs-dark spacemacs-light srcery subatomic subatomic256 brin dorsey fogus graham granger hickey junio mccarthy odersky ritchie spolsky wilson sunburn sunny-day suscolors tango-2 tango-plus tangotango tao tao-yang tao-yin termbright tommyh toxi tramp twilight ubuntu ujelly underwater waher warm-night white-sand white xresources yoshi zen-and-art zenburn zeno zerodark zweilight adwaita deeper-blue dichromacy leuven light-blue manoj-dark misterioso tango-dark tango tsdh-dark tsdh-light wheatgrass whiteboard wombat))))
 '(fci-rule-color "#969896")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(line-spacing 0.2)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(objed-cursor-color "#ff5555")
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode which-key use-package-ensure-system-package undo-tree terraform-mode smartparens protobuf-mode projectile plantuml-mode panda-theme multiple-cursors monokai-theme lsp-ui json-mode ivy-rich go-guru flycheck feature-mode editorconfig dockerfile-mode delight counsel company-lsp atom-dark-theme)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(red "#ffffff")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#b0cde7")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#969896")
     (40 . "#183691")
     (60 . "#969896")
     (80 . "#969896")
     (100 . "#969896")
     (120 . "#a71d5d")
     (140 . "#969896")
     (160 . "#969896")
     (180 . "#969896")
     (200 . "#969896")
     (220 . "#63a35c")
     (240 . "#0086b3")
     (260 . "#795da3")
     (280 . "#969896")
     (300 . "#0086b3")
     (320 . "#969896")
     (340 . "#a71d5d")
     (360 . "#969896"))))
 '(vc-annotate-very-old-color "#969896")
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
