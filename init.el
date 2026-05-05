;; ; init.el -*- lexical-binding: t; -*-

;;  This file controls what Doom modules are enabled and what order they load
;;  in. Remember to run 'doom sync' after modifying it!

;;  NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;       documentation. There you'll find a "Module Index" link where you'll find
;;       a comprehensive list of Doom's modules and what flags they support.

;;  NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;       'C-c c k' for non-vim users) to view its documentation. This works on
;;       flags as well (those symbols that start with a plus).
;;
;;       Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;       directory (for easy access to its source code).
(setq! use-package-compute-statistics t)


(doom! :input
       ;; chinese
       japanese
       ;; layout                          

       :completion
       ;;  company                        
       ;;  helm                           
       ;;  ido                            
       ;; ivy                             
       (corfu +orderless)
       (vertico +icons) 

       :ui
       ;;  deft                           
       doom                               
       ;; dashboard                          
       ;; doom-quit                       
       ;; (emoji +unicode)                
       ;; fill-column                     
       hl-todo                         
       ;; indent-guides                   
       ;; ligatures                       
       ;;  minimap                        
       ;; modeline                           
       nav-flash                       
       ;; neotree                         
       ophints                         
       ;; (popup +all)                    
       ;; tabs                            
       treemacs                     
       ;; unicode                         
       ;; vc-gutter                       
       ;;  VI-tilde-fringe                
       (window-select +switch-window)     
       ;; workspaces                      
       zen                                
       (smooth-scroll +interpolate)
       ;;  deft                    
       doom                        
       doom-dashboard              
       ;; doom-quit                
       ;; (emoji +unicode)            
       ;; fill-column              
       hl-todo                     
       ;;  indent-guides           
       ;; ligatures                
       ;;  minimap                 
       ;; (modeline +light)        
       ;; nav-flash                   
       ;;  neotree                 
       ophints                     
       ;; (popup +all)             
       ;; tabs                     
       ;; treemacs                 
       ;; unicode                     
       ;; vc-gutter
                                       
       ;;  VI-tilde-fringe         
       (window-select +switch-window)               
       ;; workspaces               
       ;; zen                      

       :editor
       ;; (meow +qwerty +override) 
       ;; (evil +everywhere)              
       ;; file-templates                  
       fold                            
       format                             
       ;; god                             
       ;; lispy                           
       multiple-cursors                   
       (objed +manual)  
       ;; parinfer                        
       rotate-text                     
       ;; snippets                        
       word-wrap                       
       whitespace

       :emacs
       (dired +dirvish) 
       ;; electric                        
       (ibuffer +icons)                            
       undo                               
       ;;  vc                             

       :term
       ;; eshell                          
       ;; shell                           
       ;; term                            
       ;; vterm                           
       eshell                         
       ;; shell                          
       ;; term                           
       ;; vterm                          

       :checkers
       (syntax +childframe)               
       ;; (spell +enchant +flyspell)      
       ;; grammar                         

       :tools
       llm
       ;; ansible
       ;; debugger                        
       ;; direnv
       ;; docker
       ;; editorconfig                    
       ;; ein                             
       (eval +overlay)                    
       ;; gist                            
       lookup                             
       (lsp +eglot)
       (debugger +lsp)                     
       ;; direnv
       ;; docker
       ;; editorconfig             
       ;; ein                      
       ;; gist                     
       lookup                       
       lsp
       biblio
       magit                              
       direnv
       ;; pass                            
       ;; make                            
       pdf                             
       ;; prodigy                         
       ;; rgb                             
       ;; taskrunner                      
       ;; terraform                       
       ;; tmux                            
       ;; upload                          

       :lang
       ;; agda                            
       (cc +lsp +tree-sitter)             
       (clojure +lsp +tree-sitter)        
       common-lisp                        
       ;; coq                             
       ;; crystal                         
       ;; csharp                          
       data                               
       ;; (dart +flutter)                 
       ;; elixir                          
       ;; elm                             
       emacs-lisp                         
       ;; erlang                          
       ;; ess                                
       ;; faust                           
       ;; (fortran +lsp)
       ;; fsharp                          
       ;; fstar                           
       ;; gdscript                        
       ;; (go +lsp)                       
       (haskell +lsp +tree-sitter)        
       hy                                 
       ;; idris                           
       ;; json                               
       ;; (java +meghanada)               
       ;; javascript                      
       (julia +snail +lsp)                 
       ;; kotlin                           
       (latex +cdlatex)                    
       ;; lean
       ;; factor
       ;; ledger                          
       ;; lua                             
       markdown                           
       ;; nim                             
       (nix +lsp)                         
       ;; ocaml                           
       (org +pandoc  +present +noter)            
       ;; php                             
       ;; plantuml                        
       ;; purescript                      
       (python +poetry)  
       ;; qt                              
       ;; racket                          
       ;; raku                            
       ;; rest                            
       ;; rst                             
       ;; (ruby +rails)                   
       (rust +lsp +tree-sitter)           
       ;; scala                           
       scheme                          
       sh                                 
       ;; ledger                   
       ;;  lua                     
       ;;  markdown                
       ;; nim                      
       (nix +lsp +tree-sitter)                      
       ;; ocaml                    
       (org +hugo +crypt)                         
       ;; php                      
       ;; plantuml                 
       ;; purescript               
       (python +poetry)  
       ;; qt                       
       ;; racket                   
       ;; raku                     
       ;; rest                     
       ;; rst                      
       ;; (ruby +rails)            
       (rust +lsp +tree-sitter)    
       ;; scala                    
       ;; scheme                   
       ;; sh                       
       ;; sml
       ;; solidity                        
       ;; swift                           
       ;; terra                           
       ;; web                             
       yaml                               

       :email
       ;; (mu4e +gmail +mbsync +org)
       (notmuch +org +afew)
       ;; (wanderlust +gmail)

       :app
       calendar
       ;; irc                             
       (rss +org)                         
       everywhere

       :config
       ;; literate
       (default +bindings +smartparens))


(add-to-list 'load-path "~/.config/emacs/.local/straight/repos/benchmark-init-el/")

(when init-file-debug
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
