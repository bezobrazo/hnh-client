;;;; ui/package.lisp
(defpackage #:hnh-ui
  (:use #:cl #:bordeaux-threads)
  (:export
   ;;ui.lisp
   #:*wdgmsg-receiver*
   #:reset-ui

   #:on-key-down
   #:on-key-up
   #:on-char
   #:on-mouse-down
   #:on-mouse-up
   #:on-mouse-move
   #:on-mouse-wheel
   #:draw-ui
   
   #:new-widget
   #:destroy-id
   #:destroy-wdg
   #:get-id
   #:get-wdg
   #:uimsg-1

   ;;widget.lisp
   #:widget
   #:parent #:children #:pos
   #:size #:hfocus #:cfocus 
   #:visible
   
   #:copy-widget
   #:link #:unlink
   #:destroy
   #:raise
   #:set-focusable #:set-visible
   #:give-focus #:take-focus
   #:mouse-move #:mouse-down #:mouse-up #:mouse-wheel
   #:process-kseq #:process-char
   #:key-down #:key-up
   #:draw
   #:tooltip
   #:wdgmsg
   #:uimsg))
