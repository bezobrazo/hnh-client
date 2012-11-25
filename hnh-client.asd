;;;; hnh-client.asd

(asdf:defsystem #:hnh-client
  :serial t
  :name "hnh-client"
  :author "Corey Furmanski"
  :license "GPL"
  :description "A Haven and Hearth experimental client alternative"
  :depends-on (#:usocket #:babel #:ironclad #:CL+SSL #:cl-glfw
                         #:flexi-streams #:imago #:cl-opengl #:cl-glu
                         #:alexandria :bordeaux-threads)
  :components (;;packages
               (:file "utils/package")
               (:file "ui/package")
               (:file "network/package")
               (:file "gfx/package")
               (:file "package")

               ;;commons
               (:file "commons" :depends-on ("package"))
               (:file "config" :depends-on ("package"))
               (:file "error" :depends-on ("package"))
               
               ;;utils
               (:file "utils/vec" :depends-on ("utils/package"))

               ;;graphics
               (:file "gfx/tex" :depends-on ("gfx/package" "utils/vec"))
               (:file "gfx/renderer" :depends-on ("gfx/package" 
                                                  "gfx/tex"
                                                  "utils/vec"))

               ;;ui
               (:file "ui/widget-def" :depends-on ("ui/package"))
               (:file "ui/ui" :depends-on ("ui/widget-def"))
               (:file "ui/widget" :depends-on ("ui/ui"))

               ;;networking
               (:file "network/utils" :depends-on ("network/package"))
               (:file "network/message" :depends-on ("network/utils"))
               (:file "network/auth" :depends-on ("network/message"))
               (:file "network/remote-ui" :depends-on ("network/message"
                                                       "ui/ui"))
               (:file "network/session" :depends-on ("network/remote-ui"))

               ;;base
               (:file "hnh-client" :depends-on ("error" 
                                                "config"
                                                "commons"
                                                "ui/ui"))))

