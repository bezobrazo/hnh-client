;;;; hnh-client.asd

(asdf:defsystem #:hnh-client
  :serial t
  :name "hnh-client"
  :author "Corey Furmanski"
  :license "GPL"
  :description "A Haven and Hearth experimental client alternative"
  :depends-on (#:usocket #:babel #:ironclad #:CL+SSL #:glop
                         #:flexi-streams #:cl-opengl #:imago)
  :components (;;utils
               (:file "utils/package")
               (:file "utils/vec2n" :depends-on ("utils/package"))

               ;;networking
               (:file "network/package")
               (:file "network/utils" :depends-on ("network/package"))
               (:file "network/message" :depends-on ("network/utils"))
               (:file "network/auth" :depends-on ("network/message"))

               
               ;;graphics
               (:file "gfx/package")
               (:file "gfx/renderer" :depends-on ("gfx/package" "utils/vec2n"))
               (:file "gfx/tex" :depends-on ("gfx/package" "utils/vec2n"))

               ;;ui
               (:file "ui/package")
               

               ;;base
               (:file "package")
               (:file "hnh-client")))

