(asdf:defsystem :graphics
  :description "Playin' with graphics stuff. I dunno! ;D"
  :author "MegaLoler"
  :serial t
  :components ((:module src
			:serial t
			:components
			((:file "package")
			 (:file "util")
			 (:file "angle")
			 (:file "color")
			 (:file "image")))))
