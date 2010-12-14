(defmacro try-function (function &rest parameters)
  `(if (functionp (quote ,function))
     (apply (quote ,function) (quote ,parameters))
     'NG))

(provide 'my-utility)