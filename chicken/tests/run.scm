(require-extension test)

(load-relative "../ehk.lambda.parens.scm")
(load-relative "../ehk.lambda.reduce.scm")

(import ehk.lambda.parens)
(import ehk.lambda.reduce)

(test-begin)
(load-relative "ehk.lambda.parens.scm")
(load-relative "ehk.lambda.reduce.scm")
(test-end)
