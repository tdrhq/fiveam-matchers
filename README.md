
# fiveam-matchers

[![tdrhq](https://circleci.com/gh/tdrhq/fiveam-matchers.svg?style=shield)](https://app.circleci.com/pipelines/github/tdrhq/fiveam-matchers?branch=master)


fiveam-matchers is an extensible, composable matchers library for fiveam.

## Examples

It's best to explain this with examples:

```lisp
(test stuff
  (let ((res (+ 1 2)))
   (assert-that res (equal-to 3))))
```

If that fails, you'll get telling you what you what the expected and
actual results are. So far so good.

What about this:

```lisp

(test stuff
  (let ((res (list (+ 1 2) (+ 2 2))))
    (assert-that res
            (has-item 3)))))
```

That makes sense too, it's basically just saying 3 is in the result
list. This is can be done with regular fiveam macros though. Let's try
something harder.

```lisp
(test stuff
   (let ((res (list "car" "foobar")))
     (assert-that res
        (has-item (starts-with "foo")))))
```

This is testing that there's one item that starts with `"foo"`. This
demonstrates the composibility of matchers. `starts-with` is a
matcher, and `has-item` is a matcher. If the test fails, you'll get a
message that looks something like this: `none of the elements matched:
a string that starts with "foo"`.

## Custom matchers

Our current set of matchers is pretty basic, we usually build matchers
as and when we need it. If you need a new matcher, it's pretty easy to
define it (and yes it composes with existing matchers)

The matcher API is inspired by Java's Hamcrest. We think it works
nicely, and it gives us a nice template of nomenclature to copy from.

Let's look at how the `starts-with` is implemented.

First we define a matcher class. The name here isn't important since
it won't be exposed to the end-user of your matcher:

```
(defclass starts-with-matcher (matcher)
  ((prefix :initarg :prefix
           :reader prefix)))
```

Define a method that creates the matcher:

```
(defmethod starts-with ((prefix string))
  (make-instance 'starts-with-matcher
                  :prefix prefix))
```

(A common pattern is defining the method with an argument that's
already a matcher, for instance the `has-item` needs this. The
function `ensure-matcher` is useful to implement this.)

Now we need to do the actual checks:

```
(defmethod matchesp ((matcher starts-with-matcher)
                     actual)
  (and
   (stringp actual)
   (str:starts-with-p (prefix matcher)
                      actual)))
```

The next two methods are not essential. There are defaults
implemented, but if you're building a matcher that will be used by
other people, we encourage implementing them. It'll give the developer
a better error message when the test fails.

Let's create a method to describe the matcher:

```
(defmethod describe-self ((matcher starts-with-matcher))
  `("a string that starts with `" ,(prefix matcher) "`"))
```

For convenience, you don't need to explicitly format the message. You
can return a list of objects that are all appended to each other. When
the test fails this is used to render what was actually expected (as
opposed to the what failed).

Finally, we need to describe why the test failed:

```
(defmethod describe-mismatch ((matcher starts-with-matcher) actual)
  `("expected `" ,actual "` to start with " ,(prefix matcher) ))
```

When you build a new matcher, try to allow the arguments to be
matchers themselves. For instance, if you create a matcher that says
an HTML tag has an attribute value, the attribute value does not have
to be a string: You can imagine a developer might want to check that
`class` attribute matches having a substring.

## API

In the following APIs, most functions that accept a matcher also accept a value, in which case it's treated as `(equal-to value)`.

* `(equal-to val)`: check if result is `equal` to `val`.
* `(is-not {matcher|value})`:  check that result does not match the given matcher
* `(has-all {matchers|value}*)`: check that result matches all of the matchers
* `(has-any {matchers|value}*)`: check if the result matches at least one of the matchers
* `(has-typep 'type)`: check if the result is of type
* `(is-string val)`: check if result is string
* `(contains {matchers|value}*)`: Check if the result is a list for which
  each element matches the corresponding matchers. (Note: the name can
  be confusing. It "reads" like the result should contain that
  element, but that's not what it is. We've kept the name to be
  consistent with Hamcrest).
* `(has-item {matcher|value})`: Check if the result is a list that has an item
  that matches the matcher.
* `(does-not-have-item {matcher|value})`: Check if the result is a list that does not have the specific item. Equivalent to `(every-item (not matcher)`, but with better descriptions.
* `(has-length num)`: Check if the result has `length` equal to the num.
* `(every-item {macher|value})`: Check if every item in the list matches the given matcher.
* `(starts-with prefix)`: Check if the result starts with the given string.

Finally, you can use the matchers using the `assert-that`:
`(assert-that test-expression {matcher}*)`. Note that you can provide
multiple matchers to that expression, it's equivalent.

## Installation

We're waiting on this to be part of the next Quicklisp distribution,
in the meantime you can use quick-patch to install:

```
(ql:quickload :quick-patch)
(quick-patch:register "https://github.com/tdrhq/fiveam-matchers.git" "master")
(quick-patch:checkout-all ".quick-patch/")
```

## Contributing

This isn't a complete set of matchers. Aspirationally, we want to have
a version of all the matchers in Hamcrest, but we'll probably just
build them as we need them. Please send us Pull Requests
for your matchers!

## Author

Arnold Noronha <arnold@screenshotbot.io>

## License

Apache License, Version 2.0
