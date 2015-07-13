[![Available on Hackage](https://img.shields.io/hackage/v/rubberband.svg)](http://hackage.haskell.org/package/rubberband) [![Build Status](https://travis-ci.org/mtolly/rubberband.svg?branch=master)](https://travis-ci.org/mtolly/rubberband)

This is a Haskell binding to the [Rubber Band](http://breakfastquay.com/rubberband/)
audio stretching library. Two interfaces are provided:

* The low-level interface, `Sound.RubberBand.Raw`, gives you simple access to the C API.

* The higher-level interface, `Sound.RubberBand.Nice`, offers a garbage-collected
  state object and easier memory management via Haskell vectors instead of pointers.

A sample application `wavstretch` is also provided to demonstrate the high-level
interface.

Now available on Hackage, so you can install with `cabal install rubberband`.
