This is a Haskell binding to the [Rubber Band](http://breakfastquay.com/rubberband/)
audio stretching library. Two interfaces are provided:

* The low-level interface, `Sound.RubberBand.Raw`, gives you simple access to the C API.

* The higher-level interface, `Sound.RubberBand.Nice`, offers a garbage-collected
  state object and easier memory management via Haskell arrays instead of pointers.

A sample application `wavstretch` is also provided to demonstrate the high-level
interface. Currently it only works on very small files due to unwise memory usage.
